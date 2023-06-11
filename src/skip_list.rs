use rand::{Rng, SeedableRng};
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::ptr::NonNull;

thread_local!(
    static ZSET_RNG: RefCell<rand::rngs::SmallRng> =
        RefCell::new(rand::rngs::SmallRng::from_entropy());
);

const MAX_LEVELS: usize = 32;
const PROBABILITY_U8: u8 = 0x40_u8; // ~ 0.25, 255 * 0.25

fn random_level() -> usize {
    let mut level = 0;
    ZSET_RNG.with(|rng| {
        let mut r = rng.borrow_mut();
        while r.gen::<u8>() < PROBABILITY_U8 {
            level += 1;
        }
    });

    if level >= MAX_LEVELS {
        level = MAX_LEVELS - 1
    }
    level
}

type NodePointer<T> = Option<NonNull<Node<T>>>;

pub struct SkipList<T> {
    head: NonNull<Node<T>>,
    tail: NodePointer<T>,
    len: usize,
    highest_level: usize,
}

struct Level<T> {
    next: NodePointer<T>,
    span: usize,
}

struct Node<T> {
    levels: Vec<Level<T>>,
    prev: NodePointer<T>,
    element: T,
    score: f64,
}

impl<T: std::fmt::Display + std::fmt::Debug> Debug for SkipList<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Skip List, len: {}, levels: {}",
            self.len, self.highest_level
        )?;
        for l in 0..=self.highest_level {
            writeln!(f, "  Level {}", l)?;
            let mut head: NodePointer<T> = Some(self.head);
            while let Some(node) = head {
                unsafe {
                    let node = &*node.as_ptr();
                    let level = &node.levels[l];
                    let prev = if l == 0 {
                        node.prev.map(|p| &(*p.as_ptr()).element)
                    } else {
                        None
                    };
                    writeln!(
                        f,
                        "      height: {} span: {} {:?} {} {} {:?}",
                        node.levels.len(),
                        level.span,
                        head,
                        node.element,
                        node.score,
                        prev
                    )?;
                    head = level.next;
                }
            }
        }
        Ok(())
    }
}

impl<T: std::default::Default + std::cmp::PartialOrd> Default for SkipList<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: std::default::Default + std::cmp::PartialOrd> Node<T> {
    fn new(element: T, score: f64, level: usize) -> Self {
        let mut levels = Vec::with_capacity(level + 1);
        for _ in 0..levels.capacity() {
            levels.push(Level {
                next: None,
                span: 0,
            })
        }
        Node {
            levels,
            prev: None,
            element,
            score,
        }
    }

    fn head() -> Self {
        let mut levels = Vec::with_capacity(MAX_LEVELS);
        for _ in 0..MAX_LEVELS {
            levels.push(Level {
                next: None,
                span: 0,
            })
        }
        let element: T = Default::default();
        Node {
            levels,
            prev: None,
            element,
            score: 0.0,
        }
    }

    fn into_element(self: Box<Self>) -> T {
        self.element
    }
}

struct Insertion<T> {
    update: [NodePointer<T>; MAX_LEVELS],
    rank: [usize; MAX_LEVELS],
}

struct Search<T> {
    update: [NodePointer<T>; MAX_LEVELS],
    head: NodePointer<T>,
}

enum RemoveBehavior {
    Retain,
    Free
}

enum RemoveResult<T> {
    NotFound,
    Freed,
    Retained(Box<Node<T>>)
}

impl<T: std::default::Default + std::cmp::PartialOrd> SkipList<T> {
    pub fn new() -> Self {
        let head = Box::new(Node::head());
        SkipList {
            head: Box::leak(head).into(),
            tail: None,
            len: 0,
            highest_level: 0,
        }
    }

    pub fn insert(&mut self, elt: T, score: f64) {
        let mut insertion = self.insertion(&elt, score);
        let level = random_level();
        if level > self.highest_level {
            for l in self.highest_level + 1..=level {
                insertion.rank[l] = 0;
                insertion.update[l] = Some(self.head);
                unsafe {
                    (*self.head.as_ptr()).levels[l].span = self.len;
                };
            }
            self.highest_level = level;
        }
        let node = Box::new(Node::new(elt, score, level));
        let node: NodePointer<T> = Some(Box::leak(node).into());
        node.map(|node| unsafe {
            for (l, mut insert_level) in (*node.as_ptr()).levels.iter_mut().enumerate() {
                if let Some(update_level) = insertion.update[l] {
                    let update_level = &mut (*update_level.as_ptr()).levels[l];
                    insert_level.next = update_level.next;
                    update_level.next = Some(NonNull::new_unchecked(node.as_ptr()));
                    let delta_span = insertion.rank[0] - insertion.rank[l];
                    insert_level.span = update_level.span - delta_span;
                    update_level.span = delta_span + 1;
                }
            }
            for l in (level + 1)..=self.highest_level {
                (*insertion.update[l].unwrap().as_ptr()).levels[l].span += 1;
            }
            (*node.as_ptr()).prev = if insertion.update[0] == Some(self.head) {
                None
            } else {
                insertion.update[0]
            };

            if let Some(next) = (*node.as_ptr()).levels[0].next {
                (*next.as_ptr()).prev = Some(NonNull::new_unchecked(node.as_ptr()));
            } else {
                self.tail = Some(NonNull::new_unchecked(node.as_ptr()));
            }
        });

        self.len += 1;
    }

    pub fn delete(&mut self, elt: T, score: f64) {
        self.remove(elt, score, RemoveBehavior::Free);
    }

    fn remove(&mut self, elt: T, score: f64, behavior: RemoveBehavior) -> RemoveResult<T> {
        let mut search = self.search(&elt, score);
        let node = unsafe { (*search.head.unwrap().as_ptr()).levels[0].next };
        if let Some(node) = node {
            let node_ref = unsafe { node.as_ref() };
            if node_ref.score == score && node_ref.element == elt {
                self.delete_node(&mut search, node);
                let node = unsafe { Box::from_raw(node.as_ptr()) };
                match behavior {
                    RemoveBehavior::Retain => {
                        return RemoveResult::Retained(node)
                    }
                    RemoveBehavior::Free => {
                        return RemoveResult::Freed
                    }
                }
            }
        }
        RemoveResult::NotFound
        //     if (x && score == x->score && sdscmp(x->ele,ele) == 0) {
        //         zslDeleteNode(zsl, x, update);
        //         if (!node)
        //             zslFreeNode(x);
        //         else
        //             *node = x;
        //         return 1;
        //     }
        //     return 0; /* not found */
    }

    fn delete_node(&mut self, search: &mut Search<T>, node: NonNull<Node<T>>) {
        unsafe {
            for l in (0..=self.highest_level).rev() {
                if let Some(update_level) = search.update[l] {
                    let update_level = &mut (*update_level.as_ptr()).levels[l];
                    if update_level.next == Some(node) {
                        update_level.span += (*node.as_ptr()).levels[l].span;
                        update_level.next = (*node.as_ptr()).levels[l].next;
                    }
                    update_level.span -= 1;
                }
            }
            if let Some(next) = (*node.as_ptr()).levels[0].next {
                (*next.as_ptr()).prev = (*node.as_ptr()).prev;
            } else {
                self.tail = (*node.as_ptr()).prev;
            }
            loop {
                if self.head.as_ref().levels[self.highest_level].next.is_none() {
                    self.highest_level -= 1;
                    continue;
                }
                break;
            }
            self.len -= 1;
        }
    }

    fn insertion(&self, elt: &T, score: f64) -> Insertion<T> {
        let mut update: [NodePointer<T>; MAX_LEVELS] = [None; MAX_LEVELS];
        let mut rank: [usize; MAX_LEVELS] = [0; MAX_LEVELS];
        let mut head: NodePointer<T> = Some(self.head);
        for l in (0..=self.highest_level).rev() {
            rank[l] = if l == self.highest_level {
                0
            } else {
                rank[l + 1]
            };
            loop {
                let level_insert = head.unwrap();
                let level_insert = unsafe { level_insert.as_ref() };
                let level = &level_insert.levels[l];
                if let Some(level_insert) = level.next {
                    let level_insert = unsafe { level_insert.as_ref() };
                    if level_insert.score < score
                        || level_insert.score == score && level_insert.element < *elt
                    {
                        rank[l] += level.span;
                        head = level.next;
                        continue;
                    }
                }
                break;
            }

            update[l] = head;
        }
        Insertion { update, rank }
    }

    fn search(&self, elt: &T, score: f64) -> Search<T> {
        let mut update: [NodePointer<T>; MAX_LEVELS] = [None; MAX_LEVELS];
        let mut head: NodePointer<T> = Some(self.head);
        for l in (0..=self.highest_level).rev() {
            loop {
                let level_insert = head.unwrap();
                let level_insert = unsafe { level_insert.as_ref() };
                let level = &level_insert.levels[l];
                if let Some(level_insert) = level.next {
                    let level_insert = unsafe { level_insert.as_ref() };
                    if level_insert.score < score
                        || level_insert.score == score && level_insert.element < *elt
                    {
                        head = level.next;
                        continue;
                    }
                }
                break;
            }

            update[l] = head;
        }
        Search { update, head }
    }

    pub fn len(&self) -> usize {
        self.len
    }
}
