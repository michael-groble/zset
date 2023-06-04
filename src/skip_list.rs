use rand::{Rng, SeedableRng};
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
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
    head: NodePointer<T>,
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
            let mut head: NodePointer<T> = self.head;
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
                        "      height: {} span: {} {:?} {} {:?}",
                        node.levels.len(),
                        level.span,
                        head,
                        node.element,
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

pub struct Iter<'a, T: 'a> {
    head: NodePointer<T>,
    tail: NodePointer<T>,
    len: usize,
    marker: PhantomData<&'a Node<T>>,
}

pub struct IterMut<'a, T: 'a> {
    head: NodePointer<T>,
    tail: NodePointer<T>,
    len: usize,
    marker: PhantomData<&'a mut Node<T>>,
}

impl<T: std::default::Default + std::cmp::PartialOrd> SkipList<T> {
    pub fn new() -> Self {
        let head = Box::new(Node::head());
        SkipList {
            head: Some(Box::leak(head).into()),
            tail: None,
            len: 0,
            highest_level: 0,
        }
    }

    pub fn insert(&mut self, elt: T, score: f64) {
        let mut update: [NodePointer<T>; MAX_LEVELS] = [None; MAX_LEVELS];
        let mut rank: [usize; MAX_LEVELS] = [0; MAX_LEVELS];
        let mut insert: NodePointer<T> = self.head;
        for l in (0..=self.highest_level).rev() {
            rank[l] = if l == self.highest_level {
                0
            } else {
                rank[l + 1]
            };
            unsafe {
                loop {
                    if let Some(level_insert) = insert {
                        // Need an unbound lifetime to get 'a
                        let level_insert = &*level_insert.as_ptr();
                        let level = &level_insert.levels[l];
                        if let Some(level_insert) = level.next {
                            let level_insert = &*level_insert.as_ptr();
                            if level_insert.score < score
                                || level_insert.score == score && level_insert.element < elt
                            {
                                rank[l] += level.span;
                                insert = level.next;
                                continue;
                            }
                        }
                    }
                    break;
                }
            }
            update[l] = insert;
        }
        let level = random_level();
        if level > self.highest_level {
            for l in self.highest_level + 1..=level {
                rank[l] = 0;
                update[l] = self.head;
                self.head.map(|node| unsafe {
                    (*node.as_ptr()).levels[l].span = self.len;
                });
            }
            self.highest_level = level;
        }
        let node = Box::new(Node::new(elt, score, level));
        let node: NodePointer<T> = Some(Box::leak(node).into());
        node.map(|node| unsafe {
            for (l, mut insert_level) in (*node.as_ptr()).levels.iter_mut().enumerate() {
                if let Some(update_level) = update[l] {
                    let update_level = &mut (*update_level.as_ptr()).levels[l];
                    insert_level.next = update_level.next;
                    update_level.next = Some(NonNull::new_unchecked(node.as_ptr()));
                    let delta_span = rank[0] - rank[l];
                    insert_level.span = update_level.span - delta_span;
                    update_level.span = delta_span + 1;
                }
            }
            for l in (level + 1)..=self.highest_level {
                (*update[l].unwrap().as_ptr()).levels[l].span += 1;
            }
            (*node.as_ptr()).prev = if update[0] == self.head {
                None
            } else {
                update[0]
            };


            if let Some(next) = (*node.as_ptr()).levels[0].next {
                (*next.as_ptr()).prev = Some(NonNull::new_unchecked(node.as_ptr()));
            } else {
                self.tail = Some(NonNull::new_unchecked(node.as_ptr()));
            }
        });

        self.len += 1;
    }

    pub fn push_back(&mut self, elt: T, score: f64) {
        self.push_back_node(Box::new(Node::new(elt, score, 0)));
    }

    fn push_back_node(&mut self, mut node: Box<Node<T>>) {
        // This method takes care not to create mutable references to whole nodes,
        // to maintain validity of aliasing pointers into `element`.
        unsafe {
            node.levels[0].next = None;
            node.prev = self.tail;
            let node = Some(Box::leak(node).into());

            match self.tail {
                None => self.head = node,
                // Not creating new mutable (unique!) references overlapping `element`.
                Some(tail) => (*tail.as_ptr()).levels[0].next = node,
            }

            self.tail = node;
            self.len += 1;
        }
    }
    fn pop_front_node(&mut self) -> Option<Box<Node<T>>> {
        // This method takes care not to create mutable references to whole nodes,
        // to maintain validity of aliasing pointers into `element`.
        self.head.map(|node| unsafe {
            let node = Box::from_raw(node.as_ptr());
            self.head = node.levels[0].next;

            match self.head {
                None => self.tail = None,
                // Not creating new mutable (unique!) references overlapping `element`.
                Some(head) => (*head.as_ptr()).prev = None,
            }

            self.len -= 1;
            node
        })
    }

    fn pop_back_node(&mut self) -> Option<Box<Node<T>>> {
        // This method takes care not to create mutable references to whole nodes,
        // to maintain validity of aliasing pointers into `element`.
        self.tail.map(|node| unsafe {
            let node = Box::from_raw(node.as_ptr());
            self.tail = node.prev;

            match self.tail {
                None => self.head = None,
                // Not creating new mutable (unique!) references overlapping `element`.
                Some(tail) => (*tail.as_ptr()).levels[0].next = None,
            }

            self.len -= 1;
            node
        })
    }

    pub fn pop_front(&mut self) -> Option<T> {
        self.pop_front_node().map(Node::into_element)
    }
    pub fn pop_back(&mut self) -> Option<T> {
        self.pop_back_node().map(Node::into_element)
    }
    pub fn len(&self) -> usize {
        self.len
    }
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            head: self.head,
            tail: self.tail,
            len: self.len,
            marker: PhantomData,
        }
    }

    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut {
            head: self.head,
            tail: self.tail,
            len: self.len,
            marker: PhantomData,
        }
    }
}

pub struct IntoIter<T> {
    list: SkipList<T>,
}

impl<T: std::default::Default + std::cmp::PartialOrd> IntoIterator for SkipList<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    /// Consumes the list into an iterator yielding elements by value.
    #[inline]
    fn into_iter(self) -> IntoIter<T> {
        IntoIter { list: self }
    }
}

impl<'a, T: std::default::Default + std::cmp::PartialOrd> IntoIterator for &'a SkipList<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Iter<'a, T> {
        self.iter()
    }
}

impl<'a, T: std::default::Default + std::cmp::PartialOrd> IntoIterator for &'a mut SkipList<T> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>;

    fn into_iter(self) -> IterMut<'a, T> {
        self.iter_mut()
    }
}

impl<'a, T: std::default::Default + std::cmp::PartialOrd> Iterator for Iter<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<&'a T> {
        if self.len == 0 {
            None
        } else {
            self.head.map(|node| unsafe {
                // Need an unbound lifetime to get 'a
                let node = &*node.as_ptr();
                self.len -= 1;
                self.head = node.levels[0].next;
                &node.element
            })
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }

    #[inline]
    fn last(mut self) -> Option<&'a T> {
        self.next_back()
    }
}

impl<'a, T: std::default::Default + std::cmp::PartialOrd> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    #[inline]
    fn next(&mut self) -> Option<&'a mut T> {
        if self.len == 0 {
            None
        } else {
            self.head.map(|node| unsafe {
                // Need an unbound lifetime to get 'a
                let node = &mut *node.as_ptr();
                self.len -= 1;
                self.head = node.levels[0].next;
                &mut node.element
            })
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }

    #[inline]
    fn last(mut self) -> Option<&'a mut T> {
        self.next_back()
    }
}

impl<T: std::default::Default + std::cmp::PartialOrd> Iterator for IntoIter<T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
        self.list.pop_front()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.list.len, Some(self.list.len))
    }
}

impl<T: std::default::Default + std::cmp::PartialOrd> DoubleEndedIterator for IntoIter<T> {
    #[inline]
    fn next_back(&mut self) -> Option<T> {
        self.list.pop_back()
    }
}

impl<'a, T: std::default::Default + std::cmp::PartialOrd> DoubleEndedIterator for IterMut<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a mut T> {
        if self.len == 0 {
            None
        } else {
            self.tail.map(|node| unsafe {
                // Need an unbound lifetime to get 'a
                let node = &mut *node.as_ptr();
                self.len -= 1;
                self.tail = node.prev;
                &mut node.element
            })
        }
    }
}

impl<'a, T: std::default::Default + std::cmp::PartialOrd> DoubleEndedIterator for Iter<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a T> {
        if self.len == 0 {
            None
        } else {
            self.tail.map(|node| unsafe {
                // Need an unbound lifetime to get 'a
                let node = &*node.as_ptr();
                self.len -= 1;
                self.tail = node.prev;
                &node.element
            })
        }
    }
}
