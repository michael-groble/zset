use rand::{Rng, SeedableRng};
use std::cell::RefCell;
use std::collections::Bound;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::mem;
use std::ops::Bound::{Excluded, Included, Unbounded};
use std::ops::RangeBounds;
use std::ptr::NonNull;

#[cfg(test)]
mod tests;

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
    marker: PhantomData<Box<Node<T>>>,
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

pub struct Iter<'a, T: 'a> {
    head: NodePointer<T>,
    tail: NodePointer<T>,
    len: usize,
    marker: PhantomData<&'a Node<T>>,
}

impl<T: std::fmt::Display + std::fmt::Debug + std::cmp::PartialOrd> Debug for SkipList<T> {
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

impl<T: std::default::Default> Default for SkipList<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: std::default::Default> Node<T> {
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
}

impl<T> Node<T> {
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

impl<T> Search<T> {
    fn node(&mut self) -> NodePointer<T> {
        match self.head {
            Some(head) => unsafe { (*head.as_ptr()).levels[0].next },
            None => None,
        }
    }
}

impl<T> SkipList<T> {
    fn pop_head_node(&mut self) -> Option<Box<Node<T>>> {
        let head = Some(self.head);
        let update: [NodePointer<T>; MAX_LEVELS] = [head; MAX_LEVELS];
        let mut search = Search { update, head };
        if let Some(node) = search.node() {
            self.delete_node(&mut search, node);
            return Some(unsafe { Box::from_raw(node.as_ptr()) });
        }
        None
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
                if self.head.as_ref().levels[self.highest_level].next.is_none()
                    && self.highest_level > 0
                {
                    self.highest_level -= 1;
                    continue;
                }
                break;
            }
            self.len -= 1;
        }
    }
}

impl<T: std::default::Default> SkipList<T> {
    pub fn new() -> Self {
        let head = Box::new(Node::head());
        SkipList {
            head: Box::leak(head).into(),
            tail: None,
            len: 0,
            highest_level: 0,
            marker: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            head: (unsafe { self.head.as_ref() }).levels[0].next,
            tail: self.tail,
            len: self.len,
            marker: PhantomData,
        }
    }
}

impl<T: std::cmp::PartialOrd> SkipList<T> {
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
        if let Some(node) = node {
            unsafe {
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
            }
        }

        self.len += 1;
    }

    pub fn delete(&mut self, elt: T, score: f64) -> bool {
        let removed = self.remove_retain(elt, score);
        removed.is_some()
    }

    pub fn update_score(&mut self, elt: T, current_score: f64, new_score: f64) {
        let mut search = self.search(&elt, current_score);
        let node = search
            .node()
            .expect("update_score called but no matching element");
        unsafe {
            let node_ref = node.as_ptr();
            assert!(current_score == (*node_ref).score && elt == (*node_ref).element);

            if ((*node_ref).prev.is_none()
                || (*(*node_ref).prev.unwrap().as_ptr()).score < new_score)
                && ((*node_ref).levels[0].next.is_none()
                    || (*(*node_ref).levels[0].next.unwrap().as_ptr()).score > new_score)
            {
                // new score does not require list to be re-ordered
                (*node_ref).score = new_score;
            } else {
                // new score requires new location
                self.delete_node(&mut search, node);
                let old = Box::from_raw(node.as_ptr());
                self.insert(old.element, new_score);
            }
        }
    }

    pub fn is_in_range<R: RangeBounds<f64>>(&self, range: R) -> bool {
        if range.is_empty() {
            return false;
        }
        unsafe {
            let node = self.tail;
            if node.is_none() || range.starts_after(node.unwrap().as_ref().score) {
                return false;
            }
            let node = self.head.as_ref().levels[0].next;
            if node.is_none() || range.ends_before(node.unwrap().as_ref().score) {
                return false;
            }
        }
        true
    }

    fn remove_retain(&mut self, elt: T, score: f64) -> Option<Box<Node<T>>> {
        let mut search = self.search(&elt, score);
        if let Some(node) = search.node() {
            let node_ref = unsafe { node.as_ref() };
            if node_ref.score == score && node_ref.element == elt {
                self.delete_node(&mut search, node);
                return Some(unsafe { Box::from_raw(node.as_ptr()) });
            }
        }
        None
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
}

impl<T> Drop for SkipList<T> {
    fn drop(&mut self) {
        struct DropGuard<'a, T>(&'a mut SkipList<T>);

        impl<'a, T> Drop for DropGuard<'a, T> {
            fn drop(&mut self) {
                // Continue the same loop we do below. This only runs when a destructor has
                // panicked. If another one panics this will abort.
                while self.0.pop_head_node().is_some() {}
            }
        }

        while let Some(node) = self.pop_head_node() {
            let guard = DropGuard(self);
            drop(node);
            mem::forget(guard);
        }
    }
}

impl<'a, T: std::default::Default + std::cmp::PartialOrd> Iterator for Iter<'a, T> {
    type Item = (f64, &'a T);

    #[inline]
    fn next(&mut self) -> Option<(f64, &'a T)> {
        if self.len == 0 {
            None
        } else {
            self.head.map(|node| unsafe {
                // Need an unbound lifetime to get 'a
                let node = &*node.as_ptr();
                self.len -= 1;
                self.head = node.levels[0].next;
                (node.score, &node.element)
            })
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }

    #[inline]
    fn last(mut self) -> Option<(f64, &'a T)> {
        self.next_back()
    }
}

impl<'a, T: std::default::Default + std::cmp::PartialOrd> DoubleEndedIterator for Iter<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<(f64, &'a T)> {
        if self.len == 0 {
            None
        } else {
            self.tail.map(|node| unsafe {
                // Need an unbound lifetime to get 'a
                let node = &*node.as_ptr();
                self.len -= 1;
                self.tail = node.prev;
                (node.score, &node.element)
            })
        }
    }
}

trait ScoreRange {
    fn is_empty(&self) -> bool;
    fn starts_after(&self, score: f64) -> bool;
    fn ends_before(&self, score: f64) -> bool;
}

impl<T: RangeBounds<f64>> ScoreRange for T {
    fn is_empty(&self) -> bool {
        let (min, minex) = match self.start_bound() {
            Included(start) => (start, false),
            Excluded(start) => (start, true),
            Unbounded => (&f64::NEG_INFINITY, false),
        };
        let (max, maxex) = match self.end_bound() {
            Included(end) => (end, false),
            Excluded(end) => (end, true),
            Unbounded => (&f64::INFINITY, false),
        };
        (min > max) || (min == max && (minex || maxex))
    }

    fn starts_after(&self, score: f64) -> bool {
        match self.start_bound() {
            Included(start) => start > &score,
            Excluded(start) => start >= &score,
            Unbounded => false,
        }
    }

    fn ends_before(&self, score: f64) -> bool {
        match self.end_bound() {
            Included(end) => end < &score,
            Excluded(end) => end <= &score,
            Unbounded => false,
        }
    }
}
