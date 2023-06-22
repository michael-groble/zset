use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::mem;
use std::ops::{Bound, RangeBounds};
use std::ptr::NonNull;

use rand::{Rng, SeedableRng};

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

type NodePointer<T, S> = Option<NonNull<Node<T, S>>>;

pub struct SkipList<T, S> {
    head: NonNull<Node<T, S>>,
    tail: NodePointer<T, S>,
    len: usize,
    highest_level: usize,
    marker: PhantomData<Box<Node<T, S>>>,
}

struct Level<T, S> {
    next: NodePointer<T, S>,
    span: usize,
}

pub struct Node<T, S> {
    levels: Vec<Level<T, S>>,
    prev: NodePointer<T, S>,
    element: Option<T>, // only None in head
    score: Option<S>,   // only None in head
}

pub struct Iter<'a, T: 'a, S> {
    head: NodePointer<T, S>,
    tail: NodePointer<T, S>,
    len: usize,
    marker: PhantomData<&'a Node<T, S>>,
}

impl<T: std::fmt::Display + std::fmt::Debug, S: std::fmt::Display> Debug for SkipList<T, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Skip List, len: {}, levels: {}",
            self.len, self.highest_level
        )?;
        for l in 0..=self.highest_level {
            writeln!(f, "  Level {}", l)?;
            let mut head: NodePointer<T, S> = Some(self.head);
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
                        node.element(),
                        node.score(),
                        prev
                    )?;
                    head = level.next;
                }
            }
        }
        Ok(())
    }
}

impl<T: std::default::Default, S: std::default::Default> Default for SkipList<T, S> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, S> Node<T, S> {
    fn head() -> Self {
        let mut levels = Vec::with_capacity(MAX_LEVELS);
        for _ in 0..MAX_LEVELS {
            levels.push(Level {
                next: None,
                span: 0,
            })
        }
        Node {
            levels,
            prev: None,
            element: None,
            score: None,
        }
    }

    pub fn element(&self) -> &T {
        self.element.as_ref().unwrap()
    }

    pub fn score(&self) -> &S {
        self.score.as_ref().unwrap()
    }
}

impl<T, S> Node<T, S> {
    fn new(element: T, score: S, level: usize) -> Self {
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
            element: Some(element),
            score: Some(score),
        }
    }

    fn into_element(self: Box<Self>) -> T {
        self.element.unwrap()
    }
}

struct Insertion<T, S> {
    update: [NodePointer<T, S>; MAX_LEVELS],
    rank: [usize; MAX_LEVELS],
}

struct Search<T, S> {
    update: [NodePointer<T, S>; MAX_LEVELS],
    head: NodePointer<T, S>,
}

impl<T, S> Search<T, S> {
    fn node(&mut self) -> NodePointer<T, S> {
        match self.head {
            Some(head) => unsafe { (*head.as_ptr()).levels[0].next },
            None => None,
        }
    }
}

impl<T, S> SkipList<T, S> {
    fn delete_node(&mut self, search: &mut Search<T, S>, node: NonNull<Node<T, S>>) {
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
            while self.head.as_ref().levels[self.highest_level].next.is_none()
                && self.highest_level > 0
            {
                self.highest_level -= 1;
            }
            self.len -= 1;
        }
    }

    fn pop_head_node(&mut self) -> Option<Box<Node<T, S>>> {
        let head = Some(self.head);
        let update: [NodePointer<T, S>; MAX_LEVELS] = [head; MAX_LEVELS];
        let mut search = Search { update, head };
        if let Some(node) = search.node() {
            self.delete_node(&mut search, node);
            return Some(unsafe { Box::from_raw(node.as_ptr()) });
        }
        None
    }

    pub fn iter(&self) -> Iter<'_, T, S> {
        Iter {
            head: (unsafe { self.head.as_ref() }).levels[0].next,
            tail: self.tail,
            len: self.len,
            marker: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

impl<T, S> SkipList<T, S>
where
    S: PartialEq + PartialOrd,
{
    pub fn is_in_range<R: RangeBounds<S>>(&self, range: R) -> bool {
        if range.is_empty() {
            return false;
        }
        unsafe {
            let node = self.tail;
            if node.is_none() || range.starts_after(node.unwrap().as_ref().score()) {
                return false;
            }
            let node = self.head.as_ref().levels[0].next;
            if node.is_none() || range.ends_before(node.unwrap().as_ref().score()) {
                return false;
            }
        }
        true
    }

    pub fn first_in_range<R: RangeBounds<S> + Clone>(
        &self,
        range: R,
    ) -> Option<(NonNull<Node<T, S>>, usize)> {
        if !self.is_in_range(range.clone()) {
            return None;
        }
        let mut node = Some(self.head);
        let mut rank: usize = 0;

        for l in (0..=self.highest_level).rev() {
            while let Some(level) =
                unsafe { Some(&node.unwrap().as_ref().levels[l]) }.filter(|level| {
                    level.next.map_or(false, |next| {
                        let next = unsafe { next.as_ref() };
                        range.starts_after(next.score())
                    })
                })
            {
                rank += level.span;
                node = level.next;
            }
        }
        node = unsafe { Some((node.unwrap().as_ref()).levels[0].next.unwrap()) };
        node.filter(|&node| !range.ends_before(unsafe { node.as_ref().score() }))
            .map(|node| (node, rank))
    }

    pub fn last_in_range<R: RangeBounds<S> + Clone>(
        &self,
        range: R,
    ) -> Option<(NonNull<Node<T, S>>, usize)> {
        if !self.is_in_range(range.clone()) {
            return None;
        }
        let mut node = self.head;
        let mut rank: usize = 0;

        for l in (0..=self.highest_level).rev() {
            while let Some(level) = unsafe { Some(&node.as_ref().levels[l]) }.filter(|level| {
                level.next.map_or(false, |next| {
                    let next = unsafe { next.as_ref() };
                    !range.ends_before(next.score())
                })
            }) {
                rank += level.span;
                node = level.next.unwrap();
            }
        }

        if range.starts_after(unsafe { node.as_ref().score() }) {
            None
        } else {
            Some((node, rank - 1))
        }
    }

    pub fn delete_range_by_score<R: RangeBounds<S>>(&mut self, range: R) -> usize {
        let mut update: [NodePointer<T, S>; MAX_LEVELS] = [None; MAX_LEVELS];
        let mut head: NodePointer<T, S> = Some(self.head);
        for l in (0..=self.highest_level).rev() {
            while let Some(next) = head
                .and_then(|head| unsafe { head.as_ref().levels[l].next })
                .filter(|next| range.starts_after(unsafe { next.as_ref().score() }))
            {
                head = Some(next)
            }

            update[l] = head;
        }
        head = unsafe { head.unwrap().as_ref().levels[0].next };
        let mut removed = 0;
        while head.map_or(false, |node| {
            !range.ends_before(unsafe { node.as_ref().score() })
        }) {
            let node = head.unwrap();
            let next = unsafe { node.as_ref().levels[0].next };
            let mut search = Search { update, head };
            self.delete_node(&mut search, node);
            unsafe { Box::from_raw(node.as_ptr()) };
            removed += 1;
            head = next;
        }
        removed
    }
}

impl<T, S> SkipList<T, S> {
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
}

impl<T: std::cmp::PartialOrd, S: std::cmp::PartialOrd + Clone + Copy> SkipList<T, S> {
    pub fn insert(&mut self, elt: T, score: S) {
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
        let node: NodePointer<T, S> = Some(Box::leak(node).into());
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
                (*node.as_ptr()).prev = insertion.update[0].filter(|update| update != &self.head);

                if let Some(next) = (*node.as_ptr()).levels[0].next {
                    (*next.as_ptr()).prev = Some(NonNull::new_unchecked(node.as_ptr()));
                } else {
                    self.tail = Some(NonNull::new_unchecked(node.as_ptr()));
                }
            }
        }

        self.len += 1;
    }

    pub fn delete(&mut self, elt: T, score: S) -> Option<T> {
        self.remove_retain(elt, score).map(|n| n.element.unwrap())
    }

    pub fn update_score(&mut self, elt: T, current_score: S, new_score: S) {
        let mut search = self.search(&elt, current_score);
        let node = search
            .node()
            .expect("update_score called but no matching element");
        unsafe {
            let node_ref = node.as_ptr();
            assert!(
                current_score == *(*node_ref).score() && elt == *(*node_ref).element(),
                "found node doesn't match"
            );

            if ((*node_ref).prev.is_none()
                || *(*(*node_ref).prev.unwrap().as_ptr()).score() < new_score)
                && ((*node_ref).levels[0].next.is_none()
                    || *(*(*node_ref).levels[0].next.unwrap().as_ptr()).score() > new_score)
            {
                // new score does not require list to be re-ordered
                (*node_ref).score = Some(new_score);
            } else {
                // new score requires new location
                self.delete_node(&mut search, node);
                let old = Box::from_raw(node.as_ptr());
                self.insert(old.element.unwrap(), new_score);
            }
        }
    }

    fn remove_retain(&mut self, elt: T, score: S) -> Option<Box<Node<T, S>>> {
        let mut search = self.search(&elt, score);
        if let Some(node) = search.node() {
            let node_ref = unsafe { node.as_ref() };
            if *node_ref.score() == score && *node_ref.element() == elt {
                self.delete_node(&mut search, node);
                return Some(unsafe { Box::from_raw(node.as_ptr()) });
            }
        }
        None
    }

    fn insertion(&self, elt: &T, score: S) -> Insertion<T, S> {
        let mut update: [NodePointer<T, S>; MAX_LEVELS] = [None; MAX_LEVELS];
        let mut rank: [usize; MAX_LEVELS] = [0; MAX_LEVELS];
        let mut head: NodePointer<T, S> = Some(self.head);
        for l in (0..=self.highest_level).rev() {
            rank[l] = if l == self.highest_level {
                0
            } else {
                rank[l + 1]
            };
            while let Some(level) =
                unsafe { Some(&head.unwrap().as_ref().levels[l]) }.filter(|level| {
                    level.next.map_or(false, |next| {
                        let next = unsafe { next.as_ref() };
                        *next.score() < score || *next.score() == score && *next.element() < *elt
                    })
                })
            {
                rank[l] += level.span;
                head = level.next;
            }
            update[l] = head;
        }
        Insertion { update, rank }
    }

    fn search(&self, elt: &T, score: S) -> Search<T, S> {
        let mut update: [NodePointer<T, S>; MAX_LEVELS] = [None; MAX_LEVELS];
        let mut head: NodePointer<T, S> = Some(self.head);

        for l in (0..=self.highest_level).rev() {
            while let Some(next) =
                unsafe { &head.unwrap().as_ref().levels[l].next }.filter(|next| {
                    let next = unsafe { next.as_ref() };
                    *next.score() < score || *next.score() == score && *next.element() < *elt
                })
            {
                head = Some(next);
            }

            update[l] = head;
        }
        Search { update, head }
    }
}

impl<T, S> Drop for SkipList<T, S> {
    fn drop(&mut self) {
        struct DropGuard<'a, T, S>(&'a mut SkipList<T, S>);

        impl<'a, T, S> Drop for DropGuard<'a, T, S> {
            fn drop(&mut self) {
                // Continue the same loop we do below. This only runs when a destructor has
                // panicked. If another one panics this will abort.
                while self.0.pop_head_node().is_some() {}
                unsafe { drop(Box::from_raw(self.0.head.as_ptr())) }
            }
        }

        while let Some(node) = self.pop_head_node() {
            let guard = DropGuard(self);
            drop(node);
            mem::forget(guard);
        }
        unsafe { drop(Box::from_raw(self.head.as_ptr())) }
    }
}

impl<'a, T, S: Clone + Copy> Iterator for Iter<'a, T, S> {
    type Item = (&'a T, S);

    #[inline]
    fn next(&mut self) -> Option<(&'a T, S)> {
        if self.len == 0 {
            None
        } else {
            self.head.map(|node| unsafe {
                // Need an unbound lifetime to get 'a
                let node = &*node.as_ptr();
                self.len -= 1;
                self.head = node.levels[0].next;
                (node.element(), *node.score())
            })
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }

    #[inline]
    fn last(mut self) -> Option<(&'a T, S)> {
        self.next_back()
    }
}

impl<'a, T, S: Clone + Copy> DoubleEndedIterator for Iter<'a, T, S> {
    #[inline]
    fn next_back(&mut self) -> Option<(&'a T, S)> {
        if self.len == 0 {
            None
        } else {
            self.tail.map(|node| unsafe {
                // Need an unbound lifetime to get 'a
                let node = &*node.as_ptr();
                self.len -= 1;
                self.tail = node.prev;
                (node.element(), *node.score())
            })
        }
    }
}

trait Ranged<V> {
    fn is_empty(&self) -> bool;
    fn starts_after(&self, value: &V) -> bool;
    fn ends_before(&self, value: &V) -> bool;
}

impl<T: RangeBounds<V>, V> Ranged<V> for T
where
    V: PartialEq + PartialOrd,
{
    fn is_empty(&self) -> bool {
        if Bound::Unbounded == self.start_bound() || Bound::Unbounded == self.end_bound() {
            false
        } else {
            let (min, minex) = match self.start_bound() {
                Bound::Included(start) => (start, false),
                Bound::Excluded(start) => (start, true),
                Bound::Unbounded => panic!(),
            };
            let (max, maxex) = match self.end_bound() {
                Bound::Included(end) => (end, false),
                Bound::Excluded(end) => (end, true),
                Bound::Unbounded => panic!(),
            };
            (min > max) || (min == max && (minex || maxex))
        }
    }

    fn starts_after(&self, value: &V) -> bool {
        match self.start_bound() {
            Bound::Included(start) => start > value,
            Bound::Excluded(start) => start >= value,
            Bound::Unbounded => false,
        }
    }

    fn ends_before(&self, value: &V) -> bool {
        match self.end_bound() {
            Bound::Included(end) => end < value,
            Bound::Excluded(end) => end <= value,
            Bound::Unbounded => false,
        }
    }
}
