use std::borrow::Borrow;
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::mem;
use std::ops::{Bound, RangeBounds};
use std::ptr::NonNull;

use rand::Rng;

#[cfg(test)]
mod tests;

#[cfg(not(feature = "benchtest"))]
thread_local!(
    static ZSET_RNG: RefCell<rand::rngs::SmallRng> =
        RefCell::new(rand::SeedableRng::from_entropy());
);
#[cfg(feature = "benchtest")]
thread_local!(
    static ZSET_RNG: RefCell<rand::rngs::SmallRng> =
        RefCell::new(rand::SeedableRng::seed_from_u64(0x0123_4567_89ab_cdef_u64));
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

struct Node<T, S> {
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

impl<T, S> Debug for SkipList<T, S>
where
    T: Debug,
    S: Debug,
{
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
                let node = unsafe { &*node.as_ptr() };
                let level = &node.levels[l];
                let prev = if l == 0 {
                    node.prev.map(|p| unsafe { &(*p.as_ptr()).element })
                } else {
                    None
                };
                writeln!(
                    f,
                    "      height: {} span: {} {:?} {:?} {:?} {:?}",
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
        Ok(())
    }
}

impl<T, S> Default for SkipList<T, S> {
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

    fn element(&self) -> &T {
        self.element.as_ref().unwrap()
    }

    fn score(&self) -> &S {
        self.score.as_ref().unwrap()
    }

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
}

type Update<T, S> = [NonNull<Node<T, S>>; MAX_LEVELS];

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

    /// removes node from skip list
    ///
    /// # Safety
    ///
    /// `update` must be consistent with removing `node`
    unsafe fn remove_node(&mut self, update: &mut Update<T, S>, node: NonNull<Node<T, S>>) {
        for l in (0..=self.highest_level).rev() {
            let update = &mut (*update[l].as_ptr()).levels[l];
            if update.next == Some(node) {
                update.span += (*node.as_ptr()).levels[l].span;
                update.next = (*node.as_ptr()).levels[l].next;
            }
            update.span -= 1;
        }
        if let Some(next) = (*node.as_ptr()).levels[0].next {
            (*next.as_ptr()).prev = (*node.as_ptr()).prev;
        } else {
            self.tail = (*node.as_ptr()).prev;
        }
        while self.head.as_ref().levels[self.highest_level].next.is_none() && self.highest_level > 0
        {
            self.highest_level -= 1;
        }
        self.len -= 1;
    }

    fn pop_head_node(&mut self) -> Option<Box<Node<T, S>>> {
        let mut update: Update<T, S> = [self.head; MAX_LEVELS];
        unsafe {
            if let Some(node) = (*update[0].as_ptr()).levels[0].next {
                self.remove_node(&mut update, node);
                return Some(Box::from_raw(node.as_ptr()));
            }
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

    fn limited_iter(
        &self,
        head: Option<(NonNull<Node<T, S>>, usize)>,
        tail: Option<(NonNull<Node<T, S>>, usize)>,
    ) -> Iter<'_, T, S> {
        if let Some((head, tail)) = head.zip(tail) {
            Iter {
                head: Some(head.0),
                tail: Some(tail.0),
                len: 1 + tail.1 - head.1,
                marker: PhantomData,
            }
        } else {
            Iter {
                head: None,
                tail: None,
                len: 0,
                marker: PhantomData,
            }
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn node_at_rank(&self, rank: usize) -> NodePointer<T, S> {
        let mut node = None;
        let one_rank = rank + 1; // to account for the 1 that comes from traversing past head

        self.descend(
            |_, traversed| traversed <= one_rank,
            |_, n, traversed| {
                if traversed == one_rank && n != self.head {
                    node = Some(n);
                    return false;
                }
                true
            },
        );
        node
    }

    pub fn rank_iter<R>(&self, range: R) -> Iter<'_, T, S>
    where
        R: RangeBounds<usize>,
    {
        let (min, max) = self.rank_bounds(range);
        let empty = (max - min) < 1;
        let first = if empty {
            None
        } else {
            match min {
                0 => unsafe { self.head.as_ref().levels[0].next },
                _ => self.node_at_rank(min),
            }
            .map(|node| (node, min))
        };
        let last = if empty {
            None
        } else {
            match max {
                max if max == self.len => self.tail,
                _ => self.node_at_rank(max - 1),
            }
            .map(|node| (node, max - 1))
        };
        self.limited_iter(first, last)
    }

    pub fn delete_range_by_rank<R, F>(&mut self, range: R, callback: F) -> usize
    where
        F: FnMut(&mut T, S),
        R: RangeBounds<usize>,
    {
        let (start, end) = self.rank_bounds(range);
        if (end - start) < 1 {
            return 0;
        }
        self.delete_while(
            |_, traversed| traversed <= start,
            |_, traversed| traversed <= end,
            callback,
        )
    }

    /// Converts a range into (min, max) tuple
    ///
    /// where min is inclusive and max is exclusive
    /// returns (0, 0) to indicate "empty" range
    fn rank_bounds<R>(&self, range: R) -> (usize, usize)
    where
        R: RangeBounds<usize>,
    {
        let empty = (0, 0);

        if self.len == 0 {
            return empty;
        }
        let start = match range.start_bound() {
            Bound::Excluded(min) => *min + 1,
            Bound::Included(min) => *min,
            Bound::Unbounded => 0,
        };
        if start > self.len - 1 {
            return empty;
        }

        let mut end = match range.end_bound() {
            Bound::Excluded(max) => *max,
            Bound::Included(max) => *max + 1,
            Bound::Unbounded => self.len,
        };
        if end > self.len {
            end = self.len;
        }

        if (end - start) > 0 {
            return (start, end);
        }
        empty
    }

    fn is_in_range<R, Q>(&self, range: &R) -> bool
    where
        R: RangeBounds<Q>,
        Q: PartialOrd,
        S: Borrow<Q> + PartialEq + PartialOrd,
    {
        if range_is_empty(range) {
            return false;
        }
        unsafe {
            let node = self.tail;
            if node.is_none() || range_starts_after(range, node.unwrap().as_ref().score()) {
                return false;
            }
            let node = self.head.as_ref().levels[0].next;
            if node.is_none() || range_ends_before(range, node.unwrap().as_ref().score()) {
                return false;
            }
        }
        true
    }

    pub fn count_in_range<R, Q>(&self, range: R) -> usize
    where
        R: RangeBounds<Q>,
        Q: PartialOrd,
        S: Borrow<Q> + PartialEq + PartialOrd,
    {
        let mut count: usize = 0;
        if let Some((_, rank)) = self.first_in_range(&range) {
            count = self.len() - rank;
            if let Some((_, rank)) = self.last_in_range(&range) {
                count -= self.len() - rank - 1;
            }
        }
        count
    }

    fn first_in_range<R, Q>(&self, range: &R) -> Option<(NonNull<Node<T, S>>, usize)>
    where
        R: RangeBounds<Q>,
        Q: PartialOrd,
        S: Borrow<Q> + PartialEq + PartialOrd,
    {
        if !self.is_in_range(range) {
            return None;
        }
        let (mut node, traversed) =
            self.descend_traversed(|next| range_starts_after(range, next.score()));
        node = unsafe { (node.as_ref()).levels[0].next.unwrap() };
        let rank = traversed; // we traversed another in the line above and rank = traversed - 1
        if !range_ends_before(range, unsafe { node.as_ref().score() }) {
            return Some((node, rank));
        }
        None
    }

    fn last_in_range<R, Q>(&self, range: &R) -> Option<(NonNull<Node<T, S>>, usize)>
    where
        R: RangeBounds<Q>,
        Q: PartialOrd,
        S: Borrow<Q> + PartialEq + PartialOrd,
    {
        if !self.is_in_range(range) {
            return None;
        }
        let (node, traversed) =
            self.descend_traversed(|next| !range_ends_before(range, next.score()));
        let rank = traversed - 1;
        if range_starts_after(range, unsafe { node.as_ref().score() }) {
            None
        } else {
            Some((node, rank))
        }
    }

    pub fn delete_range_by_score<R, Q, F>(&mut self, range: R, callback: F) -> usize
    where
        R: RangeBounds<Q>,
        Q: PartialOrd,
        S: Borrow<Q> + PartialEq + PartialOrd,
        F: FnMut(&mut T, S),
    {
        self.delete_while(
            |node, _| range_starts_after(&range, node.score()),
            |node, _| !range_ends_before(&range, node.score()),
            callback,
        )
    }

    pub fn range_iter<R, Q>(&self, range: R) -> Iter<'_, T, S>
    where
        R: RangeBounds<Q>,
        Q: PartialOrd,
        S: Borrow<Q> + PartialEq + PartialOrd,
    {
        self.limited_iter(self.first_in_range(&range), self.last_in_range(&range))
    }

    /// undefined behavior if scores are not all identical
    fn is_in_lexrange<R, K>(&self, range: &R) -> bool
    where
        T: Borrow<K> + PartialEq + PartialOrd,
        R: RangeBounds<K>,
        K: PartialOrd,
    {
        if range_is_empty(range) {
            return false;
        }
        unsafe {
            let node = self.tail;
            if node.is_none() || range_starts_after(range, node.unwrap().as_ref().element()) {
                return false;
            }
            let node = self.head.as_ref().levels[0].next;
            if node.is_none() || range_ends_before(range, node.unwrap().as_ref().element()) {
                return false;
            }
        }
        true
    }

    /// undefined behavior if scores are not all identical
    pub fn count_in_lexrange<R, K>(&self, range: R) -> usize
    where
        T: Borrow<K> + PartialEq + PartialOrd,
        R: RangeBounds<K>,
        K: PartialOrd,
    {
        let mut count: usize = 0;
        if let Some((_, rank)) = self.first_in_lexrange(&range) {
            count = self.len() - rank;
            if let Some((_, rank)) = self.last_in_lexrange(&range) {
                count -= self.len() - rank - 1;
            }
        }
        count
    }

    /// undefined behavior if scores are not all identical
    fn first_in_lexrange<R, K>(&self, range: &R) -> Option<(NonNull<Node<T, S>>, usize)>
    where
        T: Borrow<K> + PartialEq + PartialOrd,
        R: RangeBounds<K>,
        K: PartialOrd,
    {
        if !self.is_in_lexrange(range) {
            return None;
        }
        let (mut node, traversed) =
            self.descend_traversed(|next| range_starts_after(range, next.element()));
        node = unsafe { node.as_ref().levels[0].next.unwrap() };
        let rank = traversed; // we traversed another in the line above and rank = traversed - 1
        if !range_ends_before(range, unsafe { node.as_ref() }.element()) {
            return Some((node, rank));
        }
        None
    }

    /// undefined behavior if scores are not all identical
    fn last_in_lexrange<R, K>(&self, range: &R) -> Option<(NonNull<Node<T, S>>, usize)>
    where
        T: Borrow<K> + PartialEq + PartialOrd,
        R: RangeBounds<K>,
        K: PartialOrd,
    {
        if !self.is_in_lexrange(range) {
            return None;
        }
        let (node, traversed) =
            self.descend_traversed(|next| !range_ends_before(range, next.element()));
        let rank = traversed - 1;
        if range_starts_after(range, unsafe { node.as_ref().element() }) {
            None
        } else {
            Some((node, rank))
        }
    }

    /// undefined behavior if scores are not all identical
    pub fn delete_range_by_lex<R, K, F>(&mut self, range: R, callback: F) -> usize
    where
        T: Borrow<K> + PartialEq + PartialOrd,
        R: RangeBounds<K>,
        K: PartialOrd,
        F: FnMut(&mut T, S),
    {
        self.delete_while(
            |node, _| range_starts_after(&range, node.element()),
            |node, _| !range_ends_before(&range, node.element()),
            callback,
        )
    }

    /// undefined behavior if scores are not all identical
    pub fn lexrange_iter<R, K>(&self, range: R) -> Iter<'_, T, S>
    where
        T: Borrow<K> + PartialEq + PartialOrd,
        R: RangeBounds<K>,
        K: PartialOrd,
    {
        self.limited_iter(
            self.first_in_lexrange(&range),
            self.last_in_lexrange(&range),
        )
    }

    /// advances (skips) over elements while `advance` is true (given the _next_ node)
    /// deletes elements while `delete` is true (given the _next_ node)
    /// calls `callback` with deleted element & score
    /// returns the number of entries deleted
    // ```
    // fn advance<T, S>(next: &Node<T, S>, traversed: usize) -> bool
    // fn delete<T, S>(next: &Node<T, S>, traversed: usize) -> bool
    // fn callback<T, S>(deleted_element: &mut T, score: S)
    fn delete_while<A, D, C>(&mut self, advance: A, delete: D, mut callback: C) -> usize
    where
        A: Fn(&Node<T, S>, usize) -> bool,
        D: Fn(&Node<T, S>, usize) -> bool,
        C: FnMut(&mut T, S),
    {
        let mut update: [NonNull<Node<T, S>>; MAX_LEVELS] = [self.head; MAX_LEVELS];
        let mut traversed = 0;
        self.descend(advance, |l, node, t| {
            update[l] = node;
            traversed = t;
            true
        });

        let mut head = unsafe { update[0].as_ref() }.levels[0].next;
        traversed += 1;
        let mut removed = 0;
        while head.map_or(false, |node| delete(unsafe { node.as_ref() }, traversed)) {
            let node = head.unwrap();
            unsafe {
                let next = node.as_ref().levels[0].next;
                head = next;
                self.remove_node(&mut update, node);
                let node = Box::from_raw(node.as_ptr());
                callback(&mut node.element.unwrap(), node.score.unwrap());
            }
            removed += 1;
            traversed += 1;
        }
        removed
    }

    pub fn insert(&mut self, elt: T, score: S)
    where
        T: PartialOrd,
        S: PartialOrd,
    {
        let mut update: [NonNull<Node<T, S>>; MAX_LEVELS] = [self.head; MAX_LEVELS];
        let mut rank: [usize; MAX_LEVELS] = [0; MAX_LEVELS];
        self.descend(
            |next, _| *next.score() < score || *next.score() == score && *next.element() < elt,
            |l, node, traversed| {
                rank[l] = traversed;
                update[l] = node;
                true
            },
        );
        let level = random_level();
        if level > self.highest_level {
            for l in self.highest_level + 1..=level {
                rank[l] = 0;
                update[l] = self.head;
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
                for (l, insert) in (*node.as_ptr()).levels.iter_mut().enumerate() {
                    let update = &mut (*update[l].as_ptr()).levels[l];
                    insert.next = update.next;
                    update.next = Some(NonNull::new_unchecked(node.as_ptr()));
                    let delta_span = rank[0] - rank[l];
                    insert.span = update.span - delta_span;
                    update.span = delta_span + 1;
                }
                #[allow(clippy::needless_range_loop)]
                for l in (level + 1)..=self.highest_level {
                    (*update[l].as_ptr()).levels[l].span += 1;
                }
                (*node.as_ptr()).prev = if update[0] != self.head {
                    Some(update[0])
                } else {
                    None
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

    pub fn remove(&mut self, elt: T, score: S) -> Option<T>
    where
        T: PartialOrd,
        S: PartialOrd,
    {
        let mut update = self.descend_for_update(&elt, &score);
        if let Some(node) = unsafe { (*update[0].as_ptr()).levels[0].next }.filter(|node| {
            let node = unsafe { node.as_ref() };
            *node.score() == score && *node.element() == elt
        }) {
            unsafe {
                self.remove_node(&mut update, node);
                let boxed = Box::from_raw(node.as_ptr());
                return boxed.element;
            }
        }
        None
    }

    pub fn update_score(&mut self, elt: &T, current_score: &S, new_score: S)
    where
        T: PartialOrd,
        S: PartialOrd,
    {
        let mut update = self.descend_for_update(elt, current_score);
        let node = unsafe { (*update[0].as_ptr()).levels[0].next }
            .expect("update_score called but no matching element");
        unsafe {
            let node_ref = node.as_ptr();
            assert!(
                current_score == (*node_ref).score() && *elt == *(*node_ref).element(),
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
                self.remove_node(&mut update, node);
                let old = Box::from_raw(node.as_ptr());
                self.insert(old.element.unwrap(), new_score);
            }
        }
    }

    /// Descend / advance through the skip list
    ///
    /// Advances from head to find the first node where the _next_ node fails the provided test.
    /// level_callback is evaluated at the end of each level before dropping to the next.
    ///
    /// Traversing from the head to the first element counts as 1 so the rank of an element within
    /// the list is `traversed - 1`
    ///
    /// The traversal can be cut short by returning `false` from `level_callback`.
    // ```
    // fn advance<T, S>(next: &Node<T, S>, traversed: usize) {}
    // fn level_callback<T, S>(level: usize, node: NonNull<Node<T, S>>, traversed: usize) -> bool { true }
    // ```
    fn descend<F, L>(&self, advance: F, mut level_callback: L)
    where
        F: Fn(&Node<T, S>, usize) -> bool,
        L: FnMut(usize, NonNull<Node<T, S>>, usize) -> bool,
    {
        let mut node = self.head;
        let mut traversed: usize = 0;
        for l in (0..=self.highest_level).rev() {
            while let Some(level) = unsafe { Some(&node.as_ref().levels[l]) }.filter(|level| {
                level.next.map_or(false, |next| {
                    let next = unsafe { next.as_ref() };
                    advance(next, traversed + level.span)
                })
            }) {
                traversed += level.span;
                node = level.next.unwrap()
            }
            if !level_callback(l, node, traversed) {
                return;
            }
        }
    }

    fn descend_traversed<F>(&self, f: F) -> (NonNull<Node<T, S>>, usize)
    where
        F: Fn(&Node<T, S>) -> bool,
    {
        let mut node = self.head;
        let mut traversed = 0;
        self.descend(
            |next, _| f(next),
            |_, n, t| {
                node = n;
                traversed = t;
                true
            },
        );

        (node, traversed)
    }

    fn descend_for_update(&self, elt: &T, score: &S) -> Update<T, S>
    where
        T: PartialOrd,
        S: PartialOrd,
    {
        let mut update: [NonNull<Node<T, S>>; MAX_LEVELS] = [self.head; MAX_LEVELS];

        self.descend(
            |next, _| next.score() < score || next.score() == score && next.element() < elt,
            |l, node, _| {
                update[l] = node;
                true
            },
        );
        update
    }

    pub fn rank(&self, elt: &T, score: &S) -> Option<usize>
    where
        T: PartialOrd,
        S: PartialOrd,
    {
        let mut rank = None;
        self.descend(
            |next, _| next.score() < score || next.score() == score && next.element() <= elt,
            |_, node, traversed| {
                if unsafe { node.as_ref().element.as_ref() }
                    .filter(|element| *element == elt)
                    .is_some()
                {
                    rank = Some(traversed - 1);
                    return false;
                }
                true
            },
        );
        rank
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

impl<'a, T, S> Iterator for Iter<'a, T, S> {
    type Item = (&'a T, &'a S);

    #[inline]
    fn next(&mut self) -> Option<(&'a T, &'a S)> {
        if self.len == 0 {
            None
        } else {
            self.head.map(|node| {
                // Need an unbound lifetime to get 'a
                let node = unsafe { &*node.as_ptr() };
                self.len -= 1;
                self.head = node.levels[0].next;
                (node.element(), node.score())
            })
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len, Some(self.len))
    }

    #[inline]
    fn last(mut self) -> Option<(&'a T, &'a S)> {
        self.next_back()
    }
}

impl<'a, T, S> DoubleEndedIterator for Iter<'a, T, S> {
    #[inline]
    fn next_back(&mut self) -> Option<(&'a T, &'a S)> {
        if self.len == 0 {
            None
        } else {
            self.tail.map(|node| unsafe {
                // Need an unbound lifetime to get 'a
                let node = &*node.as_ptr();
                self.len -= 1;
                self.tail = node.prev;
                (node.element(), node.score())
            })
        }
    }
}

fn range_ends_before<R, T, Q>(range: &R, value: &Q) -> bool
where
    R: RangeBounds<T>,
    T: PartialOrd,
    Q: Borrow<T>,
{
    match range.end_bound() {
        Bound::Included(end) => end < value.borrow(),
        Bound::Excluded(end) => end <= value.borrow(),
        Bound::Unbounded => false,
    }
}

fn range_starts_after<R, T, Q>(range: &R, value: &Q) -> bool
where
    R: RangeBounds<T>,
    T: PartialOrd,
    Q: Borrow<T>,
{
    match range.start_bound() {
        Bound::Included(start) => start > value.borrow(),
        Bound::Excluded(start) => start >= value.borrow(),
        Bound::Unbounded => false,
    }
}

fn range_is_empty<R, T>(range: &R) -> bool
where
    R: RangeBounds<T>,
    T: PartialEq + PartialOrd,
{
    if Bound::Unbounded == range.start_bound() || Bound::Unbounded == range.end_bound() {
        false
    } else {
        let (min, minex) = match range.start_bound() {
            Bound::Included(start) => (start, false),
            Bound::Excluded(start) => (start, true),
            Bound::Unbounded => panic!(),
        };
        let (max, maxex) = match range.end_bound() {
            Bound::Included(end) => (end, false),
            Bound::Excluded(end) => (end, true),
            Bound::Unbounded => panic!(),
        };
        (min > max) || (min == max && (minex || maxex))
    }
}
