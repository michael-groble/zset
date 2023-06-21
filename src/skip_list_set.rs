// lexcount
// mpop
// mscore
// range
// rank
// remrange
// score

use crate::skip_list;
use crate::skip_list::{Score, SkipList};
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::{Bound, RangeBounds};
use std::ptr::NonNull;
use std::{hash, mem};

// try to get rid of Hash + Eq
pub struct SkipListSet<T: ?Sized + Hash + Eq, S> {
    list: SkipList<Key<T>, S>,
    hash: HashMap<Key<T>, S>,
}

struct Key<T: ?Sized + Hash + Eq> {
    value: NonNull<T>,
    marker: PhantomData<NonNull<T>>,
}

impl<T: ?Sized + Hash + Eq> Borrow<T> for Key<T> {
    fn borrow(&self) -> &T {
        unsafe { self.value.as_ref() }
    }
}

impl<T: ?Sized + Hash + Eq> Hash for Key<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        unsafe { (self.value.as_ref()).hash(state) }
    }
}

impl<T: ?Sized + Hash + Eq> Eq for Key<T> {}

impl<T: ?Sized + Hash + Eq> PartialEq for Key<T> {
    fn eq(&self, other: &Self) -> bool {
        unsafe { (self.value.as_ref()) == other.value.as_ref() }
    }
}

impl<T: ?Sized + Hash + Eq + PartialOrd> PartialOrd for Key<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        unsafe { (self.value.as_ref()).partial_cmp(other.value.as_ref()) }
    }
}

impl<T: Hash + Eq + PartialOrd, S: Score> SkipListSet<T, S> {
    pub fn insert(&mut self, element: T, score: S) {
        let boxed = Box::new(element);
        let leaked: NonNull<T> = Box::leak(boxed).into();
        // need to check if already exists in hash and update score if so
        self.list.insert(
            Key {
                value: leaked,
                marker: PhantomData,
            },
            score,
        );
        self.hash.insert(
            Key {
                value: leaked,
                marker: PhantomData,
            },
            score,
        );
    }

    pub fn delete(&mut self, element: &T) -> bool {
        if let Some((e, s)) = self.hash.remove_entry(element) {
            let elt = self.list.delete(e, s).expect("element missing from list");
            unsafe { Box::from_raw(elt.value.as_ptr()) };
            true
        } else {
            false
        }
    }

    pub fn count_in_range<R: RangeBounds<S> + Clone>(&self, range: R) -> usize {
        let mut count: usize = 0;
        if let Some((_, rank)) = self.list.first_in_range(range.clone()) {
            count = self.len() - rank;
            if let Some((_, rank)) = self.list.last_in_range(range) {
                count -= self.len() - rank - 1;
            }
        }
        count
    }
}

impl<T: ?Sized + Hash + Eq + Default, S: Default> SkipListSet<T, S> {
    pub fn new() -> Self {
        SkipListSet {
            list: SkipList::new(),
            hash: HashMap::new(),
        }
    }
}

impl<T: ?Sized + Hash + Eq, S> SkipListSet<T, S> {
    pub fn len(&self) -> usize {
        self.list.len()
    }
    pub fn iter(&self) -> Iter<'_, T, S> {
        Iter {
            iter: self.list.iter(),
        }
    }
}

pub struct Iter<'a, T: 'a + ?Sized + Hash + Eq, S> {
    iter: skip_list::Iter<'a, Key<T>, S>,
}

impl<'a, T: ?Sized + Hash + Eq, S: Clone + Copy> Iterator for Iter<'a, T, S> {
    type Item = (&'a T, S);

    #[inline]
    fn next(&mut self) -> Option<(&'a T, S)> {
        self.iter
            .next()
            .map(|(boxed, score)| unsafe { (&*(boxed.value.as_ptr()), score) })
    }
}

impl<T: ?Sized + Hash + Eq, S> Drop for SkipListSet<T, S> {
    // does not protect against panic.  memory could leak
    fn drop(&mut self) {
        for key in self.hash.keys() {
            drop(unsafe { Box::from_raw(key.value.as_ptr()) });
        }
    }
}

#[test]
fn test_basic() {
    let mut l = SkipListSet::new();
    for c in "dcbae".chars() {
        l.insert(c, c as u32);
    }
    let mut iter = l.iter();
    assert_eq!(l.len(), 5);
    assert_eq!(iter.next(), Some((&'a', 97)));
    assert_eq!(iter.next(), Some((&'b', 98)));
    assert_eq!(iter.next(), Some((&'c', 99)));
    assert_eq!(iter.next(), Some((&'d', 100)));
    assert_eq!(iter.next(), Some((&'e', 101)));
    assert_eq!(iter.next(), None);
    assert_eq!(l.delete(&'a'), true);
    assert_eq!(l.delete(&'d'), true);
    assert_eq!(l.len(), 3);
    let mut iter = l.iter();
    assert_eq!(iter.next(), Some((&'b', 98)));
    assert_eq!(iter.next(), Some((&'c', 99)));
    assert_eq!(iter.next(), Some((&'e', 101)));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_drop() {
    static mut DROPS: i32 = 0;

    #[derive(Default, PartialEq, PartialOrd, Hash, Eq)]
    struct Elem(i32);

    impl Drop for Elem {
        fn drop(&mut self) {
            unsafe {
                DROPS += 1;
            }
        }
    }

    let mut l = SkipListSet::new();
    l.insert(Elem(1), 1);
    l.insert(Elem(2), 2);
    l.insert(Elem(3), 3);
    l.insert(Elem(4), 4);
    drop(l);
    assert_eq!(unsafe { DROPS }, 4);
}

#[test]
fn test_count_in_range() {
    let mut l = SkipListSet::new();

    l.insert('a', 1);
    l.insert('b', 1);
    l.insert('c', 2);
    l.insert('d', 2);
    l.insert('e', 3);
    l.insert('f', 5);

    assert_eq!(l.count_in_range(..1), 0);
    assert_eq!(l.count_in_range(..=1), 2);
    assert_eq!(l.count_in_range(1..3), 4);
    assert_eq!(l.count_in_range(..), 6);
    assert_eq!(l.count_in_range(3..), 2);
    assert_eq!(l.count_in_range(3..4), 1);
    assert_eq!(l.count_in_range(4..5), 0);
    assert_eq!(l.count_in_range(5..), 1);
    assert_eq!(l.count_in_range((Bound::Excluded(5), Bound::Unbounded)), 0);
}
