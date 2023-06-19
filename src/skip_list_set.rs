
// card
// count
// lexcount
// mpop
// mscore
// range
// rank
// rem
// remrange
// score

use std::cmp::Ordering;
use std::collections::HashMap;
use std::hash;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ptr::NonNull;
use crate::skip_list;
use crate::skip_list::SkipList;

pub struct SkipListSet<T, S> {
    list: SkipList<Key<T>, S>,
    hash: HashMap<Key<T>, S>,
}

struct Key<T: ?Sized> {
    value: NonNull<T>
}

impl<T: Hash> Hash for Key<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        unsafe {(self.value.as_ref()).hash(state)}
    }
}

impl<T: ?Sized + PartialEq> Eq for Key<T> {}

impl<T: ?Sized + PartialEq> PartialEq for Key<T> {
    fn eq(&self, other: &Self) -> bool {
        unsafe {(self.value.as_ref()) == other.value.as_ref()}
    }
}

impl<T: ?Sized + PartialOrd> PartialOrd for Key<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        unsafe {(self.value.as_ref()).partial_cmp(other.value.as_ref())}
    }
}

impl<T: Default> Default for Key<T> {
    fn default() -> Self {
        let default: T = Default::default();
        let boxed = Box::new(default);
        let leaked : NonNull<T> = Box::leak(boxed).into();
        Self {
            value: leaked
        }
    }
}

impl<T: Hash + PartialOrd, S: Copy + PartialOrd> SkipListSet<T, S> {
    pub fn insert(&mut self, element: T, score: S) {
        let boxed = Box::new(element);
        let leaked : NonNull<T> = Box::leak(boxed).into();
        self.list.insert(Key {value: leaked}, score);
        self.hash.insert(Key {value: leaked}, score);
    }

    pub fn delete(&mut self, element: &T) -> bool {
        let element = Key { value: NonNull::from(element) };
        if let Some((e, s)) = self.hash.remove_entry(&element) {
            self.list.delete(e, s);
            true
        } else {
            false
        }
    }
}

impl<T: std::default::Default, S: std::default::Default> SkipListSet<T, S> {
    pub fn new() -> Self {
        SkipListSet {
            list: SkipList::new(),
            hash: HashMap::new(),
        }
    }
}

impl<T, S> SkipListSet<T, S> {
    pub fn len(&self) -> usize {
        self.list.len()
    }
    pub fn iter(&self) -> Iter<'_, T, S> {
        Iter {
            iter: self.list.iter()
        }
    }
}

pub struct Iter<'a, T: 'a, S> {
    iter: skip_list::Iter<'a, Key<T>, S>
}

impl<'a, T, S: Clone + Copy> Iterator for Iter<'a, T, S>
{
    type Item = (&'a T, S);

    #[inline]
    fn next(&mut self) -> Option<(&'a T, S)> {
        self.iter.next().map(|(boxed, score)| {
            unsafe { (&*(boxed.value.as_ptr()), score) }
        })
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
