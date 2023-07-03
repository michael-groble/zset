use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::hash;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::RangeBounds;
use std::ptr::NonNull;

use crate::skip_list;
use crate::skip_list::SkipList;

#[cfg(test)]
mod tests;

pub struct SkipListSet<T, S> {
    list: SkipList<Key<T>, S>,
    hash: HashMap<Key<T>, S>,
    marker: PhantomData<Box<T>>,
}

pub struct Key<T> {
    value: NonNull<T>,
}

impl<T> Borrow<T> for Key<T> {
    fn borrow(&self) -> &T {
        unsafe { self.value.as_ref() }
    }
}

impl<T> Hash for Key<T>
where
    T: Hash,
{
    fn hash<H>(&self, state: &mut H)
    where
        H: hash::Hasher,
    {
        unsafe { (self.value.as_ref()).hash(state) }
    }
}

impl<T> Eq for Key<T> where T: Eq {}

impl<T> PartialEq for Key<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        unsafe { (self.value.as_ref()) == other.value.as_ref() }
    }
}

impl<T> PartialOrd for Key<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        unsafe { (self.value.as_ref()).partial_cmp(other.value.as_ref()) }
    }
}

impl<T, S> SkipListSet<T, S> {
    pub fn new() -> Self {
        SkipListSet {
            list: SkipList::new(),
            hash: HashMap::new(),
            marker: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.list.len()
    }

    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }

    pub fn iter(&self) -> Iter<'_, T, S> {
        Iter {
            iter: self.list.iter(),
        }
    }

    pub fn rank_iter<R>(&self, range: R) -> Iter<'_, T, S>
    where
        R: RangeBounds<usize>,
    {
        Iter {
            iter: self.list.rank_iter(range),
        }
    }

    pub fn range_iter<R>(&self, range: R) -> Iter<'_, T, S>
    where
        S: PartialOrd,
        R: RangeBounds<S>,
    {
        Iter {
            iter: self.list.range_iter(range),
        }
    }

    /// undefined behavior if scores are not all identical
    pub fn lexrange_iter<R>(&self, range: R) -> Iter<'_, T, S>
    where
        T: PartialOrd,
        R: RangeBounds<T>,
    {
        Iter {
            iter: self.list.lexrange_iter(range),
        }
    }

    pub fn insert(&mut self, element: T, score: S)
    where
        T: Hash + Eq + PartialOrd,
        S: PartialOrd + Clone,
    {
        if let Some((elt, existing_score)) = self.hash.remove_entry(&element) {
            self.list.update_score(&elt, &existing_score, score.clone());
            self.hash.insert(elt, score);
        } else {
            let boxed = Box::new(element);
            let leaked: NonNull<T> = Box::leak(boxed).into();
            // for now, assume scores are trivial to clone.  if not, we could box them like element
            self.list.insert(Key { value: leaked }, score.clone());
            self.hash.insert(Key { value: leaked }, score);
        }
    }

    pub fn delete(&mut self, element: &T) -> bool
    where
        T: Hash + Eq + PartialOrd,
        S: PartialOrd,
    {
        if let Some((e, s)) = self.hash.remove_entry(element) {
            let elt = self.list.remove(e, s).expect("element missing from list");
            unsafe { Box::from_raw(elt.value.as_ptr()) };
            true
        } else {
            false
        }
    }

    pub fn count_in_range<R>(&self, range: R) -> usize
    where
        S: PartialOrd,
        R: RangeBounds<S>,
    {
        self.list.count_in_range(range)
    }

    /// undefined behavior if scores are not all identical
    pub fn count_in_lexrange<R>(&self, range: R) -> usize
    where
        T: PartialOrd,
        R: RangeBounds<T>,
    {
        self.list.count_in_lexrange(range)
    }

    pub fn get(&self, element: &T) -> Option<&S>
    where
        T: Hash + Eq,
    {
        self.hash.get(element)
    }

    pub fn rank(&self, element: &T) -> Option<(usize, &S)>
    where
        T: Hash + Eq + PartialOrd,
        S: PartialOrd,
    {
        self.hash
            .get_key_value(element)
            .and_then(|(elt, score)| self.list.rank(elt, score).map(|rank| (rank, score)))
    }

    pub fn reverse_rank(&self, element: &T) -> Option<(usize, &S)>
    where
        T: Hash + Eq + PartialOrd,
        S: PartialOrd,
    {
        self.rank(element)
            .map(|(rank, score)| (self.len() - 1 - rank, score))
    }

    pub fn delete_range_by_score<R>(&mut self, range: R) -> usize
    where
        T: Hash + Eq,
        S: PartialOrd,
        R: RangeBounds<S>,
    {
        self.list.delete_range_by_score(range, |key, _| {
            self.hash.remove(key).expect("element missing from hash");
            unsafe { Box::from_raw(key.value.as_ptr()) };
        })
    }

    pub fn delete_range_by_rank<R>(&mut self, range: R) -> usize
    where
        T: Hash + Eq,
        R: RangeBounds<usize>,
    {
        self.list.delete_range_by_rank(range, |key, _| {
            self.hash.remove(key).expect("element missing from hash");
            unsafe { Box::from_raw(key.value.as_ptr()) };
        })
    }

    /// undefined behavior if scores are not all identical
    pub fn delete_range_by_lex<R>(&mut self, range: R) -> usize
    where
        T: Hash + Eq + PartialOrd,
        R: RangeBounds<T>,
    {
        self.list.delete_range_by_lex(range, |key, _| {
            self.hash.remove(key).expect("element missing from hash");
            unsafe { Box::from_raw(key.value.as_ptr()) };
        })
    }

    pub fn pop_min(&mut self, n: usize) -> Vec<(T, S)>
    where
        T: Hash + Eq,
    {
        self.pop_range(..n)
    }

    pub fn pop_max(&mut self, n: usize) -> Vec<(T, S)>
    where
        T: Hash + Eq,
    {
        let mut result = if n > self.len() {
            self.pop_range(..)
        } else {
            self.pop_range((self.len() - n)..)
        };
        // Ugh, likely better to implement a version of delete that walks backwards from tail for this
        result.reverse();
        result
    }

    fn pop_range<R>(&mut self, range: R) -> Vec<(T, S)>
    where
        T: Hash + Eq,
        R: RangeBounds<usize>,
    {
        let mut popped = Vec::new();
        self.list.delete_range_by_rank(range, |key, score| {
            self.hash.remove(key).expect("element missing from hash");
            let boxed = unsafe { Box::from_raw(key.value.as_ptr()) };
            popped.push((*boxed, score))
        });
        popped
    }
}

impl<T, S> Default for SkipListSet<T, S> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Iter<'a, T: 'a, S: 'a> {
    iter: skip_list::Iter<'a, Key<T>, S>,
}

impl<'a, T, S> Iterator for Iter<'a, T, S> {
    type Item = (&'a T, &'a S);

    #[inline]
    fn next(&mut self) -> Option<(&'a T, &'a S)> {
        self.iter
            .next()
            .map(|(boxed, score)| unsafe { (&*(boxed.value.as_ptr()), score) })
    }
}

impl<T, S> Drop for SkipListSet<T, S> {
    // does not protect against panic.
    // memory could leak if there is a panic while dropping one of the keys
    // TODO look in to using HashMap::drain or HashMap::into_keys
    fn drop(&mut self) {
        for key in self.hash.keys() {
            drop(unsafe { Box::from_raw(key.value.as_ptr()) });
        }
    }
}
