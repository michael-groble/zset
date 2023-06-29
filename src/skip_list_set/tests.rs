use crate::skip_list_set::SkipListSet;
use std::ops::Bound;

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
fn test_reinsert_updates_score() {
    let mut l = SkipListSet::new();
    l.insert('a', 1);
    l.insert('a', 2);
    let mut iter = l.iter();
    assert_eq!(iter.next(), Some((&'a', 2)));
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

#[test]
fn test_count_in_lexrange() {
    let mut l = SkipListSet::new();

    l.insert('a', 0);
    l.insert('b', 0);
    l.insert('c', 0);
    l.insert('d', 0);
    l.insert('g', 0);
    l.insert('h', 0);

    assert_eq!(l.count_in_lexrange(..'a'), 0);
    assert_eq!(l.count_in_lexrange(..='a'), 1);
    assert_eq!(l.count_in_lexrange('a'..'e'), 4);
    assert_eq!(l.count_in_lexrange(..), 6);
    assert_eq!(l.count_in_lexrange('e'..), 2);
    assert_eq!(l.count_in_lexrange('e'..'h'), 1);
    assert_eq!(l.count_in_lexrange('e'..'g'), 0);
    assert_eq!(l.count_in_lexrange('h'..), 1);
    assert_eq!(
        l.count_in_lexrange((Bound::Excluded('h'), Bound::<char>::Unbounded)),
        0
    );
}

#[test]
fn test_get() {
    let mut l = SkipListSet::new();

    l.insert('a', 1);
    l.insert('b', 2);
    l.insert('c', 3);

    assert_eq!(l.get(&'a'), Some(&1));
    assert_eq!(l.get(&'b'), Some(&2));
    assert_eq!(l.get(&'c'), Some(&3));
    assert_eq!(l.get(&'d'), None);
}

#[test]
fn test_rank() {
    let mut l = SkipListSet::new();

    l.insert('a', 1);
    l.insert('b', 2);
    l.insert('c', 3);

    assert_eq!(l.rank(&'a'), Some((0, &1)));
    assert_eq!(l.rank(&'b'), Some((1, &2)));
    assert_eq!(l.rank(&'c'), Some((2, &3)));
    assert_eq!(l.rank(&'d'), None);
}

#[test]
fn test_reverse_rank() {
    let mut l = SkipListSet::new();

    l.insert('a', 1);
    l.insert('b', 2);
    l.insert('c', 3);

    assert_eq!(l.reverse_rank(&'a'), Some((2, &1)));
    assert_eq!(l.reverse_rank(&'b'), Some((1, &2)));
    assert_eq!(l.reverse_rank(&'c'), Some((0, &3)));
    assert_eq!(l.reverse_rank(&'d'), None);
}

#[test]
fn test_range_iter() {
    let mut l = SkipListSet::new();

    l.insert('a', 1);
    l.insert('b', 2);
    l.insert('c', 3);

    let mut iter = l.range_iter(..3);
    assert_eq!(iter.next(), Some((&'a', 1)));
    assert_eq!(iter.next(), Some((&'b', 2)));
    assert_eq!(iter.next(), None);
    // full test in SkipList
}

#[test]
fn test_lexrange_iter() {
    let mut l = SkipListSet::new();

    l.insert('a', 0);
    l.insert('b', 0);
    l.insert('c', 0);

    let mut iter = l.lexrange_iter(..'c');
    assert_eq!(iter.next(), Some((&'a', 0)));
    assert_eq!(iter.next(), Some((&'b', 0)));
    assert_eq!(iter.next(), None);
    // full test in SkipList
}

#[test]
fn test_rank_iter() {
    let mut l = SkipListSet::new();

    l.insert('a', 1);
    l.insert('b', 2);
    l.insert('c', 3);

    let mut iter = l.rank_iter(..2);
    assert_eq!(iter.next(), Some((&'a', 1)));
    assert_eq!(iter.next(), Some((&'b', 2)));
    assert_eq!(iter.next(), None);
    // full test in SkipList
}

#[test]
fn test_delete_range() {
    let mut l = SkipListSet::new();

    l.insert('a', 1.0);
    l.insert('b', 1.0);
    l.insert('c', 2.0);
    l.insert('d', 3.0);
    l.insert('e', 4.0);
    l.insert('f', 4.0);
    l.insert('g', 5.0);

    assert_eq!(l.delete_range_by_score(..1.0), 0);
    assert_eq!(l.len(), 7);
    assert_eq!(l.delete_range_by_score(..=1.0), 2);
    assert_eq!(l.len(), 5);
    assert_eq!(l.delete_range_by_score(3.1..4.0), 0);
    assert_eq!(l.len(), 5);
    assert_eq!(l.delete_range_by_score(3.0..4.0), 1);
    assert_eq!(l.len(), 4);
    assert_eq!(l.delete_range_by_score(4.0..), 3);
    assert_eq!(l.len(), 1);
    let mut iter = l.iter();
    assert_eq!(iter.next(), Some((&'c', 2.0)));
    assert_eq!(iter.next(), None)
}

#[test]
fn test_drop_delete_in_range() {
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
    l.delete_range_by_score(3..);
    assert_eq!(unsafe { DROPS }, 2);
    drop(l);
    assert_eq!(unsafe { DROPS }, 4);
}
