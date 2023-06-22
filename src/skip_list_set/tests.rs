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
