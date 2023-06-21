use super::*;
use std::cmp::Ordering;
use std::panic::catch_unwind;

#[test]
fn test_basic() {
    let mut l = SkipList::new();
    for c in "dcbae".chars() {
        l.insert(c, c as u32);
    }
    let l = l;
    let mut iter = l.iter();
    assert_eq!(l.len, 5);
    assert_eq!(iter.next(), Some((&'a', 97)));
    assert_eq!(iter.next(), Some((&'b', 98)));
    assert_eq!(iter.next(), Some((&'c', 99)));
    assert_eq!(iter.next(), Some((&'d', 100)));
    assert_eq!(iter.next(), Some((&'e', 101)));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_backward() {
    let mut l = SkipList::new();
    for c in "dcbae".chars() {
        l.insert(c, c as u32);
    }
    let l = l;
    let mut iter = l.iter();
    assert_eq!(iter.next_back(), Some((&'e', 101)));
    assert_eq!(iter.next_back(), Some((&'d', 100)));
    assert_eq!(iter.next_back(), Some((&'c', 99)));
    assert_eq!(iter.next_back(), Some((&'b', 98)));
    assert_eq!(iter.next_back(), Some((&'a', 97)));
    assert_eq!(iter.next_back(), None);
}

#[test]
fn test_delete() {
    let mut l = SkipList::new();
    for c in "dcbae".chars() {
        l.insert(c, c as u32);
    }
    for c in "bc".chars() {
        assert_eq!(l.delete(c, c as u32).is_some(), true);
    }
    assert_eq!(l.delete('a', 0).is_some(), false); // won't delete if score doesn't match
    assert_eq!(l.delete('z', 100).is_some(), false); // won't delete if element doesn't match

    let l = l;
    let mut iter = l.iter();
    assert_eq!(l.len, 3);
    assert_eq!(iter.next(), Some((&'a', 97)));
    assert_eq!(iter.next(), Some((&'d', 100)));
    assert_eq!(iter.next(), Some((&'e', 101)));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_same_score_ordering() {
    let mut l = SkipList::new();

    l.insert('d', 1);
    l.insert('c', 2);
    l.insert('b', 2);
    l.insert('a', 3);
    let mut iter = l.iter();
    assert_eq!(l.len, 4);
    assert_eq!(iter.next(), Some((&'d', 1)));
    assert_eq!(iter.next(), Some((&'b', 2)));
    assert_eq!(iter.next(), Some((&'c', 2)));
    assert_eq!(iter.next(), Some((&'a', 3)));
    assert_eq!(iter.next(), None);
    l.delete('c', 2);
    let mut iter = l.iter();
    assert_eq!(iter.next(), Some((&'d', 1)));
    assert_eq!(iter.next(), Some((&'b', 2)));
    assert_eq!(iter.next(), Some((&'a', 3)));
}

#[test]
#[should_panic(expected = "update_score called but no matching element")]
fn test_update_with_wrong_score() {
    let mut l = SkipList::new();
    l.insert('a', 1);
    l.update_score('a', 2, 1);
}

#[test]
#[should_panic(expected = "update_score called but no matching element")]
fn test_update_with_wrong_element() {
    let mut l = SkipList::new();
    l.insert('a', 1);
    l.update_score('b', 1, 2);
}

#[test]
fn test_update_score() {
    let mut l = SkipList::new();

    l.insert('c', 1.0);
    l.insert('b', 2.0);
    l.insert('a', 3.0);
    let mut iter = l.iter();
    assert_eq!(l.len, 3);
    assert_eq!(iter.next(), Some((&'c', 1.0)));
    assert_eq!(iter.next(), Some((&'b', 2.0)));
    assert_eq!(iter.next(), Some((&'a', 3.0)));
    assert_eq!(iter.next(), None);
    l.update_score('b', 2.0, 2.5);
    let mut iter = l.iter();
    assert_eq!(iter.next(), Some((&'c', 1.0)));
    assert_eq!(iter.next(), Some((&'b', 2.5)));
    assert_eq!(iter.next(), Some((&'a', 3.0)));
    l.update_score('b', 2.5, 3.5);
    let mut iter = l.iter();
    assert_eq!(iter.next(), Some((&'c', 1.0)));
    assert_eq!(iter.next(), Some((&'a', 3.0)));
    assert_eq!(iter.next(), Some((&'b', 3.5)));
}

#[test]
fn test_update_score_boxed() {
    let mut l = SkipList::new();

    l.insert(Box::new('c'), 1);
    l.insert(Box::new('b'), 2);
    l.insert(Box::new('a'), 3);
    l.update_score(Box::new('b'), 2, 5);
    let mut iter = l.iter();
    assert_eq!(iter.next(), Some((&Box::new('c'), 1)));
    assert_eq!(iter.next(), Some((&Box::new('a'), 3)));
    assert_eq!(iter.next(), Some((&Box::new('b'), 5)));
}

#[test]
fn test_in_range() {
    let mut l = SkipList::new();

    l.insert('d', 1);
    l.insert('a', 3);

    assert_eq!(l.is_in_range(..1), false);
    assert_eq!(l.is_in_range(..=1), true);
    assert_eq!(l.is_in_range(2..5), true);
    assert_eq!(l.is_in_range(3..), true);
    assert_eq!(l.is_in_range((Bound::Excluded(3), Bound::Unbounded)), false);
}

fn first_in_range<T, S: internal::Score, R: RangeBounds<S> + Clone>(
    list: &SkipList<T, S>,
    range: R,
) -> Option<((&T, usize))> {
    list.first_in_range(range)
        .map(|(node, rank)| (unsafe { node.as_ref().element() }, rank))
}

#[test]
fn test_first_in_range() {
    let mut l = SkipList::new();

    l.insert('a', 1);
    l.insert('b', 1);
    l.insert('c', 2);
    l.insert('d', 2);
    l.insert('e', 3);

    assert_eq!(first_in_range(&l, ..1), None);
    assert_eq!(first_in_range(&l, ..=1), Some((&'a', 0)));
    assert_eq!(first_in_range(&l, 2..), Some((&'c', 2)));
    assert_eq!(first_in_range(&l, 3..), Some((&'e', 4)));
    assert_eq!(
        first_in_range(&l, (Bound::Excluded(3), Bound::Unbounded)),
        None
    );
}

fn last_in_range<T, S: internal::Score, R: RangeBounds<S> + Clone>(
    list: &SkipList<T, S>,
    range: R,
) -> Option<((&T, usize))> {
    list.last_in_range(range)
        .map(|(node, rank)| (unsafe { node.as_ref().element() }, rank))
}

#[test]
fn test_last_in_range() {
    let mut l = SkipList::new();

    l.insert('a', 1);
    l.insert('b', 1);
    l.insert('c', 2);
    l.insert('d', 2);
    l.insert('e', 3);

    assert_eq!(last_in_range(&l, ..1), None);
    assert_eq!(last_in_range(&l, ..=1), Some((&'b', 1)));
    assert_eq!(last_in_range(&l, ..=2), Some((&'d', 3)));
    assert_eq!(last_in_range(&l, 2..), Some((&'e', 4)));
    assert_eq!(
        last_in_range(&l, (Bound::Excluded(3), Bound::Unbounded)),
        None
    );
}

#[test]
fn test_delete_range_by_score() {
    let mut l = SkipList::new();

    l.insert('a', 1.0);
    l.insert('b', 2.0);
    l.insert('c', 3.0);
    l.insert('d', 4.0);

    l.delete_range_by_score(2.5..2.9);
    assert_eq!(l.len, 4);
    l.delete_range_by_score(5.5..5.9);
    assert_eq!(l.len, 4);
    l.delete_range_by_score(3.0..);
    assert_eq!(l.len, 2);
    let mut iter = l.iter();
    assert_eq!(iter.next(), Some((&'a', 1.0)));
    assert_eq!(iter.next(), Some((&'b', 2.0)));
    assert_eq!(iter.next(), None);
    l.delete_range_by_score(..=1.0);
    assert_eq!(l.len, 1);
    let mut iter = l.iter();
    assert_eq!(iter.next(), Some((&'b', 2.0)));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_drop() {
    static mut DROPS: i32 = 0;

    #[derive(Default, PartialEq, PartialOrd)]
    struct Elem(i32);

    impl Drop for Elem {
        fn drop(&mut self) {
            unsafe {
                DROPS += 1;
            }
        }
    }

    let mut l = SkipList::new();
    l.insert(Elem(1), 1);
    l.insert(Elem(2), 2);
    l.insert(Elem(3), 3);
    l.insert(Elem(4), 4);
    drop(l);
    assert_eq!(unsafe { DROPS }, 4);
}

#[test]
fn test_drop_with_pop() {
    static mut DROPS: i32 = 0;

    #[derive(Default, PartialEq, PartialOrd)]
    struct Elem(i32);

    impl Drop for Elem {
        fn drop(&mut self) {
            unsafe {
                DROPS += 1;
            }
        }
    }

    let mut l = SkipList::new();
    l.insert(Elem(1), 1);
    l.insert(Elem(2), 2);
    l.insert(Elem(3), 3);
    l.insert(Elem(4), 4);

    l.pop_head_node();
    l.pop_head_node();
    assert_eq!(unsafe { DROPS }, 2);
    drop(l);
    assert_eq!(unsafe { DROPS }, 4);
}

#[test]
fn test_drop_with_range() {
    static mut DROPS: i32 = 0;

    #[derive(Default, PartialEq, PartialOrd)]
    struct Elem(i32);

    impl Drop for Elem {
        fn drop(&mut self) {
            unsafe {
                DROPS += 1;
            }
        }
    }

    let mut l = SkipList::new();
    l.insert(Elem(1), 1);
    l.insert(Elem(2), 2);
    l.insert(Elem(3), 3);
    l.insert(Elem(4), 4);

    l.delete_range_by_score(2..=3);
    assert_eq!(unsafe { DROPS }, 2);
    drop(l);
    assert_eq!(unsafe { DROPS }, 4);
}

#[test]
fn test_drop_panic() {
    static mut DROPS: i32 = 0;

    #[derive(Default, PartialEq, PartialOrd)]
    struct Elem(i32);

    impl Drop for Elem {
        fn drop(&mut self) {
            unsafe {
                DROPS += 1;
            }

            if self.0 < 0 {
                panic!("panic in `drop`");
            }
        }
    }

    let mut l = SkipList::new();
    l.insert(Elem(1), 1);
    l.insert(Elem(2), 2);
    l.insert(Elem(3), 3);
    l.insert(Elem(-1), 4);

    catch_unwind(move || drop(l)).ok();

    assert_eq!(unsafe { DROPS }, 4);
}
