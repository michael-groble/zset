use super::*;
use std::cmp::Ordering;
use std::panic::catch_unwind;

#[test]
fn test_basic() {
    let mut l: SkipList<char> = SkipList::new();
    for c in "dcbae".chars() {
        l.insert(c, c as u32 as f64);
    }
    let l = l;
    let mut iter = l.iter();
    assert_eq!(l.len, 5);
    assert_eq!(iter.next(), Some((97_f64, &'a')));
    assert_eq!(iter.next(), Some((98_f64, &'b')));
    assert_eq!(iter.next(), Some((99_f64, &'c')));
    assert_eq!(iter.next(), Some((100_f64, &'d')));
    assert_eq!(iter.next(), Some((101_f64, &'e')));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_backward() {
    let mut l: SkipList<char> = SkipList::new();
    for c in "dcbae".chars() {
        l.insert(c, c as u32 as f64);
    }
    let l = l;
    let mut iter = l.iter();
    assert_eq!(iter.next_back(), Some((101_f64, &'e')));
    assert_eq!(iter.next_back(), Some((100_f64, &'d')));
    assert_eq!(iter.next_back(), Some((99_f64, &'c')));
    assert_eq!(iter.next_back(), Some((98_f64, &'b')));
    assert_eq!(iter.next_back(), Some((97_f64, &'a')));
    assert_eq!(iter.next_back(), None);
}

#[test]
fn test_delete() {
    let mut l: SkipList<char> = SkipList::new();
    for c in "dcbae".chars() {
        l.insert(c, c as u32 as f64);
    }
    for c in "bc".chars() {
        assert_eq!(l.delete(c, c as u32 as f64), true);
    }
    assert_eq!(l.delete('a', 0_f64), false); // won't delete if score doesn't match
    assert_eq!(l.delete('z', 100_f64), false); // won't delete if element doesn't match

    let l = l;
    let mut iter = l.iter();
    assert_eq!(l.len, 3);
    assert_eq!(iter.next(), Some((97_f64, &'a')));
    assert_eq!(iter.next(), Some((100_f64, &'d')));
    assert_eq!(iter.next(), Some((101_f64, &'e')));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_same_score_ordering() {
    let mut l: SkipList<char> = SkipList::new();

    l.insert('d', 1_f64);
    l.insert('c', 2_f64);
    l.insert('b', 2_f64);
    l.insert('a', 3_f64);
    let mut iter = l.iter();
    assert_eq!(l.len, 4);
    assert_eq!(iter.next(), Some((1_f64, &'d')));
    assert_eq!(iter.next(), Some((2_f64, &'b')));
    assert_eq!(iter.next(), Some((2_f64, &'c')));
    assert_eq!(iter.next(), Some((3_f64, &'a')));
    assert_eq!(iter.next(), None);
    l.delete('c', 2_f64);
    let mut iter = l.iter();
    assert_eq!(iter.next(), Some((1_f64, &'d')));
    assert_eq!(iter.next(), Some((2_f64, &'b')));
    assert_eq!(iter.next(), Some((3_f64, &'a')));
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
    l.insert(Elem(1), 1_f64);
    l.insert(Elem(2), 2_f64);
    l.insert(Elem(3), 3_f64);
    l.insert(Elem(4), 4_f64);
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
    l.insert(Elem(1), 1_f64);
    l.insert(Elem(2), 2_f64);
    l.insert(Elem(3), 3_f64);
    l.insert(Elem(4), 4_f64);

    l.pop_head_node();
    l.pop_head_node();
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
    l.insert(Elem(1), 1_f64);
    l.insert(Elem(2), 2_f64);
    l.insert(Elem(3), 3_f64);
    l.insert(Elem(-1), 4_f64);

    catch_unwind(move || drop(l)).ok();

    assert_eq!(unsafe { DROPS }, 4);
}
