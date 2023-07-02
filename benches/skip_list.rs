use rand::seq::SliceRandom;
use test::Bencher;
use zset::skip_list::SkipList;

fn insert_sequential(b: &mut Bencher, n: usize) {
    let mut list = SkipList::new();
    let vec: Vec<usize> = (0..n).collect();

    b.iter(|| {
        for &i in vec.iter() {
            list.insert(i, i);
        }
        assert_eq!(list.delete_range_by_rank(.., |_, _| {}), n);
    })
}

#[bench]
fn bench_insert_sequential_10k(b: &mut Bencher) {
    insert_sequential(b, 10_000);
}

#[bench]
fn bench_insert_sequential_100k(b: &mut Bencher) {
    insert_sequential(b, 100_000);
}

#[bench]
fn bench_insert_sequential_500k(b: &mut Bencher) {
    insert_sequential(b, 500_000);
}

fn insert_random(b: &mut Bencher, n: usize) {
    let mut list = SkipList::new();
    let mut vec: Vec<usize> = (0..n).collect();
    let mut rng = crate::bench_rng();
    vec.shuffle(&mut rng);

    b.iter(|| {
        for &i in vec.iter() {
            list.insert(i, i);
        }
        assert_eq!(list.delete_range_by_rank(.., |_, _| {}), n);
    })
}

#[bench]
fn bench_insert_random_10k(b: &mut Bencher) {
    insert_random(b, 10_000)
}

#[bench]
fn bench_insert_random_100k(b: &mut Bencher) {
    insert_random(b, 100_000)
}

#[bench]
fn bench_insert_random_500k(b: &mut Bencher) {
    insert_random(b, 500_000)
}

fn remove(b: &mut Bencher, n: usize) {
    let mut list = SkipList::new();
    let insert: Vec<usize> = (0..n).collect();
    let mut remove = insert.to_vec();
    let mut rng = crate::bench_rng();
    remove.shuffle(&mut rng);

    b.iter(|| {
        for &i in insert.iter() {
            list.insert(i, i);
        }
        for &i in remove.iter() {
            list.remove(i, i);
        }
        assert_eq!(list.len(), 0);
    })
}

#[bench]
fn bench_remove_100k(b: &mut Bencher) {
    remove(b, 100_000)
}

#[bench]
fn bench_remove_500k(b: &mut Bencher) {
    remove(b, 500_000)
}

#[bench]
fn bench_count_in_range_100k(b: &mut Bencher) {
    let mut list = SkipList::new();
    let vec: Vec<usize> = (0..100_000).collect();
    for &i in vec.iter() {
        list.insert(i, i);
    }

    b.iter(|| {
        let mut count = 0;
        count += list.count_in_range(..25_000);
        count += list.count_in_range(25_000..50_000);
        count += list.count_in_range(50_000..75_000);
        count += list.count_in_range(75_000..);
        assert_eq!(count, 100_000)
    })
}

#[bench]
fn bench_count_in_range_500k(b: &mut Bencher) {
    let mut list = SkipList::new();
    let vec: Vec<usize> = (0..500_000).collect();
    for &i in vec.iter() {
        list.insert(i, i);
    }

    b.iter(|| {
        let mut count = 0;
        count += list.count_in_range(..125_000);
        count += list.count_in_range(125_000..250_000);
        count += list.count_in_range(250_000..375_000);
        count += list.count_in_range(375_000..);
        assert_eq!(count, 500_000)
    })
}

#[bench]
fn bench_count_in_lexrange_100k(b: &mut Bencher) {
    let mut list = SkipList::new();
    let vec: Vec<usize> = (0..100_000).collect();
    for &i in vec.iter() {
        list.insert(i, 0);
    }

    b.iter(|| {
        let mut count = 0;
        count += list.count_in_lexrange(..25_000);
        count += list.count_in_lexrange(25_000..50_000);
        count += list.count_in_lexrange(50_000..75_000);
        count += list.count_in_lexrange(75_000..);
        assert_eq!(count, 100_000)
    })
}

#[bench]
fn bench_count_in_lexrange_500k(b: &mut Bencher) {
    let mut list = SkipList::new();
    let vec: Vec<usize> = (0..500_000).collect();
    for &i in vec.iter() {
        list.insert(i, 0);
    }

    b.iter(|| {
        let mut count = 0;
        count += list.count_in_lexrange(..125_000);
        count += list.count_in_lexrange(125_000..250_000);
        count += list.count_in_lexrange(250_000..375_000);
        count += list.count_in_lexrange(375_000..);
        assert_eq!(count, 500_000)
    })
}

fn update_score(b: &mut Bencher, n: usize, multiplier: usize, increment: usize) {
    let mut list = SkipList::new();
    let vec: Vec<usize> = (0..n).collect();
    for &i in vec.iter() {
        list.insert(i, i * multiplier);
    }

    b.iter(|| {
        for i in vec.iter() {
            list.update_score(i, &(i * multiplier), i * multiplier + increment);
        }
        assert_eq!(list.len(), n);
    })
}

#[bench]
fn bench_update_score_100k(b: &mut Bencher) {
    update_score(b, 100_000, 2, 1);
}

#[bench]
fn bench_update_score_500k(b: &mut Bencher) {
    update_score(b, 500_000, 2, 1);
}

#[bench]
fn bench_update_score_reinsert_100k(b: &mut Bencher) {
    update_score(b, 100_000, 1, 3);
}

#[bench]
fn bench_update_score_reinsert_500k(b: &mut Bencher) {
    update_score(b, 500_000, 1, 3);
}
