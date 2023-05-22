use rand;
use rand::{Rng, SeedableRng};
use std::cell::RefCell;

thread_local!(
    static ZSET_RNG: RefCell<rand::rngs::SmallRng> =
        RefCell::new(rand::rngs::SmallRng::from_entropy());
);

// 1_000_000_000: 5.76 sec
fn orig() {
    let mut counts: [u32; 32] = [0; 32];
    let p_u8: u8 = 0x40_u8; // ~ 0.25, 255 * 0.25
    for _ in 1..1_000_000_000_u64 {
        let mut level = 0;
        ZSET_RNG.with(|rng| {
            let mut r = rng.borrow_mut();
            while r.gen::<u8>() < p_u8 {
                level += 1;
            }
        });

        if level > 31 {
            level = 31
        }
        counts[level] += 1;
    }
    for (i, count) in counts.iter().enumerate() {
        println!("{},{}", i, count);
    }
}

// 1_000_000_000: 6.35 sec
fn alt() {
    let mut counts: [u32; 32] = [0; 32];
    for _ in 1..1_000_000_000_u64 {
        let r = ZSET_RNG.with(|rng| rng.borrow_mut().gen::<u64>());
        let level = if r > 0x4000_0000_0000_0000u64 {
            0
        } else if r > 0x1000_0000_0000_0000u64 {
            1
        } else if r > 0x0400_0000_0000_0000u64 {
            2
        } else if r > 0x0100_0000_0000_0000u64 {
            3
        } else if r > 0x0040_0000_0000_0000u64 {
            4
        } else if r > 0x0010_0000_0000_0000u64 {
            5
        } else if r > 0x0004_0000_0000_0000u64 {
            6
        } else if r > 0x0001_0000_0000_0000u64 {
            7
        } else if r > 0x0000_4000_0000_0000u64 {
            8
        } else if r > 0x0000_1000_0000_0000u64 {
            9
        } else if r > 0x0000_0400_0000_0000u64 {
            10
        } else if r > 0x0000_0100_0000_0000u64 {
            11
        } else if r > 0x0000_0040_0000_0000u64 {
            12
        } else if r > 0x0000_0010_0000_0000u64 {
            13
        } else if r > 0x0000_0004_0000_0000u64 {
            14
        } else if r > 0x0000_0001_0000_0000u64 {
            15
        } else if r > 0x0000_0000_4000_0000u64 {
            16
        } else if r > 0x0000_0000_1000_0000u64 {
            17
        } else if r > 0x0000_0000_0400_0000u64 {
            18
        } else if r > 0x0000_0000_0100_0000u64 {
            19
        } else if r > 0x0000_0000_0040_0000u64 {
            20
        } else if r > 0x0000_0000_0010_0000u64 {
            21
        } else if r > 0x0000_0000_0004_0000u64 {
            22
        } else if r > 0x0000_0000_0001_0000u64 {
            23
        } else if r > 0x0000_0000_0000_4000u64 {
            24
        } else if r > 0x0000_0000_0000_1000u64 {
            25
        } else if r > 0x0000_0000_0000_0400u64 {
            26
        } else if r > 0x0000_0000_0000_0100u64 {
            27
        } else if r > 0x0000_0000_0000_0040u64 {
            28
        } else if r > 0x0000_0000_0000_0010u64 {
            29
        } else if r > 0x0000_0000_0000_0004u64 {
            30
        } else {
            31
        };
        counts[level] += 1;
    }
    for (i, count) in counts.iter().enumerate() {
        println!("{},{}", i, count);
    }
}

fn main() {
    alt();
}
