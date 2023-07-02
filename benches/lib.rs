#![feature(test)]
extern crate test;

use rand::SeedableRng;

mod skip_list;
mod skip_list_set;

fn bench_rng() -> rand::rngs::SmallRng {
    SeedableRng::seed_from_u64(0x0123_4567_89ab_cdef_u64)
}
