use zset::skip_list_set::SkipListSet;

fn main() {
    // skip lists are optimized for inserting at head as opposed to tail.
    // use "negative" time
    let mut list = SkipListSet::new();
    const TIME_STEPS: i64 = 10_000_000;
    const MEMBERS: i64 = 100_000;
    const STEPS_PER_CLEANUP: i64 = 1_000;
    let mut deleted = 0;
    let mut skipped = 0;
    for t in 0..TIME_STEPS {
        let member = t % MEMBERS;
        let cycle = t / MEMBERS;
        if t % STEPS_PER_CLEANUP == 0 {
            let cutoff = t - MEMBERS - 1;
            deleted += list.delete_range_by_score(-cutoff..)
        }
        // let 10% of members "forget" to update half of the time alternating cycles
        if member < MEMBERS / 10 && member % 2 == cycle % 2 {
            skipped += 1;
        } else {
            list.insert(member, -t);
        }
    }
    println!("skipped: {} deleted: {}", skipped, deleted);
}
