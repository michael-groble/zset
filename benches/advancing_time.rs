use zset::skip_list_set::SkipListSet;

fn main() {
    let mut list = SkipListSet::new();
    const TIME_STEPS: usize = 10_000_000;
    const MEMBERS: usize = 100_000;
    const STEPS_PER_CLEANUP: usize = 1_000;
    let mut deleted = 0;
    let mut skipped = 0;
    for t in 0..TIME_STEPS {
        let member = t % MEMBERS;
        let cycle = t / MEMBERS;
        if t % STEPS_PER_CLEANUP == 0 {
            let cutoff = if t > MEMBERS { t - MEMBERS } else { 0 };
            deleted += list.delete_range_by_score(..cutoff)
        }
        // let 10% of members "forget" to update half of the time alternating cycles
        if member < MEMBERS / 10 && member % 2 == cycle % 2 {
            skipped += 1;
        } else {
            list.insert(member, t);
        }
    }
    println!("skipped: {} deleted: {}", skipped, deleted);
}
