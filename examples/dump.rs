use zset::skip_list::SkipList;

fn main() {
    let mut l = SkipList::new();
    for c in "yxuvmkdnopcwhlftgjbrqziaes".chars() {
        l.insert(c, c as u32);
    }
    l.delete_range_by_lex('h'..'l', |_, _| {});
    println!("{:?}", l);
}
