Implements a [skip list](https://15721.courses.cs.cmu.edu/spring2018/papers/08-oltpindexes1/pugh-skiplists-cacm1990.pdf)
and uses that to implement a `SkipListSet` data structure suitable for implementing [Sorted Set](https://redis.io/docs/data-types/sorted-sets/) operations in Redis.  See
[this fork](https://github.com/michael-groble/mini-redis/tree/zset-support) for some sample commands added to `mini-redis`
to run `redis-benchmark` against, e.g.:

```
redis-benchmark -r 10000 -n 1000000 -t zadd
```

The skip list implementation differs from that in Redis in two main ways (besides having a little more "rust-like" api):

- It takes more memory since Redis uses a dynamically-sized node structure and this uses a fixed-sized one.
- It always reuses nodes when updating scores.  Redis creates a new node (with a new random level) when the new score
  requires the node to move location in the list.
    
    Reusing nodes has a notable improvement in the `advancing_time` benchmark but not a noticeable difference when
    incorporated in `mini-redis` and tested with `redis-benchmark`.
    See [corresponding changes](https://github.com/michael-groble/redis/compare/unstable...michael-groble:redis:reinsert-skiplist-node)
    for the Redis code base (having similarly negligible benefit).

It runs comparable to redis (configured with `save ""`):
```
redis-benchmark -r 10000 --seed 1234 -t zadd (3x each, flushall before each redis run and restart mini-redis before each run)

redis:
         avg       min       p50       p95       p99       max
       0.233     0.064     0.199     0.439     0.559     0.823
       0.254     0.080     0.239     0.455     0.567     0.775
       0.267     0.064     0.271     0.439     0.567     0.727

redis-mini:
         avg       min       p50       p95       p99       max
       0.219     0.056     0.207     0.311     0.367     0.831
       0.216     0.056     0.199     0.311     0.367     0.967
       0.215     0.072     0.199     0.311     0.367     1.103
```

the `dump` example shows the details of how the skip list data structure is represented:
```
$ cargo run --example dump
Skip List, len: 22, levels: 3
  Level 0
      height: 32 span: 1 None 0, prev: None
      height: 1 span: 1 Some('a') 97, prev: None
      height: 2 span: 1 Some('b') 98, prev: Some('a')
      height: 4 span: 1 Some('c') 99, prev: Some('b')
      height: 1 span: 1 Some('d') 100, prev: Some('c')
      height: 1 span: 1 Some('e') 101, prev: Some('d')
      height: 1 span: 1 Some('f') 102, prev: Some('e')
      height: 1 span: 1 Some('g') 103, prev: Some('f')
      height: 1 span: 1 Some('l') 108, prev: Some('g')
      height: 1 span: 1 Some('m') 109, prev: Some('l')
      height: 1 span: 1 Some('n') 110, prev: Some('m')
      height: 1 span: 1 Some('o') 111, prev: Some('n')
      height: 3 span: 1 Some('p') 112, prev: Some('o')
      height: 1 span: 1 Some('q') 113, prev: Some('p')
      height: 1 span: 1 Some('r') 114, prev: Some('q')
      height: 1 span: 1 Some('s') 115, prev: Some('r')
      height: 2 span: 1 Some('t') 116, prev: Some('s')
      height: 1 span: 1 Some('u') 117, prev: Some('t')
      height: 1 span: 1 Some('v') 118, prev: Some('u')
      height: 2 span: 1 Some('w') 119, prev: Some('v')
      height: 1 span: 1 Some('x') 120, prev: Some('w')
      height: 2 span: 1 Some('y') 121, prev: Some('x')
      height: 1 span: 0 Some('z') 122, prev: Some('y')
  Level 1
      height: 32 span: 2 None 0, prev: None
      height: 2 span: 1 Some('b') 98, prev: None
      height: 4 span: 9 Some('c') 99, prev: None
      height: 3 span: 4 Some('p') 112, prev: None
      height: 2 span: 3 Some('t') 116, prev: None
      height: 2 span: 2 Some('w') 119, prev: None
      height: 2 span: 1 Some('y') 121, prev: None
  Level 2
      height: 32 span: 3 None 0, prev: None
      height: 4 span: 9 Some('c') 99, prev: None
      height: 3 span: 10 Some('p') 112, prev: None
  Level 3
      height: 32 span: 3 None 0, prev: None
      height: 4 span: 19 Some('c') 99, prev: None
```
