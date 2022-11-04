// #[macro_use]
// extern crate pretty_assertions;

extern crate arena_pool;

#[cfg(test)]
mod test_arena_pool {
    use arena_pool::pool::{ArenaIter, ArenaPool};

    #[test]
    fn empty_pool() {
        // Neither of these does anything, but they
        // at least shouldn't panic or anything.
        let _: (ArenaPool<()>, ArenaIter<()>) = ArenaPool::new(0, 0);
        let _: (ArenaPool<()>, ArenaIter<()>) = ArenaPool::with_chunk_size(0, 0, 0);
    }
}
