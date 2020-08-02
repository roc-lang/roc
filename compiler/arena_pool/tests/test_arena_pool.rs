#[macro_use]
extern crate pretty_assertions;

extern crate arena_pool;

#[cfg(test)]
mod test_arena_pool {
    use arena_pool::{Arena, ArenaIter, ArenaPool, ArenaRef, ArenaVec, ArenaVec2d, CloneIn};

    /// Proof-of-concept of this working for Layout.
    #[derive(Debug, PartialEq, Eq, Hash)]
    pub enum Layout {
        Str,
        Int,
        Struct(ArenaVec<Layout>),
        Union(ArenaVec2d<Layout>),
        Pointer(ArenaRef<Layout>),
    }

    impl CloneIn<Layout> for Layout {
        fn clone_in(&self, arena: &mut Arena<Layout>) -> Self {
            use Layout::*;

            match self {
                Str => Str,
                Int => Int,
                Struct(layouts) => Struct(layouts.clone_in(arena)),
                Union(layouts) => Union(layouts.clone_in(arena)),
                Pointer(arena_ref) => Pointer(arena_ref.clone_in(arena)),
            }
        }
    }

    #[test]
    fn empty_pool() {
        // Neither of these does anything, but they
        // at least shouldn't panic or anything.
        let _: (ArenaPool<()>, ArenaIter<()>) = ArenaPool::new(0, 0);
        let _: (ArenaPool<()>, ArenaIter<()>) = ArenaPool::with_chunk_size(0, 0, 0);
    }
}
