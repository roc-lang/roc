use crate::pool_id::PoolId;
use smallvec::SmallVec;
use std::marker::PhantomPinned;
use std::ptr::{copy_nonoverlapping, NonNull};

pub struct ArenaRef<T> {
    pool_id: PoolId,
    ptr: NonNull<T>,
    _pin: PhantomPinned,
}

impl<T> ArenaRef<T> {
    pub fn get<'a>(&'a self, arena: &Arena<T>) -> &'a T {
        self.verify_pool_id(arena);

        // SAFETY: we know this pointer is safe to follow because it will only
        // get deallocated once the pool where it was created gets deallocated
        // (along with all of the Arenas it detached), and we just verified that
        // this ArenaRef's ID matches a pool which has not yet been deallocated.
        unsafe { self.ptr.as_ref() }
    }

    pub fn get_mut<'a>(&'a mut self, arena: &Arena<T>) -> &'a mut T {
        self.verify_pool_id(arena);

        // SAFETY: we know this pointer is safe to follow because it will only
        // get deallocated once the pool where it was created gets deallocated
        // (along with all of the Arenas it detached), and we just verified that
        // this ArenaRef's ID matches a pool which has not yet been deallocated.
        unsafe { self.ptr.as_mut() }
    }

    fn verify_pool_id(&self, arena: &Arena<T>) {
        // Verify that this ArenaRef actually came from the same pool as the
        // given Arena.
        //
        // If it didn't, we may have come from a pool that's been freed,
        // so trying to dereference our pointer could cause a use-after-free!
        assert_eq!(self.pool_id, arena.pool_id);
    }
}

pub struct ArenaVec<T> {
    pool_id: PoolId,
    buffer_ptr: NonNull<T>,
    len: usize,
    capacity: usize,
    _pin: PhantomPinned,
}

impl<T> ArenaVec<T> {
    pub fn new_in<'a>(&'a self, arena: &Arena<T>) -> Self {
        Self {
            pool_id: arena.pool_id,
            buffer_ptr: NonNull::dangling(),
            capacity: 0,
            len: 0,
            _pin: PhantomPinned,
        }
    }

    pub fn push<'a>(&'a mut self, val: T, arena: &mut Arena<T>) {
        self.verify_pool_id(arena);

        if self.len <= self.capacity {
            // We're all set!
            //
            // This empty branch is just here for branch prediction,
            // since this should be the most common case in practice.
        } else {
            // Double our capacity and reserve a new block.
            self.capacity *= 2;

            let ptr = arena.alloc_array(self.capacity);

            // SAFETY: the existing buffer must have at least self.len elements,
            // as must the new one, so copying that many between them is safe.
            unsafe {
                // Copy all elements from the current buffer into the new one
                copy_nonoverlapping(self.buffer_ptr.as_ptr(), ptr, self.len);
            }

            self.buffer_ptr = unsafe { NonNull::new_unchecked(ptr) };
        }

        // Store the element in the appropriate memory address.
        let elem_ptr = unsafe { &mut *self.buffer_ptr.as_ptr().offset(self.len as isize) };

        *elem_ptr = val;

        self.len += 1;
    }

    pub fn get<'a>(&'a self, index: usize, arena: &Arena<T>) -> Option<&'a T> {
        self.verify_pool_id(arena);

        if index < self.len {
            // SAFETY: we know this pointer is safe to follow because we've
            // done a bounds check, and because we know it will only get
            // deallocated once the pool where it was created gets deallocated
            // (along with all of the Arenas it detached), and we just verified that
            // this ArenaRef's ID matches a pool which has not yet been deallocated.
            Some(unsafe { &*self.buffer_ptr.as_ptr().offset(index as isize) })
        } else {
            None
        }
    }

    pub fn get_mut<'a>(&'a mut self, index: usize, arena: &Arena<T>) -> Option<&'a mut T> {
        self.verify_pool_id(arena);

        if index < self.len {
            // SAFETY: we know this pointer is safe to follow because we've
            // done a bounds check, and because we know it will only get
            // deallocated once the pool where it was created gets deallocated
            // (along with all of the Arenas it detached), and we just verified that
            // this ArenaRef's ID matches a pool which has not yet been deallocated.
            Some(unsafe { &mut *self.buffer_ptr.as_ptr().offset(index as isize) })
        } else {
            None
        }
    }

    fn verify_pool_id(&self, arena: &Arena<T>) {
        // Verify that this ArenaVec actually came from the same pool as the
        // given Arena.
        //
        // If it didn't, we may have come from a pool that's been freed,
        // so trying to dereference our pointer could cause a use-after-free!
        assert_eq!(self.pool_id, arena.pool_id);
    }
}

impl<T> ArenaVec<T> {}

#[derive(Default, PartialEq, Eq)]
pub struct ArenaPool<T> {
    pool_id: PoolId,
    chunks: Vec<Vec<T>>,
    num_leased: usize,
    default_chunk_capacity: usize,
}

impl<T> ArenaPool<T> {
    const DEFAULT_CHUNK_SIZE: usize = 4096;

    pub fn with_capacity(capacity: usize) -> ArenaPool<T> {
        Self::with_capacity_and_chunk_size(capacity, Self::DEFAULT_CHUNK_SIZE)
    }

    pub fn with_capacity_and_chunk_size(capacity: usize, chunk_size: usize) -> ArenaPool<T> {
        Self {
            pool_id: PoolId::default(),
            chunks: Vec::with_capacity(capacity),
            num_leased: 0,
            default_chunk_capacity: chunk_size,
        }
    }

    /// Return a new Arena, which can be given to different threads.
    /// This arena can be used to allocate ArenaRef and ArenaVec values which
    /// are compatible with any Arena leased from this pool.
    ///
    /// Before this pool gets dropped, you must call reabsorb() on every
    /// arena that has been leased - otherwise, you'll get a panic when this
    /// gets dropped! The memory safety of the system depends on all arenas
    /// having been reabsorbed before the pool gets deallocated, which is why
    /// hte pool's Drop implementation enforces it.
    pub fn lease(&mut self) -> Arena<T> {
        self.num_leased += 1;

        let mut chunks = SmallVec::with_capacity(1);

        chunks.push(Vec::with_capacity(self.default_chunk_capacity));

        Arena {
            pool_id: self.pool_id,
            chunks,
            default_chunk_capacity: self.default_chunk_capacity,
        }
    }

    /// Return an arena to the pool. (This would have been called "return" but
    /// that's a reserved keyword.)
    pub fn reabsorb(&mut self, arena: Arena<T>) {
        // Ensure we're reabsorbing an arena that was
        // actually leased by this pool in the first place!
        assert_eq!(arena.pool_id, self.pool_id);

        // Add the arena's chunks to our own, so their memory remains live
        // after the arena gets dropped. This is important, because at this
        // point their pointers can still potentially be dereferenced!
        self.chunks.extend(arena.chunks.into_iter());

        self.num_leased -= 1;
    }
}

impl<T> Drop for ArenaPool<T> {
    fn drop(&mut self) {
        // When an ArenaPool gets dropped, it must not have any leased
        // arenas remaining. If it does, there will be outstanding IDs which
        // could be used with those non-reabsorbed Arenas to read freed memory!
        // This would be a use-after-free; we panic rather than permit that.
        assert_eq!(self.num_leased, 0);
    }
}

#[derive(PartialEq, Eq)]
pub struct Arena<T> {
    pool_id: PoolId,
    chunks: SmallVec<[Vec<T>; 1]>,
    default_chunk_capacity: usize,
}

impl<T> Arena<T> {
    pub fn alloc(&mut self, val: T) -> ArenaRef<T> {
        // Every Arena should always be initialized with at least one chunk.
        debug_assert!(!self.chunks.is_empty());

        let (chunk_len, chunk_capacity) = {
            let chunk = self.chunks.last().unwrap();

            (chunk.len(), chunk.capacity())
        };

        if chunk_len == chunk_capacity {
            // We've run out of space in our last chunk. Create a new one!
            self.chunks
                .push(Vec::with_capacity(self.default_chunk_capacity));
        }

        let chunk = self.chunks.last_mut().unwrap();
        let index = chunk.len();

        chunk.push(val);

        // Get a pointer to the memory address within our particular chunk.
        let ptr: *mut T = &mut chunk[index];

        ArenaRef {
            pool_id: self.pool_id,
            ptr: unsafe { NonNull::new_unchecked(ptr) },
            _pin: PhantomPinned,
        }
    }

    fn alloc_array(&mut self, num_elems: usize) -> *mut T {
        let (chunk_len, chunk_capacity) = {
            let chunk = self.chunks.last().unwrap();

            (chunk.len(), chunk.capacity())
        };

        if chunk_len + num_elems <= chunk_capacity {
            // This will fit in the current chunk, so we'll just use that.
        } else {
            // This won't fit in our current chunk, so we'll need a new one.
            let capacity = self.default_chunk_capacity.max(num_elems);

            // This won't fit in a default-sized chunk, so we need to
            // allocate a new one just for this.
            self.chunks.push(Vec::with_capacity(capacity));
        }

        self.chunks.last_mut().unwrap().as_mut_ptr()
    }
}
