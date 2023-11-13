use roc_error_macros::internal_error;
use std::marker::PhantomPinned;
use std::ptr::{copy_nonoverlapping, NonNull};

pub struct ArenaRef<T> {
    ptr: NonNull<T>,
    _pin: PhantomPinned,
}

impl<T> ArenaRef<T> {
    pub fn get<'a, A: AsArena<T>>(&'a self, arena: &A) -> &'a T {
        arena.verify_ownership(self.ptr);

        // SAFETY: we know this pointer is safe to follow because it will only
        // get deallocated once the pool where it was created gets deallocated
        // (along with all of the Arenas it detached), and we just verified that
        // this ArenaRef's ID matches a pool which has not yet been deallocated.
        unsafe { self.ptr.as_ref() }
    }

    pub fn get_mut<'a, A: AsArena<T>>(&'a mut self, arena: &A) -> &'a mut T {
        arena.verify_ownership(self.ptr);

        // SAFETY: we know this pointer is safe to follow because it will only
        // get deallocated once the pool where it was created gets deallocated
        // (along with all of the Arenas it detached), and we just verified that
        // this ArenaRef's ID matches a pool which has not yet been deallocated.
        unsafe { self.ptr.as_mut() }
    }
}

/// Like a Vec, except the capacity you give it initially is its maximum
/// capacity forever. If you ever exceed it, it'll panic!
pub struct ArenaVec<T> {
    buffer_ptr: NonNull<T>,
    len: usize,
    capacity: usize,
    _pin: PhantomPinned,
}

impl<T> ArenaVec<T> {
    pub fn new_in(arena: &mut Arena<T>) -> Self {
        // We can't start with a NonNull::dangling pointer because when we go
        // to push elements into this, they'll try to verify the dangling
        // pointer resides in the arena it was given, which will likely panic.
        //
        // Instead, we'll take a pointer inside the array but never use it
        // other than for verification, because our capacity is 0.
        Self::with_capacity_in(0, arena)
    }

    pub fn with_capacity_in(capacity: usize, arena: &mut Arena<T>) -> Self {
        let ptr = arena.alloc_vec(capacity);

        Self {
            buffer_ptr: unsafe { NonNull::new_unchecked(ptr) },
            capacity,
            len: 0,
            _pin: PhantomPinned,
        }
    }

    pub fn push(&mut self, val: T, arena: &mut Arena<T>) {
        // Verify that this is the arena where we originally got our buffer,
        // and is therefore safe to read and to write to. (If we have sufficient
        // capacity, we'll write to it, and otherwise we'll read from it when
        // copying our buffer over to the new reserved block.)
        arena.verify_ownership(self.buffer_ptr);

        if self.len <= self.capacity {
            // We're all set!
            //
            // This empty branch is just here for branch prediction,
            // since this should be the most common case in practice.
        } else {
            // Double our capacity and reserve a new block.
            self.capacity *= 2;

            let ptr = arena.alloc_vec(self.capacity);

            // SAFETY: the existing buffer must have at least self.len elements,
            // as must the new one, so copying that many between them is safe.
            unsafe {
                // Copy all elements from the current buffer into the new one
                copy_nonoverlapping(self.buffer_ptr.as_ptr(), ptr, self.len);
            }

            self.buffer_ptr = unsafe { NonNull::new_unchecked(ptr) };
        }

        // Store the element in the appropriate memory address.
        let elem_ptr = unsafe { &mut *self.buffer_ptr.as_ptr().add(self.len) };

        *elem_ptr = val;

        self.len += 1;
    }

    pub fn get<'a>(&'a self, index: usize, arena: &Arena<T>) -> Option<&'a T> {
        arena.verify_ownership(self.buffer_ptr);

        if index < self.len {
            // SAFETY: we know this pointer is safe to follow because we've
            // done a bounds check, and because we know it will only get
            // deallocated once the pool where it was created gets deallocated
            // (along with all of the Arenas it detached), and we just verified that
            // this ArenaRef's ID matches a pool which has not yet been deallocated.
            Some(unsafe { &*self.buffer_ptr.as_ptr().add(index) })
        } else {
            None
        }
    }

    pub fn get_mut<'a>(&'a mut self, index: usize, arena: &Arena<T>) -> Option<&'a mut T> {
        arena.verify_ownership(self.buffer_ptr);

        if index < self.len {
            // SAFETY: we know this pointer is safe to follow because we've
            // done a bounds check, and because we know it will only get
            // deallocated once the pool where it was created gets deallocated
            // (along with all of the Arenas it detached), and we just verified that
            // this ArenaRef's ID matches a pool which has not yet been deallocated.
            Some(unsafe { &mut *self.buffer_ptr.as_ptr().add(index) })
        } else {
            None
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct ArenaPool<T> {
    first_chunk: Vec<T>,
    extra_chunks: Vec<Vec<T>>,
    num_leased: usize,
    default_chunk_capacity: usize,
}

impl<T> ArenaPool<T> {
    const DEFAULT_CHUNK_SIZE: usize = 1024;

    /// Be careful! Both of these arguments are of type usize.
    ///
    /// The first is the number of elements that will be in each arena.
    /// The second is the number of arenas.
    ///
    /// This returns a new Pool, and also an iterator of Arenas. These Arenas can
    /// be given to different threads, where they can be used to allocate
    /// ArenaRef and ArenaVec values which can then be dereferenced by the Arena
    /// that created them, or by this pool once those Arenas have been
    /// reabsorbed back into it.
    ///
    /// (A word of warning: if you try to use this pool to dereference ArenaRec
    /// and ArenaVec values which were allocated by arenas that have *not* yet
    /// been reabsorbed, it may work some of the time and panic other times,
    /// depending on whether the arena needed to allocate extra chunks beyond
    /// its initial chunk. tl;dr - doing that may panic, so don't try it!)
    ///
    /// Before this pool gets dropped, you must call reabsorb() on every
    /// arena that has been leased - otherwise, you'll get a panic when this
    /// gets dropped! The memory safety of the system depends on all arenas
    /// having been reabsorbed before the pool gets deallocated, which is why
    /// the pool's Drop implementation enforces it.
    pub fn new(num_arenas: usize, elems_per_arena: usize) -> (ArenaPool<T>, ArenaIter<T>) {
        Self::with_chunk_size(num_arenas, elems_per_arena, Self::DEFAULT_CHUNK_SIZE)
    }

    /// Like `new`, except you can also specify the chunk size that each
    /// arena will use to allocate its extra chunks if it runs out of space
    /// in its main buffer.
    ///
    /// Things will run fastest if that main buffer never runs out, though!
    pub fn with_chunk_size(
        num_arenas: usize,
        elems_per_arena: usize,
        chunk_size: usize,
    ) -> (ArenaPool<T>, ArenaIter<T>) {
        let mut first_chunk = Vec::with_capacity(elems_per_arena * num_arenas);
        let iter = ArenaIter {
            ptr: first_chunk.as_mut_ptr(),
            quantity_remaining: num_arenas,
            first_chunk_capacity: elems_per_arena,
        };
        let pool = Self {
            first_chunk,
            extra_chunks: Vec::new(),
            num_leased: num_arenas,
            default_chunk_capacity: chunk_size,
        };

        (pool, iter)
    }

    /// Return an arena to the pool. (This would have been called "return" but
    /// that's a reserved keyword.)
    pub fn reabsorb(&mut self, arena: Arena<T>) {
        // Ensure we're reabsorbing an arena that was
        // actually leased by this pool in the first place!
        verify_ownership(
            self.first_chunk.as_ptr(),
            self.first_chunk.capacity(),
            &self.extra_chunks,
            arena.first_chunk_ptr,
        );

        // Add the arena's extra chunks to our own, so their memory remains live
        // after the arena gets dropped. This is important, because at this
        // point their pointers can still potentially be dereferenced!
        self.extra_chunks.extend(arena.extra_chunks);

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

pub struct ArenaIter<T> {
    ptr: *mut T,
    quantity_remaining: usize,
    first_chunk_capacity: usize,
}

// Implement `Iterator` for `Fibonacci`.
// The `Iterator` trait only requires a method to be defined for the `next` element.
impl<T> Iterator for ArenaIter<T> {
    type Item = Arena<T>;

    // Here, we define the sequence using `.curr` and `.next`.
    // The return type is `Option<T>`:
    //     * When the `Iterator` is finished, `None` is returned.
    //     * Otherwise, the next value is wrapped in `Some` and returned.
    fn next(&mut self) -> Option<Arena<T>> {
        if self.quantity_remaining != 0 {
            let first_chunk_ptr = self.ptr;

            self.ptr = unsafe { self.ptr.add(self.first_chunk_capacity) };
            self.quantity_remaining -= 1;

            Some(Arena {
                first_chunk_ptr,
                first_chunk_len: 0,
                first_chunk_cap: self.first_chunk_capacity,
                extra_chunks: Vec::new(),
            })
        } else {
            None
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct Arena<T> {
    first_chunk_ptr: *mut T,
    first_chunk_len: usize,
    first_chunk_cap: usize,
    extra_chunks: Vec<Vec<T>>,
}

impl<T> Arena<T> {
    pub fn alloc(&mut self, val: T) -> ArenaRef<T> {
        let ptr: *mut T = if self.first_chunk_len < self.first_chunk_cap {
            // We have enough room in the first chunk for 1 allocation.
            self.first_chunk_len += 1;

            // Return a pointer to the next available slot.
            unsafe { self.first_chunk_ptr.add(self.first_chunk_len) }
        } else {
            // We ran out of space in the first chunk, so we turn to extra chunks.
            // First, ensure that we have an extra chunk with enough space in it.
            match self.extra_chunks.last() {
                Some(chunk) => {
                    if chunk.len() >= chunk.capacity() {
                        // We've run out of space in our last chunk. Create a new one!
                        self.extra_chunks
                            .push(Vec::with_capacity(self.first_chunk_cap));
                    }
                }
                None => {
                    // We've never had extra chunks until now. Create the first one!
                    self.extra_chunks
                        .push(Vec::with_capacity(self.first_chunk_cap));
                }
            }

            let chunk = self.extra_chunks.last_mut().unwrap();
            let index = chunk.len();

            chunk.push(val);

            // Get a pointer to a memory address within our particular chunk.
            &mut chunk[index]
        };

        ArenaRef {
            ptr: unsafe { NonNull::new_unchecked(ptr) },
            _pin: PhantomPinned,
        }
    }

    fn alloc_vec(&mut self, num_elems: usize) -> *mut T {
        if self.first_chunk_len + num_elems <= self.first_chunk_cap {
            // We have enough room in the first chunk for this vec.
            self.first_chunk_len += num_elems;

            // Return a pointer to the next available element.
            unsafe { self.first_chunk_ptr.add(self.first_chunk_len) }
        } else {
            let new_chunk_cap = self.first_chunk_cap.max(num_elems);

            // We ran out of space in the first chunk, so we turn to extra chunks.
            // First, ensure that we have an extra chunk with enough space in it.
            match self.extra_chunks.last() {
                Some(chunk) => {
                    if chunk.len() + num_elems >= chunk.capacity() {
                        // We don't have enough space in our last chunk.
                        // Create a new one!
                        self.extra_chunks.push(Vec::with_capacity(new_chunk_cap));
                    }
                }
                None => {
                    // We've never had extra chunks until now. Create the first one!
                    self.extra_chunks.push(Vec::with_capacity(new_chunk_cap));
                }
            }

            let chunk = self.extra_chunks.last_mut().unwrap();
            let index = chunk.len();

            // Get a pointer to a memory address within our particular chunk.
            &mut chunk[index]
        }
    }
}

pub trait AsArena<T> {
    fn verify_ownership(&self, ptr: NonNull<T>);
}

impl<T> AsArena<T> for ArenaPool<T> {
    fn verify_ownership(&self, ptr: NonNull<T>) {
        verify_ownership(
            self.first_chunk.as_ptr(),
            self.first_chunk.capacity(),
            &self.extra_chunks,
            ptr.as_ptr(),
        );
    }
}

impl<T> AsArena<T> for Arena<T> {
    fn verify_ownership(&self, ptr: NonNull<T>) {
        verify_ownership(
            self.first_chunk_ptr,
            self.first_chunk_cap,
            &self.extra_chunks,
            ptr.as_ptr(),
        );
    }
}

fn verify_ownership<T>(
    first_chunk_ptr: *const T,
    first_chunk_cap: usize,
    extra_chunks: &[Vec<T>],
    ptr: *const T,
) {
    let addr = ptr as usize;
    let start_addr = first_chunk_ptr as usize;
    let end_addr = start_addr + first_chunk_cap;

    if start_addr <= addr && addr < end_addr {
        // This is within our first chunk's address space, so it's verified!
    } else {
        // This wasn't within our first chunk's address space, so we need
        // to see if we can find it in one of our extra_chunks.
        for chunk in extra_chunks {
            let start_addr = chunk.as_ptr() as usize;
            let end_addr = start_addr + chunk.capacity();

            if start_addr <= addr && addr < end_addr {
                // Found it! No need to loop anymore; verification passed.
                return;
            }
        }

        // The address wasn't within any of our chunks' bounds.
        // Panic to avoid use-after-free errors!
        internal_error!("Pointer ownership verification failed.");
    }
}
