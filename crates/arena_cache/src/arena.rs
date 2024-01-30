/// An arena allocator that uses bump allocation, and can be serialized to/from a file
/// in one syscall. (On the stack, the arena is just a pointer; all of its other info is stored
/// inside its allocation, which helps the arena be serializable to/from disk in one syscall.)
///
/// Instead of using the global allocator, it uses the `alloc` module to go directly to
/// the operating system. (See that module for details.) This is designed to avoid
/// memory usage creeping up over time in long-running compiler processes (e.g. watch mode,
/// editor integrations, the repl) because we actually give memory back to the OS when
/// we're done with it (e.g. a module gets unloaded).
use crate::{
    alloc::{alloc_virtual, dealloc_virtual},
    chunk::ChunkHeader,
};

use core::{
    alloc::Layout,
    marker::PhantomData,
    mem::{align_of, size_of},
    num::NonZeroUsize,
    ptr::{self, NonNull},
};

#[cfg(not(wasm32))]
use fs::{self, File};

#[derive(Debug)]
pub struct Arena<'a> {
    /// Pointer to the first byte of our chunk. Each chunk
    /// consists of a header followed by some number of content bytes
    /// (specified in the chunk's `capacity` field.)
    chunk: *mut ChunkHeader,

    _phantom: PhantomData<&'a ()>,
}

impl<'a> Arena<'a> {
    pub fn with_capacity(capacity: usize) -> Self {
        // Safety: we know Header has a valid alignment.
        let layout = unsafe {
            Layout::from_size_align_unchecked(
                // Allocate enough space for both the header and the actual capacity
                size_of::<ChunkHeader>().saturating_add(capacity),
                align_of::<ChunkHeader>(),
            )
        };

        // Get the actual capacity back (alloc may have given us more than we asked for,
        // after rounding up for page alignment etc.)
        let (ptr, allocated_bytes) = alloc_virtual(layout);

        match NonNull::new(ptr) {
            Some(non_null) => {
                // The allocated bytes include the header, so subtract that back out.
                // In the extremely unlikely event that we end up with zero somehow,
                // this will just mean we always reallocate whenever doing a new allocation.
                let capacity_bytes =
                    NonZeroUsize::new(allocated_bytes.saturating_sub(size_of::<ChunkHeader>()));
                let header = non_null.cast();

                unsafe {
                    *(header.as_ptr()) = ChunkHeader { len: 0 };
                }

                Self {
                    header,
                    capacity: capacity_bytes,
                    _phantom: PhantomData::default(),
                }
            }
            None => {
                let todo = todo!("Handle allocation failure.");
            }
        }
    }

    /// If there is not enough space in the current allocation, goes back to the OS to do a virtual
    /// allocation (or growing the heap on WASM). This will never copy existing allocations into a
    /// new location (unlike, say, a Vec would when it resizes); instead, it will create new OS
    /// allocations as needed. When the arena gets dropped, all of those allocations will be
    /// returned to the OS (or marked as free in the wasm allocator).
    pub fn alloc(&mut self, layout: Layout) -> Option<NonNull<u8>> {
        // Happy path: we have a chunk and it has room to allocate.
        if let Some(mut chunk) = NonNull::new(self.chunk) {
            if let Some(non_null) = unsafe { chunk.as_mut().alloc(layout) } {
                return Some(non_null);
            }
        }

        // If we get to this point, it means we need to allocate a new chunk!
        unsafe {
            // Safety: we know ChunkHeader has a valid alignment, and so should the
            // original layout.
            let layout = Layout::from_size_align_unchecked(
                // Allocate enough space for both the header and the actual capacity
                size_of::<ChunkHeader>()
                    .saturating_add(ChunkHeader::DEFAULT_CAPACITY.max((layout.size() * 3) / 2)),
                // Make sure the alignment works with both the requested alignment
                // as well as the chunk header.
                layout.align().max(align_of::<ChunkHeader>()),
            );
            let (ptr, actual_bytes) = alloc_virtual(layout);
            let ptr = ptr as *mut ChunkHeader;

            // Write the header into the beginning of the new allocation.
            *ptr = ChunkHeader::from_raw_parts(
                // This new chunk has our old chunk as its previous chunk.
                // (Our old chunk might be null; that's fine!)
                self.chunk,
                // The content capacity is the total allocated bytes minus the header
                actual_bytes.saturating_sub(size_of::<ChunkHeader>()),
                // This allocation takes up some space in the chunk.
                layout.size(),
            );

            // Our current chunk is now this chunk (and its prev is our old chunk).
            self.chunk = ptr;

            // Advance past the header when returning the allocated content.
            NonNull::new(ptr.add(1).cast())
        }
    }

    /// If the given pointer (and corresponding layout) happens to be the most recently allocated
    /// thing, then reset the bump pointer back to reclaim that memory. Returns whether any memory
    /// was successfully reclaimed.
    ///
    /// # Safety
    /// If this returns `true`, then the memory backing the given slice has been reclaimed,
    /// and referencing the slice ever again would be undefined behavior.
    pub unsafe fn try_dealloc(&'a mut self, slice: &'a mut [u8]) -> bool {
        match NonNull::new(self.chunk) {
            Some(mut chunk) => chunk.as_mut().try_dealloc(slice.as_mut_ptr(), slice.len()),
            None => false,
        }
    }

    /// Get a temporary ("scratch") arena based on this one, which gets completely deallocated
    /// once the function returns.
    pub fn alloc_scratch<T: Copy>(&mut self, run: impl FnOnce(&'a mut Self) -> T) -> T {
        let todo = todo!("implement");
    }

    pub fn capacity(&self) -> usize {
        // Return the current chunk's capacity.
        match NonNull::new(self.chunk) {
            Some(non_null) => unsafe { non_null.as_ref().capacity() },
            None => 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        match NonNull::new(self.chunk) {
            Some(non_null) => unsafe { non_null.as_ref().is_empty() },
            None => true,
        }
    }

    /// Write the arena's complete contents to a file. This should only take one syscall.
    #[cfg(all(feature = "io", not(target_arch = "wasm32")))]
    pub fn write_to_file(&self, file: &mut File) -> io::Result<usize> {
        match NonNull::new(self.chunk) {
            Some(non_null) => unsafe { non_null.as_ref().write_recursive(file) },
            None => {
                // If we don't have even one chunk, then we're not writing anything to the file.
                Ok(0)
            }
        }
    }

    /// Read all the bytes from the file into dest, and then adopt those bytes of dest as one big Chunk.
    /// That chunk will have capacity set to 0 so it knows not to try to deallocate itself.
    ///
    /// If the file was too big to fit into dest, then do a normal with_capacity and read into that instead.
    /// Note that this accepts a pointer to Header because that pointer must match Header's
    /// alignment. The total number of bytes available for writing (the next argument after the
    /// pointer) does not necessarily have to be a multiple of Header.
    ///
    /// The purpose of doing it this way is so that in a batch build, we can do one big virtual alloc up front
    /// for all the arenas we'll be reading from disk, and then give each of them a dest pointer into that allocation.
    /// That way, we can avoid all the syscalls for intermediate allocations. Note that the caller should
    /// check to see if the returned Self has Storage::ReadOnly, and if so, it should mark those bytes as used!
    ///
    /// In watch mode, we pass an empty slice here, which means each of these will do their own allocation.
    /// That's desirable in watch mode because we want to unload these and release their pages back to the OS
    /// after the cache gets invalidated.
    ///
    /// Safety: It must be safe to write `bytes_available` bytes to the given pointer.
    #[cfg(all(feature = "io", not(target_arch = "wasm32")))]
    pub unsafe fn read_from_file(
        file: &mut File,
        // We verify that align_of::<ChunkHeader>() == align_of::<usize>(),
        // so it's ok to accept a pointer to usize instead of to ChunkHeader.
        // This way, we don't have to expose ChunkHeader outside this crate.
        dest: NonNull<usize>,
        bytes_available: usize,
    ) -> io::Result<Self> {
        use core::slice;
        use std::io::Read;

        // Safety: This file requires being given a pointer to at least size_of::<Header> bytes.
        if let Some(after_header) = bytes_available.checked_sub(size_of::<ChunkHeader>()) {
            let arena_contents = unsafe {
                core::slice::from_raw_parts_mut(
                    // Skip over the first Header entry, then read into the bytes from there.
                    dest.as_ptr().add(1).cast(),
                    after_header,
                )
            };

            // Read the contents of the file *after* the header.
            // We'll write the header itself afterwards.
            let bytes_read = file.read(arena_contents)?;

            // If we read all the way to the end of the buffer, then the
            // file might have more data left to read! Therefore, bail out
            // and fall back on the with_capacity strategy.
            //
            // If we read less than all the way to the end of the buffer, then
            // we must have read the entire file's contents, and we're done!
            if bytes_read < after_header {
                let chunk_ptr = dest.as_ptr().cast();

                // Write the header into the beginning of the bytes.
                unsafe {
                    *(chunk_ptr) =
                        // We set capacity to 0 to indicate that it shouldn't be freed.
                        ChunkHeader::from_raw_parts(ptr::null_mut(), 0, bytes_read);
                }

                return Ok(Self {
                    chunk: chunk_ptr,
                    _phantom: PhantomData::default(),
                });
            }
        }

        // There wasn't enough space in the given buffer. Therefore:
        // 1. Ask the OS for the exact size of the file (we don't do this by default, to avoid a syscall)
        // 2. Create a chunk using with_capacity
        // 3. Read into that arena.
        let file_size = file.metadata()?.len();

        if file_size > isize::MAX as u64 {
            let todo = todo!("file is too big to read! Return some sort of error.");
        }

        // Safety: we know ChunkHeader has a valid alignment.
        let layout = Layout::from_size_align_unchecked(
            // Allocate 1 extra capacity byte so we can tell if we potentially had a partial read.
            // (This can theoretically happen if somehow the file grew between when we read its
            // size and when we're reading its contents.)
            size_of::<ChunkHeader>()
                .saturating_add(file_size as usize)
                .saturating_add(1),
            align_of::<ChunkHeader>(),
        );
        let (chunk_ptr, capacity) = alloc_virtual(layout);
        let capacity = capacity.saturating_sub(size_of::<ChunkHeader>());
        let bytes_read = {
            let slice: &mut [u8] = slice::from_raw_parts_mut(chunk_ptr.add(1).cast(), capacity);

            file.read(slice)?
        };

        if bytes_read >= capacity {
            let todo = todo!("Somehow the file grew between when we read its size and read its contents. Error out!");
        }

        *(chunk_ptr.cast()) = ChunkHeader::from_raw_parts(prev, capacity, bytes_read);

        Ok(Self {
            chunk: chunk_ptr.cast(),
            _phantom: PhantomData::default(),
        })
    }
}

impl<'a> Drop for Arena<'a> {
    fn drop(&mut self) {
        // Recursively deallocate each of the chunks.
        let mut chunk_ptr = self.chunk;

        // Note: the original chunk pointer might be null if the arena never allocated anything.
        while !chunk_ptr.is_null() {
            unsafe {
                let ptr_to_deallocate = chunk_ptr.cast();
                let capacity;

                // We only dereference chunk_ptr in here. Afterwards, we should be totally done
                // with the chunk, and should no longer need any reference to it because we're
                // going to deallocate it!
                {
                    let chunk = *chunk_ptr;

                    capacity = chunk.capacity();

                    // Continue on to the previous chunk once this is done.
                    chunk_ptr = chunk.prev();
                }

                // If capacity was 0, we never allocated anything (meaning we got this memory
                // from somoene else), so there's nothing to release!
                if capacity > 0 {
                    unsafe {
                        // Safety: we know ChunkHeader has a valid alignment.
                        let layout = Layout::from_size_align_unchecked(
                            // Allocate enough space for both the header and the actual capacity
                            size_of::<ChunkHeader>().saturating_add(capacity),
                            align_of::<ChunkHeader>(),
                        );

                        dealloc_virtual(ptr_to_deallocate, layout);
                    }
                }
            }
        }
    }
}

#[test]
fn test_chunk_align() {
    todo!("verify that ChunkHeader's alignment == usize's aligment, so that read_from_file's signature is correct");
}
