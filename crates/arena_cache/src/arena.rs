/// An arena allocator that uses bump allocation, and can be serialized to/from a file
/// in one syscall. (On the stack it's basically a slice; all of its other information is stored
/// at the beginning of its allocation, so it can be serialized to/from disk in one syscall.)
///
/// Instead of using the global allocator, it uses the `alloc` module to go directly to
/// the operating system. (See that module for details.) This is designed to avoid
/// memory usage creeping up over time in long-running compiler processes (e.g. watch mode,
/// editor integrations, the repl) because we actually give memory back to the OS when
/// we're done with it (e.g. a module gets unloaded).
use crate::alloc::{alloc_virtual, dealloc_virtual};

use core::{
    alloc::Layout,
    marker::PhantomData,
    mem::{align_of, size_of},
    num::NonZeroUsize,
    ptr::NonNull,
};

#[cfg(all(feature = "io", not(target_arch = "wasm32")))]
use std::{fs::File, io};

#[derive(Debug)]
pub struct Arena<'a> {
    /// The arena's Header is always stored at the beginning of this slice,
    /// and then afterwards the remaining bytes are the arena's contents.
    ///
    /// Storing the header in the arena itself allows the entire arena
    /// to be serialized to/from disk in 1 syscall.
    ///
    /// The pointer itself must be aligned to Header, but the total number
    /// of bytes afterwards does not have to be a multiple of Header's size.
    /// (So, `capacity` may not be a multiple of `size_of::<Header>()`.)
    contents: *mut u8,

    /// capacity is measured in bytes, and may not be a multiple of `size_of::<Header>()`.
    /// Note that capacity does *not* include the size of the header itself! This means
    /// that the total amount of allocated memory pointed to by `contents` is actually
    /// `capacity + size_of::<Header>()`
    capacity_bytes: usize,

    /// The amount of bytes actually in use.
    len: usize,

    _phantom: PhantomData<&'a ()>,
}

#[derive(Copy, Clone, Debug)]
pub struct Header {
    /// The number of bytes actually in use so far.
    len: usize,
}

impl<'a> Arena<'a> {
    pub fn with_capacity(capacity: usize) -> Self {
        // Safety: we know Header has a valid alignment.
        let layout = unsafe {
            Layout::from_size_align_unchecked(
                // Allocate enough space for both the header and the actual capacity
                size_of::<Header>().saturating_add(capacity),
                align_of::<Header>(),
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
                    NonZeroUsize::new(allocated_bytes.saturating_sub(size_of::<Header>()));
                let header = non_null.cast();

                unsafe {
                    *(header.as_ptr()) = Header { len: 0 };
                }

                Self {
                    header,
                    capacity_bytes,
                    _phantom: PhantomData::default(),
                }
            }
            None => {
                let todo = todo!("Handle allocation failure.");
            }
        }
    }

    pub fn alloc(&self, layout: Layout) -> *mut u8 {
        // See https://fitzgeraldnick.com/2019/11/01/always-bump-downwards.html for why it bumps
        // down!

        let todo = todo!("new design: see comment");
        // ok so we don't actually need to store a header I don't think.
        // if we're loading from disk then of course we don't have excess capacity, so who cares?
        // also in that world, len is on the stack anyway.
    }

    fn header(&self) -> Header {
        unsafe { *self.header.as_ref() }
    }

    fn header_mut(&mut self) -> &mut Header {
        unsafe { self.header.as_mut() }
    }

    pub fn capacity(&self) -> usize {
        let todo = (); // TODO verify in godbolt.org that this optimizes into a no-op!
        self.capacity_bytes.map(NonZeroUsize::get).unwrap_or(0)
    }

    pub fn len(&self) -> usize {
        self.header().len
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn contents(&self) -> &[u8] {
        unsafe {
            let after_header = self.header.as_ptr().add(1);

            core::slice::from_raw_parts(after_header.cast(), self.capacity())
        }
    }

    pub fn contents_mut(&mut self) -> &mut [u8] {
        unsafe {
            let after_header = (self.header.as_ptr()).add(1);

            core::slice::from_raw_parts_mut(after_header.cast(), self.capacity())
        }
    }

    /// Write all the arena's bytes to a file.
    #[cfg(all(feature = "io", not(target_arch = "wasm32")))]
    pub fn to_file(&self, file: &mut File) -> io::Result<usize> {
        use std::io::Write;

        file.write(self.contents())
    }

    /// Read all the bytes from the file into dest, and then adopt dest as this arena's ReadOnly storage.
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
    /// Safety: It must be safe to write a Header instance to the given pointer, and also it must
    /// be safe to write an additional `bytes_available_after_header` bytes immediately after that.
    #[cfg(all(feature = "io", not(target_arch = "wasm32")))]
    pub unsafe fn from_file(
        file: &mut File,
        dest: NonNull<Header>,
        bytes_available_after_header: usize,
    ) -> io::Result<Self> {
        use std::io::Read;

        // Safety: This file requires being given a pointer to at least size_of::<Header> bytes.
        let arena_contents = unsafe {
            core::slice::from_raw_parts_mut(
                // Skip over the first Header entry, then read into the bytes from there.
                dest.as_ptr().add(1).cast(),
                bytes_available_after_header,
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
        if bytes_read < arena_contents.len() {
            // Write the header into the beginning of the bytes.
            unsafe {
                *(dest.as_ptr()) = Header { len: bytes_read };

                return Ok(Self {
                    header: dest,
                    capacity_bytes: None, // No capacity, because this is someone else's memory!
                    _phantom: PhantomData::default(),
                });
            }
        }

        // There wasn't enough space in the given buffer. Therefore:
        // 1. Ask the OS for the exact size of the file (we don't do this by default, to avoid a syscall)
        // 2. Create a Growable arena using with_capacity
        // 3. Read into that arena.
        let file_size = file.metadata()?.len();

        if file_size > isize::MAX as u64 {
            let todo = todo!("file is too big to read! Return some sort of error.");
        }

        // Allocate 1 extra capacity byte so we can tell if we potentially had a partial read.
        // (This can theoretically happen if somehow the file grew between when we read its
        // size and when we're reading its contents.)
        let mut answer = Self::with_capacity((file_size as usize).saturating_add(1));
        let bytes_read = file.read(answer.contents_mut())?;

        // Record the correct length in the arena's header
        {
            let header = answer.header_mut();

            header.len = bytes_read;
        }

        if bytes_read >= answer.capacity() {
            let todo = todo!("Somehow the file grew between when we read its size and read its contents. Error out!");
        }

        Ok(answer)
    }
}

impl<'a> Drop for Arena<'a> {
    fn drop(&mut self) {
        let capacity = self.capacity_bytes;

        // If capacity was 0, we never allocated anything, so there's nothing to release!
        if capacity > 0 {
            unsafe {
                // Safety: we know Header has a valid alignment.
                let layout = Layout::from_size_align_unchecked(
                    // Allocate enough space for both the header and the actual capacity
                    size_of::<Header>().saturating_add(capacity),
                    align_of::<Header>(),
                );

                dealloc_virtual(self.contents_mut().as_mut_ptr(), layout);
            }
        }
    }
}
