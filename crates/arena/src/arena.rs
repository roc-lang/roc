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
    arena_ref::ArenaRef,
    ArenaRefMut,
};

#[cfg(not(wasm32))]
use fs::{FileMetadata, IoError, ReadFile, WriteFile};

use core::{
    alloc::Layout,
    cell::Cell,
    mem::{align_of, size_of, MaybeUninit},
    ptr::NonNull,
    slice,
};

#[cfg(debug_assertions)]
static NEXT_ID: core::sync::atomic::AtomicU64 = core::sync::atomic::AtomicU64::new(1);

#[derive(Debug)]
pub(crate) struct Header {
    /// The next address we want to allocate into.
    next: *mut u8,

    /// The original number of bytes we had available.
    /// This is stored as a number rather than a pointer
    /// so that we can store 0 here when this is a slice.
    /// That lets Drop know not to try to deallocate it.
    original_capacity: usize,

    /// When deallocating, this verifies (in debug builds only) that
    /// we're deallocating the correct pointer. This should be inferrable
    /// from the pointer being deallocated, but that relies on the assumption
    /// that Header will only ever be used in one place, and in one way: to
    /// point to the beginning of Content in an Arena.
    #[cfg(debug_assertions)]
    original_header_ptr: *mut Self,
}

impl Header {
    fn len(&self) -> usize {
        self.next as usize - self as *const Header as usize + self.original_capacity
    }
}

impl Drop for Header {
    fn drop(&mut self) {
        if self.original_capacity > 0 {
            unsafe {
                let header_ptr = (self as *mut Self).sub(1);

                #[cfg(debug_assertions)]
                {
                    // This must be wrapped in cfg(debug_assertions)
                    // instead of using debug_assert_eq! because the
                    // original_header_ptr field only exists in debug builds.
                    assert_eq!(header_ptr, self.original_header_ptr);
                }

                let layout = Layout::from_size_align_unchecked(
                    self.original_capacity + (size_of::<Header>()),
                    align_of::<Header>(),
                );

                dealloc_virtual(header_ptr.cast(), layout);
            }
        }
    }
}

#[derive(Debug)]
#[cfg_attr(not(debug_assertions), repr(transparent))]
pub struct Arena {
    /// Pointer to the first byte of our content, *not* to the actual header (as the type suggests).
    ///
    /// We do it this way so that:
    /// - Header's Drop impl will get run when this is dropped.
    /// - The pointer itself is to the content in order to make bump operations more efficeint.
    pub(crate) content: Cell<Header>,

    #[cfg(debug_assertions)]
    id: u64,
}

impl Arena {
    /// Create a new arena using some of this arena's memory, if possible.
    /// If there isn't enough memory, returns a new arena with its own allocation
    /// and doesn't use any of this arena's memory.
    ///
    /// # Safety
    /// The given pointer must point to at least `capacity` bytes, and `capacity` must
    /// be greater than size_of::<Header>().
    pub unsafe fn from_existing_allocation(mut ptr: NonNull<usize>, capacity_bytes: u32) -> Self {
        // We don't want to expose Header outside this module, but it's very important that
        // `ptr` points to something with the
        debug_assert_eq!(core::mem::align_of_val(ptr.as_mut()), align_of::<Header>());
        debug_assert!(capacity_bytes as usize > size_of::<Header>());

        let header_ptr: *mut Header = ptr.as_ptr().cast();
        let content_ptr = header_ptr.add(1);
        let next: usize;
        let capacity: usize;

        // On 64-bit targets, (content_ptr + capacity) can never overflow, so we can
        // do a normal wrapping addition.
        #[cfg(target_pointer_width = "64")]
        {
            capacity = capacity_bytes as usize;
            next = (content_ptr as usize) + capacity;
        }

        // On targets smaller than 64-bit, it's possible for (ptr + capacity) to overflow,
        // so we do saturating arithmetic to shrink the available capacity if that happens.
        #[cfg(not(target_pointer_width = "64"))]
        {
            next = (content_ptr as usize).saturating_add(capacity_bytes as usize);
            capacity = (next - (content_ptr as usize));
        }

        // Write the header into the slot before the first byte of content.
        *header_ptr = Header {
            next: next as *mut u8,
            original_capacity: capacity,

            #[cfg(debug_assertions)]
            original_header_ptr: header_ptr,
        };

        Self::from_content_ptr(content_ptr)
    }

    /// Returns the amount of bytes used in the given buffer. Note that this might be zero
    /// if there wasn't enough room to read the entire file in! (In that case, we would
    /// have made our own allocation and not used the given buffer.)
    #[cfg(not(wasm32))]
    pub fn from_file(
        file: &mut (impl ReadFile + FileMetadata),
        buf: &mut [u8],
    ) -> Result<(Self, usize), IoError> {
        let bytes_read = file.read_into(buf)?;
        let mut ptr = unsafe { NonNull::new_unchecked(buf.as_mut_ptr()) };
        let mut alloc_size;

        if bytes_read < buf.len() {
            // We didn't fill up the buffer, so we definitely successfully read everything!
            // Continue as normal.
            alloc_size = (bytes_read as usize).saturating_add(size_of::<Header>());
        } else {
            // If the bytes we read didn't seem to have fit in the buffer, try getting the
            // desired number of bytes out of file metadata, doing a virtual alloc for that
            // many bytes, and trying again. This still might fail if the file's size on disk
            // changed between when we read its metadata and when we're reading its contents,
            // so if it fails again, we double the requested allocation size and try again.
            //
            // Eventually either it will succeed or else an allocation will fail due to being
            // too large (after doubling so many times), which will end the process.
            let mut bytes_needed = file.size_on_disk()?;

            loop {
                // Allocate enough space for both the header and the actual capacity.
                // It should be safe to cast this u64 to usize because on 32-bit targets,
                // they should never have report having more than u32::MAX bytes anyway.
                alloc_size = (bytes_needed as usize).saturating_add(size_of::<Header>());

                let (buf, capacity_bytes) = alloc_virtual(unsafe {
                    Layout::from_size_align_unchecked(alloc_size, align_of::<Header>())
                });

                let bytes_read = file.read_into(unsafe {
                    slice::from_raw_parts_mut(buf.as_ptr(), capacity_bytes)
                })?;

                ptr = buf.cast();

                if bytes_read as u64 <= bytes_needed {
                    break;
                }

                // Somehow we still didn't allocate enough space! Start doubling the amount of
                // bytes for the buffer, so that we will eventually either successfully read
                // all the bytes or else an allocation will fail, exiting the process. Either
                // way, this loop will eventually terminate.
                bytes_needed = bytes_needed.saturating_mul(2);
            }
        }

        if bytes_read >= u32::MAX as usize {
            return Err(IoError::NOT_ENOUGH_MEMORY);
        }

        Ok((
            unsafe { Self::from_existing_allocation(ptr.cast(), alloc_size as u32) },
            bytes_read,
        ))
    }

    /// Write the contents of the arena (without the header) to disk.
    /// (Header information can be inferred when reading the contents back from disk.)
    #[cfg(not(wasm32))]
    pub unsafe fn to_file(&self, file: &mut impl WriteFile) -> Result<(), IoError> {
        file.write(self.content())
    }

    pub fn with_capacity(capacity: u32) -> Self {
        // Safety: we know Header has a valid alignment.
        let layout = unsafe {
            Layout::from_size_align_unchecked(
                // Allocate enough space for both the header and the actual capacity
                size_of::<Header>().saturating_add(capacity as usize),
                align_of::<Header>(),
            )
        };

        // Get the actual capacity back (alloc may have given us more than we asked for,
        // after rounding up for page alignment etc.)
        let (non_null, allocated_bytes) = alloc_virtual(layout);

        // The allocated bytes include the header, so subtract that back out.
        // In the extremely unlikely event that we end up with zero somehow,
        // this will just mean we always reallocate whenever doing a new allocation.
        let content_capacity = allocated_bytes.saturating_sub(size_of::<Header>());
        let header_ptr = non_null.as_ptr() as *mut Header;
        let content_ptr = unsafe { header_ptr.add(1) };

        unsafe {
            *header_ptr = Header {
                next: content_ptr.add(content_capacity).cast(),
                original_capacity: content_capacity,

                #[cfg(debug_assertions)]
                original_header_ptr: header_ptr,
            };

            Self::from_content_ptr(content_ptr)
        }
    }

    pub fn reserve(&mut self, bytes: u32) {
        let todo = todo!();
    }

    unsafe fn from_content_ptr(content_ptr: *mut Header) -> Self {
        Self {
            content: Cell::from(*content_ptr),
            #[cfg(debug_assertions)]
            id: NEXT_ID.fetch_add(1, core::sync::atomic::Ordering::Relaxed),
        }
    }

    /// If there is not enough space in the current allocation, goes back to the OS to do a virtual
    /// allocation (or growing the heap on WASM). This will never copy existing allocations into a
    /// new location (unlike, say, a Vec would when it resizes); instead, it will create new OS
    /// allocations as needed. When the arena gets dropped, all of those allocations will be
    /// returned to the OS (or marked as free in the wasm allocator).
    ///
    /// This is based on bumpalo's `alloc_with` - see bumpalo's docs on why the Fn can improve perf:
    /// https://docs.rs/bumpalo/latest/bumpalo/struct.Bump.html#method.alloc_with
    pub fn alloc<T>(&mut self) -> ArenaRefMut<MaybeUninit<T>> {
        unsafe { self.alloc_layout(Layout::new::<T>()).cast() }
    }

    fn header(&self) -> &Header {
        // The header is stored right before the content
        unsafe { &*(self.content as *const Header).sub(1) }
    }

    fn header_mut(&mut self) -> &mut Header {
        // The header is stored right before the content
        unsafe { &mut *(self.content as *mut Header).sub(1) }
    }

    fn next(&self) -> *mut u8 {
        self.header().next
    }

    fn set_next(&mut self, next: *mut u8) {
        self.header_mut().next = next;
    }

    /// If there is not enough space in the current allocation, goes back to the OS to do a virtual
    /// allocation (or growing the heap on WASM). This will never copy existing allocations into a
    /// new location (unlike, say, a Vec would when it resizes); instead, it will create new OS
    /// allocations as needed. When the arena gets dropped, all of those allocations will be
    /// returned to the OS (or marked as free in the wasm allocator).
    pub fn alloc_layout(&mut self, layout: Layout) -> ArenaRefMut<u8> {
        let size = layout.size();
        let align = layout.align();
        let content_ptr = self.content as *const Header as *const u8 as usize;
        let ptr = self.next() as usize;
        let new_ptr = ptr.saturating_sub(size);

        debug_assert!(align > 0);

        // Round down to the requested alignment.
        let new_ptr = new_ptr & !(align - 1);

        if new_ptr < content_ptr {
            // Didn't have enough capacity!
            todo!("reallocate and copy");
        } else {
            self.set_next(new_ptr as *mut u8);

            // This won't overflow because if we're in this branch, new_ptr >= content_ptr
            ArenaRefMut::new_in((new_ptr - content_ptr) as u32, self)
        }
    }

    pub unsafe fn get_unchecked<'a, T>(&'a self, arena_ref: impl Into<ArenaRef<'a, T>>) -> &'a T {
        let arena_ref = arena_ref.into();

        #[cfg(debug_assertions)]
        {
            // This must be wrapped in cfg(debug_assertions)
            // instead of using debug_assert_eq! because the
            // arena.id field only exists in debug builds.
            assert_eq!(self.id, arena_ref.arena.id);
        }

        &*(self.content as *const Header as *const u8)
            .add(arena_ref.byte_offset())
            .cast()
    }

    fn content(&self) -> &[u8] {
        unsafe {
            slice::from_raw_parts(
                self.content as *const Header as *const u8,
                self.header().len(),
            )
        }
    }
}

#[cfg(all(test, not(wasm32)))]
mod from_file {
    use super::Arena;

    #[test]
    fn fails_if_read_fails() {
        use fs::{FileMetadata, IoError, ReadFile};

        struct SimFile;

        impl ReadFile for SimFile {
            fn read_into(&mut self, _buf: &mut [u8]) -> Result<usize, IoError> {
                Err(IoError::NOT_ENOUGH_MEMORY)
            }
        }

        impl FileMetadata for SimFile {
            fn size_on_disk(&mut self) -> Result<u64, IoError> {
                unreachable!("size_on_disk should not be called if the initial read fails.")
            }
        }

        let mut buf = [0; 24];
        let res = Arena::from_file(&mut SimFile, &mut buf);

        assert!(res.is_err());
    }

    #[test]
    fn retries_if_buffer_full() {
        use fs::{FileMetadata, IoError, ReadFile};
        const BUF_SIZE: usize = 24;

        struct SimFile;

        impl ReadFile for SimFile {
            fn read_into(&mut self, _buf: &mut [u8]) -> Result<usize, IoError> {
                // We fill up the entire buffer
                Ok(BUF_SIZE)
            }
        }

        impl FileMetadata for SimFile {
            fn size_on_disk(&mut self) -> Result<u64, IoError> {
                // We should end up reading the size of the file on disk.
                // It should report that it's bigger than the buffer.
                Ok(BUF_SIZE as u64 + 10)
            }
        }

        let mut buf = [0; BUF_SIZE];
        let (arena, bytes_read) = Arena::from_file(&mut SimFile, &mut buf)
            .expect("Arena::from_file failed, but was expected to succeed.");

        assert_eq!(BUF_SIZE, bytes_read);
        assert_eq!(58, arena.header().original_capacity);
    }
}
