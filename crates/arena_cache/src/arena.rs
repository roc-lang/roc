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
use fs::{File, IoError};

use core::{
    alloc::Layout,
    marker::PhantomData,
    mem::{align_of, size_of},
    ptr::{self, NonNull},
    slice,
    sync::atomic::Ordering,
};

#[cfg(debug_assertions)]
static NEXT_ID: core::sync::atomic::AtomicU64 = 1;

#[cfg(not(wasm32))]
use fs::{self, IoError, SelfClosingFile};

#[derive(Debug)]
struct Header {
    /// The next address we want to allocate into.
    next: *mut u8,

    /// The original number of bytes we had available.
    /// This is stored as a number rather than a pointer
    /// so that we can store 0 here when this is a slice.
    original_capacity: usize,
}

impl Drop for Header {
    fn drop(&mut self) {
        if self.original_capacity > 0 {
            #[cfg(unix)]
            {
                extern "C" {
                    fn munmap(addr: *mut u8, length: usize) -> i32;
                }

                let _outcome = unsafe {
                    munmap(
                        (self as *mut Self).sub(1) as *mut u8,
                        self.original_capacity as usize,
                    )
                };

                // In release builds, handle the error gracefully by doing nothing.
                // Leaking memory is a better user experience than crashing!
                #[cfg(debug_assertions)]
                {
                    if _outcome < 0 {
                        panic!("munmap() failed when attempting to dealloc a storage header for a slice! This should never happen.");
                    }
                }
            }

            #[cfg(windows)]
            {
                extern "system" {
                    fn VirtualFree(lpAddress: *mut u8, dwSize: usize, dwFreeType: u32) -> i32;
                }

                const MEM_RELEASE: u32 = 0x8000;

                let _outcome = unsafe { VirtualFree(self as *mut u8, 0, MEM_RELEASE) };

                // In release builds, handle the error gracefully by doing nothing.
                // Leaking memory is a better user experience than crashing!
                #[cfg(debug_assertions)]
                {
                    if _outcome == 0 {
                        panic!("VirtualFree() failed when attempting to dealloc a storage header for a slice! This should never happen.");
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
#[cfg_attr(not(debug_assertions), repr(transparent))]
pub struct Arena<'a> {
    /// Pointer to the first byte of our content, *not* to the actual header (as the type suggests).
    ///
    /// We do it this way so that:
    /// - Because it's a reference to a header, Header's Drop impl will get run when this is dropped.
    /// - The pointer itself is to the content in order to make bump operations more efficeint.
    content: &'a mut Header,

    #[cfg(debug_assertions)]
    id: u64,
}

impl<'a> Arena<'a> {
    /// Create a new arena using some of this arena's memory, if possible.
    /// If there isn't enough memory, returns a new arena with its own allocation
    /// and doesn't use any of this arena's memory.
    ///
    /// # Safety
    /// The given pointer must point to at least `capacity` bytes, and `capacity` must
    /// be greater than size_of::<Header>().
    pub unsafe fn from_existing_allocation(mut ptr: NonNull<u16>, mut capacity_bytes: u32) -> Self {
        // We don't want to expose Header outside this module, but it's very important that
        // `ptr` points to something with the
        debug_assert_eq!(core::mem::align_of_val(ptr.as_mut()), align_of::<Header>()); // TODO test this with u16, should fail, then change to usize
        debug_assert!(capacity_bytes as usize > size_of::<Header>());

        let header_ptr: *mut Header = ptr.as_ptr().cast();
        let content_ptr = header_ptr.add(1);
        let next: usize;
        let capacity: usize;

        // On 64-bit targets, (content_ptr + capacity) can never overflow, so we can
        // do a normal wrapping addition.
        #[cfg(target_pointer_width = "64")]
        {
            next = (content_ptr as usize) + (capacity as usize);
            capacity = capacity as usize;
        }

        // On targets smaller than 64-bit, it's possible for (ptr + capacity) to overflow,
        // so we do saturating arithmetic to shrink the available capacity if that happens.
        #[cfg(not(target_pointer_width = "64"))]
        {
            next = (content_ptr as usize).saturating_add(capacity_bytes as usize);
            capacity_bytes = (next - (content_ptr as usize));
        }

        // Write the header into the slot before the first byte of content.
        *header_ptr = Header {
            next: next as *mut u8,
            original_capacity: capacity,
        };

        Self::from_content_ptr(content_ptr)
    }

    /// Returns the amount of bytes used in the given buffer. Note that this might be zero
    /// if there wasn't enough room to read the entire file in! (In that case, we would
    /// have made our own allocation and not used the given buffer.)
    #[cfg(not(wasm32))]
    pub unsafe fn from_file(
        file: &mut impl File,
        mut ptr: NonNull<u16>,
        mut capacity_bytes: u32,
    ) -> Result<(Self, usize), IoError> {
        let mut bytes_read = file.read_into(unsafe {
            slice::from_raw_parts_mut(ptr.as_ptr() as *mut u8, capacity_bytes as usize)
        })?;

        // If the bytes we read didn't seem to have fit in the buffer, try getting the
        // desired number of bytes out of file metadata, doing an allocation for that
        // many bytes, and trying again. This still might fail if the file's size on disk
        // changed between when we read its metadata and when we're reading its contents,
        // so if it fails again, we double the requested allocation size and try again.
        //
        // Eventually either it will succeed or else an allocation will fail due to being
        // too large (after doubling so many times), which will end the process.
        if bytes_read >= capacity_bytes {
            let mut bytes_needed = file.size_on_disk()?;

            loop {
                let layout = unsafe {
                    Layout::from_size_align_unchecked(
                        // Allocate enough space for both the header and the actual capacity.
                        // It should be safe to cast this u64 to usize because on 32-bit targets,
                        // they should never have report having more than u32::MAX bytes anyway.
                        (bytes_needed as usize).saturating_add(size_of::<Header>() + 1),
                        align_of::<Header>(),
                    )
                };
                let (mut buf, capacity_bytes) = alloc_virtual(layout);

                bytes_read = file.read_into(unsafe {
                    slice::from_raw_parts_mut(buf.as_ptr(), capacity_bytes as usize)
                })?;

                ptr = buf;

                if bytes_read < capacity_bytes {
                    break;
                }

                // Somehow we still didn't allocate enough space! Start doubling the amount of
                // bytes for the buffer, so that we will eventually either successfully read
                // all the bytes or else an allocation will fail, exiting the process. Either
                // way, this loop will eventually terminate.
                bytes_needed = bytes_needed.saturating_mul(2);
            }
        }

        if bytes_read >= u32::MAX as u64 {
            return IoError::NOT_ENOUGH_MEMORY;
        }

        Ok(Self::from_existing_allocation(ptr, bytes_read as u32))
    }

    #[cfg(not(wasm32))]
    pub unsafe fn to_file(file: &mut impl File) -> Result<((), usize), IoError> {}

    pub fn with_capacity(capacity: u32) -> Self {
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
        let (non_null, allocated_bytes) = alloc_virtual(layout);

        // The allocated bytes include the header, so subtract that back out.
        // In the extremely unlikely event that we end up with zero somehow,
        // this will just mean we always reallocate whenever doing a new allocation.
        let content_capacity = allocated_bytes.saturating_sub(size_of::<Header>());
        let header_ptr = non_null.as_ptr().cast();

        unsafe {
            *header_ptr = Header::from_raw_parts(ptr::null_mut(), content_capacity, 0);

            Self::from_content_ptr(header_ptr.add(1))
        }
    }

    unsafe fn from_content_ptr(content_ptr: *mut Header) -> Self {
        Self {
            content: &mut *content_ptr,
            #[cfg(debug_assertions)]
            id: NEXT_ID.fetch_add(1, Ordering::Relaxed),
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
    pub fn alloc<T>(&mut self, make: impl FnOnce() -> T) -> ArenaRef<T> {
        // This implementation is adapted from Bumpalo copyright (c) Nick Fitzgerald,
        // licensed under the Apache License, Version 2.0
        #[inline(always)]
        unsafe fn inner_writer<T, F>(ptr: *mut T, f: F)
        where
            F: FnOnce() -> T,
        {
            // This function is translated as:
            // - allocate space for a T on the stack
            // - call f() with the return value being put onto this stack space
            // - memcpy from the stack to the heap
            //
            // Ideally we want LLVM to always realize that doing a stack
            // allocation is unnecessary and optimize the code so it writes
            // directly into the heap instead. It seems we get it to realize
            // this most consistently if we put this critical line into it's
            // own function instead of inlining it into the surrounding code.
            ptr::write(ptr, f())
        }

        let layout = Layout::new::<T>();

        unsafe {
            let p = self.alloc_layout(layout);
            let p = p.as_mut().as_ptr() as *mut T;
            inner_writer(p, make);
            &mut *p
        }
    }

    pub fn capacity(&self) -> usize {
        // Return the current chunk's capacity.
        match NonNull::new(self.header) {
            Some(non_null) => unsafe { non_null.as_ref().capacity() },
            None => 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        match NonNull::new(self.header) {
            Some(non_null) => unsafe { non_null.as_ref().is_empty() },
            None => true,
        }
    }

    /// Write the arena's complete contents to a file. This should only take one syscall.
    #[cfg(not(wasm32))]
    pub fn write_to_file(&self, file: &mut SelfClosingFile) -> Result<usize, IoError> {
        match NonNull::new(self.header) {
            Some(non_null) => unsafe { non_null.as_ref().write_recursive(file) },
            None => {
                // If we don't have even one chunk, then we're not writing anything to the file.
                Ok(0)
            }
        }
    }

    fn header(&self) -> Header {
        // The header is stored right before the content
        unsafe { *(self.content as *const Header).sub(1) }
    }

    fn header_mut(&mut self) -> &mut Header {
        // The header is stored right before the content
        unsafe { &mut *(self.content as *mut Header).sub(1) }
    }

    fn next(&self) -> *mut u8 {
        self.header().next
    }

    fn set_next(&self, next: *mut u8) {
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

    pub unsafe fn get_unchecked<T>(&self, index: ArenaRef<T>) -> &T {
        &*self.content().add(index.byte_offset()).cast()
    }

    fn bytes_remaining(&self) -> u32 {
        todo!()
    }
}

impl<'a> Drop for Arena<'a> {
    fn drop(&mut self) {
        // You're supposed to call either mem::forget() or arena.dealloc() on an
        // arena before it gets dropped.
        panic!("An arena was dropped without having been properly disposed of!");
    }
}
