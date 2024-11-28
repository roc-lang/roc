use crate::alloc::VirtualAlloc;
use core::{
    cell::Cell,
    marker::PhantomData,
    mem,
    ptr::{self, NonNull},
    slice,
};
use fs::FileIoErr;
use soa::{Index, Slice};

#[cfg(unix)]
use crate::unix::Alloc;

#[cfg(windows)]
use crate::windows::Alloc;

#[cfg(target_family = "wasm")]
use crate::wasm::Alloc;

pub struct Arena<Header> {
    /// This points to a Header followed by a bunch of bytes after it.
    data: NonNull<Header>,
    capacity: u32,
    len: u32,
    _phantom: PhantomData<Header>,
}

thread_local! {
    static LEAKED_ALLOCATION: Cell<Option<NonNull<u8>>> = Cell::new(None);
    static LEAKED_ALLOCATION_LEN: Cell<usize> = Cell::new(0);
}

/// A terabyte of virtual address space per thread should be enough for anyone.
const LEAKED_ALLOCATION_CAPACITY: usize = (1024 * 1024 * 1024) as usize;

impl<Header> Arena<Header> {
    pub fn with_capacity(capacity: u32, header: Header) -> Self {
        with_capacity::<Alloc>(capacity, header)
    }

    pub fn with_capacity_leak(capacity: u32, header: Header) -> &'static mut Self {
        // Basic idea here: if we know we're planning to leak this, just get
        // it from our big one-time threadlocal virtual alloc instead of doing several
        // of them. We will ptr::write the arena so its destructor never runs.
        let todo = todo!();
    }

    pub fn header(&self) -> &Header {
        unsafe { self.data.as_ref() }
    }

    pub fn header_mut(&mut self) -> &mut Header {
        unsafe { self.data.as_mut() }
    }

    /// Read the entire contents of the file into this arena, in a single syscall.
    /// This gets put into a "leaked" virtual allocation that never gets deallocated,
    /// as does the Arena itself. If the file read was successful, we return a &'static mut
    /// pointer to the Arena (that's allocated inside the never-freed heap memory).
    ///
    /// This is designed specifically for reading cache files from disk as efficiently
    /// as possible during non-watch builds. In that situation, it's much more efficient
    /// to do one gigantic virtual allocation up front, and never free it, compared to
    /// doing a separate virtual allocation for every single arena (when we're never going
    /// to free any of them anyway).
    #[cfg(not(target_family = "wasm"))]
    pub fn from_file(file: &mut fs::File) -> Result<&'static Self, FileIoErr> {
        from_file::<Alloc, Header>(file)
    }

    pub fn alloc<'a, T>(&'a mut self, val: T) -> Index<T> {
        let len = self.len as usize;
        let dest = self.data.as_ptr().wrapping_add(len);

        // Ensure alignment is correct
        let align_padding = dest.align_offset(mem::align_of::<T>());
        let len = (len + align_padding + mem::size_of::<T>()) as u32;

        self.reserve(len);

        self.len = len;

        let aligned_addr = dest.wrapping_add(align_padding);

        unsafe {
            ptr::write(aligned_addr as *mut T, val);
        }

        Index::new((aligned_addr as *const u8).wrapping_sub(self.data.as_ptr() as usize) as u32)
    }

    pub fn get<'a, T>(&'a self, id: Index<T>) -> &'a T {
        unsafe {
            let ptr = self.data.as_ptr().wrapping_add(id.index as usize) as *const T;
            &*ptr
        }
    }

    pub fn get_mut<'a, T>(&'a mut self, id: Index<T>) -> &'a mut T {
        unsafe {
            let ptr = self.data.as_ptr().wrapping_add(id.index as usize) as *mut T;
            &mut *ptr
        }
    }

    // pub fn reserve(&mut self, amount: u32) {
    //     let old_capacity = self.capacity;

    //     // There's nothing to do if we already have enough capacity.
    //     if amount <= old_capacity {
    //         return;

    //     let page_size = alloc

    //     unsafe {
    //         let min_new_capacity = amount as usize + mem::size_of::<Header>() + RESERVED_BYTES;
    //         let grown_capacity = (old_capacity as usize + RESERVED_BYTES) / 2 * 3;
    //         let new_capacity =
    //             (((min_new_capacity.max(grown_capacity) + PAGE_SIZE - 1) / PAGE_SIZE) * PAGE_SIZE)
    //                 - RESERVED_BYTES;

    //         #[cfg(unix)]
    //         {
    //             let hint_addr = self
    //                 .data
    //                 .as_ptr()
    //                 .wrapping_add((old_capacity + RESERVED_BYTES) as usize);
    //             let ptr = mmap(
    //                 hint_addr as *mut u8,
    //                 (new_capacity + RESERVED_BYTES) as usize,
    //                 0x1 | 0x2, // PROT_READ | PROT_WRITE
    //                 0x22,      // MAP_PRIVATE | MAP_ANONYMOUS
    //                 -1,
    //                 0,
    //             );
    //             if ptr.is_null() {
    //                 panic!("mmap returned null");
    //             }

    //             if ptr as *mut u8 != hint_addr as *mut u8 {
    //                 // Not contiguous, need new allocation
    //                 let new_ptr = mmap(
    //                     ptr::null_mut(),
    //                     (new_capacity + RESERVED_BYTES) as usize,
    //                     PROT_READ | PROT_WRITE,
    //                     MAP_PRIVATE | MAP_ANONYMOUS,
    //                     -1, // file descriptor
    //                     0,  // offset
    //                 );
    //                 if new_ptr.is_null() {
    //                     panic!("mmap returned null");
    //                 }

    //                 // Round up to the nearest 16B
    //                 let iterations = ((self.len as usize + 15) & !15) / 16;
    //                 let mut src = self.data.as_ptr();
    //                 let mut dest = new_ptr;

    //                 let todo = (); // TODO make this implemented in terms of a trait for filesystem mocking, with virtual alloc and dealloc, etc. also make a function for like "alloc failed" or whatever, which can be abort normally but in tests can verify that it gets called etc.

    //                 // Copy data to new allocation
    //                 #[cfg(target_arch = "x86_64")]
    //                 {
    //                     // Copy 16B chunks
    //                     for _ in 0..iterations {
    //                         use core::arch::x86_64::*;

    //                         _mm_store_si128(
    //                             dest as *mut __m128i,
    //                             _mm_load_si128(src as *const __m128i),
    //                         );
    //                         src = src.add(16);
    //                         dest = dest.add(16);
    //                     }
    //                 }

    //                 #[cfg(target_arch = "aarch64")]
    //                 {
    //                     // Copy 16B chunks
    //                     for _ in 0..iterations {
    //                         use core::arch::aarch64::*;

    //                         vst1q_u8(dest, vld1q_u8(src));
    //                         src = src.add(16);
    //                         dest = dest.add(16);
    //                     }
    //                 }

    //                 #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
    //                 ptr::copy_nonoverlapping(self.data.as_ptr(), new_ptr, self.len as usize);

    //                 let todo = (); // need to also clean up munmap (and windows equivalent) the original allocation if we're moving it!
    //                                // Clean up the attempted contiguous allocation
    //                 munmap(ptr, (new_capacity + RESERVED_BYTES) as usize);

    //                 self.data = NonNull::new(new_ptr as *mut u8).expect("mmap returned null");
    //             } else {
    //                 self.data = NonNull::new(ptr as *mut u8).expect("mmap returned null");
    //             }
    //         }

    //         #[cfg(windows)]
    //         {
    //             let hint_addr = self
    //                 .data
    //                 .as_ptr()
    //                 .wrapping_add(old_capacity + RESERVED_BYTES);
    //             let ptr = VirtualAlloc(
    //                 hint_addr,
    //                 new_capacity + RESERVED_BYTES,
    //                 0x1000 | 0x2000, // MEM_COMMIT | MEM_RESERVE
    //                 0x04,            // PAGE_READWRITE
    //             );

    //             if ptr.is_null() || ptr as *mut u8 != hint_addr {
    //                 // Not contiguous or failed, need new allocation
    //                 let new_ptr = VirtualAlloc(
    //                     ptr::null_mut(),
    //                     new_capacity + RESERVED_BYTES,
    //                     0x1000 | 0x2000, // MEM_COMMIT | MEM_RESERVE
    //                     0x04,            // PAGE_READWRITE
    //                 );
    //                 if new_ptr.is_null() {
    //                     panic!("VirtualAlloc returned null");
    //                 }

    //                 // Copy data to new allocation
    //                 ptr::copy_nonoverlapping(self.data.as_ptr(), new_ptr as *mut u8, self.len);

    //                 // Clean up original allocation
    //                 extern "system" {
    //                     fn VirtualFree(addr: *mut u8, size: usize, free_type: u32) -> i32;
    //                 }
    //                 VirtualFree(
    //                     self.data.as_ptr(),
    //                     0,
    //                     0x8000, // MEM_RELEASE
    //                 );

    //                 self.data =
    //                     NonNull::new(new_ptr as *mut u8).expect("VirtualAlloc returned null");
    //             } else {
    //                 self.data = NonNull::new(ptr as *mut u8).expect("VirtualAlloc returned null");
    //             }
    //         }

    //         #[cfg(target_arch = "wasm32")]
    //         {
    //             let new_ptr = wee_alloc::WeeAlloc::INIT.alloc(new_capacity + RESERVED_BYTES);
    //             let new_ptr = NonNull::new(new_ptr as *mut u8).expect("wee_alloc returned null");

    //             // Copy existing data
    //             ptr::copy_nonoverlapping(self.data.as_ptr(), new_ptr.as_ptr(), self.len);

    //             self.data = new_ptr;
    //         }

    //         self.capacity = new_capacity;
    //     }
    // }

    pub fn alloc_slice<T>(&mut self, capacity: usize) -> Slice<T> {
        let align = core::mem::align_of::<T>();
        let size = core::mem::size_of::<T>() * capacity;

        // Round up current length for alignment
        let start = (self.len as usize + align - 1) & !(align - 1);

        // Reserve space for the new slice
        self.reserve((start + size) as u32);

        Slice::new(start as u32, capacity as u16)
    }

    pub unsafe fn get_ptr<T>(&self, slice: Slice<T>) -> *mut T {
        self.data.as_ptr().wrapping_add(slice.start as usize) as *mut T
    }
}

fn from_file<A: VirtualAlloc, Header>(
    file: &mut fs::File,
) -> Result<&'static Arena<Header>, FileIoErr> {
    LEAKED_ALLOCATION.with(|alloc_cell| unsafe {
        LEAKED_ALLOCATION_LEN.with(|alloc_len_cell| {
            let mut alloc_len = alloc_len_cell.get();
            let mut arena_ptr = alloc_cell
                .get()
                // We allocate this but never dealloc it. That's why it's leaked!
                .unwrap_or_else(|| {
                    let answer = A::virtual_alloc(LEAKED_ALLOCATION_CAPACITY);
                    alloc_cell.set(Some(answer));
                    answer
                });

            // This loop will run exactly 1 or 2 times, and will early return
            // one way or another on either the first or second iteration.
            loop {
                let ptr = arena_ptr.as_ptr().add(mem::size_of::<Arena<Header>>());
                let original_len = alloc_len - mem::size_of::<Arena<Header>>();
                let buf = slice::from_raw_parts_mut(ptr.cast(), original_len);
                let result = file.read_into(buf);

                match u32::try_from(buf.len()) {
                    Ok(len) => {
                        if len as usize == original_len {
                            // We filled up the buffer completely,
                            // meaning the file read got truncated.
                            if original_len < LEAKED_ALLOCATION_CAPACITY {
                                // Reallocate a fresh leaked allocation and try again.
                                arena_ptr = A::virtual_alloc(LEAKED_ALLOCATION_CAPACITY);
                                alloc_len = 0;

                                alloc_cell.set(Some(arena_ptr));
                                alloc_len_cell.set(alloc_len);

                                continue;
                            } else {
                                // The file was too big even for a new leaked allocation!
                                return Err(FileIoErr::TooBig);
                            }
                        }

                        // Store the arena in the allocation, right before the contents of
                        // the file, and return a &'static pointer to it. (It can be &'static
                        // because we never deallocate this memory.) This way, it's clear that
                        // the arena is readonly, Send & Sync, and will never get dropped!
                        let arena_ptr = arena_ptr.as_ptr() as *mut Arena<Header>;

                        ptr::write(
                            arena_ptr,
                            Arena {
                                data: NonNull::new_unchecked(ptr.cast()),
                                capacity: len,
                                len,
                                _phantom: PhantomData,
                            },
                        );

                        return result.map(|_buf| mem::transmute(arena_ptr));
                    }
                    Err(_) => {
                        // The file was more than u32::MAX bytes large.
                        return Err(FileIoErr::TooBig);
                    }
                }
            }
        })
    })
}

impl<Header> Drop for Arena<Header> {
    fn drop(&mut self) {
        unsafe {
            Alloc::virtual_dealloc(
                self.data, capacity, /* TODO capacity plus whatever other stuff is involved */
            )
        };
    }
}
