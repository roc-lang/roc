use crate::{alloc, ArenaRef, ArenaRefMut};
use core::{
    alloc::Layout,
    mem::size_of,
    ptr::{self, NonNull},
};

#[cfg(not(target_arch = "wasm32"))]
use fs::{IoError, SelfClosingFile};

#[derive(Debug)]
pub(crate) struct Header {
    /// The next address we want to allocate into.
    pub next: *mut u8,

    /// The original number of bytes we had available.
    /// This is stored as a number rather than a pointer
    /// so that we can store 0 here when this is a slice.
    original_capacity: usize,
}

impl Header {
    pub unsafe fn from_raw_parts(next: *mut u8, original_capacity: usize) -> Self {
        Self {
            next,
            original_capacity,
        }
    }
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
