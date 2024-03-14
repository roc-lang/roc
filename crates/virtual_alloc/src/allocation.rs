/// OS-level virtual memory allocation and deallocation functions, for use in the `arena` module.
/// The goal here is to avoid managing free lists and instead to directly ask the OS for memory.
/// In long-running compiler processes (e.g. watch mode, editor integrations, repl), this can
/// prevent memory usage from slowly growing over time because we're actually goving memory
/// back to the OS when we're done with it.
///
/// Since we should only use these to allocate memory for an entire module at a time, this should
/// result in 1 total syscall per module, which should be fine in terms of performance.
///
/// As of this writing, wasm uses the wee_alloc crate to emulate virtual memory by managing a free
/// list behind the scenes, since wasm only supports growing the heap and that's it. Although
/// wasm doesn't have a watch mode, it does have long-running processes in the form of the repl
/// and also potentially in the future a playground.
use core::{alloc::Layout, cell::Cell, fmt, ptr::NonNull};

#[derive(Debug)]
pub struct Allocation {
    pages: Cell<Page>,
    layout: Layout,
    len: usize,
}

#[derive(Clone, Copy)]
#[repr(transparent)]
struct Page {
    _bytes: [u8; PAGE_SIZE],
}

impl fmt::Debug for Page {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Page")
    }
}

// All allocations will be rounded up to this number of bytes.
//
// https://devblogs.microsoft.com/oldnewthing/20210510-00/?p=105200
// 16KiB should be accepted by all Windows systems
#[cfg(any(windows, unix))]
const PAGE_SIZE: usize = 16384;

// All allocations will be rounded up to this number of bytes.
//
// In wasm, "each page is sized 64KiB" according to
// https://developer.mozilla.org/en-US/docs/webassembly/reference/memory/size
#[cfg(wasm32)]
const PAGE_SIZE: usize = 65536;

/// We use wee_alloc for allocations on wasm because wasm natively supports only growing the heap,
/// not releasing anything. Releasing has to be built in userspace, which wee_alloc provides.
#[cfg(wasm32)]
static WEE_ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[derive(Debug, PartialEq, Eq)]
pub struct AllocFailed;

impl Allocation {
    /// This may round the requested number of bytes up to the nearest page size,
    /// depending on target OS.
    pub fn alloc_virtual(layout: Layout) -> Result<Self, AllocFailed> {
        let size = layout.size();

        #[cfg(unix)]
        {
            use core::{ffi::c_void, ptr};

            extern "C" {
                fn mmap(
                    addr: *mut c_void,
                    length: usize,
                    prot: i32,
                    flags: i32,
                    fd: i32,
                    offset: i64,
                ) -> *mut c_void;
            }

            const MAP_FAILED: *mut c_void = -1isize as *mut c_void;
            const PROT_READ: i32 = 1;
            const PROT_WRITE: i32 = 2;
            const MAP_PRIVATE: i32 = 0x0002;

            #[cfg(target_os = "macos")]
            const MAP_ANONYMOUS: i32 = 0x1000;

            #[cfg(target_os = "linux")]
            const MAP_ANONYMOUS: i32 = 0x0020;

            // Round up to nearest OS page size or the requested alignment,
            // whichevever is bigger.
            let size = round_up_to(size, layout.align().max(PAGE_SIZE));

            // Safety: We rounded up `size` to the correct multiple already.
            let answer = unsafe {
                mmap(
                    ptr::null_mut(),
                    size,
                    PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANONYMOUS,
                    -1,
                    0,
                )
            };
            // We should never return a size smaller than what was requested!
            debug_assert!(size >= layout.size());

            match NonNull::new(answer) {
                Some(non_null) if answer != MAP_FAILED => {
                    // Once https://github.com/rust-lang/rust/issues/117691 lands,
                    // this can become non_null.read()
                    let pages = unsafe { (non_null.as_ptr() as *mut Page).read() };

                    Ok(Self {
                        pages: Cell::new(pages),
                        len: 0,
                        layout,
                    })
                }
                _ => {
                    #[cfg(debug_assertions)]
                    {
                        panic!("alloc_virtual for layout {:?} and rounded-off size {} failed with return value {} (MAP_FAILED == it? {}), and OS error {:?}", layout, size, answer as usize, MAP_FAILED == answer, std::io::Error::last_os_error());
                    }

                    #[cfg(not(debug_assertions))]
                    {
                        crash::unrecoverable!(ALLOC_FAILED_EXIT_CODE, ALLOC_FAILED_MESSAGE)
                    }
                }
            }
        }

        #[cfg(windows)]
        {
            use core::{ffi::c_void, ptr};

            extern "system" {
                fn VirtualAlloc(
                    lpAddress: *mut c_void,
                    dwSize: usize,
                    flAllocationType: u32,
                    flProtect: u32,
                ) -> *mut c_void;
            }

            // Round up to nearest 4096B
            let size = {
                const PAGE_MULTIPLE: usize = 4096; // Pages must be a multiple of this.

                (size + (PAGE_MULTIPLE - 1)) & !(PAGE_MULTIPLE - 1)
            };

            const MEM_COMMIT: u32 = 0x1000;
            const MEM_RESERVE: u32 = 0x2000;;
            const PAGE_READWRITE: u32 = 0x04;

            // Safety: We rounded up `size` to the correct multiple already.
            let ptr = unsafe {
                VirtualAlloc(
                    ptr::null_mut(),
                    size,
                    MEM_COMMIT | MEM_RESERVE,
                    PAGE_READWRITE,
                )
            };

            // We should never return a size smaller than what was requested!
            debug_assert!(size >= layout.size());

            match NonNull::new(ptr) {
                Some(non_null) => (non_null.cast(), size),
                None => crash::unrecoverable!(ALLOC_FAILED_MESSAGE, ALLOC_FAILED_EXIT_CODE),
            }
        }

        #[cfg(wasm32)]
        {
            let ptr = unsafe { WEE_ALLOC.alloc(layout) };

            // We should never return a size smaller than what was requested!
            debug_assert!(size >= layout.size());

            match NonNull::new(ptr) {
                Some(non_null) => (non_null.cast(), size),
                None => {
                    extern "C" {
                        fn alloc_failed(ptr: *const u8, len: usize) -> !;
                    }

                    alloc_failed(ALLOC_FAILED_MESSAGE.as_ptr(), ALLOC_FAILED_MESSAGE.len());
                }
            }
        }
    }

    pub fn bytes_remaining(&self) -> usize {
        self.layout.size().saturating_sub(self.len)
    }

    pub fn slice_mut(&mut self, layout: Layout) -> &mut [u8] {
        // We won't return a slice that's bigger than the number of
        // allocated bytes we have left!
        let available_len = self.bytes_remaining();
        let desired_len = available_len.min(layout.size());
        let desired_align = self.layout.align().max(layout.align());

        // Figure out how much padding we need to achieve the desired alignment
        let ptr = self.pages.as_ptr() as *mut u8;
        let padding_needed = round_up_to(ptr as usize, desired_align).saturating_sub(ptr as usize);

        // Figure out what the actual length of the slice will be,
        // taking into account necessary padding and how mny bytes are left.
        let actual_len = desired_len
            .saturating_add(padding_needed)
            .min(available_len);

        // Advance the pointer past the padding.
        let ptr = unsafe { ptr.add(padding_needed) };

        // After adding the padding, the pointer should now be aligned correctly!
        debug_assert_eq!(ptr as usize % desired_align, 0);

        // Record the new length
        self.len += actual_len;

        unsafe { core::slice::from_raw_parts_mut(ptr, actual_len) }
    }
}

impl Drop for Allocation {
    fn drop(&mut self) {
        let ptr = self.pages.as_ptr();
        let layout = self.layout;
        let size = layout.size();

        #[cfg(unix)]
        {
            use core::ffi::c_void;

            extern "C" {
                fn munmap(addr: *mut c_void, length: usize) -> i32;
            }

            // If deallocation fails, panic in debug builds so we can try to diagnose it
            // (and so that it will fail tests), but silently continue in release builds
            // because a memory leak is generally a better user experience than a crash.
            let _answer = unsafe { munmap(ptr as *mut c_void, size) };

            #[cfg(debug_assertions)]
            {
                if _answer < 0 {
                    panic!("Tried to deallocate address {:?} but it failed!", ptr);
                }
            }
        }

        #[cfg(windows)]
        {
            use core::ffi::c_void;

            extern "system" {
                fn VirtualFree(lpAddress: *mut c_void, dwSize: usize, dwFreeType: u32) -> i32;
            }

            const MEM_RELEASE: u32 = 0x8000;

            // When calling VirtualAlloc with MEM_RELEASE, the second argument must be 0.
            // https://learn.microsoft.com/en-us/windows/win32/api/memoryapi/nf-memoryapi-virtualfree#parameters
            let _answer = unsafe { VirtualFree(ptr as *mut c_void, 0, MEM_RELEASE) };

            #[cfg(debug_assertions)]
            {
                if _answer == 0 {
                    panic!("Tried to deallocate address {:?} but it failed!", ptr);
                }
            }
        }

        #[cfg(wasm32)]
        {
            let _ptr = unsafe { WEE_ALLOC.dealloc(layout) };

            // If deallocation fails, panic in debug builds so we can try to diagnose it
            // (and so that it will fail tests), but silently continue in release builds
            // because a memory leak is generally a better user experience than a crash.
            #[cfg(debug_assertions)]
            {
                if _ptr.is_null() {
                    panic!("Tried to deallocate address {:?} but it failed!", ptr);
                }
            }
        }
    }
}

#[test]
fn verify_page_size() {
    let os_page_size = unsafe {
        #[cfg(unix)]
        {
            extern "C" {
                fn getpagesize() -> i32;
            }

            getpagesize() as usize
        }

        #[cfg(windows)]
        {
            // https://devblogs.microsoft.com/oldnewthing/20210510-00/?p=105200
            // 16KiB should be accepted by all Windows systems
            16384
        }

        #[cfg(wasm)]
        {
            // In wasm, "each page is sized 64KiB" according to
            // https://developer.mozilla.org/en-US/docs/webassembly/reference/memory/size
            65536
        }
    };

    assert_eq!(os_page_size, PAGE_SIZE);
}

fn round_up_to(num: usize, rounding_to: usize) -> usize {
    (num + (rounding_to - 1)) & !(rounding_to - 1)
}
