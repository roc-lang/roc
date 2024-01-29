use core::alloc::Layout;

/// OS-level virtual memory allocation and deallocation functions, for use in the `arena`` module.
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

#[cfg(wasm32)]
static WEE_ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[cfg(wasm32)]
const WEE_ALLOC_ALIGN: usize = 0x10000; // wasm memory pages are 64KiB

/// Returns the pointer and also how many bytes were actually allocated,
/// since it will round up to the nearest page size depending on target OS.
pub(crate) fn alloc_virtual(layout: Layout) -> (*mut u8, usize) {
    let size = layout.size();
    let align = layout.align();

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

        const MAP_FAILED: *mut c_void = !0 as *mut c_void;
        const PROT_READ: i32 = 1;
        const PROT_WRITE: i32 = 2;
        const MAP_PRIVATE: i32 = 0x0002;
        const MAP_ANONYMOUS: i32 = 0x0020;

        // Round up to nearest 4096B
        let size = {
            const PAGE_MULTIPLE: usize = 4096; // Pages must be a multiple of this.

            (size + (PAGE_MULTIPLE - 1)) & !(PAGE_MULTIPLE - 1)
        };

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

        if answer == MAP_FAILED {
            let todo = todo!("Handle mapping failed");
        } else {
            (answer as *mut u8, size)
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

        if ptr == ptr::null_mut::<c_void>() {
            let todo =
                todo!("Handle allocation failed. Use GetLastError to find out what happened.");
        }

        (ptr as *mut u8, size)
    }

    #[cfg(wasm32)]
    {
        use core::ptr;

        // Round up to nearest page
        let size = (size + (WEE_ALLOC_ALIGN - 1)) & !(WEE_ALLOC_ALIGN - 1);

        // Safety: We rounded up `size` to the correct multiple already.
        let ptr =
            unsafe { WEE_ALLOC.alloc(Layout::from_size_align_unchecked(size, WEE_ALLOC_ALIGN)) };

        if ptr.is_null() {
            let todo = todo!("Handle allocation error");
        }

        (ptr, size)
    }
}

pub(crate) unsafe fn dealloc_virtual(ptr: *mut u8, layout: Layout) {
    let size = layout.size();
    let align = layout.align();

    #[cfg(unix)]
    {
        use core::ffi::c_void;

        extern "C" {
            fn munmap(addr: *mut c_void, length: usize) -> i32;
        }

        if munmap(ptr as *mut c_void, size) < 0 {
            let todo = todo!("Check errno to see what the error was.");
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
        if VirtualFree(ptr as *mut c_void, 0, MEM_RELEASE) == 0 {
            let todo = todo!(
                "Handle error using GetLastError and incorporate size somehow to avoid unused warning: {}",
                size
            );
        }
    }

    #[cfg(wasm32)]
    {
        use core::{alloc::Layout, ptr};

        let ptr =
            unsafe { WEE_ALLOC.dealloc(Layout::from_size_align_unchecked(size, WEE_ALLOC_ALIGN)) };

        if ptr.is_null() {
            let todo = todo!("Handle deallocation error");
        }

        (ptr, size)
    }
}
