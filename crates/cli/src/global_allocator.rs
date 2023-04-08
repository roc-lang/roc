use mimalloc::MiMalloc;
use std::{
    alloc::{GlobalAlloc, Layout},
    sync::atomic::{AtomicUsize, Ordering},
};

/// Use bump allocation instead of MiMalloc
static mut USE_BUMP_ALLOCATOR: bool = false;

/// How many bytes in the arena have already been handed out.
static mut ARENA_LEN: AtomicUsize = AtomicUsize::new(0);

/// Arena size for the bump allocator (in bytes). This is virtual memory that will be mmap'd in.
/// Because of demand paging, the OS will only translate this into physical memory when we run out
/// of space in previously mapped pages. So the only downside to a big allocation here is using up virtual addresses.
const ARENA_CAPACITY: usize = 1_000_000_000_000; // a terabyte of virtual memory should be enough for anybody

/// The backing memory for the arena
static mut ARENA_PTR: *mut u8 = std::ptr::null_mut();

pub struct RocGlobalAlloc;

#[cfg(unix)]
fn get_page_size() -> usize {
    unsafe { libc::sysconf(libc::_SC_PAGESIZE) as usize }
}

#[cfg(windows)]
fn get_page_size() -> usize {
    let mut system_info = std::mem::MaybeUninit::uninit();

    unsafe {
        winapi::um::sysinfoapi::GetSystemInfo(system_info.as_mut_ptr());

        system_info.assume_init().dwPageSize as usize
    }
}

impl RocGlobalAlloc {
    /// # Safety
    /// This function is not thread-safe! Only call it when you are sure nothing else is potentially changing bump mode
    /// or performing allocations when the bump allocator is active.
    pub unsafe fn enable_bump_mode() {
        // The arena has never been initialized, so mmap it.
        if ARENA_PTR.is_null() {
            let page_size = get_page_size();
            let bytes_to_mmap = ARENA_CAPACITY + (ARENA_CAPACITY % page_size);

            #[cfg(unix)]
            {
                ARENA_PTR = libc::mmap(
                    std::ptr::null_mut(),
                    bytes_to_mmap,
                    libc::PROT_READ | libc::PROT_WRITE,
                    libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                    -1,
                    0,
                ) as *mut u8;
            }

            #[cfg(windows)]
            {
                use winapi::um::memoryapi::VirtualAlloc;
                use winapi::um::winnt::{MEM_COMMIT, MEM_RESERVE, PAGE_READWRITE};

                ARENA_PTR = VirtualAlloc(
                    std::ptr::null_mut(),
                    bytes_to_mmap,
                    MEM_COMMIT | MEM_RESERVE,
                    PAGE_READWRITE,
                ) as *mut u8;
            }

            if ARENA_PTR.is_null() {
                panic!("Memory mapping failed when trying to initialize the global bump arena");
            }
        }

        // This should be set after initialization is complete; otherwise, using things like dbg! prior
        // to here will fail.
        USE_BUMP_ALLOCATOR = true;
    }

    /// # Safety
    /// This function is not thread-safe! Only call it when you are sure nothing else is potentially changing bump mode
    /// or performing allocations when the bump allocator is active.
    pub unsafe fn disable_bump_mode() {
        USE_BUMP_ALLOCATOR = false;
    }
}

fn is_inside_arena(ptr: *mut u8) -> bool {
    let end_of_arena = unsafe { ARENA_PTR.add(ARENA_CAPACITY) };

    unsafe { ptr >= ARENA_PTR && ptr < end_of_arena }
}

unsafe impl GlobalAlloc for RocGlobalAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        if USE_BUMP_ALLOCATOR {
            let align = layout.align();
            let bytes = layout.size();

            loop {
                let old_len = ARENA_LEN.load(Ordering::SeqCst);

                // Calculate padding necessary to end up with an address that satisfies our alignment requirements
                let padding = (align - old_len % align) % align;

                let new_len = old_len + padding + bytes;

                // Try to update ARENA_LEN, making sure it hasn't changed since we used it in our padding calculation.
                if ARENA_LEN
                    .compare_exchange(old_len, new_len, Ordering::SeqCst, Ordering::SeqCst)
                    .is_ok()
                {
                    // It hasn't changed; we're good! (Otherwise we'll automatically loop back and try again.)

                    // Store the new allocation immediately after the end of the previous allocation.
                    let alloc_ptr = ARENA_PTR.add(old_len + (align - old_len % align) % align);

                    // Make sure we didn't allocate past the end of the arena;
                    // otherwise if anyone tries to write into this pointer, we could get
                    // data corruption or a segfault. So return null instead of doing that.
                    return if new_len <= ARENA_CAPACITY {
                        alloc_ptr
                    } else {
                        std::ptr::null_mut()
                    };
                }
            }
        } else {
            MiMalloc.alloc(layout)
        }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        if !is_inside_arena(ptr) {
            MiMalloc.dealloc(ptr, layout)
        }
    }
}
