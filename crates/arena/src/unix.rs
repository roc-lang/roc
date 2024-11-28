use crate::{
    alloc::{round_up_capacity, VirtualAlloc},
    byte_writer::ByteWriter,
};
use core::{
    cell::Cell,
    fmt::Write,
    num::NonZeroUsize,
    ptr::{self, NonNull},
};

pub(crate) struct Alloc;

impl VirtualAlloc for Alloc {
    fn virtual_alloc(capacity: usize) -> NonNull<u8> {
        unsafe {
            let ptr = mmap(
                ptr::null_mut(),
                round_up_capacity(capacity as usize, page_size()),
                PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANONYMOUS,
                -1, // fd
                0,  // offset
            );

            NonNull::new(ptr).unwrap_or_else(|| oom(capacity))
        }
    }

    fn virtual_dealloc(non_null: NonNull<u8>, capacity: usize) {
        unsafe {
            let failed = munmap(non_null.as_ptr(), round_up_capacity(capacity, page_size())) == -1;

            if failed && cfg!(debug_assertions) {
                perror(b"Roc compiler warning (becuase this is a debug build of the compiler): munmap failed\0".as_ptr());
            }
        }
    }
}

fn oom(capacity: usize) -> ! {
    const BEFORE_BYTES: &str =
        "The Roc compiler exited before finishing because it ran out of memory when trying to allocate ";
    const AFTER_BYTES: &str = " bytes.\0";

    // u64::MAX has 20 digits when written out.
    const BUF_CAPACITY: usize = BEFORE_BYTES.len() + AFTER_BYTES.len() + 20;

    let mut buf = [0u8; BUF_CAPACITY];
    let mut writer = ByteWriter(&mut buf);
    let res = write!(writer, "{}{}{}", BEFORE_BYTES, capacity, AFTER_BYTES);

    unsafe {
        if let Ok(()) = res {
            perror(writer.0.as_ptr());
        } else {
            // Writing to the buffer failed, so perror without printing the capacity.
            perror(
                b"The Roc compiler exited before finishing because it ran out of memory.\0"
                    .as_ptr(),
            );
        }

        abort();
    }
}

thread_local! {
    // This is a threadlocal so that there's no contention across threads. Each thread will only have to read it once, ever.
    static CACHED_PAGE_SIZE: Cell<Option<NonZeroUsize>> = Cell::new(None);
}

fn page_size() -> NonZeroUsize {
    CACHED_PAGE_SIZE.with(|cell| {
        cell.get().unwrap_or_else(|| {
            // Get the actual page size from the OS.
            match NonZeroUsize::new(unsafe { getpagesize() } as usize) {
                Some(size) => {
                    cell.set(Some(size));
                    size
                }
                None => {
                    const MSG: &[u8] = b"The Roc compiler exited before finishing because the operating system's getpagesize() returned 0.\n";

                    unsafe {
                        write(STDERR, MSG.as_ptr(), MSG.len());
                        abort();
                    };
                }
            }
        })
    })
}

const PROT_READ: i32 = 0x1;
const PROT_WRITE: i32 = 0x2;
const MAP_PRIVATE: i32 = 0x02;
const MAP_ANONYMOUS: i32 = 0x20;

const STDERR: i32 = 2;

extern "C" {
    fn mmap(addr: *mut u8, length: usize, prot: i32, flags: i32, fd: i32, offset: i64) -> *mut u8;
    fn munmap(addr: *mut u8, length: usize) -> i32;
    fn perror(msg: *const u8);
    fn write(fd: i32, buf: *const u8, n: usize) -> isize;
    fn abort() -> !;
    fn getpagesize() -> i32;
}
