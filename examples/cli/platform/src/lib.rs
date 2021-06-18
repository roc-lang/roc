#![allow(non_snake_case)]

use core::alloc::Layout;
use core::ffi::c_void;
use core::mem;
use core::mem::MaybeUninit;
use errno::{errno, Errno};
use libc::{self, c_char, c_int};
use roc_std::{alloca, RocCallResult, RocList, RocResult, RocStr};

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed"]
    fn roc_main(output: *mut u8) -> ();

    #[link_name = "roc__mainForHost_1_size"]
    fn roc_main_size() -> i64;

    #[link_name = "roc__mainForHost_1_Fx_caller"]
    fn call_Fx(
        flags: &'static u8,
        function_pointer: *const u8,
        closure_data: *const u8,
        output: *mut u8,
    ) -> ();

    #[allow(dead_code)]
    #[link_name = "roc__mainForHost_1_Fx_size"]
    fn size_Fx() -> i64;

    #[link_name = "roc__mainForHost_1_Fx_result_size"]
    fn size_Fx_result() -> i64;

    fn malloc(size: usize) -> *mut c_void;
    fn realloc(c_ptr: *mut c_void, size: usize) -> *mut c_void;
    fn free(c_ptr: *mut c_void);
}

#[no_mangle]
pub unsafe fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    return malloc(size);
}

#[no_mangle]
pub unsafe fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    return realloc(c_ptr, new_size);
}

#[no_mangle]
pub unsafe fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    return free(c_ptr);
}

#[no_mangle]
pub fn roc_fx_putChar(foo: i64) -> () {
    let character = foo as u8 as char;
    print!("{}", character);

    ()
}

#[no_mangle]
pub fn roc_fx_putLine(line: RocStr) -> () {
    let bytes = line.as_slice();
    let string = unsafe { std::str::from_utf8_unchecked(bytes) };
    println!("{}", string);

    ()
}

#[no_mangle]
pub fn roc_fx_httpGetUtf8(url: RocStr) -> RocResult<RocStr, RocStr> {
    match ureq::get(unsafe { url.as_str() }).call() {
        Ok(resp) => match resp.into_string() {
            Ok(contents) => RocResult::Ok(RocStr::from_slice(contents.as_bytes())), // TODO make roc::Result!
            // TODO turn this error into an enum!
            Err(err) => RocResult::Err(RocStr::from_slice(format!("{:?}", err).as_bytes())),
        },
        // TODO turn this error into an enum!
        Err(err) => RocResult::Err(RocStr::from_slice(format!("{:?}", err).as_bytes())),
    }
}

#[no_mangle]
pub fn roc_fx_getLine() -> RocStr {
    use std::io::{self, BufRead};

    let stdin = io::stdin();
    let line1 = stdin.lock().lines().next().unwrap().unwrap();

    RocStr::from_slice(line1.as_bytes())
}

unsafe fn call_the_closure(function_pointer: *const u8, closure_data_ptr: *const u8) -> i64 {
    let size = size_Fx_result() as usize;

    alloca::with_stack_bytes(size, |buffer| {
        let buffer: *mut std::ffi::c_void = buffer;
        let buffer: *mut u8 = buffer as *mut u8;

        call_Fx(
            &0,
            function_pointer,
            closure_data_ptr as *const u8,
            buffer as *mut u8,
        );

        let output = &*(buffer as *mut RocCallResult<()>);

        match output.into() {
            Ok(_) => 0,
            Err(e) => panic!("failed with {}", e),
        }
    })
}

#[no_mangle]
pub fn rust_main() -> isize {
    let size = unsafe { roc_main_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();

    unsafe {
        let buffer = std::alloc::alloc(layout);

        roc_main(buffer);

        let output = &*(buffer as *mut RocCallResult<()>);

        match output.into() {
            Ok(()) => {
                let function_pointer = {
                    // this is a pointer to the location where the function pointer is stored
                    // we pass just the function pointer
                    let temp = buffer.offset(8) as *const i64;

                    (*temp) as *const u8
                };

                let closure_data_ptr = buffer.offset(16);

                let result =
                    call_the_closure(function_pointer as *const u8, closure_data_ptr as *const u8);

                std::alloc::dealloc(buffer, layout);

                result
            }
            Err(msg) => {
                std::alloc::dealloc(buffer, layout);

                panic!("Roc failed with message: {}", msg);
            }
        }
    };

    // Exit code
    0
}

<<<<<<< HEAD
/// A C file descriptor.
pub struct Fd(c_int);

#[no_mangle]
pub unsafe fn roc_fx_open(roc_path: RocStr) -> RocResult<Fd, Errno> {
    const BUF_BYTES: usize = 256;
=======
pub struct Fd(c_int);

#[no_mangle]
pub unsafe fn roc_fx_readAll(roc_path: RocStr) -> RocResult<RocList<u8>, Errno> {
    const BUF_BYTES: usize = 1024;

    // I know that since Rust 1.39 mem::uninitialized() is deprecated in favor
    // of MaybeUninit, but I couldn't get this to work with MaybeUninit.
    #[allow(deprecated)]
    let mut buf: [u8; BUF_BYTES] = mem::uninitialized();
    let fd: c_int;

    // If the path fits in the stack-allocated buffer we'll later use for
    // buffered reading, then we can use it now to avoid a heap allocation
    // when translating our `RocStr` to a null-terminated `char*`.
    {
        let path_len = roc_path.len();
        let path_fits_in_buf = path_len > BUF_BYTES;
        let c_path: *mut c_char;

        if path_fits_in_buf {
            roc_path.write_c_str(&mut buf as *mut u8);

            c_path = buf.as_mut_ptr() as *mut c_char;
        } else {
            c_path = roc_alloc(path_len, mem::align_of::<c_char>() as u32) as *mut c_char;

            roc_path.write_c_str(c_path as *mut u8);
        }

        fd = libc::open(c_path, libc::O_RDONLY);

        // Deallocate c_path now (if necessary) so we can safely do early returns
        // from here on.
        if !path_fits_in_buf {
            roc_dealloc(c_path as *mut c_void, mem::align_of_val(&c_path) as u32);
        }
    }

    // We'll use our own position and libc::pread rather than using libc::read
    // repeatedly and letting the fd store its own position. This way we don't
    // have to worry about concurrent modifications of the fd's position.
    let mut position = 0;

    // if libc::open returned -1, that means there was an error
    if fd != -1 {
        let mut list = RocList::empty();

        loop {
            let bytes_read = libc::pread(fd, buf.as_mut_ptr() as *mut c_void, BUF_BYTES, position);

            if bytes_read == BUF_BYTES as isize {
                // The read was successful, and there may be more bytes to read.
                // Append the bytes to the list and continue looping!
                let slice = core::slice::from_raw_parts(buf.as_ptr(), bytes_read as usize);

                list.append_slice(slice);
            } else if bytes_read >= 0 {
                // The read was successful, and there are no more bytes
                // to read (because bytes_read was less than the requested
                // BUF_BYTES, but it was also not negative - which would have
                // indicated an error).
                let slice = core::slice::from_raw_parts(buf.as_ptr(), bytes_read as usize);

                list.append_slice(slice);

                // We're done!
                return RocResult::Ok(list);
            } else {
                // bytes_read was negative, so we got a read error!
                break;
            }

            position += bytes_read as i64;
        }
    }

    RocResult::Err(errno())
}

#[no_mangle]
pub unsafe fn roc_fx_open(roc_path: RocStr) -> RocResult<Fd, Errno> {
    const BUF_BYTES: usize = 1024;
>>>>>>> 4835d4d65... Implement some file I/O stuff in cli host

    let mut buf: MaybeUninit<[u8; BUF_BYTES]> = MaybeUninit::uninit();

    // If the path fits in the stack-allocated buffer, we can avoid a heap
    // allocation when translating our `RocStr` to a null-terminated `char*`.
    let path_len = roc_path.len();
    let path_fits_in_buf = path_len > BUF_BYTES;
    let c_path: *mut c_char;

    if path_fits_in_buf {
        roc_path.write_c_str(buf.as_mut_ptr() as *mut u8);

        // NOTE buf may be only partially filled, so we don't `assume_init`!
        c_path = buf.as_mut_ptr() as *mut c_char;
    } else {
        c_path = roc_alloc(path_len, mem::align_of::<c_char>() as u32) as *mut c_char;

        roc_path.write_c_str(c_path as *mut u8);
    }

    let fd = libc::open(c_path, libc::O_RDONLY);

    // Now that the call to `open` is done, deallocate c_path if necessary>
    if !path_fits_in_buf {
        roc_dealloc(c_path as *mut c_void, mem::align_of_val(&c_path) as u32);
    }

    // if libc::open returned -1, that means there was an error
    if fd != -1 {
        RocResult::Ok(Fd(fd))
    } else {
        RocResult::Err(errno())
    }
}

#[no_mangle]
pub unsafe fn roc_fx_read(fd: Fd, bytes: usize) -> RocResult<RocList<u8>, Errno> {
    const BUF_BYTES: usize = 1024;

    let mut buf: MaybeUninit<[u8; BUF_BYTES]> = MaybeUninit::uninit();

    // We'll use our own position and libc::pread rather than using libc::read
    // repeatedly and letting the fd store its own position. This way we don't
    // have to worry about concurrent modifications of the fd's position.
    let mut list = RocList::empty();
    let mut position: usize = 0;

    loop {
        // Make sure we never read more than the buffer size, and also that
        // we never read past the originally-requested number of bytes.
        let bytes_to_read = BUF_BYTES.min(bytes - position);
        let bytes_read = libc::pread(
            fd.0,
            buf.as_mut_ptr() as *mut c_void,
            bytes_to_read,
            position as i64,
        );

        // NOTE buf may be only partially filled, so we don't `assume_init`!

        if bytes_read == bytes_to_read as isize {
            // The read was successful, and there may be more bytes to read.
            // Append the bytes to the list and continue looping!
            let slice = core::slice::from_raw_parts(buf.as_ptr() as *const u8, bytes_read as usize);

            list.append_slice(slice);
        } else if bytes_read >= 0 {
            // The read was successful, and there are no more bytes
            // to read (because bytes_read was less than the requested
            // bytes_to_read, but it was also not negative - which would have
            // indicated an error).
            let slice = core::slice::from_raw_parts(buf.as_ptr() as *const u8, bytes_read as usize);

            list.append_slice(slice);

            // We're done!
            return RocResult::Ok(list);
        } else {
            // bytes_read was negative, so we got a read error!
            break;
        }

        position += bytes_read as usize;
    }

    RocResult::Err(errno())
}
