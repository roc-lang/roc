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

/// A C file descriptor.
pub struct Fd(c_int);

impl Into<c_int> for Fd {
    fn into(&self) -> c_int {
        self.0;
    }
}

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

/// Negative panic_code values are reserved for the roc compiler and builtins.
/// Non-negative panic codes can be used for custom panic codes in the host.
#[cold]
#[no_mangle]
pub unsafe fn roc_panic(panic_code: i32, panic_data: *const c_void) -> ! {
    todo!(
        "Use libunwind to unwind the stack. Verify that refcounts get decremented along the way!"
    );
}

pub enum ReadErr {
    /// The file's contents grew between when the read function checked its
    /// size from its metadata and when the actual read of its contents occurred.
    FileGrewDuringRead,
}

impl ReadErr {
    pub fn from_errno(errno: c_int) -> Self {
        todo!("convert errno to ReadErr");
    }
}

/// If the given list is unique and has enough capacity to read the requested
/// number of bytes, then the bytes are written in-place. If it's shared, or
/// if there is not enough capacity to read the requested number of bytes, then
/// an new list will be automatically allocated (and returned).
///
/// This writes bytes until any of the following happens:
/// * The end of the file is reached
/// * The end of the list is reached
/// * The total number of bytes read has reached the `bytes` argument
///
/// This may result in zero bytes being read if (for example) `list` is empty,
/// or if `index` is greater than or equal to the length of the list.
#[no_mangle]
pub unsafe fn roc_fx_read(
    fd: Fd,
    index: usize,
    bytes: usize,
    mut list: RocList<u8>,
) -> RocResult<(usize, RocList<u8>), ReadErr> {
    // This is the minimum required capacity we need.
    let mut capacity = index.saturating_add(bytes).min(isize::MAX as usize);

    let alloc_fresh_list = || {
        // Safety: We know capacity is no more than isize::MAX, due to
        // the .min() call above, so it's safe to use
        // with_capacity_unchecked here.
        let list = RocList::with_capacity_unchecked(capacity);

        if true {
            // We'll only be writing into this uninitialized memory from
            // `index` onward, so we need to zero out the bytes before `index`.
            // Otherwise, someone could read the previously-set data that was
            // in there, which would be a vulnerability!
            todo!("fill the list from index 0 to `index` with zeroes");
        }

        list
    };

    // We're going to write into the list directly, so we need a unique copy
    // with at least enough capacity to hold all the bytes we're going to read.
    match list.storage() {
        Storage::Refcounted(_) | Storage::ReadOnly => {
            // This list is shared, so we need to allocate a fresh one that's
            // safe to write into.
            list = alloc_fresh_list();
        }
        Storage::Capacity(list_capacity) => {
            if list_capacity >= capacity {
                capacity = list_capacity;
            } else {
                // The list's capacity is less than the minimum capacity we
                // need, so we need to allocate a new list.
                list = alloc_fresh_list();
            }
        }
    }

    // Never read past the end of the list.
    //
    // Do saturating_sub here so that if index >= capacity (which would
    // mean we'd attempt write to memory outside the list) we will instead
    // read 0 bytes. This saturating_sub is crucial for memory safety!
    let bytes_to_read = capacity.saturating_sub(index).min(bytes);

    let bytes_read = libc::pread(
        fd.into(),
        list.as_mut_ptr() as *mut c_void,
        bytes_to_read,
        index as i64,
    );

    if bytes_read >= 0 {
        RocResult::Ok((bytes_read as usize, list))
    } else {
        RocResult::Err(ReadErr::from_errno(errno()))
    }
}

/// We use our own position and libc::pread rather than calling libc::read
/// repeatedly and letting the fd store its own position. This way we don't
/// have to worry about concurrent modifications of the fd's position.
///
/// Note that it's possible for a write to happen while a read is in progress,
/// and there's no way to prevent this other than locking the file - but
/// locking files in Linux is buggy, rarely used, and now essentially deprecated - https://man7.org/linux/man-pages/man2/fcntl.2.html
///
/// https://stackoverflow.com/questions/35595685/write2-read2-atomicity-between-processes-in-linux
///
/// As such, all we can really say here is "beware - this could happen" - and
/// that you can do advisory locking to prevent it from happening in processes
/// that opt into cooperating with the advisory lock (i.e. multiple running
/// instances of this program).
#[no_mangle]
pub unsafe fn roc_fx_read_all(fd: Fd, mut list: RocList) -> RocResult<RocList<u8>, ReadErr> {
    if true {
        todo!("Can we lock the file for writing, so that this becomes atomic and we no longer need FileGrewDuringWrite?");
    }

    // Find out how many bytes the file has on disk, and make a list with
    // that many elements.
    let bytes_in_file = libc::meta(fd.into()).size;
    let mut list = RocList::with_capacity(bytes_in_file);

    // Attempt to read 1 more byte than necessary. This way, if we actually
    // successfully read that extra byte, we know the file grew between when
    // we looked at its size metadata attribute and when we read its contents.
    let bytes_to_read = bytes_in_file.saturating_add(1);

    let bytes_read = libc::pread(
        fd.into(),
        list.as_mut_ptr() as *mut c_void,
        bytes_to_read,
        0,
    );

    if bytes_read == bytes_in_file as isize {
        // The read was successful, and we read exactly as many bytes as we
        // expected to.
        RocResult::Ok(list)
    } else if bytes_read < 0 {
        // bytes_read was negative, so we got a read error!
        RocResult::Err(ReadErr::from_errno(errno()))
    } else if bytes_read < bytes_to_read {
        // The read did not error out (because this was non-negative), but the
        // number of bytes read was less than expected. This is possible because
        // the file could shrink between when we got the size from its metadata
        // and when we actually read it.
        //
        // Adjust length accordingly and call it a day; leave the extra capacity
        // for later use.
        list.len = bytes_read as usize;

        RocResult::Ok(list)
    } else {
        // The read did not error out (because this was non-negative), but the
        // number of bytes read was more than expected. This is possible because
        // the file could grow between when we got the size from its metadata
        // and when we actually read it.
        //
        // Unfortunately, there's no reliable way to recoevr from this scenario.
        // * If we try to read additional bytes until we run out, we may end
        //   up with the wrong bytes in the list if the earlier bytes (that
        //   we've already read into the list) changed. That would leave us
        //   with a list of bytes containing stale bytes from the first read
        //   followed by up-to-date bytes from the second read.
        // * If we try to redo the entire read, we may end up in the same
        //   situation as before; the race condition could happen again. It's
        //   less likely, of course, but it could happen especially if something
        //   were repeatedly appending to the file. Theoretically this function
        //   could get stuck in a retry loop if we did this.
        //
        // Instead of potentially giving invalid data or getting stuck in a
        // retry loop, we give an error and let the application author decide
        // how to handle it. (For example, trying to lock the file before
        // reading it, or copying it somewhere else and then reading the copy.)

        RocResult::Err(ReadErr::FileGrewDuringRead)
    }
}
