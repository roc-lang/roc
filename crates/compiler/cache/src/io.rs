use core::{ffi::{c_int, c_char, c_void}, mem::MaybeUninit};

use crate::iovec::IoVec;

#[cfg(unix)]
extern "C" {
    // Source for preadv and pwritev: https://www.man7.org/linux/man-pages/man2/readv.2.html
    // Source for off_t being equivalent to i64: https://docs.rs/libc/latest/libc/type.off_t.html
    fn preadv(fd: c_int, io_vecs: *mut IoVec, num_io_vecs: c_int, offset: i64) -> isize;
    fn pwritev(fd: c_int, io_vecs: *const IoVec, num_io_vecs: c_int, offset: i64) -> isize;

    // Source: https://www.man7.org/linux/man-pages/man2/open.2.html
    fn open(pathname: *const c_char, flags: c_int) -> c_int;

    // Source: https://www.man7.org/linux/man-pages/man2/read.2.html
    fn read(fd: c_int, buf: *mut c_void, count: usize) -> isize;

    // Source: https://www.man7.org/linux/man-pages/man2/write.2.html
    fn write(fd: c_int, buf: *const c_void, count: usize) -> isize;
}

pub enum ReadErr {}
pub enum WriteErr {}

pub fn read<T, const N: usize>(
    path: &[u8],
    from_header: impl Fn([u8; N]) -> *mut IoVec,
    from_iovec: impl Fn(&[IoVec]) -> T
) -> Result<T, ReadErr> {
    // Safety: fopen is safe
    let fd = unsafe { fopen() };

    todo!("read the header that tells us the sizes of all the IOVecs")

    let io_vec_info_header = ();
    let io_vecs: &mut [IoVec] = &mut []; // TODO allocate etc

    // Saftey: we know fd is open here
    let bytes_read = unsafe { readv(fd, io_vecs.as_mut_ptr(), io_vecs.len()) };

    // Safety: fclose is safe
    unsafe { fclose(fd); }

    todo!("translate bytes_read into an error, if applicable");

    Ok(from_iovec(io_vecs))
}

pub fn write(path: &[u8], header: &[u8], io_vecs: &[IoVec]) -> Result<(), WriteErr> {
    // Safety: fopen is safe
    let fd = unsafe { fopen() };

    todo!("write the header that says all the sizes of all the IOVecs")

    // Saftey: we know fd is open here
    let bytes_written = writev(fd, io_vecs.as_ptr(), io_vecs.len() as c_int);

    // Safety: fclose is safe
    unsafe { fclose(fd); }

    todo!("translate bytes_wirten");

    Ok(())
}


//     pub fn load_from_cache(path: &'a [u8]) -> Result<Self, SaveErr> {

//         let file_size_in_bytes = read_file_size()?;
//         let allocation = Arena::with_capacity(file_size_in_bytes);
//         let iovecs = Iovecs::from_header(fd)?;

//         readv(iovecs.as_mut_ptr().cast(), iovecs.len())
//     }

//     pub fn save_to_cache(&self, path: &'a [u8]) -> Result<(), SaveErr> {
//         let file_size = IOVECS_HEADER_SIZE +
//         let iovecs = self.as_iovecs();

//         iovecs.write_header()?;

//         writev(iovecs.as_ptr().cast(), iovecs.len());
//     }

//     fn as_iovecs(&self) -> Iovecs {

//     }
// }

// struct Iovecs {
//     idents: Iovec,
//     scopes: Iovec,
// }
