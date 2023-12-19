use core::ffi::c_void;

// Source: https://www.man7.org/linux/man-pages/man3/iovec.3type.html
#[repr(C)]
pub struct IoVec {
    /// Starting address
    pub base: *const c_void,
    /// Size of the memory pointed to by base
    pub len: usize,
}
