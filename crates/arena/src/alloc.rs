use core::ptr::NonNull;

pub(crate) trait VirtualAlloc {
    unsafe fn virtual_alloc(capacity: usize) -> NonNull<u8>;
    unsafe fn virtual_dealloc(ptr: NonNull<u8>, capacity: usize);
    fn oom(bytes_attemped: usize) -> !;
}

/// We need to reserve at least 15 capacity bytes at the end of the arena, so that
/// if we want to do unaligned 128-bit loads on (for example) strings,
/// in order to do SIMD things on them, there's always enough memory reserved
/// at the end of the arena so that we aren't potentially reading into
/// a page we don't have access to (which would cause a segfault).
pub(crate) const RESERVED_BYTES: usize = 15;

#[inline(always)]
pub(crate) fn round_up_capacity(capacity: usize, page_size: usize) -> usize {
    ((capacity + RESERVED_BYTES + page_size - 1) / page_size) * page_size
}
