use crate::alloc;
use core::{
    alloc::Layout,
    ptr::{self, NonNull},
};

#[cfg(not(target_arch = "wasm32"))]
use fs::{File, IoError};

#[derive(Copy, Clone, Debug)]
pub(crate) struct ChunkHeader {
    /// Used when deallocating chunks.
    prev: *mut ChunkHeader,

    /// The total number of bytes we have available to store content in this chunk.
    capacity: usize,

    /// The number of bytes actually in use for the content of this chunk.
    /// (This can exceed capacity if we got our allocation from someone else's
    /// memory; e.g. capacity is 0 but len is nonzero.)
    content_len: usize,
}

impl ChunkHeader {
    pub(crate) const DEFAULT_CAPACITY: usize = 4000;

    pub unsafe fn from_raw_parts(
        prev: *mut ChunkHeader,
        content_capacity: usize,
        content_len: usize,
    ) -> Self {
        Self {
            prev,
            capacity: content_capacity,
            content_len,
        }
    }

    pub(crate) fn prev(&self) -> Option<NonNull<ChunkHeader>> {
        NonNull::new(self.prev)
    }

    pub(crate) fn capacity(&self) -> usize {
        self.capacity
    }

    pub(crate) fn len(&self) -> usize {
        self.content_len
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns None if there wasn't enough space in this chunk's capacity
    pub(crate) fn alloc(&mut self, layout: Layout) -> Option<NonNull<u8>> {
        // See https://fitzgeraldnick.com/2019/11/01/always-bump-downwards.html for why it bumps
        // down!
        let todo = todo!("allocate");
    }

    pub(crate) unsafe fn try_dealloc(&mut self, ptr: *mut u8, size: usize) -> bool {
        let todo = todo!("Look at the pointer and layout; if it's the most recent thing we allocated, bump back over it.");
    }

    pub(crate) fn contents(&self) -> &[u8] {
        unsafe {
            let after_header = (self as *const Self).add(1);

            core::slice::from_raw_parts(after_header.cast(), self.capacity)
        }
    }

    pub(crate) fn contents_mut(&mut self) -> &mut [u8] {
        unsafe {
            let after_header = (self as *mut Self).add(1);

            core::slice::from_raw_parts_mut(after_header.cast(), self.capacity)
        }
    }

    /// Write this chunk and (recursively) all of its previous chunks to disk, using a single writev sycall.
    ///
    /// https://linux.die.net/man/2/writev
    #[cfg(unix)]
    pub(crate) fn write_recursive(&self, file: &mut File) -> Result<usize, IoError> {
        use core::mem::{align_of, MaybeUninit};

        const MAX_ON_STACK: usize = 8;

        // Allocate space for contiguous chunks. If it turns out when we're traversing the linked list
        // that we encounter more than this many chunks (very unlikely), resort to a full OS allocation.
        let mut iov_array: [MaybeUninit<IoVec>; MAX_ON_STACK] = [
            MaybeUninit::uninit(),
            MaybeUninit::uninit(),
            MaybeUninit::uninit(),
            MaybeUninit::uninit(),
            MaybeUninit::uninit(),
            MaybeUninit::uninit(),
            MaybeUninit::uninit(),
            MaybeUninit::uninit(),
        ];

        unsafe {
            // By default, store the iovecs on the stack. Later, we may switch this pointer to an OS
            // allocation if we run out.
            let mut iov_storage: NonNull<IoVec> =
                NonNull::new_unchecked(&mut iov_array as *mut MaybeUninit<IoVec> as _);
            let mut num_iovecs: usize = 0;
            let mut chunk = NonNull::new_unchecked(self as *const Self as *mut Self);
            let mut heap_allocated_bytes = None;

            loop {
                // We're about to exceed the maximum number of iovecs that we can
                // store on the stack, so it's time to switch to the slow path!
                if num_iovecs == MAX_ON_STACK {
                    // First, count up the rest of the iovecs so we know how much to allocate;
                    let mut iovecs_needed = num_iovecs + 1;
                    let mut prev_chunk = NonNull::new(chunk.as_ref().prev);

                    // Keep going until we hit a null `prev`
                    while let Some(ptr) = prev_chunk {
                        iovecs_needed += 1;
                        prev_chunk = NonNull::new(ptr.as_ref().prev);
                    }

                    if iovecs_needed > i32::MAX as usize {
                        let todo = todo!("handle too many iovecs needed");
                    }

                    // Now that we know how many iovecs we need, allocate enough space for them.
                    let (ptr, total_bytes_allocated) = {
                        let layout =
                            Layout::from_size_align_unchecked(iovecs_needed, align_of::<IoVec>());

                        alloc::alloc_virtual(layout)
                    };

                    heap_allocated_bytes = Some(total_bytes_allocated);

                    // Switch from the stack-allocated array to the new allocation.
                    iov_storage = ptr.cast();

                    // Copy all the existing entries into the new allocation.
                    core::ptr::copy_nonoverlapping(
                        &iov_array as *const MaybeUninit<IoVec> as _,
                        iov_storage.as_mut(),
                        MAX_ON_STACK,
                    );
                }

                // Add the new entry.
                *iov_storage.as_ptr().add(num_iovecs) = IoVec {
                    // Skip over the header; we don't want to write it to disk!
                    base: chunk.as_ptr().add(1).cast(),
                    // Only include the actual content, not the whole capacity.
                    // This way, we effectively compact the chunks as we write them.
                    len: chunk.as_ref().content_len,
                };

                num_iovecs += 1;

                // Continue the loop with this chunk's previous entry.
                // (We'll reverse them all at the end before calling writev.)
                match NonNull::new(chunk.as_ref().prev) {
                    Some(prev) => {
                        chunk = prev;
                    }
                    None => {
                        // We ran out of previous chunks; break!
                        break;
                    }
                }
            }

            // We built up the iovecs in reverse order, so reverse them back before
            // giving them to writev. Otherwise writev will write them backwards!
            reverse_in_place(iov_storage.as_ptr(), num_iovecs);

            let bytes_written = writev(file.fd(), iov_storage.as_ptr(), num_iovecs as i32);

            // If we had to resort to an OS allocation to store all the iovecs, free it.
            // We no longer need the allocation now that writev has been called, and this
            // is our last opportunity to free it before we return.
            if let Some(bytes) = heap_allocated_bytes {
                // If heap_allocated_bytes is Some, then we should not be pointed at the
                // stack-allocated array!
                debug_assert_ne!(
                    iov_storage.as_ptr(),
                    &iov_array as *const MaybeUninit<IoVec> as _
                );

                let layout = Layout::from_size_align_unchecked(bytes, align_of::<IoVec>());

                alloc::dealloc_virtual(iov_storage.as_ptr().cast(), layout);
            }

            if bytes_written >= 0 {
                Ok(bytes_written as usize)
            } else {
                Err(IoError::most_recent())
            }
        }
    }
}

#[cfg(unix)]
// https://www.man7.org/linux/man-pages/man3/iovec.3type.html
#[repr(C)]
struct IoVec {
    base: *const u8,
    len: usize,
}

#[cfg(unix)]
extern "C" {
    // https://linux.die.net/man/2/writev
    fn writev(fd: i32, iov: *const IoVec, iovcnt: i32) -> isize;
}

fn reverse_in_place<T>(array: *mut T, len: usize) {
    let mut start = 0;
    let mut end = len - 1;

    while start < end {
        unsafe {
            // Swap the elements using pointers
            ptr::swap(array.add(start), array.add(end));
        }
        start += 1;
        end -= 1;
    }
}
