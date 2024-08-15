// Loads Roc source files (from strings or from files) into a structure which is
// guaranteed to have the following properties, all of which the SIMD parser requires:
// - 16B alignment
// - byte length is a multiple of 64
// - if the source bytes were not a multiple of 64, the extra space is filled with trailing newlines
//
// (Trailing newlines are the filler of choice because they are irrelevant to the parser.)
//
// It does this as efficiently as possible by using branchless SIMD to fill padding bytes,
// and reading the contents of the file directly into an arena in as few syscalls as possible.

use bumpalo::{self, Bump};
use core::{
    alloc::Layout,
    mem::{align_of, MaybeUninit},
    ptr::{self, NonNull},
};

#[cfg(not(test))]
/// We store both line and column numbers as u16s, so the largest possible file you could open
/// would be every line having the longest possible column length, or u16::MAX * u16::MAX.
const MAX_ROC_SOURCE_FILE_SIZE: usize = u16::MAX as usize * u16::MAX as usize; // 4GB

#[cfg(test)]
const MAX_ROC_SOURCE_FILE_SIZE: usize = 1024; // small enough that we can create a tempfile to exercise this scenario

pub struct Src64<'a> {
    /// These bytes are guaranteed to have a 16B-aligned address (so the parser can do 128-bit SIMD on it).
    /// This slice is guaranteed to have a length that's a multiple of 64B, because the parser iterates in
    /// chunks of 64B. (If extra bytes are needed to make it a multiple of 64B, we add trailing newlines
    /// because the parser ignores those.)
    bytes: &'a [u8],
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FileErr {
    FileWasEmpty,
    ReadErr,
    FileWasTooBig(usize),
    ErrReadingFileSize,
    FileOpenFailed,
}

impl<'a> Src64<'a> {
    const BYTES_ALIGNMENT: usize = 64;

    /// The underlying source bytes that originally came from a file or from a string.
    ///
    /// These bytes are guaranteed to have a 16B-aligned address (so the parser can do 128-bit SIMD on it).
    /// This slice is guaranteed to have a length that's a multiple of 64B, because the parser iterates in
    /// chunks of 64B. (If extra bytes are needed to make it a multiple of 64B, we add trailing newlines
    /// because the parser ignores those.)
    pub fn bytes(&self) -> &[u8] {
        self.bytes
    }

    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    /// Returns None if the given string exceeds the maximum size of a Roc source file.
    pub fn from_str(arena: &'a Bump, src: &'a str) -> Option<Src64<'a>> {
        let src_len = src.len();

        if src_len == 0 {
            return None;
        }

        let capacity = round_up_to_nearest_64(src_len);

        debug_assert_eq!(capacity % 64, 0);

        if capacity == src_len && src.as_ptr().align_offset(Self::BYTES_ALIGNMENT) == 0 {
            // If the string already happens to meet our capacity and alignment requirements, just return it.
            return Some(Self {
                bytes: src.as_bytes(),
            });
        }

        // Safety: we got capacity by rounding up to the nearest 64B
        let dest = unsafe { allocate_chunks(arena, capacity)? }.as_ptr();

        // Safety: `dest` has a length of `capacity`, which has been rounded up to a multiple of 64.
        unsafe {
            let trailing_newlines_needed = capacity - src_len;

            // Start writing newlines right after the last of the bytes we got from the file.
            write_newlines(dest.add(src_len), trailing_newlines_needed);
        };

        // Safety: we just allocated `dest` to have len >= src.len(), and they're both u8 arrays.
        unsafe {
            ptr::copy_nonoverlapping(src.as_bytes().as_ptr(), dest, src_len);
        }

        Some(Self {
            // Safety: all the bytes should now be initialized
            bytes: unsafe { core::slice::from_raw_parts_mut(dest, capacity) },
        })
    }

    #[cfg(any(unix, windows))] // This is not available on wasm32. We could make it work with WASI if desired.
    pub fn from_file(arena: &'a Bump, path: &std::path::Path) -> Result<Self, FileErr> {
        use core::ffi::c_void;

        let file = match std::fs::File::open(path) {
            Ok(file) => file,
            Err(_) => {
                return Err(FileErr::FileOpenFailed);
            }
        };

        let file_size = match file.metadata() {
            Ok(metadata) => {
                #[cfg(unix)]
                {
                    use std::os::unix::prelude::MetadataExt;

                    metadata.size() as usize
                }

                #[cfg(windows)]
                {
                    use std::os::windows::prelude::MetadataExt;

                    metadata.file_size() as usize
                }
            }
            Err(_io_err) => {
                return Err(FileErr::ErrReadingFileSize);
            }
        };

        if file_size == 0 {
            return Err(FileErr::FileWasEmpty);
        }

        let capacity = round_up_to_nearest_64(file_size);

        // Safety: round_up_to_nearest_u64 will give us a capacity that is
        // at least 64, and also a multiple of 64.
        match unsafe { allocate_chunks(arena, capacity) } {
            Some(buf) => {
                // Read bytes equal to file_size into the arena allocation.
                //
                // We use the native OS read() operation here to avoid UB; file.read_exact()
                // only reads into a slice, and constructing a slice with uninitialized
                // data is UB (per the slice::from_raw_parts docs). The allocation is uninitialized here,
                // and initializing it would be a waste of CPU cycles because we're about to overwrite
                // those bytes with bytes from the file anyway.
                let bytes_read = {
                    #[cfg(unix)]
                    unsafe {
                        use std::os::fd::AsRawFd;

                        // This extern lets us avoid an entire libc crate dependency.
                        extern "C" {
                            // https://linux.die.net/man/2/read
                            pub fn read(
                                fd: core::ffi::c_int,
                                buf: *mut c_void,
                                count: usize,
                            ) -> isize;
                        }

                        read(file.as_raw_fd(), buf.as_ptr() as *mut c_void, file_size) as usize
                    }

                    #[cfg(windows)]
                    unsafe {
                        use std::os::windows::io::AsRawHandle;

                        // This extern lets us avoid an entire winapi crate dependency.
                        extern "system" {
                            // https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile
                            pub fn ReadFile(
                                hFile: *mut c_void,
                                lpBuffer: *mut c_void,
                                nNumberOfBytesToRead: u32,
                                lpNumberOfBytesRead: *mut u32,
                                lpOverlapped: *mut c_void, // this should be a pointer to a struct, but we always pass null.
                            ) -> i32;
                        }

                        let mut bytes_read = core::mem::MaybeUninit::uninit();

                        // We should have already errored out if file_size exceeded u32::MAX,
                        // due to our maximum source file size. This debug_assert! is here to
                        // make sure casting file_size to u32 is safe in the ReadFile call.
                        debug_assert!(MAX_ROC_SOURCE_FILE_SIZE <= u32::MAX as usize);

                        ReadFile(
                            file.as_raw_handle() as *mut c_void,
                            buf.as_ptr() as *mut c_void,
                            file_size as u32,
                            bytes_read.as_mut_ptr(),
                            core::ptr::null_mut(),
                        );

                        bytes_read.assume_init() as usize
                    }
                };

                // We can close the file now; we're done with it.
                drop(file);

                // It's crucial that we successfully read the entire file; otherwise, it would be unsafe
                // to make a slice out of it because we might not have overwritten the uninitialized
                // memory leading up to the newlines at the end!
                //
                // Note that on UNIX, bytes_read might be -1 if this was a file read error. This
                // condition will catch that too, since we know file_size won't be (-1isize as usize)
                // beacuse if it was, then this match would have taken the None branch due to
                // (-1isize as usize) exceeding our maximum file size.
                if bytes_read != file_size {
                    return Err(FileErr::ReadErr);
                }

                // Before we write newlines to the last chunk, branchlessly prefetch the first four 64-byte chunks.
                // We're about to have a cache miss due to loading the last chunk from main memory (DMA will have
                // written it there without having gone through the CPU), and if we don't prefetch here, then we'll
                // immediately get a second cache miss when we start traversing the loaded file. The prefetch means
                // by the time we finish resolving the first cache miss on the last chunk, continuing with the first
                // chunk(s) won't be a cache miss anymore because they'll already be in cache.
                //
                // We can do further prefetches in the actual tokenization loop.
                #[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
                {
                    // We know capacity >= 64, so this will never wrap.
                    let last_chunk_offset = capacity - 64;

                    // Prefetch the first 64-byte chunk.
                    prefetch_read(buf, 0);

                    // Prefetch the second 64-byte chunk, using min() to branchlessly avoid prefetching an address we might not own.
                    prefetch_read(buf, 64.min(last_chunk_offset));

                    // Prefetch the third 64-byte chunk, using min() to branchlessly avoid prefetching an address we might not own.
                    prefetch_read(buf, 128.min(last_chunk_offset));

                    // Prefetch the fourth 64-byte chunk, using min() to branchlessly avoid prefetching an address we might not own.
                    prefetch_read(buf, 192.min(last_chunk_offset));

                    // Further prefetching can happen in the tokenization loop. Now that we've prefetched the first pages,
                    // we should be able to prefetch the others in the tokenization loop before it needs to read them.
                }

                // We may have coincidentally had a file size that was a multiple of 64, but if not,
                // we'll need to fill the allocation with trailing newlines so we aren't tokenizing
                // uninitialized memory.
                if capacity > file_size {
                    debug_assert!(capacity - file_size < 64);
                    let trailing_newlines_needed = capacity - file_size;

                    // Safety: `buf_ptr` has a length of `capacity`, which has been rounded up to a multiple of 64.
                    unsafe {
                        // Start writing newlines right after the last of the bytes we got from the file.
                        write_newlines(buf.as_ptr().add(file_size), trailing_newlines_needed);
                    };
                }

                // Safety: bytes_ptr came from an allocation of `capacity` bytes, it's had
                // newlines filled at the end, and `file_size` bytes written over the rest.
                let bytes = unsafe { core::slice::from_raw_parts_mut(buf.as_ptr(), capacity) };

                Ok(Self { bytes })
            }
            None => Err(FileErr::FileWasTooBig(file_size)),
        }
    }
}

fn round_up_to_nearest_64(num: usize) -> usize {
    // Round up to the nearest 64. (Writing this out as 64 - 1 so it's clearer where the numbers came from.)
    // We can do saturating addition here rather than overflow checking, because if we overflow usize::MAX,
    // we will most definitely be over the max source file size and return None anyway.
    (num.saturating_add(64 - 1)) & !(64 - 1)
}

/// Safety: capacity must be a multiple of 64, and must be at least 64.
unsafe fn allocate_chunks(arena: &Bump, capacity: usize) -> Option<NonNull<u8>> {
    // Compare capacity here instead of size because this file limit is based on what we can record row and line
    // numbers for, and those can theoretically oveflow on the trailing newlines we may have added.
    // This distinction will most likely come up in practice zero times ever, but it could come up in fuzzing.
    if capacity > MAX_ROC_SOURCE_FILE_SIZE {
        return None;
    }

    debug_assert!(capacity >= 64);
    debug_assert!(capacity % 64 == 0);

    // Safety: the rules we follow are https://doc.rust-lang.org/core/alloc/struct.Layout.html#method.from_size_align_unchecked
    // `align` is valid because it's hardcoded, and we already rounded `capacity` up to something even bigger.
    // We align it to 64B so that it's on cache line boundaries on many CPUs, which makes prefetching simpler.
    let layout = unsafe { Layout::from_size_align_unchecked(capacity, Src64::BYTES_ALIGNMENT) };

    // We have to use alloc_layout here because we have stricter alignment requirements than normal slices.
    Some(arena.alloc_layout(layout))
}

/// This is branchless so there can't be mispredictions.
///
/// Safety: this pointer must have an alignment of at least 64,
/// and the length must be both at least 64 and also a multiple of 64.
unsafe fn write_newlines(dest: *mut u8, len: usize) {
    debug_assert!(len <= 64);

    #[cfg(target_feature = "sse2")]
    {
        use core::arch::x86_64::{__m128i, _mm_set1_epi8, _mm_storeu_si128};

        let mut buf: MaybeUninit<[__m128i; 4]> = MaybeUninit::uninit();
        let newline = _mm_set1_epi8(b'\n' as i8);
        let ptr = buf.as_mut_ptr() as *mut __m128i;

        debug_assert_eq!(ptr.align_offset(align_of::<__m128i>()), 0);

        _mm_storeu_si128(ptr.add(0), newline);
        _mm_storeu_si128(ptr.add(1), newline);
        _mm_storeu_si128(ptr.add(2), newline);
        _mm_storeu_si128(ptr.add(3), newline);

        core::ptr::copy_nonoverlapping(ptr as *const u8, dest, len);
    }

    #[cfg(target_feature = "neon")]
    {
        use core::arch::aarch64::{int8x16_t, vdupq_n_s8, vst1q_s8};

        let mut buf: MaybeUninit<[int8x16_t; 4]> = MaybeUninit::uninit();
        let newline = vdupq_n_s8(b'\n' as i8);
        let ptr = buf.as_mut_ptr() as *mut i8;

        debug_assert_eq!(ptr.align_offset(align_of::<int8x16_t>()), 0);

        vst1q_s8(ptr.add(0), newline);
        vst1q_s8(ptr.add(16), newline);
        vst1q_s8(ptr.add(32), newline);
        vst1q_s8(ptr.add(48), newline);

        core::ptr::copy_nonoverlapping(ptr as *const u8, dest, len);
    }

    #[cfg(not(any(target_feature = "sse2", target_feature = "neon")))]
    {
        // We don't have access to SIMD, so do eight 64-bit writes instead of four 128-bit writes.
        let mut buf: MaybeUninit<[u64; 8]> = MaybeUninit::uninit();
        let newline_repeated = (b'\n' as u64) * 0x0101010101010101;
        let ptr = buf.as_mut_ptr() as *mut u64;

        debug_assert_eq!(ptr.align_offset(align_of::<u64>()), 0);

        *ptr.add(0) = newline_repeated;
        *ptr.add(1) = newline_repeated;
        *ptr.add(2) = newline_repeated;
        *ptr.add(3) = newline_repeated;
        *ptr.add(4) = newline_repeated;
        *ptr.add(5) = newline_repeated;
        *ptr.add(6) = newline_repeated;
        *ptr.add(7) = newline_repeated;

        core::ptr::copy_nonoverlapping(ptr as *const u8, dest, len);
    }
}

#[cfg(any(target_arch = "x86_64", target_arch = "aarch64"))]
#[inline(always)]
fn prefetch_read<T>(non_null_ptr: NonNull<T>, offset: usize) {
    // Use inline asm until this is stabilized:
    // https://doc.rust-lang.org/std/intrinsics/fn.prefetch_read_data.html

    #[cfg(target_arch = "x86_64")]
    unsafe {
        core::arch::asm!(
            "prefetcht0 [{}]",
            in(reg) non_null_ptr.as_ptr().add(offset)
        );
    }

    #[cfg(target_arch = "aarch64")]
    unsafe {
        core::arch::asm!(
            "prfm PLDL1KEEP, [{}]",
            in(reg) non_null_ptr.as_ptr().add(offset)
        );
    }
}

#[cfg(test)]
mod src64_tests {
    use super::{FileErr, Src64, MAX_ROC_SOURCE_FILE_SIZE};
    use bumpalo::Bump;
    use quickcheck::{quickcheck, Arbitrary, Gen};
    use std::fs::File;
    use std::io::Write;
    use tempfile::tempdir;

    fn expect_from_str(arena: &Bump, contents: &str, expected: &Result<Vec<u8>, FileErr>) {
        match Src64::from_str(arena, contents) {
            Some(actual) => {
                assert_eq!(actual.len() % 64, 0);
                assert_eq!(
                    expected.as_ref().ok(),
                    Some(&actual.bytes().into()),
                    "Src64::from_str had unexpected output"
                )
            }
            None => {
                assert_eq!(
                    expected.as_ref().ok(),
                    None,
                    "Src64::from_str had unexpected output"
                )
            }
        }
    }

    fn expect_from_file(arena: &Bump, contents: &str, expected: &Result<Vec<u8>, FileErr>) {
        let dir = tempdir().expect("Failed to create temp dir");
        let file_path = dir.path().join("temp_file");

        // Write contents to the temp file
        {
            let mut file = File::create(&file_path).expect("Failed to create temp file");
            file.write_all(contents.as_bytes())
                .expect("Failed to write to temp file");
        }

        match Src64::from_file(arena, &file_path) {
            Ok(actual) => {
                assert_eq!(actual.len() % 64, 0);
                assert_eq!(
                    expected,
                    &Ok(actual.bytes().into()),
                    "Src64::from_file had unexpected output"
                )
            }
            Err(err) => assert_eq!(
                expected,
                &Err(err),
                "Src64::from_file had unexpected output"
            ),
        }
    }

    /// Runs both Src64::from_str and Src64::from_file on the given str, then
    /// asserts the output of both of those functions is equal to `expected`.
    /// (Since from_str returns an Option, we call .ok() on `expected` before comparing it.)
    fn expect_from(contents: &str, expected: Result<Vec<u8>, FileErr>) {
        let arena = Bump::new();

        expect_from_str(&arena, contents, &expected);
        expect_from_file(&arena, contents, &expected);
    }

    #[test]
    fn empty() {
        expect_from("", Err(FileErr::FileWasEmpty));
    }

    #[test]
    fn one_newline() {
        expect_from("\n", Ok([b'\n'; 64].into()));
    }

    #[test]
    fn one_byte() {
        expect_from(
            "x",
            Ok({
                let mut vec: Vec<u8> = [b'\n'; 64].as_mut_slice().into();

                vec[0] = b'x';

                vec
            }),
        );
    }

    #[test]
    fn two_bytes() {
        expect_from(
            "xy",
            Ok({
                let mut vec: Vec<u8> = [b'\n'; 64].as_mut_slice().into();

                vec[0] = b'x';
                vec[1] = b'y';

                vec
            }),
        );
    }

    #[test]
    fn max_file_size() {
        let bytes = [b'z'; MAX_ROC_SOURCE_FILE_SIZE];

        expect_from(
            core::str::from_utf8(bytes.as_slice()).unwrap(),
            Ok(bytes.into()),
        );
    }

    #[test]
    fn too_big() {
        let bytes = [b'z'; MAX_ROC_SOURCE_FILE_SIZE + 1];

        expect_from(
            core::str::from_utf8(bytes.as_slice()).unwrap(),
            Err(FileErr::FileWasTooBig(bytes.len())),
        );
    }

    #[derive(Debug, Clone)]
    struct FileBytes(Vec<u8>);

    impl Arbitrary for FileBytes {
        fn arbitrary(g: &mut Gen) -> Self {
            let len = g.size() % (MAX_ROC_SOURCE_FILE_SIZE + 1); // Wrap around to avoid clustering
                                                                 //
            FileBytes((0..len).map(|_| u8::arbitrary(g)).collect())
        }
    }

    quickcheck! {
        /// Creates a tempfile containing arbitrary bytes, then reads it with Str::from_file. Asserts that:
        /// - the returned Result<Str64> is Ok
        /// - its length is a multiple of 64
        /// - it's at least as long as the input bytes were
        /// - it starts_with the input bytes
        fn from_arb_file(bytes: FileBytes) -> bool {
            let FileBytes(bytes) = bytes;

            let dir = tempdir().expect("Failed to create temp dir");
            let file_path = dir.path().join("temp_file");

            // Write random bytes to the temp file
            {
                let mut file = File::create(&file_path).expect("Failed to create temp file");
                file.write_all(&bytes).expect("Failed to write to temp file");
            }

            let arena = Bump::new();

            match Src64::from_file(&arena, &file_path) {
                Ok(src64) => {
                    let len = src64.len();

                    len % 64 == 0 && len >= bytes.len() && src64.bytes().starts_with(&bytes)
                }
                Err(_) => false
            }
        }
    }
}
