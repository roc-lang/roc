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
    mem::{self, MaybeUninit},
    ptr::{self, NonNull},
    slice,
};
use std::{
    fs::File,
    io::{self, Read},
    path::Path,
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

#[derive(Debug)]
pub enum FileErr {
    IoErr(io::Error),
    FileWasEmpty,
    FileWasTooBig(usize),
}

// We can't derive PartialEq because io::Error doesn't have it.
impl PartialEq for FileErr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::IoErr(left), Self::IoErr(right)) => left.kind() == right.kind(),
            (Self::FileWasTooBig(left), Self::FileWasTooBig(right)) => left == right,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl From<io::Error> for FileErr {
    fn from(err: io::Error) -> Self {
        FileErr::IoErr(err)
    }
}

impl<'a> Src64<'a> {
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

        if capacity == src_len && src.as_ptr().align_offset(16) == 0 {
            // If the string already happens to meet our capacity and alignment requirements, just return it.
            return Some(Self {
                bytes: src.as_bytes(),
            });
        }

        // Safety: we got capacity by rounding up to the nearest 64B
        let dest = unsafe { allocate_and_pad_with_newlines(arena, capacity)? };

        // Safety: we just allocated `dest` to have len >= src.len(), and they're both u8 slices.
        unsafe {
            ptr::copy_nonoverlapping(
                src.as_bytes().as_ptr(),
                dest.as_mut_ptr() as *mut u8,
                src_len,
            );
        }

        Some(Self {
            // Safety: all the bytes should now be initialized, so we can transmute away their MaybeUninit marker.
            bytes: unsafe { mem::transmute::<&'a [MaybeUninit<u8>], &'a [u8]>(dest) },
        })
    }

    pub fn from_file(arena: &'a Bump, path: &Path) -> Result<Self, FileErr> {
        let mut file = File::open(path).map_err(Into::<FileErr>::into)?;
        let file_size = file.metadata()?.len() as usize;

        if file_size == 0 {
            return Err(FileErr::FileWasEmpty);
        }

        let capacity = round_up_to_nearest_64(file_size);

        // Safety: we got capacity by rounding up to the nearest 64B
        match unsafe { allocate_and_pad_with_newlines(arena, capacity) } {
            Some(bytes) => {
                {
                    // Read the contents of the file directly into `bytes`. If the file size isn't a multiple of 64,
                    // it's no problem; we already made its capacity a multiple of 64, and the last 64B now end in newlines.
                    //
                    // Safety: By design, read_exact returns an error if it doesn't completely fill the buffer
                    // it was given. However, in this case, we explicitly want it to do that, in order to preserve
                    // our newline padding at the end. This is safe because we know that the buffer's length will
                    // actually be strictly longer than what we're setting it to, because capacity > file_size.
                    //
                    // The `read` function is not an appropriate substitute for `read_exact` here, because:
                    // - It's allowed to read fewer than the specified number of bytes, which is not what we want
                    // - The docs - https://doc.rust-lang.org/std/io/trait.Read.html#tymethod.read - explicitly say
                    //   that "It is your responsibility to make sure that buf is initialized before calling read.
                    //   Calling read with an uninitialized buf (of the kind one obtains via MaybeUninit<T>) is
                    //   not safe, and can lead to undefined behavior." That UB is exactly what we would be doing here
                    //   if we used read on this. Using `read_exact` in this unusual way is safer than using `read`
                    //   in a way that is explicitly documented to be UB.
                    file.read_exact(unsafe {
                        slice::from_raw_parts_mut(bytes.as_ptr() as _, file_size)
                    })
                    .map_err(Into::<FileErr>::into)?;
                }

                // Safety: we have to do this transmute because read_exact requires &[u8].
                // Fortunately, after the next call, it will be fully initialized anyway.
                //
                // Once https://doc.rust-lang.org/std/io/trait.Read.html#method.read_buf_exact is stabilized,
                // we can use that instead of &mut MaybeUninit<u8> and transmuting like this.
                let bytes =
                    unsafe { mem::transmute::<&'a mut [MaybeUninit<u8>], &'a mut [u8]>(bytes) };

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
unsafe fn allocate_and_pad_with_newlines(
    arena: &Bump,
    capacity: usize,
) -> Option<&mut [MaybeUninit<u8>]> {
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
    let layout = unsafe { Layout::from_size_align_unchecked(capacity, 16) };

    // We have to use alloc_layout here because we have stricter alignment requirements than normal slices.
    let buf_ptr: NonNull<u8> = arena.alloc_layout(layout);

    // Safety: we just allocated this pointer to have a capacity of `size` bytes.
    let contents = unsafe { slice::from_raw_parts_mut(buf_ptr.as_ptr() as _, capacity) };

    // Safety: `contents` has a length of `capacity`, which has been rounded up to a multiple of 64.
    unsafe { fill_last_64_bytes_with_newlines(contents) };

    Some(contents)
}

/// This is branchless so there can't be mispredictions. We know the slice's length is a multiple of 64,
/// so we can just always do four SIMD writes and call it a day. (Eight if we don't have SIMD.)
///
/// Safety: this slice must have an alignment of at least 64,
/// and its length must be both at least 64 and also a multiple of 64.
unsafe fn fill_last_64_bytes_with_newlines(slice: &mut [MaybeUninit<u8>]) {
    let len = slice.len();

    debug_assert_eq!(
        slice.as_ptr() as usize % 16,
        0,
        "The slice's alignment must be at least 16."
    );
    debug_assert_eq!(len % 64, 0, "The slice's length must be a multiple of 64.");
    debug_assert!(len >= 64, "The slice's length must be at least 64.");

    // Safety: this function's docs note that it must be given a slice with at least 64 bytes in it.
    let last_64_bytes = slice.as_mut_ptr().add(len - 64);

    #[cfg(target_feature = "sse2")]
    {
        use core::arch::x86_64::{__m128i, _mm_set1_epi8, _mm_storeu_si128};

        let newline = _mm_set1_epi8(b'\n' as i8);
        let ptr = last_64_bytes as *mut __m128i;

        _mm_storeu_si128(ptr.add(0), newline);
        _mm_storeu_si128(ptr.add(1), newline);
        _mm_storeu_si128(ptr.add(2), newline);
        _mm_storeu_si128(ptr.add(3), newline);
    }

    #[cfg(target_feature = "neon")]
    {
        use core::arch::aarch64::{vdupq_n_s8, vst1q_s8};

        let newline = vdupq_n_s8(b'\n' as i8);
        let ptr = last_64_bytes as *mut i8;

        vst1q_s8(ptr.add(0), newline);
        vst1q_s8(ptr.add(16), newline);
        vst1q_s8(ptr.add(32), newline);
        vst1q_s8(ptr.add(48), newline);
    }

    #[cfg(not(any(target_feature = "sse2", target_feature = "neon")))]
    {
        // We don't have access to SIMD, so do eight 64-bit writes instead of four 128-bit writes.
        let newline_repeated = (b'\n' as u64) * 0x0101010101010101;
        let ptr = last_64_bytes as *mut u64;

        *ptr.add(0) = newline_repeated;
        *ptr.add(1) = newline_repeated;
        *ptr.add(2) = newline_repeated;
        *ptr.add(3) = newline_repeated;
        *ptr.add(4) = newline_repeated;
        *ptr.add(5) = newline_repeated;
        *ptr.add(6) = newline_repeated;
        *ptr.add(7) = newline_repeated;
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
