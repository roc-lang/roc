use crate::bucket::Bucket;
use core::{
    fmt, mem,
    num::{NonZeroU16, NonZeroUsize},
    slice, str,
};
use nonempty_str::NonEmptyStr;
use small_str_slice::SmallStrSlice;

#[derive(Copy, Clone)]
pub union InternedStrId {
    small: [u8; 2],
    big: NonZeroU16,
}

impl fmt::Debug for InternedStrId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_inlined() {
            write!(f, "InlinedStr({:?})", unsafe { self.inlined_str() })
        } else {
            write!(f, "BigStr({:?})", unsafe { self.big })
        }
    }
}

impl InternedStrId {
    // Inlined ASCII string of length 1:
    // 00000000 0xxxxxxx
    //
    // Inlined ASCII string of length 2: (if multibyte, we store it in the 4-bucket with two 0 bytes for padding.)
    // 0xxxxxxx 0xxxxxxx
    //
    // Bucketed string of length 3-4:
    // 1xxxxxxx 00xxxxxx
    //
    // Bucketed string of length 5-8:
    // 1xxxxxxx 01xxxxxx
    //
    // Bucketed string of length 9-16:
    // 1xxxxxxx 10xxxxxx
    //
    // Bucketed string of length 17+:
    // 1xxxxxxx 11xxxxxx

    fn is_inlined(self) -> bool {
        self.first_byte() < 128
    }

    fn first_byte(self) -> u8 {
        unsafe { *self.small.get_unchecked(0) }
    }

    fn second_byte(self) -> u8 {
        unsafe { *self.small.get_unchecked(1) }
    }

    fn inlined_len(self) -> NonZeroUsize {
        let is_first_byte_nonzero = self.first_byte() != 0;

        unsafe { NonZeroUsize::new_unchecked((1 + (is_first_byte_nonzero as u8)) as usize) }
    }

    fn bucketed_len(self) -> NonZeroUsize {
        todo!();
    }

    fn heap_len(self) -> NonZeroUsize {
        todo!();
    }

    fn len(self) -> NonZeroUsize {
        let inlined_len = self.inlined_len();
        let bucketed_len = self.bucketed_len();
        let heap_len = self.heap_len();

        let non_inlined = if let Bucket::StrHeap = self.bucket() {
            heap_len
        } else {
            bucketed_len
        };

        if self.is_inlined() {
            inlined_len
        } else {
            non_inlined
        }
    }

    fn bucket(self) -> Bucket {
        let bucket_from_tag = unsafe { mem::transmute(((self.big.get() >> 6) & 0b11) as u8) };

        if self.is_inlined() {
            Bucket::Inlined
        } else {
            bucket_from_tag
        }
    }

    fn index_within_bucket(self) -> u16 {
        let if_not_inlined = (self.big.get() >> 8) as u16;

        if self.is_inlined() {
            0
        } else {
            if_not_inlined
        }
    }

    unsafe fn inlined_str(self) -> SmallStrSlice<'static> {
        let len = 1 + unsafe { (*self.small.get_unchecked(1) != 0) as usize };
        let bytes = slice::from_raw_parts(&self as *const Self as *const u8, len);
        let nonempty_str = NonEmptyStr::new_unchecked(str::from_utf8_unchecked(bytes));

        SmallStrSlice::new_small_unchecked(nonempty_str)
    }

    unsafe fn bucketed_str<'a>(self, bucket_start: &'a [u8]) -> SmallStrSlice<'a> {
        let bucket = self.bucket();
        let len = self.bucketed_len();
        let entry_size = bucket.entry_size();
        let index = self.bucket().as_index();
        let slice_ptr = bucket_start.as_ptr().add(index * entry_size);
        let bytes = slice::from_raw_parts(slice_ptr, len);
        let nonempty_str = NonEmptyStr::new_unchecked(str::from_utf8_unchecked(bytes));

        SmallStrSlice::new_small_unchecked(nonempty_str)
    }

    pub fn to_str<'a>(self, bucket_start: &'a [u8]) -> &'a str {
        let inlined_str = unsafe { self.inlined_str() };
        let bucketed_str = unsafe { self.bucketed_str(bucket_start) };
        let heap_str = self.heap_str();

        let non_inlined = if let Bucket::StrHeap = self.bucket() {
            heap_str
        } else {
            bucketed_str
        };

        if self.is_inlined() {
            inlined_str
        } else {
            non_inlined
        }
    }
}
