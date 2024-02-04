use core::{
    mem::size_of,
    num::{NonZeroU8, NonZeroUsize},
    slice, str,
};

use crate::sized_str::{Str1, Str16, Str2, Str4, Str8};

#[derive(Copy, Clone, Debug)]
#[repr(transparent)]
pub struct StrId(u32);

impl StrId {
    /// We use 3 bits for the tag
    const TAG_BITS: u32 = 3;
    const _MAX: u32 = u32::MAX >> Self::TAG_BITS;
    const TAG_OFFSET: u32 = size_of::<u32>() as u32 - Self::TAG_BITS;

    fn bucket(self) -> Bucket {
        unsafe { core::mem::transmute(self.0 >> Self::TAG_OFFSET) }
    }

    pub fn to_str<'a>(&self, finder: &'a StrFinder<'a>) -> &'a str {
        todo!("no, this is all wrong. what needs to happen is that we have some parallel Vec of Option<NonZeroU16> IDs (so, 65K declarable unique identifiers per file), and each time we reset the scope, we zero all those out. I *think* it might also be possible that we can recycle those - e.g. maybe it's 65K for the deepest scope - but I'm not certain.");
        finder.to_str(self.bucket(), self.0 as usize)
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Bucket {
    Str1 = 0,
    Str2 = 1 << StrId::TAG_OFFSET,
    Str4 = 2 << StrId::TAG_OFFSET,
    Str8 = 3 << StrId::TAG_OFFSET,
    Str16 = 4 << StrId::TAG_OFFSET,
    StrBig = 5 << StrId::TAG_OFFSET,
}

impl Bucket {
    fn as_index(self) -> u32 {
        (self as u32) >> StrId::TAG_OFFSET
    }

    fn size(self) -> u32 {
        // 2^n gives the size:
        // index 0 is Str1
        // index 1 is Str2
        // index 2 is Str4
        // index 3 is Str8
        // index 4 is Str16
        1 << self.as_index()
    }
}

pub struct StrFinder<'a> {
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////// NOTE: These first few slots cannot be moved! Self::to_str relies on their ordering here ////////
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    str1: &'a [Str1],
    str2: &'a [Str2],
    str4: &'a [Str4],
    str8: &'a [Str8],
    str16: &'a [Str16],
    // TODO str_big goes here
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
    //////// END NOTE ///////////////////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////////////////////////////////////
}

impl<'a> StrFinder<'a> {
    pub fn id(&self, needle: &[NonZeroU8]) -> Option<StrId> {
        unsafe {
            match needle.len() {
                0 => None,
                1 => {
                    Str1::from(*needle.get_unchecked(0)) // we know this is safe because we just matched on len
                        .first_index_in(self.str1)
                        .map(|index| Self::str1_id(self, index))
                }
                len @ 2 => Str2::from_raw_parts(needle.as_ptr(), NonZeroUsize::new_unchecked(len))
                    .first_index_in(self.str2)
                    .map(|index| Self::str2_id(self, index)),
                len @ (3 | 4) => {
                    Str4::from_raw_parts(needle.as_ptr(), NonZeroUsize::new_unchecked(len))
                        .first_index_in(self.str4)
                        .map(|index| Self::str4_id(self, index))
                }
                // Use `|` rather than `..` because `..` adds extraneous branches on top of the jump table
                #[allow(clippy::manual_range_patterns)]
                len @ (5 | 6 | 7 | 8) => {
                    Str8::from_raw_parts(needle.as_ptr(), NonZeroUsize::new_unchecked(len))
                        .first_index_in(self.str8)
                        .map(|index| Self::str8_id(self, index))
                }
                // Use `|` rather than `..` because `..` adds extraneous branches on top of the jump table
                #[allow(clippy::manual_range_patterns)]
                len @ (9 | 10 | 11 | 12 | 13 | 14 | 15 | 16) => {
                    Str16::from_raw_parts(needle.as_ptr(), NonZeroUsize::new_unchecked(len))
                        .first_index_in(self.str16)
                        .map(|index| Self::str16_id(self, index))
                }
                _len => {
                    todo!();
                    // StrBig::from_raw_parts(needle.as_ptr(), NonZeroUsize::new_unchecked(len))
                    // .first_index_in(self.str_big)
                    // .map(|index| Self::str_big_id(&self, index))
                }
            }
        }
    }

    fn str1_id(&self, index: usize) -> StrId {
        StrId(index as u32 & Bucket::Str1 as u32)
    }

    fn str2_id(&self, index: usize) -> StrId {
        StrId(index as u32 & Bucket::Str2 as u32)
    }

    fn str4_id(&self, index: usize) -> StrId {
        StrId(index as u32 & Bucket::Str4 as u32)
    }

    fn str8_id(&self, index: usize) -> StrId {
        StrId(index as u32 & Bucket::Str8 as u32)
    }

    fn str16_id(&self, index: usize) -> StrId {
        StrId(index as u32 & Bucket::Str16 as u32)
    }

    fn _str_big_id(&self, index: usize) -> StrId {
        StrId(index as u32 & Bucket::StrBig as u32)
    }

    fn to_str(&self, bucket: Bucket, offset_into_bucket: usize) -> &str {
        if bucket != Bucket::StrBig {
            // Branchlessly get the string out of the appropriate bucket
            let bucket_index = bucket.as_index() as usize;
            let bucket_size = bucket.size() as usize;

            unsafe {
                let slice = *(self as *const Self as *const &[u8]).add(bucket_index);
                let elem_ptr = slice.as_ptr().add(offset_into_bucket * bucket_size);

                str::from_utf8_unchecked(slice::from_raw_parts(elem_ptr, bucket_size))
            }
        } else {
            todo!("Handle StrBig")
        }
    }
}
