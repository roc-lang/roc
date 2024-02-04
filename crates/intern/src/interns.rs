use core::mem::size_of;

use crate::intern_key::InternKey;
use arena::{AsU32, String, Vec16};
use sized_str::{Str16, Str4, Str8, StrBig};

pub struct Interns<'a> {
    // Buckets. These must appear at the beginning of the struct, and in this order!
    // We rely on this being true for some pointer arithmetic.
    // Note that we don't need to store str1 or str2 here because we store those strings
    // inline in the u16 Id itself.
    str4: Bucket<'a, Str4>,
    str8: Bucket<'a, Str8>,
    str16: Bucket<'a, Str16>,
    str_big: Bucket<'a, StrBig>,
}

type Bucket<'a, T> = Vec16<'a, T>;

impl<'a> Interns<'a> {
    pub fn key_from_str(&mut self, str: &str) -> InternKey {
        // todo jump table to convert str into bucket (and/or inline) based on length 0..16+
        // todo either inline or find in that bucket; if not found, generate new id and insert

        todo!();
    }

    /// The reason this writes to a buffer rather than returning the string is that sometimes
    /// the key itself stores the string inline, so we would have to retern a pointer to the
    /// key itself - which would not go well.
    pub fn write_str_from_key(&self, key: InternKey, buf: &mut String<'a, impl AsU32>) {
        let is_inline = key.is_inline();
        let inline_len = key.inline_len();
        let bucket = key.bucket();
        let bucket_size = bucket.str_size();
        let inline_offset = inline_len;
        let bucket_offset = key.index_within_bucket() * size_of::<Bucket<'a, Str4>>();
        let ptr_offset = if is_inline {
            inline_offset
        } else {
            bucket_offset
        };
        let self_ptr = self as *const Self as *const u8;
        let bucket_ptr = unsafe {
            (self_ptr as *const Bucket<'a, Str4>)
                .add(key.bucket().as_bucket_index())
                .cast()
        };
        let base_ptr = if is_inline { self_ptr } else { bucket_ptr };
        let str_ptr = unsafe { base_ptr.add(ptr_offset) };
        let len = if is_inline {
            inline_len
        } else {
            todo!("");
        }
    }
}
