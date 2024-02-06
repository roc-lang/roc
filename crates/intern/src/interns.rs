use crate::intern_key::InternKey;
use arena::{Arena, AsU32, String, Vec16};
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
    pub fn write_str_from_key(
        &'a self,
        key: InternKey,
        arena: &'a mut Arena<'a>,
        buf: &mut String<'a, impl AsU32>,
    ) {
        use crate::intern_key::Bucket;

        unsafe {
            match key.bucket() {
                Bucket::Str4 => {
                    self.str4
                        .get_unchecked(arena, key.index_within_bucket())
                        .write(buf);
                }
                Bucket::Str8 => {
                    self.str8
                        .get_unchecked(arena, key.index_within_bucket())
                        .write(buf);
                }
                Bucket::Str16 => {
                    self.str16
                        .get_unchecked(arena, key.index_within_bucket())
                        .write(buf);
                }
                Bucket::StrBig => {
                    self.str_big
                        .get_unchecked(arena, key.index_within_bucket())
                        .write(buf);
                }
                Bucket::Inline => {
                    todo!("Write either 0 or 1 bytes to the buffer.");
                }
            }
        }
    }
}
