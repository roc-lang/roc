// This should be no_std, but we want to be able to use dbg! in development and std conveniences in testing
// Having this be no_std isn't strictly necessary, but it reduces the risk of accidental heap allocations.
#![cfg_attr(not(any(debug_assertions, test)), no_std)]

struct Vec16<'a, T> {
    offset: u32
    len: u16,
    capacity: u16,
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct InternKey(NonZeroI16);

impl InternKey {
    fn is_interned(&self) -> bool {
        let key: i16 = self.0.into();

        key.is_negative()
    }

    fn interned_len(&self) -> usize {
        todo!("Return whether the second byte is 0");
    }

    fn bucket_index(&self) -> usize {
        todo!("Return this based on the tag; if this is interned, return 0");
    }

    fn index_inside_bucket(&self) -> usize {
        todo!("Get rid of the tag to return this");
    }
}

pub struct Interns<'a, Id: Into<NonZeroU16> + Copy, Str: Into<SizedStr>> {
    // Buckets. These must appear at the beginning of the struct, and in this order!
    // We rely on this being true for some pointer arithmetic.
    // Note that we don't need to store str1 or str2 here because we store those strings
    // inline in the u16 Id itself.
    str4: Vec16<'a, Str4>,
    str8: Vec16<'a, Str8>,
    str16: Vec16<'a, Str16>,
    str_big: Vec16<'a, StrBig>,
}

// If the string is 1 or 2 characters long, since Roc identifiers have to start with an ASCII character,
// we can use a 0 bit on the first character as a flag for a "small ident optimization": we store the
// ident inside the u16 itself. There's a separate question of scope, of course. That's out of (ha) scope here.
// (If in the future we relax that, then we can just decline to do this optimization if the first
// character is not in the ASCII range.) Actually let's just do that now. For length=1B strings we don't need
// to check it (only possible valid UTF-8 is if it's ASCII), and for length=2 strings we just need to check
// if either i8 is negative. (If so, then it's a 2-byte character.) Also, since we know the original was 2B
// in length, we can definitely just stick it in Str4 and call it a day.


// 3 of the 16 bits in the Id are reserved for the tag telling us which bucket this is.
// The other 13 bits are for the index into that bucket (so, 2^13 = 8,192 strings per bucket).
// In order to exhaust this on variables (for example), you would need something like 8k variables
// and they're all 3-4 chars, or 5-8, or 9-16. If that ever does happen, we can always introduce
// a layer of indirection: have each u16 identifier be an index into a Vec of (BucketId, u16 offset into that bucket)
// and then do an extra lookup on resolution. But this is faster, so let's default to it and see if
// it ever comes up as a problem in practice. Seems very unlikely (and of course you can always say,
// "hey, don't do that."
enum Bucket {
    Str1 = 0,
    Str2,
    Str4,
    Str8,
    Str16,
    StrBig,
    Inline,
}

impl<'a> Interns<'a> {
    pub fn key_from_str(&mut self, str: &NativeStr) -> InternKey {
        // todo jump table to convert str into bucket (and/or inline) based on length 0..16+
        // todo either inline or find in that bucket; if not found, generate new id and insert

        todo!();
    }

    /// The reason this writes to a buffer rather than returning the string is that sometimes
    /// the key itself stores the string inline, so we would have to retern a pointer to the
    /// key itself - which would not go well.
    pub fn write_str_from_key(&self, key: InternKey, &mut NativeStrBuf) {
        let key_u16: u16 = key.0.into();
        let is_interned = key.is_interned();
        let base_ptr = if is_interned {
            &key_u16 as *const u16 as *const u8
        } else {
            self as *const Self as *const u8
        };
        let base_ptr = base_ptr.add(key.bucket_index());
        let ptr_offset = key.index_inside_bucket();

        // Branchlessly determine the offset into the base pointer


        // TODO determine bucket from id
        // TODO given bucket id and offset into bucket, return str
        todo!();
    }
}
