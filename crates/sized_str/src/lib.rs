// This should be no_std, but we want to be able to use dbg! in development and std conveniences in testing
// Having this be no_std isn't strictly necessary, but it reduces the risk of accidental heap allocations.
#![cfg_attr(not(any(debug_assertions, test)), no_std)]

use core::mem::size_of;
use core::num::{NonZeroU32, NonZeroU64, NonZeroU8, NonZeroUsize};
use core::{fmt, slice};

#[repr(align(16))]
struct AlignedU128(u128);

macro_rules! make_str_n {
    ($num_lanes:expr, $name:ident, $chunk_name:ident, $chunks_name:ident, $int:ty, $nonzero_int:ty) => {
        /// A string of length 1 to 4 bytes.
        ///
        /// Internally, this is represented as a NonZeroU32, which means
        /// if you wrap one of these in an Option, Option::None will be 0.
        #[derive(Copy, Clone, PartialEq, Eq)]
        pub struct $name($nonzero_int);

        #[repr(align(16))]
        pub struct $chunk_name([$nonzero_int; $chunks_name::NUM_LANES]);

        pub struct $chunks_name<'a>(&'a [$chunk_name]);

        impl<'a> $chunks_name<'a> {
            const NUM_LANES: usize = $num_lanes;
            const ALIGN: usize = Self::NUM_LANES * core::mem::align_of::<$name>();

            /// # Safety
            /// The given slice must:
            /// - have a nonzero length that's a multiple of 4
            /// - point to a memory address that's disible by 16
            pub unsafe fn new_unchecked(slice: &'a [$name]) -> Self {
                debug_assert!(!slice.is_empty());
                debug_assert_eq!(slice.len() % Self::NUM_LANES, 0);
                debug_assert_eq!(
                    0,
                    ((slice as *const _) as *const $int) as usize % Self::ALIGN
                );

                core::mem::transmute(slice)
            }

            pub fn as_slice(&self) -> &[$name] {
                // Internally, we have chunks of 4; adjust the returned slice's length accordingly.
                unsafe {
                    core::slice::from_raw_parts(
                        self.0.as_ptr().cast(),
                        self.0.len() * Self::NUM_LANES,
                    )
                }
            }
        }

        impl $name {
            #[allow(clippy::len_without_is_empty)]
            #[cfg(target_endian = "little")] // This implementation relies on little-endian byte ordering for the int
            pub fn len(&self) -> usize {
                // NonZeroU__::trailing_zeros compiles to a single instruction on x64, whereas
                // u__::trailing_zeros compiles to a conditional branch. This is becuase apparently some
                // CPUs do different things when asked to count leading or trailing zeros of the number 0.
                core::mem::size_of::<Self>() - (self.0.trailing_zeros() as usize / 8)
            }

            pub fn as_str(&self) -> &str {
                unsafe {
                    let bytes: &[u8] =
                        slice::from_raw_parts((self as *const $name).cast(), self.len());

                    core::str::from_utf8_unchecked(bytes)
                }
            }
        }

        #[cfg(any(debug_assertions, test))]
        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.as_str().fmt(f)
            }
        }
    };
}

make_str_n!(4, Str4, Str4Chunk, Str4Chunks, u32, NonZeroU32);

impl Str4 {
    /// This is only needed in aarch64; in x64, we call _mm_set_epi32 directly
    #[cfg(target_arch = "aarch64")]
    const ANSWER_MASK: [u32; 4] = [0, 1, 2, 3];

    /// Returns Some(first index where self occurs in the given slice) or else None if it wasn't found.
    ///
    /// # Safety
    /// The slice's pointer must point to at least 16B of accessible memory, even if its length means
    /// we'll never need to look that far. The reason for this is that we start by dereferencing the
    /// slice's pointer into a 16B structure, and then discarding any memory garbage we find.
    /// So there must be 16B there that we can safely dereference, even if it contains garbage!
    pub unsafe fn first_index_in(&self, slice: &[Str4]) -> Option<usize> {
        let len = slice.len();
        let first_elem = slice.as_ptr();
        let slice_end: *const u32 = first_elem.add(len).cast();
        let mut current_elem: *const u32;

        // Each SIMD register holds 4 u32s
        const NUM_LANES: usize = 4;
        const ALIGN: usize = 16;

        #[cfg(target_arch = "aarch64")]
        use core::arch::aarch64;

        #[cfg(target_arch = "x86_64")]
        use core::arch::x86_64;

        #[cfg(target_arch = "wasm32")]
        use core::arch::wasm32;

        // Initialize the answer mask and the needle we're searching for.
        let answer_mask;
        let needle;

        #[cfg(target_arch = "aarch64")]
        {
            answer_mask = aarch64::vld1q_u32(Self::ANSWER_MASK.as_ptr()); // Load ANSWER_MASK into a SIMD register for later
            needle = aarch64::vdupq_n_u32(*(self as *const Self).cast()); // Load self into each lane of a SIMD register
        }

        #[cfg(target_arch = "x86_64")]
        {
            answer_mask = x86_64::_mm_set_epi32(3, 2, 1, 0);
            needle = x86_64::_mm_set1_epi32(*(self as *const Self).cast());
        }

        #[cfg(target_arch = "wasm32")]
        {
            answer_mask = wasm32::i32x4(0, 1, 2, 3);
            needle = wasm32::i32x4_splat(*(self as *const Self).cast());
        }

        let success = |elem_ptr, offset| {
            Some(len - (dbg!(slice_end.offset_from(elem_ptr)) + dbg!(offset)) as usize)
        };

        macro_rules! check_equality {
            ($ptr:expr, $elem_ptr:expr) => {{
                // This pointer should have the correct alignment for a 16B-aligned SIMD register
                debug_assert_eq!(($ptr as *const core::ffi::c_void).align_offset(ALIGN), 0);

                #[cfg(target_arch = "aarch64")]
                {
                    // Compare each lane of the chunk to the needle for equality.
                    let equality_reg = aarch64::vceqq_u32(aarch64::vld1q_u32($ptr), needle);

                    // If lane-wise equality returned any nonzero values, we found a match.
                    if aarch64::vmaxvq_u32(equality_reg) != 0 {
                        // Do a bitwise AND with answer_mask, whose lanes are initialized to [0, 1, 2, 3]
                        // and then get the scalar min of that, which will tell us the first lane's index
                        // where we found an occurrence of the needle. (If multiple lanes matched, min will
                        // give us the first one that matched, which is what we want given find's semantics.)
                        let lane_index =
                            aarch64::vminvq_u32(aarch64::vandq_u32(equality_reg, answer_mask)) as isize;

                        return success($elem_ptr, lane_index);
                    }
                }

                #[cfg(target_arch = "x86_64")]
                {
                    // Compare each lane of the chunk to the needle for equality.
                    let equality_reg = x86_64::_mm_cmpeq_epi32(x86_64::_mm_loadu_si128($ptr), needle);

                    // Get a mask of the most significant bit of each element in the equality comparison,
                    // and store it in the 4 least significant bits of the return value. (All other bits are 0.)
                    let mask = x86_64::_mm_movemask_ps(x86_64::_mm_castsi128_ps(equality_reg));

                    // Check if the mask was nonzero by converting to NonZeroU32,
                    // because NonZeroU32 has a more efficient trailing_zeros() than u32 does.
                    if let Some(mask) = NonZeroU32::new(mask as u32) {
                        #[cfg(target_endian = "little")]
                        let lane_index = mask.trailing_zeros();

                        return success($elem_ptr, lane_index);
                    }
                }

                #[cfg(target_arch = "wasm32")]
                {
                    // Compare each lane of the chunk to the needle for equality.
                    let equality_reg = wasm32::i32x4_eq(wasm32::v128_load($ptr), needle);

                    // Get a mask of the most significant bit of each element in the equality comparison,
                    // and store it in the 4 least significant bits of the return value. (All other bits are 0.)
                    let mask = wasm32::i32x4_bitmask(equality_reg);

                    // Check if the mask was nonzero by converting to NonZeroU64,
                    // because NonZeroU32 has a more efficient trailing_zeros() than u32 does.
                    if let Some(mask) = NonZeroU32::new(mask as u32) {
                        #[cfg(target_endian = "little")]
                        let lane_index = mask.trailing_zeros();

                        return success($elem_ptr, lane_index);
                    }
                }
            }};
        }

        // Special-case the first chunk, which may not be 16B-aligned.
        // (Subsequent chunks will definitely be 16B-aligned.)
        {
            // We need to shift in this many zeros to discard any memory garbage.
            let empty_lanes = dbg!(NUM_LANES.saturating_sub(len));

            todo!("what needs to happen here is:// see comment");
            // Currently we're loading an i128 and bit shifting it. I don't think this is actually what we want to do.
            // I think the problem we're seeing is that the bit shifting is running afoul of endianness, so
            // we don't want to get u128 involved at all. Instead what we want to do is to pass in a mask
            // (only on this first equality check, so maybe a Fn instead) which is all 1s for the first N lanes
            // which are valid, and 0s for the others. Then we put that mask into a register and do a bitwise &
            // with the equality register, such that all the lanes that we loaded with garbage data will be
            // considered unequal no matter what. So to do that we just have to figure out how to construct
            // the mask efficiently!
            let lane_mask = AlignedU128(!0 << (empty_lanes * size_of::<Str4>())); // not sure if this is it...

            // First, load a u128 with the given bytes. Wrap in a struct to ensure 16B alignment.
            // (This pointer dereference is why the pointer must point to at least 16B of accessible memory.
            // Even if we may end up discarding a lot of these bytes, this is the fastest way to load them!)
            let int = AlignedU128(
                u128::from_be_bytes(*first_elem.cast()) >> (empty_lanes * size_of::<Str4>()),
            );

            // Get a pointer to these 128 bits, then load it into a 128-bit SIMD register.
            {
                let src_ptr = (&int as *const AlignedU128).cast();

                // Give an offset that subtracts the zero padding, so the answer index is
                // relative to the start of the slice rather than incorrectly offset by the zeros.
                check_equality!(src_ptr, first_elem.add(empty_lanes).cast());
            }

            // Advance to the next 16B boundary so we can do SIMD loads on each subsequent chunk.
            // If we were already on a 16B boundary, then branchlessly advance by NUM_LANES - otherwise
            // the first iteration of the loop will redo the exact same work we just did.
            let align_offset = first_elem.align_offset(ALIGN);
            let amount_to_advance = if align_offset == 0 {
                NUM_LANES
            } else {
                align_offset
            };

            current_elem = first_elem.add(amount_to_advance).cast();
        }

        while (current_elem as usize + size_of::<AlignedU128>()) < slice_end as usize {
            check_equality!(current_elem.cast(), current_elem.cast());

            // Advance to the next chunk
            current_elem = current_elem.add(NUM_LANES);
        }

        // There may have been leftovers
        let last_chunk = slice_end.offset(-(NUM_LANES as isize));
        let last_chunk = last_chunk.add(last_chunk.align_offset(ALIGN));

        check_equality!(last_chunk.cast(), last_chunk.cast());

        None
    }

    /// Returns the first 4 bytes of the input slice as a Str4.
    /// (Other bytes past the first 4 are ignored.)
    /// If there are fewer than 4 input bytes, pads the end with zeros internally.
    ///
    /// # Safety
    /// There must be at least 8 bytes of safely accessible memory starting from the pointer.
    ///
    /// Note: These unusual API requirements are for performance; they avoid a memcpy call and branching!
    pub unsafe fn from_raw_parts(input: *const NonZeroU8, len: NonZeroUsize) -> Self {
        let int = u32::from_be_bytes(*input.cast());
        let zeros_needed = 4usize.saturating_sub(len.into());

        // Safety: as noted in this function's safety section, this slice must not be empty
        Self(NonZeroU32::new_unchecked(int >> zeros_needed))
    }
}

make_str_n!(2, Str8, Str8Chunk, Str8Chunks, u64, NonZeroU64);

impl Str8 {
    /// Returns Some(first index where self occurs in the given slice) or else None if it wasn't found.
    pub fn first_index_in(&self, slice: Str8Chunks<'_>) -> Option<usize> {
        // Each SIMD register holds 4 u32s
        const NUM_LANES: usize = 4;
        const ALIGN: usize = NUM_LANES * core::mem::align_of::<Str8>();

        #[cfg(target_arch = "aarch64")]
        use core::arch::aarch64;

        #[cfg(target_arch = "x86_64")]
        use core::arch::x86_64;

        #[cfg(target_arch = "wasm32")]
        use core::arch::wasm32;

        unsafe {
            let mut current_elem: *const u64 = slice.0.as_ptr().cast(); // Start at the beginning of the slice
            let slice_end: *const u64 = current_elem.add(slice.0.len()); // Pointer to right after slice's last elem
            let needle;

            #[cfg(not(target_arch = "aarch64"))]
            let answer_mask;

            #[cfg(target_arch = "aarch64")]
            {
                needle = aarch64::vdupq_n_u64(*(self as *const Self).cast()); // Load self into each lane of a SIMD register
            }

            #[cfg(target_arch = "x86_64")]
            {
                answer_mask = x86_64::_mm_set_epi64x(1, 0);
                needle = x86_64::_mm_set1_epi64x(*(self as *const Self).cast());
            }

            #[cfg(target_arch = "wasm32")]
            {
                answer_mask = wasm32::i64x2(0, 1);
                needle = wasm32::i64x2_splat(*(self as *const Self).cast());
            }

            let success = |current_elem, lane_index| {
                Some((slice_end.offset_from(current_elem)) as usize + lane_index)
            };

            while current_elem < slice_end {
                let current_reg;

                // Advance to the next chunk
                current_elem = current_elem.add(NUM_LANES);

                // current_elem should always have the correct alignment
                debug_assert_eq!(current_elem as usize % ALIGN, 0);

                #[cfg(target_arch = "aarch64")]
                {
                    current_reg = aarch64::vld1q_u64(current_elem);

                    // Compare each lane of the chunk to the needle for equality.
                    let equality_reg = aarch64::vceqq_u64(current_reg, needle);

                    let lane0 = aarch64::vgetq_lane_u64(equality_reg, 0);
                    let lane1 = aarch64::vgetq_lane_u64(equality_reg, 1);

                    // If lane-wise equality returned any nonzero values, we found a match.
                    if lane0 != 0 || lane1 != 0 {
                        // If lane0 matched, prefer that one regardless of whether lane1 also matched.
                        // Do this calculation branchlessly.
                        let lane_index = (lane0 != 0) as usize;

                        return success(current_elem, lane_index);
                    }
                }

                #[cfg(target_arch = "x86_64")]
                {
                    current_reg = x86_64::_mm_loadu_si128(current_elem.cast());

                    // Compare each lane of the chunk to the needle for equality.
                    let equality_reg = x86_64::_mm_cmpeq_epi32(current_reg, needle);

                    // Get a mask of the most significant bit of each element in the equality comparison,
                    // and store it in the 4 least significant bits of the return value. (All other bits are 0.)
                    let mask = x86_64::_mm_movemask_ps(x86_64::_mm_castsi128_ps(equality_reg));

                    // Check if the mask was nonzero by converting to NonZeroU64,
                    // because NonZeroU32 has a more efficient trailing_zeros() than u32 does.
                    if let Some(mask) = NonZeroU32::new(mask as u32) {
                        #[cfg(target_endian = "little")]
                        let lane_index = mask.trailing_zeros() as usize;

                        return success(current_elem, lane_index);
                    }
                }

                #[cfg(target_arch = "wasm32")]
                {
                    current_reg = wasm32::v128_load(current_elem.cast());

                    // Compare each lane of the chunk to the needle for equality.
                    let equality_reg = wasm32::i32x4_eq(current_reg, needle);

                    // Get a mask of the most significant bit of each element in the equality comparison,
                    // and store it in the 4 least significant bits of the return value. (All other bits are 0.)
                    let mask = wasm32::i32x4_bitmask(equality_reg);

                    // Check if the mask was nonzero by converting to NonZeroU64,
                    // because NonZeroU32 has a more efficient trailing_zeros() than u32 does.
                    if let Some(mask) = NonZeroU32::new(mask) {
                        #[cfg(target_endian = "little")]
                        let lane_index = mask.trailing_zeros() as usize;

                        return success(current_elem, lane_index);
                    }
                }
            }
        }

        None
    }

    /// Returns the first 8 bytes of the input slice as a Str8.
    /// (Other bytes past the first 8 are ignored.)
    /// If there are fewer than 8 input bytes, pads the end with zeros internally.
    ///
    /// # Safety
    /// There must be at least 8 bytes of safely accessible memory starting from the pointer.
    ///
    /// Note: These unusual API requirements are for performance; they avoid a memcpy call and branching!
    pub unsafe fn from_raw_parts(input: *const NonZeroU8, len: NonZeroUsize) -> Self {
        let int = u64::from_be_bytes(*input.cast());
        let zeros_needed = 8usize.saturating_sub(len.into());

        Self(NonZeroU64::new_unchecked(int >> zeros_needed))
    }
}

#[cfg(test)]
mod str4_from_str {
    use crate::Str4;
    use core::mem::size_of;

    const SIZE: usize = size_of::<Str4>();

    #[test]
    fn len() {
        let mut buf = String::with_capacity(10);

        for len in SIZE..buf.capacity() {
            for ch_code in 0..len {
                let ch = char::from_u32(ch_code as u32 + 97).unwrap();

                buf.push(ch);
            }

            let input =
                unsafe { Str4::from_raw_parts(buf.as_ptr().cast(), buf.len().try_into().unwrap()) };

            assert_eq!(input.len(), buf.len().min(SIZE));

            buf.clear();
        }
    }

    #[test]
    fn first_index_in() {
        let mut buf = String::with_capacity(10);
        let mut src = Vec::with_capacity(20);
        let mut last_input = None;
        let mut base_char_code = 97;

        for len in SIZE..buf.capacity() {
            for ch in 0..len {
                let ch = char::from_u32(base_char_code + ch as u32).unwrap();

                buf.push(ch);
            }

            base_char_code += 1;

            let input =
                unsafe { Str4::from_raw_parts(buf.as_ptr().cast(), buf.len().try_into().unwrap()) };

            assert_eq!(input.len(), buf.len().min(SIZE));

            src.push(input);

            buf.clear();

            last_input = Some(input);
        }

        assert_ne!(src.len(), 0);

        for index in 0..src.len() {
            let expected = Some(src.len() - index);
            let actual = unsafe { dbg!(last_input.unwrap()).first_index_in(dbg!(&src[index..])) };

            assert_eq!(
                expected, actual,
                "first_index_in(&src[{index}..]) was {:?} but expected {:?}",
                actual, expected
            );
        }
    }
}

#[cfg(test)]
mod str8_from_str {
    use crate::Str8;
    use core::mem::size_of;

    const SIZE: usize = size_of::<Str8>();

    #[test]
    fn multiple_chars() {
        let mut buf = String::with_capacity(2 + SIZE * 2);

        for len in SIZE..buf.capacity() {
            for ch_code in 0..len {
                let ch = char::from_u32(ch_code as u32 + 97).unwrap();

                buf.push(ch);
            }

            let input =
                unsafe { Str8::from_raw_parts(buf.as_ptr().cast(), buf.len().try_into().unwrap()) };

            assert_eq!(input.len(), buf.len().min(SIZE));

            buf.clear();
        }
    }
}
