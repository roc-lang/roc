use std::num::{NonZeroU32, NonZeroU64, NonZeroU8};

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
            const ALIGN: usize = Self::NUM_LANES * std::mem::align_of::<$name>();

            /// Safety: The given slice must:
            /// - have a nonzero length that's a multiple of 4
            /// - point to a memory address that's disible by 16
            pub unsafe fn new_unchecked(slice: &'a [$name]) -> Self {
                debug_assert!(!slice.is_empty());
                debug_assert_eq!(slice.len() % Self::NUM_LANES, 0);
                debug_assert_eq!(
                    0,
                    ((slice as *const _) as *const $int) as usize % Self::ALIGN
                );

                std::mem::transmute(slice)
            }

            pub fn as_slice(&self) -> &[$name] {
                // Internally, we have chunks of 4; adjust the returned slice's length accordingly.
                unsafe {
                    std::slice::from_raw_parts(
                        self.0.as_ptr().cast(),
                        self.0.len() * Self::NUM_LANES,
                    )
                }
            }
        }

        impl $name {
            #[cfg(target_endian = "little")] // This implementation relies on little-endian byte ordering for the int
            pub fn len(&self) -> usize {
                // NonZeroU__::trailing_zeros compiles to a single instruction on x64, whereas
                // u__::trailing_zeros compiles to a conditional branch. This is becuase apparently some
                // CPUs do different things when asked to count leading or trailing zeros of the number 0.
                std::mem::size_of::<Self>() - (self.0.trailing_zeros() as usize / 8)
            }

            /// Returns the first 4 bytes of the input as a Str4.
            /// (Other bytes past the first 4 are ignored.)
            /// If there are fewer than 4 input bytes, pads the end with zeros internally.
            ///
            /// Safety: The input slice must not be empty.
            pub unsafe fn from_nonempty_bytes(input: &[NonZeroU8]) -> Self {
                debug_assert!(!input.is_empty());

                let int_input = <$int>::from_be_bytes(from_bytes(input));

                // Safety: as noted in this function's safety section, this slice must not be empty
                Self(unsafe { <$nonzero_int>::new_unchecked(int_input) })
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
    pub fn first_index_in(&self, slice: Str4Chunks<'_>) -> Option<usize> {
        // Each SIMD register holds 4 u32s
        const NUM_LANES: usize = 4;
        const ALIGN: usize = NUM_LANES * std::mem::align_of::<Str4>();

        #[cfg(target_arch = "aarch64")]
        use std::arch::aarch64::*;

        #[cfg(target_arch = "x86_64")]
        use std::arch::x86_64::*;

        #[cfg(target_arch = "wasm32")]
        use std::arch::wasm32::*;

        unsafe {
            let mut current_elem: *const u32 = slice.0.as_ptr().cast(); // Start at the beginning of the slice
            let slice_end: *const u32 = current_elem.add(slice.0.len()); // Pointer to right after slice's last elem
            let answer_mask;
            let needle;

            #[cfg(target_arch = "aarch64")]
            {
                answer_mask = vld1q_u32(Self::ANSWER_MASK.as_ptr()); // Load ANSWER_MASK into a SIMD register for later
                needle = vdupq_n_u32(*(self as *const Self).cast()); // Load self into each lane of a SIMD register
            }

            #[cfg(target_arch = "x86_64")]
            {
                answer_mask = _mm_set_epi32(3, 2, 1, 0);
                needle = _mm_set1_epi32(*(self as *const Self).cast());
            }

            let success = |current_elem, lane_index| {
                Some(slice_end as usize - current_elem as usize + lane_index)
            };

            while current_elem < slice_end {
                let current_chunk;

                // Advance to the next chunk
                current_elem = current_elem.add(NUM_LANES);

                // current_elem should always have the correct alignment
                debug_assert_eq!(current_elem as usize % ALIGN, 0);

                #[cfg(target_arch = "aarch64")]
                {
                    current_chunk = vld1q_u32(current_elem);
                }

                #[cfg(target_arch = "x86_64")]
                {
                    current_chunk = _mm_loadu_si128(current_elem.cast());
                }

                #[cfg(target_arch = "wasm32")]
                {
                    current_chunk = v128_load(current_elem.cast());
                }

                #[cfg(target_arch = "aarch64")]
                {
                    // Compare each lane of the chunk to the needle for equality.
                    let equality_reg = vceqq_u32(current_chunk, needle);

                    // If lane-wise equality returned any nonzero values, we found a match.
                    if vmaxvq_u32(equality_reg) != 0 {
                        // Do a bitwise AND with answer_mask, whose lanes are initialized to [0, 1, 2, 3]
                        // and then get the scalar min of that, which will tell us the first lane's index
                        // where we found an occurrence of the needle. (If multiple lanes matched, min will
                        // give us the first one that matched, which is what we want given find's semantics.)
                        let lane_index = vminvq_u32(vandq_u32(equality_reg, answer_mask)) as usize;

                        return success(current_elem, lane_index);
                    }
                }

                #[cfg(target_arch = "x86_64")]
                {
                    // Compare each lane of the chunk to the needle for equality.
                    let equality_reg = _mm_cmpeq_epi32(current_chunk, needle);

                    // Get a mask of the most significant bit of each element in the equality comparison,
                    // and store it in the 4 least significant bits of the return value. (All other bits are 0.)
                    let mask = _mm_movemask_ps(_mm_castsi128_ps(equality_reg));

                    // Check if the mask was nonzero by converting to NonZeroU32,
                    // because NonZeroU32 has a more efficient trailing_zeros() than u32 does.
                    if let Some(mask) = NonZeroU32::new(mask) {
                        #[cfg(target_endian = "little")]
                        let lane_index = mask.trailing_zeros() as usize;

                        return success(current_elem, lane_index);
                    }
                }

                #[cfg(target_arch = "wasm32")]
                {
                    // Compare each lane of the chunk to the needle for equality.
                    let equality_reg = i32x4_eq(current_chunk, needle);

                    // Get a mask of the most significant bit of each element in the equality comparison,
                    // and store it in the 4 least significant bits of the return value. (All other bits are 0.)
                    let mask = i32x4_bitmask(equality_reg);

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
}

/// Returns the first CAPACITY bytes of the input string.
/// (Other bytes past the first CAPACITY are ignored.)
/// If there are fewer than CAPACITY input bytes, pads the end with zeroes.
fn from_bytes<const CAPACITY: usize>(input: &[NonZeroU8]) -> [u8; CAPACITY] {
    let mut answer = [0u8; CAPACITY];
    let copy_len = input.len().min(CAPACITY);

    // Safety: it's always safe to transmute NonZeroU8 to u8
    let u8_input: &[u8] = unsafe { std::mem::transmute(input) };

    answer[..copy_len].copy_from_slice(&u8_input[..copy_len]);

    answer
}

make_str_n!(2, Str8, Str8Chunk, Str8Chunks, u64, NonZeroU64);

impl Str8 {
    /// Returns Some(first index where self occurs in the given slice) or else None if it wasn't found.
    pub fn first_index_in(&self, slice: Str8Chunks<'_>) -> Option<usize> {
        // Each SIMD register holds 4 u32s
        const NUM_LANES: usize = 4;
        const ALIGN: usize = NUM_LANES * std::mem::align_of::<Str8>();

        #[cfg(target_arch = "aarch64")]
        use std::arch::aarch64::*;

        #[cfg(target_arch = "x86_64")]
        use std::arch::x86_64::*;

        #[cfg(target_arch = "wasm32")]
        use std::arch::wasm32::*;

        unsafe {
            let mut current_elem: *const u64 = slice.0.as_ptr().cast(); // Start at the beginning of the slice
            let slice_end: *const u64 = current_elem.add(slice.0.len()); // Pointer to right after slice's last elem
            let needle;

            #[cfg(not(target_arch = "aarch64"))]
            let answer_mask;

            #[cfg(target_arch = "aarch64")]
            {
                needle = vdupq_n_u64(*(self as *const Self).cast()); // Load self into each lane of a SIMD register
            }

            #[cfg(target_arch = "x86_64")]
            {
                answer_mask = _mm_set_epi64(1, 0);
                needle = _mm_set1_epi64(*(self as *const Self).cast());
            }

            let success = |current_elem, lane_index| {
                Some(slice_end as usize - current_elem as usize + lane_index)
            };

            while current_elem < slice_end {
                let current_chunk;

                // Advance to the next chunk
                current_elem = current_elem.add(NUM_LANES);

                // current_elem should always have the correct alignment
                debug_assert_eq!(current_elem as usize % ALIGN, 0);

                #[cfg(target_arch = "aarch64")]
                {
                    current_chunk = vld1q_u64(current_elem);
                }

                #[cfg(target_arch = "x86_64")]
                {
                    current_chunk = _mm_loadu_si128(current_elem.cast());
                }

                #[cfg(target_arch = "wasm32")]
                {
                    current_chunk = v128_load(current_elem.cast());
                }

                #[cfg(target_arch = "aarch64")]
                {
                    // Compare each lane of the chunk to the needle for equality.
                    let equality_reg = vceqq_u64(current_chunk, needle);

                    let lane0 = vgetq_lane_u64(equality_reg, 0);
                    let lane1 = vgetq_lane_u64(equality_reg, 1);

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
                    // Compare each lane of the chunk to the needle for equality.
                    let equality_reg = _mm_cmpeq_epi32(current_chunk, needle);

                    // Get a mask of the most significant bit of each element in the equality comparison,
                    // and store it in the 4 least significant bits of the return value. (All other bits are 0.)
                    let mask = _mm_movemask_ps(_mm_castsi128_ps(equality_reg));

                    // Check if the mask was nonzero by converting to NonZeroU64,
                    // because NonZeroU32 has a more efficient trailing_zeros() than u32 does.
                    if let Some(mask) = NonZeroU32::new(mask) {
                        #[cfg(target_endian = "little")]
                        let lane_index = mask.trailing_zeros() as usize;

                        return success(current_elem, lane_index);
                    }
                }

                #[cfg(target_arch = "wasm32")]
                {
                    // Compare each lane of the chunk to the needle for equality.
                    let equality_reg = i32x4_eq(current_chunk, needle);

                    // Get a mask of the most significant bit of each element in the equality comparison,
                    // and store it in the 4 least significant bits of the return value. (All other bits are 0.)
                    let mask = i32x4_bitmask(equality_reg);

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
}

#[cfg(test)]
mod str4_from_str {
    use crate::Str4;

    const SIZE: usize = 4;

    #[test]
    fn multiple_chars() {
        let mut buf = String::with_capacity(10);

        for len in SIZE..buf.capacity() {
            for ch_code in 0..len {
                let ch = char::from_u32(ch_code as u32 + 97).unwrap();

                buf.push(ch);
            }

            let input = unsafe { Str4::from_nonempty_bytes(std::mem::transmute(buf.as_str())) };

            assert_eq!(input.len(), buf.len().min(SIZE));

            buf.clear();
        }
    }
}

#[cfg(test)]
mod str8_from_str {
    use crate::Str8;

    const SIZE: usize = 8;

    #[test]
    fn multiple_chars() {
        let mut buf = String::with_capacity(2 + SIZE * 2);

        for len in SIZE..buf.capacity() {
            for ch_code in 0..len {
                let ch = char::from_u32(ch_code as u32 + 97).unwrap();

                buf.push(ch);
            }

            let input = unsafe { Str8::from_nonempty_bytes(std::mem::transmute(buf.as_str())) };

            assert_eq!(input.len(), buf.len().min(SIZE));

            buf.clear();
        }
    }
}
