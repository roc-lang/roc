struct Str8;
struct Str16;

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(align(4))]
pub struct Str4([u8; Str4::CAPACITY]);

impl Str4 {
    pub const EMPTY: Self = Self([0; Self::CAPACITY]);

    const CAPACITY: usize = 4;

    /// This is only needed in aarch64; in x64, we call _mm_set_epi32 directly
    #[cfg(target_arch = "aarch64")]
    const ANSWER_MASK: [u32; Self::CAPACITY] = [0, 1, 2, 3];

    pub fn len(&self) -> usize {
        Self::CAPACITY - self.0.trailing_zeros()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the first 4 bytes of the input string as a Str4.
    /// (Other bytes past the first 4 are ignored.)
    /// If there are fewer than 4 input bytes, pads the end with zeroes.
    pub fn from_str(input: &str) -> Self {
        Self(from_str::<{ Self::CAPACITY }>(input))
    }

    /// Returns Some(first index where self occurs in the given slice) or else None if it wasn't found.
    pub fn first_index_in<'a>(&self, slice: &'a [Self]) -> Option<usize> {
        if slice.is_empty() {
            return None;
        }

        // Each SIMD register holds 4 u32s
        const NUM_LANES: usize = 4;
        const ALIGN: usize = NUM_LANES * std::mem::align_of::<Str4>();

        unsafe {
            #[cfg(target_arch = "aarch64")]
            use std::arch::aarch64::*;

            #[cfg(target_arch = "x86_64")]
            use std::arch::x86_64::*;

            #[cfg(target_arch = "wasm32")]
            use std::arch::wasm32::*;

            let mut current_elem: *const u32 = slice.as_ptr().cast(); // Start at the beginning of the slice
            let slice_end: *const u32 = current_elem.add(slice.len()); // Pointer to right after slice's last elem

            // The beginning of the slice may not have the necessary alignment to be loaded directly into a
            // SIMD register. Also, the slice may be shorter than the number of lanes in the SIMD register.
            //
            // To ensure safety, the first time we load a SIMD register with strings, we zero-pad both sides;
            // the start is zero-padded as necessary to ensure alignment, and the end is zeros by default.
            let mut current_chunk = {
                let zeros_needed = (slice.as_ptr() as usize) % ALIGN;
                let elems_to_copy = NUM_LANES - zeros_needed;

                // This wrapper type ensures the alignment we need.
                let mut array = Str4x4([Self::EMPTY; NUM_LANES]);

                array.0[elems_to_copy..].copy_from_slice(&slice[..zeros_needed]);

                // We already processed these elems, so advance past them.
                current_elem = current_elem.add(elems_to_copy);

                // After advancing current_elem, it should now have the correct alignment.
                debug_assert_eq!(current_elem as usize % ALIGN, 0);

                let array_ptr = array.0.as_ptr();

                // The array should have the correct alignment for us to use vld1q_u32 on it.
                debug_assert_eq!(array_ptr as usize % ALIGN, 0);

                #[cfg(target_arch = "aarch64")]
                {
                    vld1q_u32(array_ptr.cast())
                }

                #[cfg(target_arch = "x86_64")]
                {
                    _mm_loadu_si128(array_ptr.cast())
                }

                #[cfg(target_arch = "wasm32")]
                {
                    v128_load(current_elem.cast())
                }
            };

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

            loop {
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

                    if mask != 0 {
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

                    if mask != 0 {
                        let lane_index = mask.trailing_zeros() as usize;

                        return success(current_elem, lane_index);
                    }
                }

                // Advance to the next chunk
                current_elem = current_elem.add(NUM_LANES);

                if current_elem >= slice_end {
                    return None;
                }

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
            }
        }
    }
}

#[repr(align(16))]
struct Str4x4([Str4; 4]);

impl From<[u8; Str4::CAPACITY]> for Str4 {
    fn from(value: [u8; Str4::CAPACITY]) -> Self {
        Self(value)
    }
}

impl From<Str4> for [u8; Str4::CAPACITY] {
    fn from(value: Str4) -> Self {
        value.0
    }
}

/// Returns the first CAPACITY bytes of the input string.
/// (Other bytes past the first CAPACITY are ignored.)
/// If there are fewer than CAPACITY input bytes, pads the end with zeroes.
fn from_str<const CAPACITY: usize>(input: &str) -> [u8; CAPACITY] {
    let mut answer = [0u8; CAPACITY];
    let copy_len = input.len().min(CAPACITY);
    let input_bytes = &input.as_bytes()[..copy_len];

    // The input string should have no bytes that are 0.
    // (Or at least none of the bytes we're using from it should be 0.)
    debug_assert_eq!(input_bytes.iter().filter(|b| b == 0).count(), 0);

    answer[..copy_len].copy_from_slice(input_bytes);

    answer
}
