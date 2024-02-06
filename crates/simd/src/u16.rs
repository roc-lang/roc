use core::num::NonZeroU16;

/// This is only needed in aarch64; in x64, we call _mm_set_epi32 directly
#[cfg(target_arch = "aarch64")]
#[cfg(target_endian = "little")]
const ANSWER_MASK: [u16; 8] = [7, 6, 5, 4, 3, 2, 1, 0];

/// Our SIMD vectors will be u16x8
const SIMD128_LANES: usize = 8;
const SIMD128_ALIGN: usize = 16;

/// Returns Some(first index where self occurs in the given slice) or else None if it wasn't found.
/// (Essentially the same as `slice.iter().position(|e| e == needle)`, except using SIMD.)
///
/// # Safety
/// There must bet at least least 16B of accessible memory after the end of the slice.
/// This is because we keep iterating in chunks of 16B
/// The reason for this is that we start by dereferencing the
/// slice's pointer into a 16B structure, and then discarding any memory garbage we find.
/// So there must be 16B there that we can safely dereference, even if it contains garbage!
pub unsafe fn first_index_in(needle: NonZeroU16, slice: &[NonZeroU16]) -> Option<usize> {
    let len = slice.len();
    let first_elem = slice.as_ptr();
    let slice_end: *const u32 = first_elem.add(len).cast();
    let mut current_elem: *const u32;

    #[cfg(target_arch = "aarch64")]
    use core::arch::aarch64;

    #[cfg(target_arch = "x86_64")]
    use core::arch::x86_64;

    #[cfg(target_arch = "wasm32")]
    use core::arch::wasm32;

    // Initialize the answer mask and the needle we're searching for.
    let answer_mask;
    let needle_reg;

    #[cfg(target_arch = "aarch64")]
    {
        answer_mask = aarch64::vld1q_u16(ANSWER_MASK.as_ptr()); // Load ANSWER_MASK into a SIMD register for later
        needle_reg = aarch64::vdupq_n_u16(needle.get()); // Load self into each lane of a SIMD register
    }

    #[cfg(target_arch = "x86_64")]
    {
        #[cfg(target_endian = "little")]
        {
            answer_mask = x86_64::_mm_set_epi16(7, 6, 5, 4, 3, 2, 1, 0);
        }

        needle_reg = x86_64::_mm_set1_epi16(*(self as *const Self).cast());
    }

    #[cfg(target_arch = "wasm32")]
    {
        #[cfg(target_endian = "little")]
        {
            answer_mask = wasm32::i16x8(7, 6, 5, 4, 3, 2, 1, 0);
        }
        needle_reg = wasm32::i16x8_splat(*(self as *const Self).cast());
    }

    let success = |ptr: *const NonZeroU16, offset: usize| {
        let index = (ptr.offset_from(first_elem) + (offset as isize)) as usize;

        // It's possible that we got a false positive because we encountered memory garbage
        // after the last element, and it happened to be a match. If that's the case, it must
        // mean we examined all the elements and didn't find a match, so return None.
        // (Note that this does not optimize to a conditional branch; it's very cheap.)
        if index < len {
            Some(index)
        } else {
            None
        }
    };

    macro_rules! check_equality {
        ($ptr:expr) => {{
            #[cfg(target_arch = "aarch64")]
            {
                // Compare each lane of the chunk to the needle for equality.
                // Apply a mask afterwards so we can discard lanes that contained garbage memory.
                let equality_reg = aarch64::vceqq_u16(aarch64::vld1q_u16($ptr), needle_reg);

                // If lane-wise equality returned any nonzero values, we found a match.
                if aarch64::vmaxvq_u16(equality_reg) != 0 {
                    // Do a bitwise AND with answer_mask, whose lanes are initialized to [7, 6, ..., 0]
                    // and then get the scalar min of that, which will tell us the first lane's index
                    // (from the end; this is necessary because if we did it from the beginning, we'd
                    // have to use min instead of max, but then min would always return 0 for the not-found ones)
                    // where we found an occurrence of the needle. (If multiple lanes matched, then after
                    // we have subtracted this from Self::SIMD128_LANES, we'll end up with the first lane that matched,
                    // which is what we want this function to do.)
                    let lane_index =
                        SIMD128_LANES - (aarch64::vmaxvq_u16(aarch64::vandq_u16(equality_reg, answer_mask)) as usize) - 1;

                    return success($ptr, lane_index);
                }
            }

            #[cfg(target_arch = "x86_64")]
            {
                // Compare each lane of the chunk to the needle for equality.
                let equality_reg = x86_64::_mm_cmpeq_epi16(x86_64::_mm_loadu_si128($ptr), needle_reg);

                // Get a mask of the most significant bit of each element in the equality comparison,
                // and store it in the 4 least significant bits of the return value. (All other bits are 0.)
                let mask = x86_64::_mm_movemask_ps(x86_64::_mm_castsi128_ps(equality_reg));

                // Check if the mask was nonzero by converting to NonZeroU32,
                // because NonZeroU32 has a more efficient trailing_zeros() than u32 does.
                if let Some(mask) = NonZeroU16::new(mask as u16) {
                    #[cfg(target_endian = "little")]
                    {
                        let lane_index = mask.trailing_zeros() as usize;

                        return success($ptr, lane_index);
                    }
                }
            }

            #[cfg(target_arch = "wasm32")]
            {
                // Compare each lane of the chunk to the needle for equality.
                let equality_reg = wasm32::i16x8_eq(wasm32::v128_load($ptr), needle);

                // Get a mask of the most significant bit of each element in the equality comparison,
                // and store it in the 4 least significant bits of the return value. (All other bits are 0.)
                let mask = wasm32::i16x8_bitmask(equality_reg);

                // Check if the mask was nonzero by converting to NonZeroU64,
                // because NonZeroU32 has a more efficient trailing_zeros() than u32 does.
                if let Some(mask) = NonZeroU32::new(mask as u32) {
                    #[cfg(target_endian = "little")]
                    {
                        let lane_index = mask.trailing_zeros() as usize;

                        return success($ptr, lane_index);
                    }
                }
            }
        }};
    }

    // Special-case the first chunk, which may not be 16B-aligned.
    // (Subsequent chunks will definitely be 16B-aligned.)
    {
        check_equality!(first_elem.cast());

        // Advance to the next 16B boundary so we can do aligned loads on each subsequent chunk.
        // If we were already on a 16B boundary, then branchlessly advance by Self::SIMD128_LANES - otherwise
        // the first iteration of the loop will redo the exact same work we just did.
        let align_offset = first_elem.align_offset(SIMD128_ALIGN);
        let amount_to_advance = if align_offset == 0 {
            SIMD128_LANES
        } else {
            align_offset
        };

        current_elem = first_elem.add(amount_to_advance).cast();
    }

    while slice_end.offset_from(current_elem) > 0 {
        check_equality!(current_elem.cast());

        // Advance to the next chunk
        current_elem = current_elem.add(SIMD128_LANES);
    }

    None
}
