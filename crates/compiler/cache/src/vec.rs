use core::alloc::Layout;
use core::marker::PhantomData;
use core::mem::{size_of, align_of};
use core::ptr::{self, null};

use bumpalo::{Bump, AllocErr};

/// Like a std::vec::Vec, except:
/// - Has a lifetime that's associated with a particular Arena
/// - 32-bit length and capacity, not usize
pub struct Vec<'a, T> {
    first_elem: *const T,
    len: u32,
    cap: u32,

    _phantom: PhantomData<&'a T>,
}

impl<'a, T> Default for Vec<'a, T> {
    fn default() -> Self {
        Vec {
            first_elem: null(),
            len: 0,
            cap: 0,

            _phantom: PhantomData::default(),
        }
    }
}

pub enum VecErr {
    AllocFailed(AllocErr),
    LenOverflow
}

impl<'a, T> Vec<'a, T> {
    const MAX_CAPACITY: usize = (u32::MAX as usize).min(isize::MAX as usize);

    pub fn with_capacity(arena: &'a Bump, capacity: u32) -> Result<Self, VecErr> {
        let mut answer = Self::default();

        answer.resize(arena, capacity as usize)?;

        Ok(answer)
    }

    pub fn push(&mut self, arena: &'a Bump, elem: T) -> Result<(), VecErr> {
        self.extend_from_iter(arena, core::iter::once(elem))
    }

    pub fn extend_from_iter(&mut self, arena: &'a Bump, elems: impl ExactSizeIterator<Item=T>) -> Result<(), VecErr> {
        let old_len = self.len as usize;
        // Safety: we can use saturating_add here because MAX_CAPACITY is
        // no more than isize::MAX and we error if this exceeds that.
        // Putting those together, overflow will error here.
        let new_len = elems.len().saturating_add(old_len);

        if new_len <= Self::MAX_CAPACITY {
            let cap = self.cap as usize;

            // If we can't fit the new length in the new capacity, resize and
            // copy the old elements into the new allocation if necessary.
            if cap < new_len {
                let new_cap = (cap / 2).saturating_mul(3).min(new_len);
                let old_ptr = self.first_elem;

                self.resize(arena, new_cap)?;

                // Now that we've resized into a potentially-new allocation,
                // copy the old elements into the new location.
                let new_ptr = self.first_elem as *mut T;

                // If the pointer didn't change, no need to copy. Also,
                // if the old pointer was null, there's nothing to copy (and copying would be UB).
                if old_ptr != new_ptr  && !old_ptr.is_null() {
                    unsafe { ptr::copy_nonoverlapping(old_ptr, new_ptr, old_len) }
                }
            }

            // Safety: we already verified that increasing the length by this much doesn't overflow.
            let mut next = unsafe { self.first_elem.add(old_len) } as *mut T;

            for elem in elems {
                unsafe {
                    // Safety: this should always have the correct alignment,
                    // and next.add should always succeed because we've predetermined
                    // that there's enoguh room to do this.
                    *next = elem;
                    next = next.add(1);
                }
            }

            // Safety: we've already verified that new_len <= Self::MAX_CAPACITY,
            // which itself is no more than u32::MAX.
            self.len = new_len as u32;

            Ok(())
        } else {
            Err(VecErr::LenOverflow)
        }
    }

    fn resize(&mut self, arena: &'a Bump, requested_capacity: usize) -> Result<(), VecErr> {
        let capacity = requested_capacity.min(Self::MAX_CAPACITY);

        // If we're not actually capable of adding more capacity, then resizing failed.
        if requested_capacity <= capacity {
            return Err(VecErr::LenOverflow);
        }

        match capacity.checked_mul(size_of::<T>()) {
            Some(size_in_bytes) => {
                match Layout::from_size_align(size_in_bytes, align_of::<T>()) {
                    Ok(layout) => {
                        match arena.try_alloc_layout(layout) {
                            Ok(ptr) => {
                                // Safety: we already verified capacity is no more than MAX_CAPACITY,
                                // which in turn is no more than u32::MAX.
                                self.cap = capacity as u32;
                                self.first_elem = ptr.as_ptr().cast();


                                Ok(())
                            }
                            Err(alloc_err) => {
                                Err(VecErr::AllocFailed(alloc_err))
                            }
                        }
                    }
                    Err(_) => Err(VecErr::LenOverflow)
                }
            }
            None => {
                Err(VecErr::LenOverflow)
            }
        }
    }
}
