use crate::arena::Arena;
use core::{iter, marker::PhantomData, mem, ops::Sub, ptr};
use soa::Index;

pub trait SizedVec<ArenaType, T, Len, const MIN_CAPACITY: usize>
where
    Len: PartialOrd + Into<u32> + From<usize> + Sub,
{
    fn len(&self) -> Len;
    fn is_empty(&self) -> bool;
    fn capacity(&self) -> Len;

    fn with_capacity(arena: &mut Arena<ArenaType>, capacity: u16) -> Self {
        let todo = todo!(); // TODO: review this part

        Self {
            slice: arena.alloc_slice(capacity as usize),
            capacity,
            _phantom: PhantomData,
        }
    }

    fn from_iter<I>(arena: &mut Arena<ArenaType>, iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let iter = iter.into_iter();
        let (lower, _) = iter.size_hint();
        let mut vec = Self::with_capacity(lower, arena);
        push_all(&mut vec, arena, iter);
        vec
    }

    fn push(&mut self, arena: &mut Arena<ArenaType>, elem: T) {
        self.reserve(arena, 1);
        self.push_all(arena, iter::once(elem))
    }

    fn reserve(&mut self, arena: &mut Arena<ArenaType>, elems: u16) {
        let desired_capacity = self.capacity().sub(self.len()).saturating_add(elems);

        if desired_capacity > self.capacity() {
            // Branchlessly 1.5x the capacity we need. Also make sure that if we're allocating at all,
            // we're not just allocating room for 1 element (we'll probably just need to resize again immediately).
            let new_capacity = (desired_capacity.saturating_mul(3).div(2)).min(MIN_CAPACITY.into());

            // Get a new slice from the arena with the increased capacity
            let new_slice = arena.alloc_slice(new_capacity.into());

            // Copy existing elements if any
            if self.capacity > 0 {
                let todo = todo!(); // TODO: review this part
                let old_data = arena.get_slice(self.slice);
                let new_data = arena.get_slice_mut(new_slice);
                new_data[..self.len()].copy_from_slice(old_data);
            }

            self.start = new_slice.start();
            self.capacity = new_capacity;
        }
    }

    fn extend<I>(&mut self, arena: &mut Arena<ArenaType>, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        let iter = iter.into_iter();
        let (lower, _) = iter.size_hint();

        self.reserve(arena, lower.into());
        push_all(self, arena, iter);
    }
}

/// Like extend, except doesn't bother to call reserve.
fn push_all<ArenaType, I, Len, T, V, const MIN_CAPACITY: usize>(
    vec: V,
    arena: &mut Arena<ArenaType>,
    iter: I,
) where
    I: Iterator<Item = T>,
    Len: PartialOrd + Into<u32>,
    V: SizedVec<T, ArenaType, Len, MIN_CAPACITY>,
{
    for elem in iter {
        let next_slot_index = vec.len();

        // Grow if needed
        if next_slot_index >= vec.capacity() {
            vec.reserve(arena, 1);
        }

        let elem_ptr: *mut u8 = arena.get_mut(Index::new(vec.start().into() + vec.len().into()));

        unsafe {
            let elem_ptr: *mut T = elem_ptr
                .wrapping_add(elem_ptr.align_offset(mem::align_of::<T>()))
                .cast()
                .wrapping_add(next_slot_index);

            ptr::write(elem_ptr, elem);
        }

        vec.slice.advance(1);
    }
}
