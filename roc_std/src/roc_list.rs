#![deny(unsafe_op_in_unsafe_fn)]

use core::{
    cell::Cell,
    cmp::{self, Ordering},
    ffi::c_void,
    fmt::Debug,
    intrinsics::copy_nonoverlapping,
    mem::{self, ManuallyDrop},
    ops::Deref,
    ptr::{self, NonNull},
};

use crate::{roc_alloc, roc_dealloc, roc_realloc, storage::Storage};

#[repr(C)]
pub struct RocList<T> {
    elements: Option<NonNull<ManuallyDrop<T>>>,
    length: usize,
    capacity: usize,
}

impl<T> RocList<T> {
    #[inline(always)]
    fn alloc_alignment() -> usize {
        mem::align_of::<T>().max(mem::align_of::<Storage>())
    }

    pub fn empty() -> Self {
        Self {
            elements: None,
            length: 0,
            capacity: 0,
        }
    }

    /// Create an empty RocList with enough space preallocated to store
    /// the requested number of elements.
    pub fn with_capacity(elems: usize) -> Self {
        Self {
            elements: Some(Self::elems_with_capacity(elems)),
            length: 0,
            capacity: elems,
        }
    }

    fn elems_with_capacity(elems: usize) -> NonNull<ManuallyDrop<T>> {
        let alignment = Self::alloc_alignment();
        let alloc_ptr = unsafe { roc_alloc(elems * mem::size_of::<T>(), alignment as u32) };

        Self::elems_from_allocation(NonNull::new(alloc_ptr).unwrap_or_else(|| {
            todo!("Call roc_panic with the info that an allocation failed.");
        }))
    }

    fn elems_from_allocation(allocation: NonNull<c_void>) -> NonNull<ManuallyDrop<T>> {
        let alignment = Self::alloc_alignment();
        let alloc_ptr = allocation.as_ptr();

        unsafe {
            let elem_ptr = alloc_ptr
                .cast::<u8>()
                .add(alignment)
                .cast::<ManuallyDrop<T>>();

            // Initialize the reference count.
            alloc_ptr
                .cast::<Storage>()
                .write(Storage::new_reference_counted());

            // The original alloc pointer was non-null, and this one is the alloc pointer
            // with `alignment` bytes added to it, so it should be non-null too.
            NonNull::new_unchecked(elem_ptr)
        }
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Note that there is no way to convert directly to a Vec.
    ///
    /// This is because RocList values are not allocated using the system allocator, so
    /// handing off any heap-allocated bytes to a Vec would not work because its Drop
    /// implementation would try to free those bytes using the wrong allocator.
    ///
    /// Instead, if you want a Rust Vec, you need to do a fresh allocation and copy the
    /// bytes over - in other words, calling this `as_slice` method and then calling `to_vec`
    /// on that.
    pub fn as_slice(&self) -> &[T] {
        &*self
    }

    #[inline(always)]
    fn elements_and_storage(&self) -> Option<(NonNull<ManuallyDrop<T>>, &Cell<Storage>)> {
        let elements = self.elements?;
        let storage = unsafe { &*elements.as_ptr().cast::<Cell<Storage>>().sub(1) };
        Some((elements, storage))
    }

    pub(crate) fn storage(&self) -> Option<Storage> {
        self.elements_and_storage()
            .map(|(_, storage)| storage.get())
    }

    /// Useful for doing memcpy on the elements. Returns NULL if list is empty.
    pub(crate) unsafe fn ptr_to_first_elem(&self) -> *const T {
        unsafe { core::mem::transmute(self.elements) }
    }

    /// Useful for doing memcpy on the underlying allocation. Returns NULL if list is empty.
    pub(crate) unsafe fn ptr_to_allocation(&self) -> *const c_void {
        unsafe { (self.ptr_to_first_elem() as *const usize).sub(1).cast() }
    }
}

impl<T> RocList<T>
where
    T: Clone,
{
    /// Increase a RocList's capacity by at least the requested number of elements (possibly more).
    ///
    /// May return a new RocList, if the provided one was not unique.
    pub fn reserve(self, num_elems: usize) -> Self {
        let new_elems;
        let old_elements_ptr;

        match self.elements_and_storage() {
            Some((elements, storage)) => {
                if storage.get().is_unique() {
                    unsafe {
                        let original_ptr = self.ptr_to_allocation();

                        // Try to reallocate in-place.
                        let new_ptr = roc_realloc(
                            original_ptr as *mut _,
                            num_elems,
                            self.length,
                            Self::alloc_alignment() as u32,
                        );

                        if new_ptr == original_ptr as *mut _ {
                            // We successfully reallocated in-place; we're done!
                            return self;
                        } else {
                            // We got back a different allocation; copy the existing elements
                            // into it. We don't need to increment their refcounts because
                            // The existing allocation that references to them is now gone and
                            // no longer referencing them.
                            new_elems = Self::elems_from_allocation(
                                NonNull::new(new_ptr).unwrap_or_else(|| {
                                    todo!("Reallocation failed");
                                }),
                            );
                            old_elements_ptr = elements.as_ptr();
                        }

                        // Note that realloc automatically deallocates the old allocation,
                        // so we don't need to call roc_dealloc here.
                    }
                } else {
                    // Make a new allocation
                    new_elems = Self::elems_with_capacity(num_elems);
                    old_elements_ptr = elements.as_ptr();

                    // Decrease the current allocation's reference count.
                    let mut new_storage = storage.get();
                    let needs_dealloc = new_storage.decrease();

                    if needs_dealloc {
                        let alignment = Self::alloc_alignment();

                        // Unlike in Drop, do *not* decrement the refcounts of all the elements!
                        // The new allocation is referencing them, so instead of incrementing them all
                        // all just to decrement them again here, we neither increment nor decrement them.
                        unsafe {
                            // Release the memory.
                            roc_dealloc(
                                old_elements_ptr.cast::<u8>().sub(alignment).cast(),
                                alignment as u32,
                            );
                        }
                    } else {
                        if !new_storage.is_readonly() {
                            // Write the storage back.
                            storage.set(new_storage);
                        }
                    }
                }
            }
            None => {
                // This is an empty list, so `reserve` is the same as `with_capacity`.
                return Self::with_capacity(num_elems);
            }
        }

        unsafe {
            // Copy the old elements to the new allocation.
            copy_nonoverlapping(old_elements_ptr, new_elems.as_ptr(), self.length);
        }

        let length = self.length;

        // We already manually cleaned up `self` by now, so don't let its Drop run too.
        mem::forget(self);

        Self {
            elements: Some(new_elems),
            length,
            capacity: num_elems,
        }
    }

    pub fn from_slice(slice: &[T]) -> Self {
        let mut list = Self::empty();
        list.extend_from_slice(slice);
        list
    }

    pub fn extend_from_slice(&mut self, slice: &[T]) {
        // TODO: Can we do better for ZSTs? Alignment might be a problem.

        if slice.is_empty() {
            return;
        }

        let alignment = Self::alloc_alignment();
        let elements_offset = alignment;

        let new_size = elements_offset + mem::size_of::<T>() * (self.len() + slice.len());

        let non_null_elements = if let Some((elements, storage)) = self.elements_and_storage() {
            // Decrement the list's refence count.
            let mut copy = storage.get();
            let is_unique = copy.decrease();

            if is_unique {
                // If the memory is not shared, we can reuse the memory.
                let old_size = elements_offset + (mem::size_of::<T>() * self.len());

                let new_ptr = unsafe {
                    let ptr = elements.as_ptr().cast::<u8>().sub(alignment).cast();

                    roc_realloc(ptr, new_size, old_size, alignment as u32).cast()
                };

                Self::elems_from_allocation(NonNull::new(new_ptr).unwrap_or_else(|| {
                    todo!("Reallocation failed");
                }))
            } else {
                if !copy.is_readonly() {
                    // Write the decremented reference count back.
                    storage.set(copy);
                }

                // Allocate new memory.
                let new_elements = Self::elems_with_capacity(new_size);

                // Copy the old elements to the new allocation.
                unsafe {
                    copy_nonoverlapping(elements.as_ptr(), new_elements.as_ptr(), self.length);
                }

                new_elements
            }
        } else {
            Self::elems_with_capacity(new_size)
        };

        self.elements = Some(non_null_elements);

        let elements = self.elements.unwrap().as_ptr();

        let append_ptr = unsafe { elements.add(self.len()) };

        // Use .cloned() to increment the elements' reference counts, if needed.
        for (i, new_elem) in slice.iter().cloned().enumerate() {
            unsafe {
                // Write the element into the slot, without dropping it.
                append_ptr
                    .add(i)
                    .write(ptr::read(&ManuallyDrop::new(new_elem)));
            }

            // It's important that the length is increased one by one, to
            // make sure that we don't drop uninitialized elements, even when
            // a incrementing the reference count panics.
            self.length += 1;
        }

        self.capacity = self.length
    }
}

impl<T> Deref for RocList<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        if let Some(elements) = self.elements {
            let elements = ptr::slice_from_raw_parts(elements.as_ptr().cast::<T>(), self.length);

            unsafe { &*elements }
        } else {
            &[]
        }
    }
}

impl<T> Default for RocList<T> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<T, U> PartialEq<RocList<U>> for RocList<T>
where
    T: PartialEq<U>,
{
    fn eq(&self, other: &RocList<U>) -> bool {
        self.deref() == other.deref()
    }
}

impl<T> Eq for RocList<T> where T: Eq {}

impl<T, U> PartialOrd<RocList<U>> for RocList<T>
where
    T: PartialOrd<U>,
{
    fn partial_cmp(&self, other: &RocList<U>) -> Option<cmp::Ordering> {
        // If one is longer than the other, use that as the ordering.
        match self.length.partial_cmp(&other.length) {
            Some(Ordering::Equal) => {}
            ord => return ord,
        }

        // If they're the same length, compare their elements
        for index in 0..self.len() {
            match self[index].partial_cmp(&other[index]) {
                Some(Ordering::Equal) => {}
                ord => return ord,
            }
        }

        // Capacity is ignored for ordering purposes.
        Some(Ordering::Equal)
    }
}

impl<T> Ord for RocList<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        // If one is longer than the other, use that as the ordering.
        match self.length.cmp(&other.length) {
            Ordering::Equal => {}
            ord => return ord,
        }

        // If they're the same length, compare their elements
        for index in 0..self.len() {
            match self[index].cmp(&other[index]) {
                Ordering::Equal => {}
                ord => return ord,
            }
        }

        // Capacity is ignored for ordering purposes.
        Ordering::Equal
    }
}

impl<T> Debug for RocList<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<T> Clone for RocList<T> {
    fn clone(&self) -> Self {
        // Increment the reference count
        if let Some((_, storage)) = self.elements_and_storage() {
            let mut new_storage = storage.get();

            if !new_storage.is_readonly() {
                new_storage.increment_reference_count();
                storage.set(new_storage);
            }
        }

        Self {
            elements: self.elements,
            length: self.length,
            capacity: self.capacity,
        }
    }
}

impl<T> Drop for RocList<T> {
    fn drop(&mut self) {
        if let Some((elements, storage)) = self.elements_and_storage() {
            // Decrease the list's reference count.
            let mut new_storage = storage.get();
            let needs_dealloc = new_storage.decrease();

            if needs_dealloc {
                unsafe {
                    // Drop the stored elements.
                    for index in 0..self.len() {
                        let elem_ptr = elements.as_ptr().add(index);

                        mem::drop::<T>(ManuallyDrop::take(&mut *elem_ptr));
                    }

                    let alignment = Self::alloc_alignment();

                    // Release the memory.
                    roc_dealloc(
                        elements.as_ptr().cast::<u8>().sub(alignment).cast(),
                        alignment as u32,
                    );
                }
            } else {
                if !new_storage.is_readonly() {
                    // Write the storage back.
                    storage.set(new_storage);
                }
            }
        }
    }
}

impl<T> From<&[T]> for RocList<T>
where
    T: Clone,
{
    fn from(slice: &[T]) -> Self {
        Self::from_slice(slice)
    }
}

impl<T> IntoIterator for RocList<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { list: self, idx: 0 }
    }
}

pub struct IntoIter<T> {
    list: RocList<T>,
    idx: usize,
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.list.len() <= self.idx {
            return None;
        }

        let elements = self.list.elements?;
        let element_ptr = unsafe { elements.as_ptr().add(self.idx) };
        self.idx += 1;

        // Return the element.
        Some(unsafe { ManuallyDrop::into_inner(element_ptr.read()) })
    }
}

impl<T> Drop for IntoIter<T> {
    fn drop(&mut self) {
        // If there are any elements left that need to be dropped, drop them.
        if let Some(elements) = self.list.elements {
            // Set the list's length to zero to prevent double-frees.
            // Note that this leaks if dropping any of the elements panics.
            let len = mem::take(&mut self.list.length);

            // Drop the elements that haven't been returned from the iterator.
            for i in self.idx..len {
                mem::drop::<T>(unsafe { ManuallyDrop::take(&mut *elements.as_ptr().add(i)) })
            }
        }
    }
}
