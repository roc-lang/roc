#![deny(unsafe_op_in_unsafe_fn)]

use core::{
    cell::Cell, cmp, fmt::Debug, intrinsics::copy_nonoverlapping, ops::Deref, ptr::NonNull,
};

use crate::{rc::ReferenceCount, roc_alloc, roc_dealloc, roc_realloc, storage::Storage};

#[repr(C)]
pub struct RocList<T>
where
    T: ReferenceCount,
{
    elements: Option<NonNull<T>>,
    length: usize,
}

impl<T> RocList<T>
where
    T: ReferenceCount,
{
    pub fn empty() -> Self {
        RocList {
            elements: None,
            length: 0,
        }
    }

    pub fn from_slice(slice: &[T]) -> Self {
        let mut list = Self::empty();
        list.extend_from_slice(slice);
        list
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn as_slice(&self) -> &[T] {
        &*self
    }

    pub fn extend_from_slice(&mut self, slice: &[T]) {
        // TODO: Can we do better for ZSTs? Alignment might be a problem.

        if slice.is_empty() {
            return;
        }

        let alignment = cmp::max(core::mem::align_of::<T>(), core::mem::align_of::<Storage>());
        let elements_offset = alignment;

        let new_size = elements_offset + core::mem::size_of::<T>() * (self.len() + slice.len());

        let new_ptr = if let Some((elements, storage)) = self.elements_and_storage() {
            // Decrement the lists refence count.
            let mut copy = storage.get();
            let is_unique = copy.decrease();

            if is_unique {
                // If the memory is not shared, we can reuse the memory.
                let old_size = elements_offset + core::mem::size_of::<T>() * self.len();
                unsafe {
                    let ptr = elements.as_ptr().cast::<u8>().sub(alignment).cast();
                    roc_realloc(ptr, new_size, old_size, alignment as u32).cast()
                }
            } else {
                if !copy.is_readonly() {
                    // Write the decremented reference count back.
                    storage.set(copy);
                }

                // Allocate new memory.
                let new_ptr = unsafe { roc_alloc(new_size, alignment as u32) };
                let new_elements = unsafe { new_ptr.cast::<u8>().add(alignment).cast::<T>() };

                // Initialize the reference count.
                unsafe {
                    let storage_ptr = new_elements.cast::<Storage>().sub(1);
                    storage_ptr.write(Storage::new_reference_counted());
                }

                // Copy the old elements to the new allocation.
                unsafe {
                    copy_nonoverlapping(elements.as_ptr(), new_elements, self.length);
                }

                new_ptr
            }
        } else {
            // Allocate new memory.
            let new_ptr = unsafe { roc_alloc(new_size, alignment as u32) };
            let new_elements = unsafe { new_ptr.cast::<u8>().add(elements_offset).cast::<T>() };

            // Initialize the reference count.
            unsafe {
                let storage_ptr = new_elements.cast::<Storage>().sub(1);
                storage_ptr.write(Storage::new_reference_counted());
            }

            new_ptr
        };

        let elements = unsafe { new_ptr.cast::<u8>().add(elements_offset).cast::<T>() };

        let non_null_elements = NonNull::new(elements).unwrap();
        self.elements = Some(non_null_elements);

        let elements = self.elements.unwrap().as_ptr();

        let append_ptr = unsafe { elements.add(self.len()) };
        for (i, element) in slice.iter().enumerate() {
            // Increment the element's reference count.
            element.increment();

            // Write the element into the slot.
            unsafe {
                let element = core::ptr::read(element);
                append_ptr.add(i).write(element);
            }

            // It's important that the length is increased one by one, to
            // make sure that we don't drop uninitialized elements, even when
            // a incrementing the reference count panics.
            self.length += 1;
        }
    }

    fn elements_and_storage(&self) -> Option<(NonNull<T>, &Cell<Storage>)> {
        let elements = self.elements?;
        let storage = unsafe { &*elements.as_ptr().cast::<Cell<Storage>>().sub(1) };
        Some((elements, storage))
    }
}

impl<T> Deref for RocList<T>
where
    T: ReferenceCount,
{
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        if let Some(elements) = self.elements {
            let elements = core::ptr::slice_from_raw_parts(elements.as_ptr(), self.length);
            unsafe { &*elements }
        } else {
            &[]
        }
    }
}

impl<T> Default for RocList<T>
where
    T: ReferenceCount,
{
    fn default() -> Self {
        Self::empty()
    }
}

impl<T, U> PartialEq<RocList<U>> for RocList<T>
where
    T: PartialEq<U> + ReferenceCount,
    U: ReferenceCount,
{
    fn eq(&self, other: &RocList<U>) -> bool {
        self.deref() == other.deref()
    }
}

impl<T> Eq for RocList<T> where T: Eq + ReferenceCount {}

impl<T> Debug for RocList<T>
where
    T: Debug + ReferenceCount,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.deref().fmt(f)
    }
}

unsafe impl<T> ReferenceCount for RocList<T>
where
    T: ReferenceCount,
{
    fn increment(&self) {
        // Increment list's the reference count.
        if let Some((_, storage)) = self.elements_and_storage() {
            let mut copy = storage.get();
            if !copy.is_readonly() {
                copy.increment_reference_count();
                storage.set(copy);
            }

            // Increment the children's the reference counts.
            self.iter().for_each(T::increment);
        }
    }

    unsafe fn decrement(ptr: *const Self) {
        let this = unsafe { &*ptr };
        let (elements, storage) = if let Some((elements, storage)) = this.elements_and_storage() {
            (elements, storage)
        } else {
            return;
        };

        // Decrement the refence counts of the contained values.
        for i in 0..this.len() {
            unsafe {
                T::decrement(elements.as_ptr().add(i));
            }
        }

        // Decrease the list's reference count.
        let mut copy = storage.get();
        let can_be_released = copy.decrease();

        if !can_be_released {
            if !copy.is_readonly() {
                // Write the storage back.
                storage.set(copy);
            }
            return;
        }

        // Release the memory.
        let alignment = cmp::max(core::mem::align_of::<T>(), core::mem::align_of::<Storage>());
        unsafe {
            roc_dealloc(
                elements.as_ptr().cast::<u8>().sub(alignment).cast(),
                alignment as u32,
            );
        }
    }
}

impl<T> Clone for RocList<T>
where
    T: ReferenceCount,
{
    fn clone(&self) -> Self {
        // Increment the reference counts.
        self.increment();

        // Create a copy.
        Self {
            elements: self.elements,
            length: self.length,
        }
    }
}

impl<T> Drop for RocList<T>
where
    T: ReferenceCount,
{
    fn drop(&mut self) {
        unsafe {
            Self::decrement(self);
        }
    }
}

impl<T> IntoIterator for RocList<T>
where
    T: ReferenceCount,
{
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { list: self, idx: 0 }
    }
}

pub struct IntoIter<T>
where
    T: ReferenceCount,
{
    list: RocList<T>,
    idx: usize,
}

impl<T> Iterator for IntoIter<T>
where
    T: ReferenceCount,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.list.len() <= self.idx {
            return None;
        }

        let elements = self.list.elements?;
        let element_ptr = unsafe { elements.as_ptr().add(self.idx) };
        self.idx += 1;

        // Return the element.
        let element = unsafe { element_ptr.read() };
        Some(element)
    }
}

impl<T> Drop for IntoIter<T>
where
    T: ReferenceCount,
{
    fn drop(&mut self) {
        // Check if there are any elements left of which we need to decrement
        // the refence counts.
        let elements = if let Some(elements) = self.list.elements {
            elements
        } else {
            return;
        };

        // Set the list's length to zero to prevent double-frees.
        // Note that this leaks if decrementing any of the elements' reference
        // counts panics.
        let len = core::mem::take(&mut self.list.length);

        // Decrement the reference counts of the elements that haven't been
        // returned from the iterator.
        for i in self.idx..len {
            unsafe {
                T::decrement(elements.as_ptr().add(i));
            }
        }
    }
}
