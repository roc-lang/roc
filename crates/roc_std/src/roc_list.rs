#![deny(unsafe_op_in_unsafe_fn)]

use core::{
    cell::Cell,
    cmp::{self, Ordering},
    ffi::c_void,
    fmt::Debug,
    hash::Hash,
    intrinsics::copy_nonoverlapping,
    iter::FromIterator,
    mem::{self, ManuallyDrop},
    ops::{Deref, DerefMut},
    ptr::{self, NonNull},
};
use std::{cmp::max, ops::Range};

use crate::{
    roc_alloc, roc_dealloc, roc_realloc, storage::Storage, RocRefcounted, ROC_REFCOUNT_CONSTANT,
};

#[cfg(feature = "serde")]
use core::marker::PhantomData;
#[cfg(feature = "serde")]
use serde::{
    de::{Deserializer, Visitor},
    ser::{SerializeSeq, Serializer},
    Deserialize, Serialize,
};

#[repr(C)]
pub struct RocList<T>
where
    T: RocRefcounted,
{
    elements: Option<NonNull<ManuallyDrop<T>>>,
    length: usize,
    // This technically points to directly after the refcount.
    // This is an optimization that enables use one code path for regular lists and slices for geting the refcount ptr.
    capacity_or_ref_ptr: usize,
}

impl<T> RocList<T>
where
    T: RocRefcounted,
{
    #[inline(always)]
    fn alloc_alignment() -> usize {
        mem::align_of::<T>().max(mem::align_of::<Storage>())
    }

    fn alloc_to_elem_offset() -> usize {
        let min_offset = if T::is_refcounted() {
            2 * mem::size_of::<usize>()
        } else {
            mem::size_of::<usize>()
        };
        max(Self::alloc_alignment(), min_offset)
    }

    pub fn empty() -> Self {
        Self {
            elements: None,
            length: 0,
            capacity_or_ref_ptr: 0,
        }
    }

    /// Create an empty RocList with enough space preallocated to store
    /// the requested number of elements.
    pub fn with_capacity(num_elems: usize) -> Self {
        Self {
            elements: Some(Self::elems_with_capacity(num_elems)),
            length: 0,
            capacity_or_ref_ptr: num_elems,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.into_iter()
    }

    /// Used for both roc_alloc and roc_realloc - given the number of elements,
    /// returns the number of bytes needed to allocate, taking into account both the
    /// size of the elements as well as the size of Storage.
    fn alloc_bytes(num_elems: usize) -> usize {
        Self::alloc_to_elem_offset() + (num_elems * mem::size_of::<T>())
    }

    fn elems_with_capacity(num_elems: usize) -> NonNull<ManuallyDrop<T>> {
        let alloc_ptr =
            unsafe { roc_alloc(Self::alloc_bytes(num_elems), Self::alloc_alignment() as u32) };

        Self::elems_from_allocation(NonNull::new(alloc_ptr).unwrap_or_else(|| {
            todo!("Call roc_panic with the info that an allocation failed.");
        }))
    }

    fn elems_from_allocation(allocation: NonNull<c_void>) -> NonNull<ManuallyDrop<T>> {
        let offset = Self::alloc_to_elem_offset() - core::mem::size_of::<usize>();
        let alloc_ptr = allocation.as_ptr();

        unsafe {
            let elem_ptr = Self::elem_ptr_from_alloc_ptr(alloc_ptr).cast::<ManuallyDrop<T>>();

            // Initialize the reference count.
            let rc_ptr = alloc_ptr.add(offset);
            rc_ptr
                .cast::<Storage>()
                .write(Storage::new_reference_counted());

            // The original alloc pointer was non-null, and this one is the alloc pointer
            // with `alignment` bytes added to it, so it should be non-null too.
            NonNull::new_unchecked(elem_ptr)
        }
    }

    pub fn len(&self) -> usize {
        self.length & (isize::MAX as usize)
    }

    pub fn is_seamless_slice(&self) -> bool {
        ((self.length | self.capacity_or_ref_ptr) as isize) < 0
    }

    pub fn capacity(&self) -> usize {
        if !self.is_seamless_slice() {
            self.capacity_or_ref_ptr
        } else {
            self.len()
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_unique(&self) -> bool {
        if let Some(storage) = self.storage() {
            storage.is_unique()
        } else {
            // If there is no storage, this list is empty.
            // An empty list is always unique.
            true
        }
    }

    pub fn is_readonly(&self) -> bool {
        if let Some(storage) = self.storage() {
            storage.is_readonly()
        } else {
            false
        }
    }

    pub fn as_mut_ptr(&mut self) -> *mut T {
        self.as_mut_slice().as_mut_ptr()
    }

    pub fn as_ptr(&self) -> *const T {
        self.as_slice().as_ptr()
    }

    /// Marks a list as readonly. This means that it will be leaked.
    /// For constants passed in from platform to application, this may be reasonable.
    ///
    /// # Safety
    ///
    /// A value can be read-only in Roc for 3 reasons:
    ///   1. The value is stored in read-only memory like a constant in the app.
    ///   2. Our refcounting maxes out. When that happens, we saturate to read-only.
    ///   3. This function is called
    ///
    /// Any value that is set to read-only will be leaked.
    /// There is no way to tell how many references it has and if it is safe to free.
    /// As such, only values that should have a static lifetime for the entire application run
    /// should be considered for marking read-only.
    pub unsafe fn set_readonly(&mut self) {
        if let Some((_, storage)) = self.elements_and_storage() {
            // Only safe to write to the pointer if it is not constant (0)
            if !matches!(storage.get(), Storage::Readonly) {
                storage.set(Storage::Readonly);
            }
        }
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
        self
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
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        &mut *self
    }

    #[inline(always)]
    fn elements_and_storage(&self) -> Option<(NonNull<ManuallyDrop<T>>, &Cell<Storage>)> {
        let elements = self.elements?;

        let offset = if T::is_refcounted() {
            1
        } else {
            match mem::align_of::<T>() {
                16 => 1,
                8 | 4 | 2 | 1 => 0,
                other => unreachable!("invalid alignment {other}"),
            }
        };

        let storage = unsafe { &*self.ptr_to_allocation().cast::<Cell<Storage>>().add(offset) };
        Some((elements, storage))
    }

    pub(crate) fn storage(&self) -> Option<Storage> {
        self.elements_and_storage()
            .map(|(_, storage)| storage.get())
    }

    /// Useful for doing memcpy on the elements. Returns NULL if list is empty.
    pub(crate) fn ptr_to_first_elem(&self) -> *const T {
        unsafe { core::mem::transmute(self.elements) }
    }

    /// Useful for doing memcpy on the underlying allocation. Returns NULL if list is empty.
    fn ptr_to_allocation(&self) -> *mut c_void {
        let offset = Self::alloc_to_elem_offset();
        if self.is_seamless_slice() {
            ((self.capacity_or_ref_ptr << 1) - offset) as *mut _
        } else {
            unsafe { self.ptr_to_first_elem().cast::<u8>().sub(offset) as *mut _ }
        }
    }

    fn allocation_element_count(&self) -> usize {
        if self.is_seamless_slice() && T::is_refcounted() {
            unsafe { self.ptr_to_refcount().sub(1).read() }
        } else {
            self.len()
        }
    }

    #[allow(unused)]
    pub(crate) fn ptr_to_refcount(&self) -> *mut usize {
        if self.is_seamless_slice() {
            ((self.capacity_or_ref_ptr << 1) - std::mem::size_of::<usize>()) as *mut _
        } else {
            unsafe { self.ptr_to_first_elem().cast::<usize>().sub(1) as *mut _ }
        }
    }

    unsafe fn elem_ptr_from_alloc_ptr(alloc_ptr: *mut c_void) -> *mut c_void {
        let offset = Self::alloc_to_elem_offset();
        unsafe { alloc_ptr.cast::<u8>().add(offset).cast() }
    }

    pub fn append(&mut self, value: T) {
        self.push(value)
    }

    pub fn push(&mut self, value: T) {
        if self.capacity() <= self.len() {
            // reserve space for (at least!) one more element
            self.reserve(1);
        }

        let elements = self.elements.unwrap().as_ptr();
        let append_ptr = unsafe { elements.add(self.len()) };

        unsafe {
            // Write the element into the slot, without dropping it.
            ptr::write(append_ptr, ManuallyDrop::new(value));
        }

        // It's important that the length is increased one by one, to
        // make sure that we don't drop uninitialized elements, even when
        // a incrementing the reference count panics.
        self.length += 1;
    }

    /// # Safety
    ///
    /// - `bytes` must be allocated for `cap` elements
    /// - `bytes` must be initialized for `len` elements
    /// - `bytes` must be preceded by a correctly-aligned refcount (usize)
    /// - `cap` >= `len`
    pub unsafe fn from_raw_parts(bytes: *mut T, len: usize, cap: usize) -> Self {
        Self {
            elements: NonNull::new(bytes.cast()),
            length: len,
            capacity_or_ref_ptr: cap,
        }
    }
}

impl<T> RocList<T>
where
    T: Clone + RocRefcounted,
{
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

        let new_len = self.len() + slice.len();
        let non_null_elements = if let Some((elements, storage)) = self.elements_and_storage() {
            // Decrement the list's refence count.
            let mut copy = storage.get();
            let is_unique = copy.decrease();

            if is_unique {
                // If we have enough capacity, we can add to the existing elements in-place.
                if self.capacity() >= new_len {
                    elements
                } else {
                    // There wasn't enough capacity, so we need a new allocation.
                    // Since this is a unique RocList, we can use realloc here.
                    let new_ptr = unsafe {
                        roc_realloc(
                            self.ptr_to_allocation(),
                            Self::alloc_bytes(new_len),
                            Self::alloc_bytes(self.capacity()),
                            Self::alloc_alignment() as u32,
                        )
                    };

                    self.capacity_or_ref_ptr = new_len;

                    Self::elems_from_allocation(NonNull::new(new_ptr).unwrap_or_else(|| {
                        todo!("Reallocation failed");
                    }))
                }
            } else {
                if !copy.is_readonly() {
                    // Write the decremented reference count back.
                    storage.set(copy);
                }

                // Allocate new memory.
                self.capacity_or_ref_ptr = slice.len();
                let new_elements = Self::elems_with_capacity(slice.len());

                // Copy the old elements to the new allocation.
                unsafe {
                    copy_nonoverlapping(elements.as_ptr(), new_elements.as_ptr(), self.len());
                }
                // Clear the seamless slice bit since we now have clear ownership.
                self.length = self.len();

                new_elements
            }
        } else {
            self.capacity_or_ref_ptr = slice.len();
            Self::elems_with_capacity(slice.len())
        };

        self.elements = Some(non_null_elements);

        let elements = self.elements.unwrap().as_ptr();

        let append_ptr = unsafe { elements.add(self.len()) };

        // Use .cloned() to increment the elements' reference counts, if needed.
        for (i, new_elem) in slice.iter().cloned().enumerate() {
            let dst = unsafe { append_ptr.add(i) };
            unsafe {
                // Write the element into the slot, without dropping it.
                ptr::write(dst, ManuallyDrop::new(new_elem));
            }

            // It's important that the length is increased one by one, to
            // make sure that we don't drop uninitialized elements, even when
            // a incrementing the reference count panics.
            self.length += 1;
        }
    }
}

impl<T> RocList<T>
where
    T: RocRefcounted,
{
    #[track_caller]
    pub fn slice_range(&self, range: Range<usize>) -> Self {
        match self.try_slice_range(range) {
            Some(x) => x,
            None => panic!("slice index out of range"),
        }
    }

    pub fn try_slice_range(&self, range: Range<usize>) -> Option<Self> {
        if self.as_slice().get(range.start..range.end).is_none() {
            None
        } else {
            // increment the refcount
            std::mem::forget(self.clone());

            let element_ptr = self.as_slice()[range.start..]
                .as_ptr()
                .cast::<ManuallyDrop<T>>();

            let capacity_or_ref_ptr =
                (self.ptr_to_first_elem() as usize) >> 1 | isize::MIN as usize;

            let roc_list = RocList {
                elements: NonNull::new(element_ptr as *mut ManuallyDrop<T>),
                length: range.end - range.start,
                capacity_or_ref_ptr,
            };

            Some(roc_list)
        }
    }

    /// Increase a RocList's capacity by at least the requested number of elements (possibly more).
    ///
    /// May return a new RocList, if the provided one was not unique.
    pub fn reserve(&mut self, num_elems: usize) {
        let new_len = num_elems + self.len();
        let new_elems;
        let old_elements_ptr;

        match self.elements_and_storage() {
            Some((elements, storage)) => {
                if storage.get().is_unique() && !self.is_seamless_slice() {
                    unsafe {
                        let old_alloc = self.ptr_to_allocation();

                        // Try to reallocate in-place.
                        let new_alloc = roc_realloc(
                            old_alloc,
                            Self::alloc_bytes(new_len),
                            Self::alloc_bytes(self.capacity()),
                            Self::alloc_alignment() as u32,
                        );

                        if new_alloc == old_alloc {
                            // We successfully reallocated in-place; we're done!
                            return;
                        } else {
                            // We got back a different allocation; copy the existing elements
                            // into it. We don't need to increment their refcounts because
                            // The existing allocation that references to them is now gone and
                            // no longer referencing them.
                            new_elems = Self::elems_from_allocation(
                                NonNull::new(new_alloc).unwrap_or_else(|| {
                                    todo!("Reallocation failed");
                                }),
                            );
                        }

                        // Note that realloc automatically deallocates the old allocation,
                        // so we don't need to call roc_dealloc here.
                    }
                } else {
                    // Make a new allocation
                    new_elems = Self::elems_with_capacity(new_len);
                    old_elements_ptr = elements.as_ptr();

                    unsafe {
                        // Copy the old elements to the new allocation.
                        copy_nonoverlapping(old_elements_ptr, new_elems.as_ptr(), self.len());
                    }

                    // Decrease the current allocation's reference count.
                    let mut new_storage = storage.get();

                    if !new_storage.is_readonly() {
                        let needs_dealloc = new_storage.decrease();

                        if needs_dealloc {
                            // Unlike in Drop, do *not* decrement the refcounts of all the elements!
                            // The new allocation is referencing them, so instead of incrementing them all
                            // all just to decrement them again here, we neither increment nor decrement them.
                            unsafe {
                                roc_dealloc(
                                    self.ptr_to_allocation(),
                                    Self::alloc_alignment() as u32,
                                );
                            }
                        } else {
                            // Write the storage back.
                            storage.set(new_storage);
                        }
                    }
                }
            }
            None => {
                // This is an empty list, so `reserve` is the same as `with_capacity`.
                self.update_to(Self::with_capacity(new_len));

                return;
            }
        }

        self.update_to(Self {
            elements: Some(new_elems),
            length: self.len(),
            capacity_or_ref_ptr: new_len,
        });
    }

    /// Replace self with a new version, without letting `drop` run in between.
    fn update_to(&mut self, mut updated: Self) {
        // We want to replace `self` with `updated` in a way that makes sure
        // `self`'s `drop` never runs. This is the proper way to do that:
        // swap them, and then forget the "updated" one (which is now pointing
        // to the original allocation).
        mem::swap(self, &mut updated);
        mem::forget(updated);
    }
}

impl<T> Deref for RocList<T>
where
    T: RocRefcounted,
{
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        if let Some(elements) = self.elements {
            let elements = ptr::slice_from_raw_parts(elements.as_ptr().cast::<T>(), self.len());

            unsafe { &*elements }
        } else {
            &[]
        }
    }
}

impl<T> DerefMut for RocList<T>
where
    T: RocRefcounted,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        if let Some(elements) = self.elements {
            let ptr = elements.as_ptr().cast::<T>();
            let elements = ptr::slice_from_raw_parts_mut(ptr, self.length);

            unsafe { &mut *elements }
        } else {
            &mut []
        }
    }
}

impl<T> Default for RocList<T>
where
    T: RocRefcounted,
{
    fn default() -> Self {
        Self::empty()
    }
}

impl<T, U> PartialEq<RocList<U>> for RocList<T>
where
    U: RocRefcounted,
    T: PartialEq<U> + RocRefcounted,
{
    fn eq(&self, other: &RocList<U>) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl<T> Eq for RocList<T> where T: Eq + RocRefcounted {}

impl<T, U> PartialOrd<RocList<U>> for RocList<T>
where
    U: RocRefcounted,
    T: PartialOrd<U> + RocRefcounted,
{
    fn partial_cmp(&self, other: &RocList<U>) -> Option<cmp::Ordering> {
        // If one is longer than the other, use that as the ordering.
        match self.len().partial_cmp(&other.len()) {
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
    T: Ord + RocRefcounted,
{
    fn cmp(&self, other: &Self) -> Ordering {
        // If one is longer than the other, use that as the ordering.
        match self.len().cmp(&other.len()) {
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
    T: Debug + RocRefcounted,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<T> Clone for RocList<T>
where
    T: RocRefcounted,
{
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
            capacity_or_ref_ptr: self.capacity_or_ref_ptr,
        }
    }
}

impl<T> RocRefcounted for RocList<T>
where
    T: RocRefcounted,
{
    fn inc(&mut self) {
        if self.elements.is_none() {
            // Empty, non-allocated list, no refcounting to do.
            return;
        }

        let ptr = self.ptr_to_refcount();
        unsafe {
            let value = std::ptr::read(ptr);
            // Only safe to write to the pointer if it is not constant (0)
            if value != ROC_REFCOUNT_CONSTANT {
                std::ptr::write(ptr, (value as isize + 1) as usize);
            }
        }
    }

    fn dec(&mut self) {
        if let Some((_, storage)) = self.elements_and_storage() {
            // Decrease the list's reference count.
            let mut new_storage = storage.get();

            if !new_storage.is_readonly() {
                let needs_dealloc = new_storage.decrease();

                if needs_dealloc {
                    let alloc_ptr = self.ptr_to_allocation();
                    unsafe {
                        // Dec the stored elements in the underlying allocation.
                        if T::is_refcounted() {
                            let elements_ptr = Self::elem_ptr_from_alloc_ptr(alloc_ptr) as *mut T;
                            let len = self.allocation_element_count();
                            for index in 0..len {
                                (*elements_ptr.add(index)).dec()
                            }
                        }

                        // Release the memory.
                        roc_dealloc(alloc_ptr, Self::alloc_alignment() as u32);
                    }
                } else {
                    // Write the storage back.
                    storage.set(new_storage);
                }
            }
        }
    }

    fn is_refcounted() -> bool {
        true
    }
}

impl<T> Drop for RocList<T>
where
    T: RocRefcounted,
{
    fn drop(&mut self) {
        self.dec()
    }
}

impl<T> From<&[T]> for RocList<T>
where
    T: Clone + RocRefcounted,
{
    fn from(slice: &[T]) -> Self {
        Self::from_slice(slice)
    }
}

impl<T, const SIZE: usize> From<[T; SIZE]> for RocList<T>
where
    T: RocRefcounted,
{
    fn from(array: [T; SIZE]) -> Self {
        Self::from_iter(array)
    }
}

impl<'a, T> IntoIterator for &'a RocList<T>
where
    T: RocRefcounted,
{
    type Item = &'a T;
    type IntoIter = core::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.as_slice().iter()
    }
}

impl<T: Hash> Hash for RocList<T>
where
    T: RocRefcounted,
{
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        // This is the same as Rust's Vec implementation, which
        // just delegates to the slice implementation. It's a bit surprising
        // that Hash::hash_slice doesn't automatically incorporate the length,
        // but the slice implementation indeed does explicitly call self.len().hash(state);
        //
        // To verify, click the "source" links for:
        //     Vec: https://doc.rust-lang.org/std/vec/struct.Vec.html#impl-Hash
        //     slice: https://doc.rust-lang.org/std/primitive.slice.html#impl-Hash
        self.len().hash(state);

        Hash::hash_slice(self.as_slice(), state);
    }
}

impl<T> FromIterator<T> for RocList<T>
where
    T: RocRefcounted,
{
    fn from_iter<I>(into: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let iter = into.into_iter();

        if core::mem::size_of::<T>() == 0 {
            let count = iter.count();
            return Self {
                elements: Some(Self::elems_with_capacity(count)),
                length: count,
                capacity_or_ref_ptr: count,
            };
        }

        let mut list = {
            let (min_len, maybe_max_len) = iter.size_hint();
            let init_capacity = maybe_max_len.unwrap_or(min_len);
            Self::with_capacity(init_capacity)
        };

        let mut elements = list.elements.unwrap().as_ptr();
        for new_elem in iter {
            // If the size_hint didn't give us a max, we may need to grow. 1.5x seems to be good, based on:
            // https://archive.ph/Z2R8w and https://github.com/facebook/folly/blob/1f2706/folly/docs/FBVector.md
            if list.length == list.capacity() {
                list.reserve(list.capacity() / 2);
                elements = list.elements.unwrap().as_ptr();
            }

            unsafe {
                elements
                    .add(list.length)
                    .write(ptr::read(&ManuallyDrop::new(new_elem)));
            }
            list.length += 1;
        }

        list
    }
}

#[cfg(feature = "serde")]
impl<T: Serialize> Serialize for RocList<T>
where
    T: RocRefcounted,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.len()))?;
        for item in self {
            seq.serialize_element(item)?;
        }
        seq.end()
    }
}

#[cfg(feature = "serde")]
impl<'de, T> Deserialize<'de> for RocList<T>
where
    // TODO: I'm not sure about requiring clone here. Is that fine? Is that
    // gonna mean lots of extra allocations?
    T: Deserialize<'de> + core::clone::Clone + RocRefcounted,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_seq(RocListVisitor::new())
    }
}

// This is a RocList that is checked to ensure it is unique or readonly such that it can be sent between threads safely.
#[repr(transparent)]
pub struct SendSafeRocList<T>(RocList<T>)
where
    T: RocRefcounted;

unsafe impl<T> Send for SendSafeRocList<T> where T: Send + RocRefcounted {}

impl<T> RocRefcounted for SendSafeRocList<T>
where
    T: RocRefcounted,
{
    fn inc(&mut self) {
        self.0.inc()
    }

    fn dec(&mut self) {
        self.0.dec()
    }

    fn is_refcounted() -> bool {
        true
    }
}

impl<T> Clone for SendSafeRocList<T>
where
    T: Clone + RocRefcounted,
{
    fn clone(&self) -> Self {
        if self.0.is_readonly() {
            SendSafeRocList(self.0.clone())
        } else {
            // To keep self send safe, this must copy.
            SendSafeRocList(RocList::from_slice(&self.0))
        }
    }
}

impl<T> From<RocList<T>> for SendSafeRocList<T>
where
    T: Clone + RocRefcounted,
{
    fn from(l: RocList<T>) -> Self {
        if l.is_unique() || l.is_readonly() {
            SendSafeRocList(l)
        } else {
            // This is not unique, do a deep copy.
            // TODO: look into proper into_iter that takes ownership.
            // Then this won't need clone and will skip and refcount inc and dec for each element.
            SendSafeRocList(RocList::from_slice(&l))
        }
    }
}

impl<T> From<SendSafeRocList<T>> for RocList<T>
where
    T: RocRefcounted,
{
    fn from(l: SendSafeRocList<T>) -> Self {
        l.0
    }
}

#[repr(transparent)]
pub struct ReadOnlyRocList<T>(RocList<T>)
where
    T: RocRefcounted;

unsafe impl<T> Send for ReadOnlyRocList<T> where T: Send + RocRefcounted {}
unsafe impl<T> Sync for ReadOnlyRocList<T> where T: Sync + RocRefcounted {}

impl<T> RocRefcounted for ReadOnlyRocList<T>
where
    T: RocRefcounted,
{
    fn inc(&mut self) {}

    fn dec(&mut self) {}

    fn is_refcounted() -> bool {
        true
    }
}

impl<T> Clone for ReadOnlyRocList<T>
where
    T: Clone + RocRefcounted,
{
    fn clone(&self) -> Self {
        ReadOnlyRocList(self.0.clone())
    }
}

impl<T> From<RocList<T>> for ReadOnlyRocList<T>
where
    T: Clone + RocRefcounted,
{
    fn from(mut l: RocList<T>) -> Self {
        if l.is_unique() {
            unsafe { l.set_readonly() };
        }
        if l.is_readonly() {
            ReadOnlyRocList(l)
        } else {
            // This is not unique, do a deep copy.
            ReadOnlyRocList::from(RocList::from_slice(&l))
        }
    }
}

impl<T> From<ReadOnlyRocList<T>> for RocList<T>
where
    T: RocRefcounted,
{
    fn from(l: ReadOnlyRocList<T>) -> Self {
        l.0
    }
}

#[cfg(feature = "serde")]
struct RocListVisitor<T>
where
    T: RocRefcounted,
{
    marker: PhantomData<T>,
}

#[cfg(feature = "serde")]
impl<T> RocListVisitor<T>
where
    T: RocRefcounted,
{
    fn new() -> Self {
        RocListVisitor {
            marker: PhantomData,
        }
    }
}

#[cfg(feature = "serde")]
impl<'de, T> Visitor<'de> for RocListVisitor<T>
where
    T: Deserialize<'de> + core::clone::Clone + RocRefcounted,
{
    type Value = RocList<T>;

    fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(formatter, "a list of strings")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let mut out = match seq.size_hint() {
            Some(hint) => RocList::with_capacity(hint),
            None => RocList::empty(),
        };

        while let Some(next) = seq.next_element()? {
            // TODO: it would be ideal to call `out.push` here, but we haven't
            // implemented that yet! I think this is also why we need Clone.
            out.extend_from_slice(&[next])
        }

        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::RocDec;

    #[no_mangle]
    pub unsafe extern "C" fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
        unsafe { libc::malloc(size) }
    }

    #[no_mangle]
    pub unsafe extern "C" fn roc_realloc(
        c_ptr: *mut c_void,
        new_size: usize,
        _old_size: usize,
        _alignment: u32,
    ) -> *mut c_void {
        unsafe { libc::realloc(c_ptr, new_size) }
    }

    #[no_mangle]
    pub unsafe extern "C" fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
        unsafe { libc::free(c_ptr) }
    }

    #[test]
    fn compare_list_dec() {
        // RocDec is special because it's alignment is 16
        let a = RocList::from_slice(&[RocDec::from(1), RocDec::from(2)]);
        let b = RocList::from_slice(&[RocDec::from(1), RocDec::from(2)]);

        assert_eq!(a, b);
    }

    #[test]
    fn clone_list_dec() {
        // RocDec is special because it's alignment is 16
        let a = RocList::from_slice(&[RocDec::from(1), RocDec::from(2)]);
        let b = a.clone();

        assert_eq!(a, b);

        drop(a);
        drop(b);
    }

    #[test]
    fn compare_list_str() {
        let a = RocList::from_slice(&[crate::RocStr::from("ab")]);
        let b = RocList::from_slice(&[crate::RocStr::from("ab")]);

        assert_eq!(a, b);

        drop(a);
        drop(b);
    }

    #[test]
    fn readonly_list_is_sendsafe() {
        let mut x = RocList::from_slice(&[1, 2, 3, 4, 5]);
        unsafe { x.set_readonly() };
        assert!(x.is_readonly());

        let y = x.clone();
        let z = y.clone();

        let safe_x = SendSafeRocList::from(x);
        let new_x = RocList::from(safe_x);
        assert!(new_x.is_readonly());
        assert!(y.is_readonly());
        assert!(z.is_readonly());
        assert_eq!(new_x.as_slice(), &[1, 2, 3, 4, 5]);

        let ptr = new_x.ptr_to_allocation();

        drop(y);
        drop(z);
        drop(new_x);

        // free the underlying memory
        unsafe { crate::roc_dealloc(ptr, std::mem::align_of::<usize>() as u32) }
    }
}
