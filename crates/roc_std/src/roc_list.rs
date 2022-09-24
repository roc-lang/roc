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
    ops::Deref,
    ptr::{self, NonNull},
};

use crate::{roc_alloc, roc_dealloc, roc_realloc, storage::Storage};

#[cfg(feature = "serde")]
use core::marker::PhantomData;
#[cfg(feature = "serde")]
use serde::{
    de::{Deserializer, Visitor},
    ser::{SerializeSeq, Serializer},
    Deserialize, Serialize,
};

#[repr(C)]
pub struct RocList<T> {
    elements: Option<NonNull<ManuallyDrop<T>>>,
    length: usize,
    capacity: usize,
}

impl<T> RocList<T> {
    #[inline(always)]
    fn alloc_alignment() -> u32 {
        mem::align_of::<T>().max(mem::align_of::<Storage>()) as u32
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
    pub fn with_capacity(num_elems: usize) -> Self {
        Self {
            elements: Some(Self::elems_with_capacity(num_elems)),
            length: 0,
            capacity: num_elems,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.into_iter()
    }

    /// Used for both roc_alloc and roc_realloc - given the number of elements,
    /// returns the number of bytes needed to allocate, taking into account both the
    /// size of the elements as well as the size of Storage.
    fn alloc_bytes(num_elems: usize) -> usize {
        mem::size_of::<Storage>() + (num_elems * mem::size_of::<T>())
    }

    fn elems_with_capacity(num_elems: usize) -> NonNull<ManuallyDrop<T>> {
        let alloc_ptr = unsafe { roc_alloc(Self::alloc_bytes(num_elems), Self::alloc_alignment()) };

        Self::elems_from_allocation(NonNull::new(alloc_ptr).unwrap_or_else(|| {
            todo!("Call roc_panic with the info that an allocation failed.");
        }))
    }

    fn elems_from_allocation(allocation: NonNull<c_void>) -> NonNull<ManuallyDrop<T>> {
        let alloc_ptr = allocation.as_ptr();

        unsafe {
            let elem_ptr = Self::elem_ptr_from_alloc_ptr(alloc_ptr).cast::<ManuallyDrop<T>>();

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

    pub fn get(&self, index: usize) -> Option<&T> {
        if self.len() <= index {
            return None;
        }

        let elements = self.elements?;
        let element_ptr = unsafe { elements.as_ptr().add(index) };

        // Return the element.
        Some(unsafe { ManuallyDrop::into_inner(element_ptr.cast::<ManuallyDrop<&T>>().read()) })
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
        let storage = unsafe { &*self.ptr_to_allocation().cast::<Cell<Storage>>() };
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
    pub(crate) unsafe fn ptr_to_allocation(&self) -> *mut c_void {
        unsafe {
            self.ptr_to_first_elem()
                .cast::<u8>()
                .sub(Self::alloc_alignment() as usize) as *mut _
        }
    }

    unsafe fn elem_ptr_from_alloc_ptr(alloc_ptr: *mut c_void) -> *mut c_void {
        unsafe {
            alloc_ptr
                .cast::<u8>()
                .add(Self::alloc_alignment() as usize)
                .cast()
        }
    }
}

impl<T> RocList<T>
where
    T: Clone,
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
                if self.capacity() >= slice.len() {
                    elements
                } else {
                    // There wasn't enough capacity, so we need a new allocation.
                    // Since this is a unique RocList, we can use realloc here.
                    let new_ptr = unsafe {
                        roc_realloc(
                            storage.as_ptr().cast(),
                            Self::alloc_bytes(new_len),
                            Self::alloc_bytes(self.capacity),
                            Self::alloc_alignment(),
                        )
                    };

                    self.capacity = new_len;

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
                let new_elements = Self::elems_with_capacity(slice.len());

                // Copy the old elements to the new allocation.
                unsafe {
                    copy_nonoverlapping(elements.as_ptr(), new_elements.as_ptr(), self.length);
                }

                new_elements
            }
        } else {
            Self::elems_with_capacity(slice.len())
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

impl<T> RocList<T> {
    /// Increase a RocList's capacity by at least the requested number of elements (possibly more).
    ///
    /// May return a new RocList, if the provided one was not unique.
    pub fn reserve(&mut self, num_elems: usize) {
        let new_len = num_elems + self.length;
        let new_elems;
        let old_elements_ptr;

        match self.elements_and_storage() {
            Some((elements, storage)) => {
                if storage.get().is_unique() {
                    unsafe {
                        let old_alloc = self.ptr_to_allocation();

                        // Try to reallocate in-place.
                        let new_alloc = roc_realloc(
                            old_alloc,
                            Self::alloc_bytes(new_len),
                            Self::alloc_bytes(self.capacity),
                            Self::alloc_alignment(),
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
                        copy_nonoverlapping(old_elements_ptr, new_elems.as_ptr(), self.length);
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
                                roc_dealloc(self.ptr_to_allocation(), Self::alloc_alignment());
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
            length: self.length,
            capacity: new_len,
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

            if !new_storage.is_readonly() {
                let needs_dealloc = new_storage.decrease();

                if needs_dealloc {
                    unsafe {
                        // Drop the stored elements.
                        for index in 0..self.len() {
                            ManuallyDrop::drop(&mut *elements.as_ptr().add(index));
                        }

                        // Release the memory.
                        roc_dealloc(self.ptr_to_allocation(), Self::alloc_alignment());
                    }
                } else {
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

impl<T, const SIZE: usize> From<[T; SIZE]> for RocList<T> {
    fn from(array: [T; SIZE]) -> Self {
        Self::from_iter(array)
    }
}

impl<'a, T> IntoIterator for &'a RocList<T> {
    type Item = &'a T;
    type IntoIter = core::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.as_slice().iter()
    }
}

impl<T: Hash> Hash for RocList<T> {
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

impl<T> FromIterator<T> for RocList<T> {
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
                capacity: count,
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
            if list.length == list.capacity {
                list.reserve(list.capacity / 2);
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
impl<T: Serialize> Serialize for RocList<T> {
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
    T: Deserialize<'de> + core::clone::Clone,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_seq(RocListVisitor::new())
    }
}

#[cfg(feature = "serde")]
struct RocListVisitor<T> {
    marker: PhantomData<T>,
}

#[cfg(feature = "serde")]
impl<T> RocListVisitor<T> {
    fn new() -> Self {
        RocListVisitor {
            marker: PhantomData,
        }
    }
}

#[cfg(feature = "serde")]
impl<'de, T> Visitor<'de> for RocListVisitor<T>
where
    T: Deserialize<'de> + core::clone::Clone,
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
