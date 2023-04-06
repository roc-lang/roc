#![deny(unsafe_op_in_unsafe_fn)]

use crate::{roc_alloc, roc_dealloc, storage::Storage};
use core::{
    cell::Cell,
    cmp::{self, Ordering},
    fmt::Debug,
    mem,
    ops::Deref,
    ptr::{self, NonNull},
};

#[repr(C)]
pub struct RocBox<T> {
    contents: NonNull<T>,
}

impl<T> RocBox<T> {
    pub fn new(contents: T) -> Self {
        let alignment = Self::alloc_alignment();
        let bytes = mem::size_of::<T>() + alignment;

        let ptr = unsafe { roc_alloc(bytes, alignment as u32) };

        if ptr.is_null() {
            todo!("Call roc_panic with the info that an allocation failed.");
        }

        // Initialize the reference count.
        let refcount_one = Storage::new_reference_counted();
        unsafe { ptr.cast::<Storage>().write(refcount_one) };

        let contents = unsafe {
            let contents_ptr = ptr.cast::<u8>().add(alignment).cast::<T>();

            core::ptr::write(contents_ptr, contents);

            // We already verified that the original alloc pointer was non-null,
            // and this one is the alloc pointer with `alignment` bytes added to it,
            // so it should be non-null too.
            NonNull::new_unchecked(contents_ptr)
        };

        Self { contents }
    }

    /// # Safety
    ///
    /// The box must be unique in order to leak it safely
    pub unsafe fn leak(self) -> *mut T {
        let ptr = self.contents.as_ptr() as *mut T;
        core::mem::forget(self);
        ptr
    }

    #[inline(always)]
    fn alloc_alignment() -> usize {
        mem::align_of::<T>().max(mem::align_of::<Storage>())
    }

    pub fn into_inner(self) -> T {
        unsafe { ptr::read(self.contents.as_ptr() as *mut T) }
    }

    fn storage(&self) -> &Cell<Storage> {
        let alignment = Self::alloc_alignment();

        unsafe {
            &*self
                .contents
                .as_ptr()
                .cast::<u8>()
                .sub(alignment)
                .cast::<Cell<Storage>>()
        }
    }
}

impl<T> Deref for RocBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.contents.as_ref() }
    }
}

impl<T, U> PartialEq<RocBox<U>> for RocBox<T>
where
    T: PartialEq<U>,
{
    fn eq(&self, other: &RocBox<U>) -> bool {
        self.deref() == other.deref()
    }
}

impl<T> Eq for RocBox<T> where T: Eq {}

impl<T, U> PartialOrd<RocBox<U>> for RocBox<T>
where
    T: PartialOrd<U>,
{
    fn partial_cmp(&self, other: &RocBox<U>) -> Option<cmp::Ordering> {
        let self_contents = unsafe { self.contents.as_ref() };
        let other_contents = unsafe { other.contents.as_ref() };

        self_contents.partial_cmp(other_contents)
    }
}

impl<T> Ord for RocBox<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        let self_contents = unsafe { self.contents.as_ref() };
        let other_contents = unsafe { other.contents.as_ref() };

        self_contents.cmp(other_contents)
    }
}

impl<T: core::hash::Hash> core::hash::Hash for RocBox<T> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.contents.hash(state)
    }
}

impl<T> Debug for RocBox<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<T> Clone for RocBox<T> {
    fn clone(&self) -> Self {
        let storage = self.storage();
        let mut new_storage = storage.get();

        // Increment the reference count
        if !new_storage.is_readonly() {
            new_storage.increment_reference_count();
            storage.set(new_storage);
        }

        Self {
            contents: self.contents,
        }
    }
}

impl<T> Drop for RocBox<T> {
    fn drop(&mut self) {
        let storage = self.storage();
        let contents = self.contents;

        // Decrease the list's reference count.
        let mut new_storage = storage.get();
        let needs_dealloc = new_storage.decrease();

        if needs_dealloc {
            unsafe {
                // Drop the stored contents.
                let contents_ptr = contents.as_ptr();

                mem::drop::<T>(ptr::read(contents_ptr));

                let alignment = Self::alloc_alignment();

                // Release the memory.
                roc_dealloc(
                    contents.as_ptr().cast::<u8>().sub(alignment).cast(),
                    alignment as u32,
                );
            }
        } else if !new_storage.is_readonly() {
            // Write the storage back.
            storage.set(new_storage);
        }
    }
}
