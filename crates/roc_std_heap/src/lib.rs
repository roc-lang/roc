//! This heap module attempts to make simple single type heaps.
//! These are used for allocating and deallocating resources beyond the scope of roc.
//!
//! For example, roc can not free file resources.
//! Instead, they will be tracked and reference counted in a side heap.
//! When freed, the underlying resource can be released.
//!
//! To make checking resource types quick, all heaps allocate to a single mmap.
//! Then a simple range check on a pointer can confirm is a pointer is into a specific heap.

use memmap2::MmapMut;
use roc_std::RocBox;
use std::{
    cell::UnsafeCell,
    ffi::c_void,
    io::{Error, ErrorKind, Result},
    marker::PhantomData,
    mem, ptr,
    sync::Mutex,
};

const REFCOUNT_CONSTANT: usize = 0;

/// ThreadSafeRefcountedResourceHeap is a threadsafe version of the refcounted heap that can avoid a wrapping Mutex and RefCell.
/// This is very important for dealloc performance.
/// No lock is needed to check if a pointer is in range of the underlying mmap.
/// This leads to a solid perf bump over the naive lock everywhere solution.
/// Otherwise, alloc and dealloc, always use a mutex, but are much rarer to be called.
/// If perf becomes a problem once basic-cli has threading, we should consider sharding the heap by thread.
pub struct ThreadSafeRefcountedResourceHeap<T> {
    heap: UnsafeCell<RefcountedResourceHeap<T>>,
    guard: Mutex<()>,
}

impl<T> ThreadSafeRefcountedResourceHeap<T> {
    pub fn new(max_elements: usize) -> Result<ThreadSafeRefcountedResourceHeap<T>> {
        RefcountedResourceHeap::new(max_elements).map(|heap| ThreadSafeRefcountedResourceHeap {
            heap: UnsafeCell::new(heap),
            guard: Mutex::new(()),
        })
    }

    pub fn alloc_for(&self, data: T) -> Result<RocBox<()>> {
        let _g = self.guard.lock().unwrap();
        unsafe { &mut *self.heap.get() }.alloc_for(data)
    }

    /// # Safety
    ///
    /// This function will drop the pointed to data. It must be initialized.
    pub unsafe fn dealloc<U>(&self, ptr: *const U) {
        let _g = self.guard.lock().unwrap();
        (*self.heap.get()).dealloc(ptr)
    }

    // This is safe to call at any time with no lock!
    pub fn in_range<U>(&self, ptr: *const U) -> bool {
        unsafe { &*self.heap.get() }.in_range(ptr)
    }

    pub fn box_to_resource<'a>(data: RocBox<()>) -> &'a mut T {
        RefcountedResourceHeap::box_to_resource(data)
    }

    pub fn box_to_refcount<'a>(data: &RocBox<()>) -> &'a mut usize {
        RefcountedResourceHeap::<T>::box_to_refcount(data)
    }

    pub fn promote_all_to_constant(&self) {
        let _g = self.guard.lock().unwrap();
        unsafe { &mut *self.heap.get() }.promote_all_to_constant();
    }
}

unsafe impl<T> Sync for ThreadSafeRefcountedResourceHeap<T> {}
unsafe impl<T> Send for ThreadSafeRefcountedResourceHeap<T> {}

#[repr(C)]
struct Refcounted<T>(usize, T);

/// HeapOfRefcounted is a wrapper around Heap for data that roc stores with a refcount.
/// It will return a pointer to right after the refcount (what a `Box {}` would expect in Roc).
pub struct RefcountedResourceHeap<T>(Heap<Refcounted<T>>);

impl<T> RefcountedResourceHeap<T> {
    pub fn new(max_elements: usize) -> Result<RefcountedResourceHeap<T>> {
        Heap::new(max_elements).map(|heap| RefcountedResourceHeap(heap))
    }

    pub fn alloc_for(&mut self, data: T) -> Result<RocBox<()>> {
        self.0.alloc().map(|alloc_ptr| {
            unsafe { std::ptr::write(alloc_ptr, Refcounted(1, data)) };
            let box_ptr = alloc_ptr as usize + mem::size_of::<usize>();
            unsafe { std::mem::transmute(box_ptr) }
        })
    }

    /// # Safety
    ///
    /// This function will drop the pointed to data. It must be initialized.
    pub unsafe fn dealloc<U>(&mut self, ptr: *const U) {
        self.0.dealloc(ptr as _);
    }

    pub fn in_range<U>(&self, ptr: *const U) -> bool {
        self.0.in_range(ptr as _)
    }

    pub fn box_to_resource<'a>(data: RocBox<()>) -> &'a mut T {
        let box_ptr: usize = unsafe { std::mem::transmute(data) };

        let alloc_ptr = (box_ptr - mem::size_of::<usize>()) as *mut Refcounted<T>;
        let alloc: &mut Refcounted<T> = unsafe { &mut *alloc_ptr };
        &mut alloc.1
    }

    pub fn box_to_refcount<'a>(data: &RocBox<()>) -> &'a mut usize {
        let box_ptr: &usize = unsafe { std::mem::transmute(data) };

        let rc_ptr = (*box_ptr - mem::size_of::<usize>()) as *mut usize;
        unsafe { &mut *rc_ptr }
    }

    /// Promotes all live references to constants.
    /// Does this my walking all allocations and setting the refcount to zero (constant).
    /// It will also end up walking freed elements, but their bytes are uninitialized and don't matter.
    /// This is great for calling after an init function where all lived data is guaranteed to live until the server finishes running.
    pub fn promote_all_to_constant(&mut self) {
        for i in 0..self.0.elements {
            let offset = i * Heap::<Refcounted<T>>::node_size();
            let elem_ptr = unsafe { self.0.data.as_mut_ptr().add(offset) };
            let rc_ptr = elem_ptr as *mut usize;
            unsafe { *rc_ptr = REFCOUNT_CONSTANT };
        }
    }
}

/// The Heap is one mmap of data that can be interpreted multiple ways.
///
/// It can be view as list of unions between `T` and `usize`.
/// In the case of a `T`, it is an allocated element.
/// In the case of a `usize`, it is part of the freed list.
/// The value of the `usize` is the next free node.
///
/// Note: If we ever need better multithreaded performance,
/// we could shard the heap and lock individual shards.
pub struct Heap<T> {
    data: MmapMut,
    elements: usize,
    max_elements: usize,
    free_list: *const c_void,
    phantom: PhantomData<T>,
}

unsafe impl<T> Send for Heap<T> {}

impl<T> Heap<T> {
    pub fn new(max_elements: usize) -> Result<Heap<T>> {
        debug_assert!(max_elements > 0);

        let max_bytes = max_elements * Self::node_size();
        Ok(Self {
            data: MmapMut::map_anon(max_bytes)?,
            elements: 0,
            max_elements,
            free_list: ptr::null(),
            phantom: PhantomData,
        })
    }

    pub fn alloc(&mut self) -> Result<*mut T> {
        if !self.free_list.is_null() {
            // Open slot on the free list.
            let root = self.free_list as *const *const c_void;
            let next = unsafe { *root };
            self.free_list = next;

            // Convert root into a `*mut T` for use.
            return Ok(root as *mut T);
        }

        // If has available memory allocate at end.
        if self.elements < self.max_elements {
            let offset = self.elements * Self::node_size();
            let elem_ptr = unsafe { self.data.as_mut_ptr().add(offset) };
            self.elements += 1;
            return Ok(elem_ptr as *mut T);
        }

        Err(Error::from(ErrorKind::OutOfMemory))
    }

    /// # Safety
    ///
    /// This function will drop the pointed to data. It must be initialized.
    pub unsafe fn dealloc(&mut self, elem_ptr: *mut T) {
        debug_assert!(self.in_range(elem_ptr));

        // Just push the freed value to the start of the free list.
        let old_root = self.free_list;
        self.free_list = elem_ptr as *const c_void;
        *(self.free_list as *mut *const c_void) = old_root;

        // Free the underlying resource.
        std::ptr::drop_in_place(elem_ptr);
    }

    pub fn in_range(&self, elem_ptr: *mut T) -> bool {
        let start = self.data.as_ptr();
        let offset = self.elements * Self::node_size();
        let end = unsafe { start.add(offset) };
        (start as usize) <= (elem_ptr as usize) && (elem_ptr as usize) < (end as usize)
    }

    const fn node_size() -> usize {
        let a = mem::size_of::<usize>();
        let b = mem::size_of::<T>();
        if a > b {
            a
        } else {
            b
        }
    }
}

#[cfg(test)]
mod test {
    use std::u128;

    use super::*;

    #[test]
    fn alloc_to_limit() {
        let limit = 4;
        let mut heap = Heap::<u32>::new(limit).unwrap();
        let mut ptrs = vec![];
        while let Ok(ptr) = heap.alloc() {
            ptrs.push(ptr);
        }

        assert_eq!(ptrs.len(), limit);
        for ptr in ptrs {
            assert!(heap.in_range(ptr));
        }
    }

    #[test]
    fn reuse_freed_elems() {
        let limit = 4;
        let mut heap = Heap::<u128>::new(limit).unwrap();
        let a = heap.alloc().unwrap();
        let b = heap.alloc().unwrap();
        let c = heap.alloc().unwrap();
        let d = heap.alloc().unwrap();

        unsafe { heap.dealloc(c) };
        assert_eq!(c, heap.alloc().unwrap());

        assert!(heap.alloc().is_err());

        unsafe {
            heap.dealloc(d);
            heap.dealloc(a);
            heap.dealloc(b);
        }

        // These should be reused in reverse order.
        assert_eq!(b, heap.alloc().unwrap());
        assert_eq!(a, heap.alloc().unwrap());
        assert_eq!(d, heap.alloc().unwrap());

        assert!(heap.alloc().is_err());
    }
}
