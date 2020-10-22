/// A bucket
use libc::{c_void, calloc, free, mmap, munmap, MAP_ANONYMOUS, MAP_PRIVATE, PROT_READ, PROT_WRITE};
use std::marker::PhantomData;
use std::mem::{self, size_of};
use std::ptr::null;
use std::{u16, u8};

const BUCKET_BYTES: usize = 4096;

pub struct NodeId<T> {
    pub bucket_id: BucketId<T>,
    pub slot: BucketSlot<T>,
}

pub struct BucketId<T> {
    value: u16,
    _phantom: PhantomData<T>,
}

impl<T> BucketId<T> {
    fn from_u16(value: u16) -> Self {
        BucketId {
            value,
            _phantom: PhantomData::default(),
        }
    }
}

pub struct BucketSlot<T> {
    value: u8,
    _phantom: PhantomData<T>,
}

impl<T> BucketSlot<T> {
    fn from_u8(value: u8) -> Self {
        BucketSlot {
            value,
            _phantom: PhantomData::default(),
        }
    }
}

pub struct Buckets<T> {
    buckets: Vec<Bucket<T>>,
}

impl<T> Buckets<T> {
    pub fn add(&mut self) -> Result<BucketId<T>, ()> {
        let num_buckets = self.buckets.len();

        if num_buckets <= u16::MAX as usize {
            let bucket_id = BucketId::from_u16(num_buckets as u16);

            self.buckets.push(Bucket::default());

            Ok(bucket_id)
        } else {
            Err(())
        }
    }

    pub fn get<'a>(&'a self, node_id: NodeId<T>) -> Option<&'a T> {
        self.buckets
            .get(node_id.bucket_id.value as usize)
            .and_then(|bucket| bucket.get(node_id.slot))
    }
}

pub struct Bucket<T> {
    next_unused_slot: u16,
    first_slot: *mut T,
    _phantom: PhantomData<T>,
}

impl<T> Bucket<T> {
    /// If there's room left in the bucket, adds the item and returns
    /// the slot where it was put. If there was no room left, returns Err(()).
    pub fn add(&mut self, node: T) -> Result<BucketSlot<T>, ()> {
        // Once next_unused_slot exceeds u8::MAX, we have no room left.
        if self.next_unused_slot <= u8::MAX as u16 {
            let chosen_slot = self.next_unused_slot as u8;

            unsafe { self.put_unchecked(node, chosen_slot) };
            self.next_unused_slot += 1;

            Ok(BucketSlot::from_u8(chosen_slot))
        } else {
            // No room left!
            Err(())
        }
    }

    /// If the given slot is available, inserts the given node into it.
    /// Otherwise, returns the node that was in the already-occupied slot.
    pub fn insert(&mut self, node: T, slot: BucketSlot<T>) -> Result<(), &T> {
        let slot = slot.value;

        unsafe {
            if self.is_available(slot) {
                self.put_unchecked(node, slot);

                Ok(())
            } else {
                Err(self.get_unchecked(slot))
            }
        }
    }

    pub fn get<'a>(&'a self, slot: BucketSlot<T>) -> Option<&'a T> {
        unsafe {
            let slot_ptr = self.first_slot.offset(slot.value as isize) as *const T;
            let value = &*slot_ptr;

            if *mem::transmute::<&T, &[u8; 16]>(value) != [0; 16] {
                Some(value)
            } else {
                None
            }
        }
    }

    unsafe fn put_unchecked(&mut self, node: T, slot: u8) {
        let slot_ptr = self.first_slot.offset(slot as isize);

        *slot_ptr = node;
    }

    unsafe fn get_unchecked<'a>(&'a self, slot: u8) -> &'a T {
        &*self.first_slot.offset(slot as isize)
    }

    // A slot is available iff its bytes are all zeroes
    unsafe fn is_available(&self, slot: u8) -> bool {
        let slot_ptr = self.first_slot.offset(slot as isize) as *const [u8; 16];

        *slot_ptr == [0; 16]
    }
}

impl<T> Default for Bucket<T> {
    fn default() -> Self {
        // It's only safe to store this as a *const T if T is 16 bytes.
        // This is designed to be used exclusively with 16-byte nodes!
        debug_assert_eq!(size_of::<T>(), 16);

        let first_slot = if page_size::get() == 4096 {
            unsafe {
                // mmap exactly one memory page (4096 bytes)
                mmap(
                    null::<c_void>() as *mut c_void,
                    BUCKET_BYTES,
                    PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANONYMOUS,
                    0,
                    0,
                )
            }
        } else {
            // Somehow the page size is not 4096 bytes, so fall back on calloc.
            // (We use calloc over malloc because we rely on the bytes having
            // been zeroed to tell which slots are available.)
            unsafe { calloc(1, BUCKET_BYTES) }
        } as *mut T;

        Bucket {
            next_unused_slot: 0,
            first_slot,
            _phantom: PhantomData::default(),
        }
    }
}

impl<T> Drop for Bucket<T> {
    fn drop(&mut self) {
        if page_size::get() == 4096 {
            unsafe {
                munmap(self.first_slot as *mut c_void, BUCKET_BYTES);
            }
        } else {
            unsafe {
                free(self.first_slot as *mut c_void);
            }
        }
    }
}
