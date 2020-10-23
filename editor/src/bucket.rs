/// A bucket of 16-byte nodes. The node value 0 is reserved for the bucket's
/// use, and valid nodes may never have that value.
///
/// By design, each bucket is 4096 bytes large. When you make a bucket, it
/// uses mmap to reserve one anonymous memory page in which to store nodes.
/// Since nodes are 16 bytes, one bucket can store 256 nodes; you can access
/// a particular node by its BucketSlot, which is an opaque wrapper around a u8.
///
/// Buckets also use the node value 0 (all 0 bits) to mark slots as unoccupied.
/// This is important for performance.
use libc::{c_void, calloc, free, mmap, munmap, MAP_ANONYMOUS, MAP_PRIVATE, PROT_READ, PROT_WRITE};
use std::marker::PhantomData;
use std::mem::{self, size_of};
use std::ptr::null;

const BUCKET_BYTES: usize = 4096;

pub struct NodeId<T: Sized> {
    pub bucket_id: BucketId<T>,
    pub slot: BucketSlot<T>,
}

impl<T: Sized> NodeId<T> {
    fn next_slot(&self) -> Self {
        NodeId {
            bucket_id: self.bucket_id,
            slot: self.slot.increment(),
        }
    }
}

pub struct BucketId<T: Sized> {
    value: u16,
    _phantom: PhantomData<T>,
}

impl<T: Sized> BucketId<T> {
    fn from_u16(value: u16) -> Self {
        BucketId {
            value,
            _phantom: PhantomData::default(),
        }
    }
}

pub struct BucketSlot<T: Sized> {
    value: u8,
    _phantom: PhantomData<T>,
}

impl<T: Sized> BucketSlot<T> {
    fn from_u8(value: u8) -> Self {
        BucketSlot {
            value,
            _phantom: PhantomData::default(),
        }
    }

    fn increment(&self) -> Self {
        BucketSlot {
            value: self.value + 1,
            _phantom: PhantomData::default(),
        }
    }
}

pub struct Buckets {
    buckets: Vec<Bucket>,
    // free_1node_slots: Vec<NodeId<T>>,
}

impl Buckets {
    // fn find_space_for(&mut self, nodes: u8) -> Result<BucketId<T>, ()> {}

    pub fn add<T: Sized>(&mut self, node: T) -> Result<BucketId<T>, ()> {
        let num_buckets = self.buckets.len();

        if num_buckets <= u16::MAX as usize {
            let bucket_id = BucketId::from_u16(num_buckets as u16);

            self.buckets.push(Bucket::default());

            Ok(bucket_id)
        } else {
            Err(())
        }
    }

    fn get_unchecked<'a, T: Sized>(&'a self, node_id: NodeId<T>) -> &'a T {
        self.buckets
            .get(node_id.bucket_id.value as usize)
            .unwrap()
            .get_unchecked(node_id.slot.value)
    }

    pub fn get<'a, T: Sized>(&'a self, node_id: NodeId<T>) -> Option<&'a T> {
        self.buckets
            .get(node_id.bucket_id.value as usize)
            .and_then(|bucket| bucket.get(node_id.slot))
    }
}

struct Bucket {
    next_unused_slot: u16,
    first_slot: *mut [u8; 16],
}

impl Bucket {
    /// If there's room left in the bucket, adds the item and returns
    /// the slot where it was put. If there was no room left, returns Err(()).
    pub fn add<T: Sized>(&mut self, node: T) -> Result<BucketSlot<T>, ()> {
        // It's only safe to store this as a *const T if T is 16 bytes.
        // This is designed to be used exclusively with 16-byte nodes!
        debug_assert_eq!(size_of::<T>(), 16);

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
    pub fn insert<T: Sized>(&mut self, node: T, slot: BucketSlot<T>) -> Result<(), &T> {
        // It's only safe to use this if T is 16 bytes.
        // This is designed to be used exclusively with 16-byte nodes!
        debug_assert_eq!(size_of::<T>(), 16);

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

    pub fn get<'a, T: Sized>(&'a self, slot: BucketSlot<T>) -> Option<&'a T> {
        // It's only safe to store this as a *const T if T is 16 bytes.
        // This is designed to be used exclusively with 16-byte nodes!
        debug_assert_eq!(size_of::<T>(), 16);

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

    unsafe fn put_unchecked<T: Sized>(&mut self, node: T, slot: u8) {
        // It's only safe to store this as a *const T if T is 16 bytes.
        // This is designed to be used exclusively with 16-byte nodes!
        debug_assert_eq!(size_of::<T>(), 16);

        let slot_ptr = self.first_slot.offset(slot as isize);

        *slot_ptr = node;
    }

    unsafe fn get_unchecked<'a, T>(&'a self, slot: u8) -> &'a T {
        &*self.first_slot.offset(slot as isize)
    }

    // A slot is available iff its bytes are all zeroes
    unsafe fn is_available(&self, slot: u8) -> bool {
        let slot_ptr = self.first_slot.offset(slot as isize) as *const [u8; 16];

        *slot_ptr == [0; 16]
    }
}

impl Default for Bucket {
    fn default() -> Self {
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
        } as *mut [u8; 16];

        Bucket {
            next_unused_slot: 0,
            first_slot,
            _phantom: PhantomData::default(),
        }
    }
}

impl Drop for Bucket {
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

/// A non-empty list inside a bucket. It takes 4B of memory.
///
/// This is internally represented as an array of at most 255 nodes, which
/// can grow to 256+ nodes by having the last nodeent be a linked list Cons
/// cell which points to another such backing array which has more nodes.
///
/// In practice, these will almost be far below 256 nodes, but in theory
/// they can be enormous in length thanks to the linked list fallback.
///
/// Since these are non-empty lists, we need separate variants for collections
/// that can be empty, e.g. EmptyRecord and EmptyList. In contrast, we don't
/// need an EmptyList or EmptyWhen, since although those use BucketList
/// to store their branches, having zero branches is syntactically invalid.
/// Same with Call and Closure, since all functions must have 1+ arguments.
pub struct BucketList<T: Sized> {
    first_node_id: NodeId<T>,
    first_segment_len: u8,
}

impl<T: Sized> BucketList<T> {
    /// If given a first_segment_len of 0, that means this is a BucketList
    /// consisting of 256+ nodes. The first 255 are stored in the usual
    /// array, and then there's one more nodeent at the end which continues
    /// the list with a new length and NodeId value. BucketList iterators
    /// automatically do these jumps behind the scenes when necessary.
    pub fn new(first_node_id: NodeId<T>, first_segment_len: u8) -> Self {
        BucketList {
            first_segment_len,
            first_node_id,
        }
    }

    pub fn into_iter<'a>(self, buckets: &'a Buckets) -> impl Iterator<Item = &'a T> {
        self.into_bucket_list_iter(buckets)
    }

    /// Private version of into_iter which exposes the implementation detail
    /// of BucketListIter. We don't want that struct to be public, but we
    /// actually do want to have this separate function for code reuse
    /// in the iterator's next() method.
    fn into_bucket_list_iter<'a>(self, buckets: &'a Buckets) -> BucketListIter<'a, T> {
        let first_segment_len = self.first_segment_len;
        let continues_with_cons = first_segment_len == 0;
        let len_remaining = if continues_with_cons {
            // We have 255 nodes followed by a Cons cell continuing the list.
            u8::MAX
        } else {
            first_segment_len
        };

        BucketListIter {
            continues_with_cons,
            len_remaining,
            node_id: self.first_node_id,
            buckets,
        }
    }
}

struct BucketListIter<'a, T: Sized> {
    node_id: NodeId<T>,
    len_remaining: u8,
    continues_with_cons: bool,
    buckets: &'a Buckets,
}

impl<'a, T: Sized> Iterator for BucketListIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.len_remaining {
            0 => match self.continues_with_cons {
                // We're done! This is by far the most common case, so we put
                // it first to avoid branch mispredictions.
                False => None,
                // We need to continue with a Cons cell.
                True => {
                    // Since we have continues_with_cons set, the next slot
                    // will definitely be occupied with a BucketList struct.
                    let node = self.buckets.get_unchecked(self.node_id.next_slot());
                    let next_list = unsafe { mem::transmute::<&T, &BucketList<T>>(node) };

                    // Replace the current iterator with an iterator into that
                    // list, and then continue with next() on that iterator.
                    let next_iter = next_list.into_bucket_list_iter(self.buckets);

                    self.node_id = next_iter.node_id;
                    self.len_remaining = next_iter.len_remaining;
                    self.continues_with_cons = next_iter.continues_with_cons;

                    self.next()
                }
            },
            1 => {
                self.len_remaining = 0;

                // Don't advance the node pointer's slot, because that might
                // advance past the end of the bucket!

                Some(self.buckets.get_unchecked(self.node_id))
            }
            len_remaining => {
                // Get the current node
                let node = self.buckets.get_unchecked(self.node_id);

                // Advance the node pointer to the next slot in the current bucket
                self.node_id = self.node_id.next_slot();
                self.len_remaining = len_remaining - 1;

                Some(node)
            }
        }
    }
}
