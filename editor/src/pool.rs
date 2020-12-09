/// A pool of 32-byte nodes. The node value 0 is reserved for the pool's
/// use, and valid nodes may never have that value.
///
/// Internally, the pool is divided into pages of 4096 bytes. It stores nodes
/// into one page at a time, and when it runs out, it uses mmap to reserve an
/// anonymous memory page in which to store nodes.
///
/// Since nodes are 32 bytes, one page can store 128 nodes; you can access a
/// particular node by its NodeId, which is an opaque wrapper around a pointer.
///
/// Pages also use the node value 0 (all 0 bits) to mark nodes as unoccupied.
/// This is important for performance.
use libc::{c_void, calloc, free, mmap, munmap, MAP_ANONYMOUS, MAP_PRIVATE, PROT_READ, PROT_WRITE};
use std::mem::size_of;
use std::ptr::null;

pub const NODE_SIZE: usize = 32;

// Pages are an internal concept which never leave this module.
const PAGE_BYTES: usize = 4096;
const NODES_PER_PAGE: usize = PAGE_BYTES / NODE_SIZE;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NodeId<T: Sized>(*const T);

pub struct Pool {
    pages: Vec<Page>,
    // free_1node_slots: Vec<NodeId<T>>,
}

impl Pool {
    // fn find_space_for(&mut self, nodes: u8) -> Result<PageId<T>, ()> {}

    pub fn add<T: Sized>(&mut self) -> Result<NodeId<T>, ()> {
        let num_pages = self.buckets.len();

        match self.pages.last() {}

        if self.next_unused_node.offset_from(self.first_node) < NODES_PER_PAGE {
            let bucket = Page::default();

            self.buckets.push(bucket);

            Ok(NodeId(bucket.first_node as *const T))
        } else {
            Err(())
        }
    }

    fn get_unchecked<'a, T: Sized>(&'a self, node_id: NodeId<T>) -> &'a T {
        unsafe {
            self.buckets
                .get(node_id.bucket_id.value as usize)
                .unwrap()
                .get_unchecked(node_id.node.value)
        }
    }

    pub fn get<'a, T: Sized>(&'a self, node_id: NodeId<T>) -> Option<&'a T> {
        self.buckets
            .get(node_id.bucket_id.value as usize)
            .and_then(|bucket| bucket.get(node_id.node))
    }
}

struct Page {
    #[allow(dead_code)]
    next_unused_node: *const [u8; NODE_SIZE],
    first_node: *mut [u8; NODE_SIZE],
}

impl Page {
    /// If there's room left in the bucket, adds the item and returns
    /// the node where it was put. If there was no room left, returns Err(()).
    #[allow(dead_code)]
    pub fn add<T: Sized>(&mut self, node: T) -> Result<NodeId<T>, ()> {
        // It's only safe to store this as a *const T if T is the size of a node.
        debug_assert_eq!(size_of::<T>(), NODE_SIZE);

        // Once next_unused_node exceeds NODES_PER_PAGE, we have no room left.
        if self.next_unused_node <= NODES_PER_PAGE {
            let chosen_node = self.next_unused_node;

            unsafe { *chosen_node = node };
            self.next_unused_node = self.next_unused_node.add(1);

            Ok(NodeId(chosen_node))
        } else {
            // No room left!
            Err(())
        }
    }

    /// If the given node is available, inserts the given node into it.
    /// Otherwise, returns the node that was in the already-occupied node.
    #[allow(dead_code)]
    pub fn insert<T: Sized>(&mut self, node: T, node: NodeId<T>) -> Result<(), &T> {
        // It's only safe to store this as a *const T if T is the size of a node.
        debug_assert_eq!(size_of::<T>(), NODE_SIZE);

        let node = node.0;

        unsafe {
            if self.is_available(node) {
                self.put_unchecked(node, node);

                Ok(())
            } else {
                Err(self.get_unchecked(node))
            }
        }
    }

    pub fn get<'a, T: Sized>(&'a self, node: NodeId<T>) -> Option<&'a T> {
        // It's only safe to store this as a *const T if T is the size of a node.
        debug_assert_eq!(size_of::<T>(), NODE_SIZE);

        unsafe {
            let node_ptr = self.first_node.offset(node.value as isize) as *const T;
            let value: &[u8; NODE_SIZE] = &*(node_ptr as *const [u8; NODE_SIZE]);

            if *value != [0; NODE_SIZE] {
                Some(&*(value as *const [u8; NODE_SIZE] as *const T))
            } else {
                None
            }
        }
    }

    unsafe fn get_unchecked<T>(&self, node: u8) -> &T {
        &*(self.first_node.offset(node as isize) as *const T)
    }

    // A node is available iff its bytes are all zeroes
    unsafe fn is_available<T>(&self, node_id: NodeId<T>) -> bool {
        debug_assert_eq!(size_of::<T>(), NODE_SIZE);

        *node_id.0 == [0; NODE_SIZE]
    }
}

impl Default for Page {
    fn default() -> Self {
        let first_node = if page_size::get() == 4096 {
            unsafe {
                // mmap exactly one memory page (4096 bytes)
                mmap(
                    null::<c_void>() as *mut c_void,
                    PAGE_BYTES,
                    PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANONYMOUS,
                    0,
                    0,
                )
            }
        } else {
            // Somehow the page size is not 4096 bytes, so fall back on calloc.
            // (We use calloc over malloc because we rely on the bytes having
            // been zeroed to tell which nodes are available.)
            unsafe { calloc(1, PAGE_BYTES) }
        } as *mut [u8; NODE_SIZE];

        Page {
            next_unused_node: first_node,
            first_node,
        }
    }
}

impl Drop for Page {
    fn drop(&mut self) {
        if page_size::get() == 4096 {
            unsafe {
                munmap(self.first_node as *mut c_void, PAGE_BYTES);
            }
        } else {
            unsafe {
                free(self.first_node as *mut c_void);
            }
        }
    }
}

#[derive(Debug)]
pub struct PageStr {
    first_node_id: NodeId<()>,
    first_segment_len: u8,
}

#[test]
fn size_of_bucket_str() {
    assert_eq!(std::mem::size_of::<PageList<()>>(), 4);
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
/// need an EmptyList or EmptyWhen, since although those use PageList
/// to store their branches, having zero branches is syntactically invalid.
/// Same with Call and Closure, since all functions must have 1+ arguments.
#[derive(Debug)]
pub struct PageList<T: Sized> {
    first_node_id: NodeId<T>,
    first_segment_len: u8,
}

#[test]
fn size_of_bucket_list() {
    assert_eq!(std::mem::size_of::<PageList<()>>(), 4);
}

impl<'a, T: 'a + Sized> PageList<T> {
    /// If given a first_segment_len of 0, that means this is a PageList
    /// consisting of 256+ nodes. The first 255 are stored in the usual
    /// array, and then there's one more nodeent at the end which continues
    /// the list with a new length and NodeId value. PageList iterators
    /// automatically do these jumps behind the scenes when necessary.
    pub fn new(first_node_id: NodeId<T>, first_segment_len: u8) -> Self {
        PageList {
            first_segment_len,
            first_node_id: first_node_id.bucket_id,
            first_node_sl: first_node_id.node,
        }
    }

    pub fn into_iter(self, buckets: &'a Pages) -> impl Iterator<Item = &'a T> {
        self.bucket_list_iter(buckets)
    }

    /// Private version of into_iter which exposes the implementation detail
    /// of PageListIter. We don't want that struct to be public, but we
    /// actually do want to have this separate function for code reuse
    /// in the iterator's next() method.
    fn bucket_list_iter(&self, buckets: &'a Pages) -> PageListIter<'a, T> {
        let first_segment_len = self.first_segment_len;
        let continues_with_cons = first_segment_len == 0;
        let len_remaining = if continues_with_cons {
            // We have 255 nodes followed by a Cons cell continuing the list.
            u8::MAX
        } else {
            first_segment_len
        };

        PageListIter {
            continues_with_cons,
            len_remaining,
            bucket_id: self.first_node_id,
            node: self.first_node_sl,
            buckets,
        }
    }
}

struct PageListIter<'a, T: Sized> {
    current_node_id: NodeId<T>,
    len_remaining: u8,
    continues_with_cons: bool,
    buckets: &'a Pages,
}

impl<'a, T: Sized> Iterator for PageListIter<'a, T>
where
    T: 'a,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.len_remaining {
            0 => match self.continues_with_cons {
                // We're done! This is by far the most common case, so we put
                // it first to avoid branch mispredictions.
                false => None,
                // We need to continue with a Cons cell.
                true => {
                    let node_id = NodeId {
                        bucket_id: self.bucket_id,
                        node: self.node,
                    }
                    .next_node();

                    // Since we have continues_with_cons set, the next node
                    // will definitely be occupied with a PageList struct.
                    let node = self.buckets.get_unchecked(node_id);
                    let next_list = unsafe { &*(node as *const T as *const PageList<T>) };

                    // Replace the current iterator with an iterator into that
                    // list, and then continue with next() on that iterator.
                    let next_iter = next_list.bucket_list_iter(self.buckets);

                    self.bucket_id = next_iter.bucket_id;
                    self.node = next_iter.node;
                    self.len_remaining = next_iter.len_remaining;
                    self.continues_with_cons = next_iter.continues_with_cons;

                    self.next()
                }
            },
            1 => {
                self.len_remaining = 0;

                // Don't advance the node pointer's node, because that might
                // advance past the end of the bucket!

                Some(self.buckets.get_unchecked(NodeId {
                    bucket_id: self.bucket_id,
                    node: self.node,
                }))
            }
            len_remaining => {
                // Get the current node
                let node_id = NodeId {
                    bucket_id: self.bucket_id,
                    node: self.node,
                };
                let node = self.buckets.get_unchecked(node_id);

                // Advance the node pointer to the next node in the current bucket
                self.node = self.node.increment();
                self.len_remaining = len_remaining - 1;

                Some(node)
            }
        }
    }
}
