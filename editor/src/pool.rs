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
use libc::{c_void, MAP_ANONYMOUS, MAP_PRIVATE, PROT_READ, PROT_WRITE};
use std::marker::PhantomData;
use std::mem::size_of;
use std::ptr::null;

pub const NODE_BYTES: usize = 32;

// Each page has 128 slots. Each slot holds one 32B node
// This means each page is 4096B, which is the size of a memory page
// on typical systems where the compiler will be run.
//
// Nice things about this system include:
// * Allocating a new page is as simple as asking the OS for a memory page.
// * Since each node is 32B, each node's memory address will be a multiple of 16.
// * Thanks to the free lists and our consistent chunk sizes, we should
//   end up with very little fragmentation.
// * Finding a slot for a given node should be very fast: see if the relevant
//   free list has any openings; if not, try the next size up.
//
// Less nice things include:
// * This system makes it very hard to ever give a page back to the OS.
//   We could try doing the Mesh Allocator strategy: whenever we allocate
//   something, assign it to a random slot in the page, and then periodically
//   try to merge two pages into one (by locking and remapping them in the OS)
//   and then returning the redundant physical page back to the OS. This should
//   work in theory, but is pretty complicated, and we'd need to schedule it.
//   Keep in mind that we can't use the Mesh Allocator itself because it returns
//   usize pointers, which would be too big for us to have 16B nodes.
//   On the plus side, we could be okay with higher memory usage early on,
//   and then later use the Mesh strategy to reduce long-running memory usage.

#[derive(Debug, PartialEq, Eq)]
pub struct NodeId<T> {
    index: u32,
    _phantom: PhantomData<T>,
}

impl<T> Clone for NodeId<T> {
    fn clone(&self) -> Self {
        NodeId {
            index: self.index,
            _phantom: PhantomData::default(),
        }
    }
}

impl<T> Copy for NodeId<T> {}

pub struct Pool {
    nodes: *mut [u8; NODE_BYTES],
    num_nodes: u32,
    capacity: u32,
    // free_1node_slots: Vec<NodeId<T>>,
}

impl Pool {
    pub fn with_capacity(nodes: u32) -> Self {
        // round up number of nodes requested to nearest page size in bytes
        let bytes_per_page = page_size::get();
        let node_bytes = NODE_BYTES * nodes as usize;
        let leftover = node_bytes % bytes_per_page;
        let bytes_to_mmap = if leftover == 0 {
            node_bytes
        } else {
            node_bytes + bytes_per_page - leftover
        };

        let nodes = unsafe {
            // mmap anonymous memory pages - that is, contiguous virtual memory
            // addresses from the OS which will be lazily translated into
            // physical memory one 4096-byte page at a time, once we actually
            // try to read or write in that page's address range.
            libc::mmap(
                null::<c_void>() as *mut c_void,
                bytes_to_mmap,
                PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANONYMOUS,
                0,
                0,
            )
        } as *mut [u8; NODE_BYTES];

        // This is our actual capacity, in nodes.
        // It might be higher than the requested capacity due to rounding up
        // to nearest page size.
        let capacity = (bytes_to_mmap / NODE_BYTES) as u32;

        Pool {
            nodes,
            num_nodes: 0,
            capacity,
        }
    }

    pub fn add<T>(&mut self, node: T) -> NodeId<T> {
        // It's only safe to store this if T is the same size as S.
        debug_assert_eq!(size_of::<T>(), NODE_BYTES);

        let index = self.num_nodes;

        if index < self.capacity {
            let node_ptr = unsafe { self.nodes.offset(index as isize) } as *mut T;

            unsafe { *node_ptr = node };

            self.num_nodes = index + 1;

            NodeId {
                index,
                _phantom: PhantomData::default(),
            }
        } else {
            todo!("pool ran out of capacity. TODO reallocate the nodes pointer to map to a bigger space. Can use mremap on Linux, but must memcpy lots of bytes on macOS and Windows.");
        }
    }

    /// Reserves the given number of contiguous node slots, and returns
    /// the NodeId of the first one. We only allow reserving 2^32 in a row.
    fn reserve<T>(&mut self, _nodes: u32) -> NodeId<T> {
        todo!("Implement Pool::reserve");
    }

    pub fn get<'a, T>(&'a self, node_id: NodeId<T>) -> &'a T {
        unsafe {
            let node_ptr = self.nodes.offset(node_id.index as isize) as *mut T;

            &*node_ptr
        }
    }

    // A node is available iff its bytes are all zeroes
    #[allow(dead_code)]
    fn is_available<T>(&self, node_id: NodeId<T>) -> bool {
        debug_assert_eq!(size_of::<T>(), NODE_BYTES);

        unsafe {
            let node_ptr = self.nodes.offset(node_id.index as isize) as *const [u8; NODE_BYTES];

            *node_ptr == [0; NODE_BYTES]
        }
    }
}

impl Drop for Pool {
    fn drop(&mut self) {
        unsafe {
            libc::munmap(
                self.nodes as *mut c_void,
                NODE_BYTES * self.capacity as usize,
            );
        }
    }
}

/// A string containing at most 2^32 pool-allocated bytes.
#[derive(Debug)]
pub struct PoolStr {
    first_node_id: NodeId<()>,
    len: u32,
}

#[test]
fn pool_str_size() {
    assert_eq!(size_of::<PoolStr>(), 8);
}

/// An array of at most 2^32 pool-allocated nodes.
#[derive(Debug)]
pub struct PoolVec<T> {
    first_node_id: NodeId<T>,
    len: u32,
}

#[test]
fn pool_vec_size() {
    assert_eq!(size_of::<PoolVec<()>>(), 8);
}

impl<'a, T: 'a + Sized> PoolVec<T> {
    /// If given a slice of length > 128, the first 128 nodes will be stored in
    /// the usual array, and then there's one more node at the end which
    /// continues the list with a new length and NodeId value. PoolVec
    /// iterators automatically do these jumps behind the scenes when necessary.
    pub fn new<I: ExactSizeIterator<Item = T>, S>(nodes: I, pool: &mut Pool) -> Self {
        debug_assert!(nodes.len() <= u32::MAX as usize);
        debug_assert!(size_of::<T>() <= NODE_BYTES);

        let len = nodes.len() as u32;

        if len > 0 {
            let first_node_id = pool.reserve(len);
            let index = first_node_id.index as isize;
            let mut next_node_ptr = unsafe { pool.nodes.offset(index) } as *mut T;

            for node in nodes {
                unsafe {
                    *next_node_ptr = node;

                    next_node_ptr = next_node_ptr.offset(1);
                }
            }

            PoolVec { first_node_id, len }
        } else {
            PoolVec {
                first_node_id: NodeId {
                    index: 0,
                    _phantom: PhantomData::default(),
                },
                len: 0,
            }
        }
    }

    pub fn iter<S>(self, pool: &'a Pool) -> impl ExactSizeIterator<Item = &'a T> {
        self.pool_list_iter(pool)
    }

    /// Private version of into_iter which exposes the implementation detail
    /// of PoolVecIter. We don't want that struct to be public, but we
    /// actually do want to have this separate function for code reuse
    /// in the iterator's next() method.
    #[inline(always)]
    fn pool_list_iter(&self, pool: &'a Pool) -> PoolVecIter<'a, T> {
        PoolVecIter {
            pool,
            current_node_id: self.first_node_id,
            len_remaining: self.len,
        }
    }

    pub fn free<S>(self, pool: &'a mut Pool) {
        // zero out the memory
        unsafe {
            let index = self.first_node_id.index as isize;
            let node_ptr = pool.nodes.offset(index) as *mut c_void;
            let bytes = self.len as usize * NODE_BYTES;

            libc::memset(node_ptr, 0, bytes);
        }

        // TODO insert it into the pool's free list
    }
}

struct PoolVecIter<'a, T> {
    pool: &'a Pool,
    current_node_id: NodeId<T>,
    len_remaining: u32,
}

impl<'a, T> ExactSizeIterator for PoolVecIter<'a, T>
where
    T: 'a,
{
    fn len(&self) -> usize {
        self.len_remaining as usize
    }
}

impl<'a, T> Iterator for PoolVecIter<'a, T>
where
    T: 'a,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let len_remaining = self.len_remaining;

        if len_remaining > 1 {
            // Get the current node
            let index = self.current_node_id.index;
            let node_ptr = unsafe { self.pool.nodes.offset(index as isize) } as *const T;

            // Advance the node pointer to the next node in the current page
            self.current_node_id = NodeId {
                index: index + 1,
                _phantom: PhantomData::default(),
            };
            self.len_remaining = len_remaining - 1;

            Some(unsafe { &*node_ptr })
        } else if len_remaining == 1 {
            self.len_remaining = 0;

            // Don't advance the node pointer's node, because that might
            // advance past the end of the page!

            let index = self.current_node_id.index;
            let node_ptr = unsafe { self.pool.nodes.offset(index as isize) } as *const T;

            Some(unsafe { &*node_ptr })
        } else {
            // len_remaining was 0
            None
        }
    }
}

#[test]
fn mmap_benchmark() {
    use std::time::SystemTime;

    let start_time = SystemTime::now();
    let pool = Pool::with_capacity(1234567890);
    let end_time = SystemTime::now();
    let elapsed = end_time.duration_since(start_time).unwrap();

    println!("Time to init pool: {:?}", elapsed);

    // touch a bunch of pages to see how long they take to fault in
    for i in 1..100 {
        let start_time = SystemTime::now();
        unsafe {
            *pool.nodes.offset(i * 10) = [1; NODE_BYTES];
        };
        let end_time = SystemTime::now();
        let elapsed = end_time.duration_since(start_time).unwrap();

        println!("Time to touch page {}: {:?}", i, elapsed);
    }

    assert_eq!(pool.capacity, 128);
}
