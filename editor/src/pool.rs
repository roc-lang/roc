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
use std::mem::size_of;
use std::ptr::null;

pub const NODE_SIZE: usize = 32;

// Pages are an internal concept which never leave this module.
const PAGE_BYTES: usize = 4096;
const NODES_PER_PAGE: u8 = (PAGE_BYTES / NODE_SIZE) as u8;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NodeId<T: Sized>(*const T);

pub struct Pool {
    pages: Vec<Page>,
    // free_1node_slots: Vec<NodeId<T>>,
}

impl Pool {
    /// Returns a pool with a capacity equal to the given number of 4096-byte pages.
    // pub fn with_pages(pages: usize) {
    //     todo!();
    // }

    // fn find_space_for(&mut self, nodes: u8) -> Result<PageId<T>, ()> {}

    pub fn add<T: Sized>(&mut self, node: T) -> NodeId<T> {
        // It's only safe to store this as a *mut T if T is the size of a node.
        debug_assert_eq!(size_of::<T>(), NODE_SIZE);

        match self.pages.last_mut() {
            Some(page) if page.node_count < NODES_PER_PAGE => Pool::add_to_page(page, node),
            _ => {
                // This page is either full or doesn't exist, so create a new one.
                let mut page = Page::default();
                let node_id = Pool::add_to_page(&mut page, node);

                self.pages.push(page);

                node_id
            }
        }
    }

    /// Reserves the given number of contiguous node slots, and returns
    /// the NodeId of the first one. We only allow reserving 2^32 in a row.
    fn reserve<T: Sized>(&mut self, _nodes: u32) -> NodeId<T> {
        todo!("Implement Pool::reserve");
    }

    fn add_to_page<T: Sized>(page: &mut Page, node: T) -> NodeId<T> {
        unsafe {
            let node_ptr = (page.first_node as *const T).offset(page.node_count as isize) as *mut T;

            *node_ptr = node;

            page.node_count += 1;

            NodeId(node_ptr)
        }
    }

    pub fn get<'a, T: Sized>(&'a self, node_id: NodeId<T>) -> &'a T {
        unsafe { &*node_id.0 }
    }

    // A node is available iff its bytes are all zeroes
    #[allow(dead_code)]
    unsafe fn is_available<T>(&self, node_id: NodeId<T>) -> bool {
        debug_assert_eq!(size_of::<T>(), NODE_SIZE);

        let ptr = node_id.0 as *const [u8; NODE_SIZE];

        *ptr == [0; NODE_SIZE]
    }
}

struct Page {
    first_node: *const [u8; NODE_SIZE],
    node_count: u8,
}

impl Default for Page {
    fn default() -> Self {
        let first_node = if page_size::get() == 4096 {
            unsafe {
                // mmap exactly one memory page (4096 bytes)
                libc::mmap(
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
            unsafe { libc::calloc(1, PAGE_BYTES) }
        } as *mut [u8; NODE_SIZE];

        Page {
            first_node,
            node_count: 0,
        }
    }
}

impl Drop for Page {
    fn drop(&mut self) {
        if page_size::get() == 4096 {
            unsafe {
                libc::munmap(self.first_node as *mut c_void, PAGE_BYTES);
            }
        } else {
            unsafe {
                libc::free(self.first_node as *mut c_void);
            }
        }
    }
}

/// A string of at most 2^32 bytes, allocated in a pool if it fits in a Page,
/// or using malloc as a fallback if not. Like std::str::String, this has
/// both a length and capacity.
#[derive(Debug)]
pub struct PoolStr {
    first_node_id: NodeId<()>,
    len: u32,
    cap: u32,
}

#[test]
fn pool_str_size() {
    assert_eq!(size_of::<PoolStr>(), size_of::<usize>() + 8);
}

/// An array of at most 2^32 elements, allocated in a pool if it fits in a Page,
/// or using malloc as a fallback if not. Like std::vec::Vec, this has both
/// a length and capacity.
#[derive(Debug)]
pub struct PoolVec<T: Sized> {
    first_node_id: NodeId<T>,
    len: u32,
    cap: u32,
}

#[test]
fn pool_vec_size() {
    assert_eq!(size_of::<PoolVec<()>>(), size_of::<usize>() + 8);
}

impl<'a, T: 'a + Sized> PoolVec<T> {
    /// If given a slice of length > 128, the first 128 nodes will be stored in
    /// the usual array, and then there's one more node at the end which
    /// continues the list with a new length and NodeId value. PoolVec
    /// iterators automatically do these jumps behind the scenes when necessary.
    pub fn new<I: ExactSizeIterator<Item = T>>(nodes: I, pool: &mut Pool) -> Self {
        debug_assert!(nodes.len() <= u32::MAX as usize);
        debug_assert!(size_of::<T>() <= NODE_SIZE);

        let len = nodes.len() as u32;

        if len > 0 {
            if len <= NODES_PER_PAGE as u32 {
                let first_node_id = pool.reserve(len);
                let mut next_node_ptr = first_node_id.0 as *mut T;

                for node in nodes {
                    unsafe {
                        *next_node_ptr = node;

                        next_node_ptr = next_node_ptr.offset(1);
                    }
                }

                PoolVec {
                    first_node_id,
                    len,
                    cap: len,
                }
            } else {
                let first_node_ptr = unsafe {
                    // mmap enough memory to hold it
                    libc::mmap(
                        null::<c_void>() as *mut c_void,
                        len as usize,
                        PROT_READ | PROT_WRITE,
                        MAP_PRIVATE | MAP_ANONYMOUS,
                        0,
                        0,
                    )
                };

                PoolVec {
                    first_node_id: NodeId(first_node_ptr as *const T),
                    len,
                    cap: len,
                }
            }
        } else {
            PoolVec {
                first_node_id: NodeId(std::ptr::null()),
                len: 0,
                cap: 0,
            }
        }
    }

    pub fn iter(self, pool: &'a Pool) -> impl ExactSizeIterator<Item = &'a T> {
        self.pool_list_iter(pool)
    }

    /// Private version of into_iter which exposes the implementation detail
    /// of PoolVecIter. We don't want that struct to be public, but we
    /// actually do want to have this separate function for code reuse
    /// in the iterator's next() method.
    #[inline(always)]
    fn pool_list_iter(&self, pool: &'a Pool) -> PoolVecIter<'a, T> {
        PoolVecIter {
            _pool: pool,
            current_node_id: NodeId(self.first_node_id.0),
            len_remaining: self.len,
        }
    }

    pub fn free(self) {
        if self.len <= NODES_PER_PAGE as u32 {
            // If this was small enough to fit in a Page, then zero it out.
            unsafe {
                libc::memset(
                    self.first_node_id.0 as *mut c_void,
                    0,
                    self.len as usize * NODE_SIZE,
                );
            }

        // TODO insert it into the pool's free list
        } else {
            // This was bigger than a Page, so we mmap'd it. Now we free it!
            unsafe {
                libc::munmap(self.first_node_id.0 as *mut c_void, self.len as usize);
            }
        }
    }
}

struct PoolVecIter<'a, T: Sized> {
    /// This iterator returns elements which have the same lifetime as the pool
    _pool: &'a Pool,
    current_node_id: NodeId<T>,
    len_remaining: u32,
}

impl<'a, T: Sized> ExactSizeIterator for PoolVecIter<'a, T>
where
    T: 'a,
{
    fn len(&self) -> usize {
        self.len_remaining as usize
    }
}

impl<'a, T: Sized> Iterator for PoolVecIter<'a, T>
where
    T: 'a,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let len_remaining = self.len_remaining;

        if len_remaining > 1 {
            // Get the current node
            let node_ptr = self.current_node_id.0;

            // Advance the node pointer to the next node in the current page
            self.current_node_id = NodeId(unsafe { node_ptr.offset(1) });
            self.len_remaining = len_remaining - 1;

            Some(unsafe { &*node_ptr })
        } else if len_remaining == 1 {
            self.len_remaining = 0;

            // Don't advance the node pointer's node, because that might
            // advance past the end of the page!

            Some(unsafe { &*self.current_node_id.0 })
        } else {
            // len_remaining was 0
            None
        }
    }
}
