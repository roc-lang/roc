/// A memory pool of 32-byte nodes. The node value 0 is reserved for the pool's
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
use std::any::type_name;
use std::ffi::c_void;
use std::marker::PhantomData;
use std::mem::{align_of, size_of, MaybeUninit};

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
//
// With this system, we can allocate up to 4B nodes. If we wanted to keep
// a generational index in there, like https://crates.io/crates/sharded-slab
// does, we could use some of the 32 bits for that. For example, if we wanted
// to have a 5-bit generational index (supporting up to 32 generations), then
// we would have 27 bits remaining, meaning we could only support at most
// 134M nodes. Since the editor has a separate Pool for each module, is that
// enough for any single module we'll encounter in practice? Probably, and
// especially if we allocate super large collection literals on the heap instead
// of in the pool.
//
// Another possible design is to try to catch reuse bugs using an "ASan" like
// approach: in development builds, whenever we "free" a particular slot, we
// can add it to a dev-build-only "freed nodes" list and don't hand it back
// out (so, we leak the memory.) Then we can (again, in development builds only)
// check to see if we're about to store something in zeroed-out memory; if so, check
// to see if it was

#[derive(Debug, Eq)]
pub struct NodeId<T> {
    pub(super) index: u32,
    pub(super) _phantom: PhantomData<T>,
}

impl<T> Clone for NodeId<T> {
    fn clone(&self) -> Self {
        NodeId {
            index: self.index,
            _phantom: PhantomData,
        }
    }
}

impl<T> PartialEq for NodeId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> Copy for NodeId<T> {}

#[derive(Debug)]
pub struct Pool {
    pub(super) nodes: *mut [MaybeUninit<u8>; NODE_BYTES],
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
            #[cfg(unix)]
            {
                use libc::{MAP_ANONYMOUS, MAP_PRIVATE, PROT_READ, PROT_WRITE};

                libc::mmap(
                    std::ptr::null_mut(),
                    bytes_to_mmap,
                    PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANONYMOUS,
                    0,
                    0,
                )
            }
            #[cfg(windows)]
            {
                use winapi::um::memoryapi::VirtualAlloc;
                use winapi::um::winnt::PAGE_READWRITE;
                use winapi::um::winnt::{MEM_COMMIT, MEM_RESERVE};

                VirtualAlloc(
                    std::ptr::null_mut(),
                    bytes_to_mmap,
                    MEM_COMMIT | MEM_RESERVE,
                    PAGE_READWRITE,
                )
            }
        } as *mut [MaybeUninit<u8>; NODE_BYTES];

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
        // It's only safe to store this if T fits in S.
        debug_assert!(
            size_of::<T>() <= NODE_BYTES,
            "{} has a size of {}, but it needs to be at most {}",
            type_name::<T>(),
            size_of::<T>(),
            NODE_BYTES
        );

        let node_id = self.reserve(1);

        let node_ptr = self.get_ptr(node_id);

        unsafe { node_ptr.write(MaybeUninit::new(node)) };

        node_id
    }

    /// Reserves the given number of contiguous node slots, and returns
    /// the NodeId of the first one. We only allow reserving 2^32 in a row.
    pub(super) fn reserve<T>(&mut self, nodes: u32) -> NodeId<T> {
        // TODO once we have a free list, look in there for an open slot first!
        let index = self.num_nodes;

        if index < self.capacity {
            self.num_nodes = index + nodes;

            NodeId {
                index,
                _phantom: PhantomData,
            }
        } else {
            todo!("pool ran out of capacity. TODO reallocate the nodes pointer to map to a bigger space. Can use mremap on Linux, but must memcpy lots of bytes on macOS and Windows.");
        }
    }

    pub fn get<'b, T>(&self, node_id: NodeId<T>) -> &'b T {
        unsafe {
            let node_ptr = self.get_ptr(node_id) as *const T;

            &*node_ptr
        }
    }

    pub fn get_mut<T>(&mut self, node_id: NodeId<T>) -> &mut T {
        unsafe {
            let node_ptr = self.get_ptr(node_id) as *mut T;

            &mut *node_ptr
        }
    }

    pub fn set<T>(&mut self, node_id: NodeId<T>, element: T) {
        unsafe {
            let node_ptr = self.get_ptr(node_id);

            node_ptr.write(MaybeUninit::new(element));
        }
    }

    fn get_ptr<T>(&self, node_id: NodeId<T>) -> *mut MaybeUninit<T> {
        let node_offset = unsafe { self.nodes.offset(node_id.index as isize) };

        // This checks if the node_offset is aligned to T
        assert!(0 == (node_offset as usize) & (align_of::<T>() - 1));

        node_offset as *mut MaybeUninit<T>
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

impl<T> std::ops::Index<NodeId<T>> for Pool {
    type Output = T;

    fn index(&self, node_id: NodeId<T>) -> &Self::Output {
        self.get(node_id)
    }
}

impl<T> std::ops::IndexMut<NodeId<T>> for Pool {
    fn index_mut(&mut self, node_id: NodeId<T>) -> &mut Self::Output {
        self.get_mut(node_id)
    }
}

impl Drop for Pool {
    fn drop(&mut self) {
        unsafe {
            #[cfg(unix)]
            {
                libc::munmap(
                    self.nodes as *mut c_void,
                    NODE_BYTES * self.capacity as usize,
                );
            }
            #[cfg(windows)]
            {
                use winapi::um::memoryapi::VirtualFree;
                use winapi::um::winnt::MEM_RELEASE;

                VirtualFree(
                    self.nodes as *mut c_void,
                    NODE_BYTES * self.capacity as usize,
                    MEM_RELEASE,
                );
            }
        }
    }
}
