use super::pool::{NodeId, Pool, NODE_BYTES};
use super::shallow_clone::ShallowClone;
use std::any::type_name;
use std::cmp::Ordering;
use std::ffi::c_void;
use std::marker::PhantomData;
use std::mem::size_of;

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
    pub fn empty(pool: &mut Pool) -> Self {
        Self::new(std::iter::empty(), pool)
    }

    pub fn with_capacity(len: u32, pool: &mut Pool) -> Self {
        debug_assert!(
            size_of::<T>() <= NODE_BYTES,
            "{} has a size of {}",
            type_name::<T>(),
            size_of::<T>()
        );

        if len == 0 {
            Self::empty(pool)
        } else {
            let first_node_id = pool.reserve(len);

            PoolVec { first_node_id, len }
        }
    }

    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn new<I: ExactSizeIterator<Item = T>>(nodes: I, pool: &mut Pool) -> Self {
        debug_assert!(nodes.len() <= u32::MAX as usize);
        debug_assert!(size_of::<T>() <= NODE_BYTES);

        let len = nodes.len() as u32;

        if len > 0 {
            let first_node_id = pool.reserve(len);
            let index = first_node_id.index as isize;
            let mut next_node_ptr = unsafe { pool.nodes.offset(index) } as *mut T;

            for (indx_inc, node) in nodes.enumerate() {
                unsafe {
                    *next_node_ptr = node;

                    next_node_ptr = pool.nodes.offset(index + (indx_inc as isize) + 1) as *mut T;
                }
            }

            PoolVec { first_node_id, len }
        } else {
            PoolVec {
                first_node_id: NodeId {
                    index: 0,
                    _phantom: PhantomData,
                },
                len: 0,
            }
        }
    }

    pub fn iter(&self, pool: &'a Pool) -> impl ExactSizeIterator<Item = &'a T> {
        self.pool_list_iter(pool)
    }

    pub fn iter_mut(&self, pool: &'a mut Pool) -> impl ExactSizeIterator<Item = &'a mut T> {
        self.pool_list_iter_mut(pool)
    }

    pub fn iter_node_ids(&self) -> impl ExactSizeIterator<Item = NodeId<T>> {
        self.pool_list_iter_node_ids()
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

    #[inline(always)]
    fn pool_list_iter_mut(&self, pool: &'a Pool) -> PoolVecIterMut<'a, T> {
        PoolVecIterMut {
            pool,
            current_node_id: self.first_node_id,
            len_remaining: self.len,
        }
    }

    #[inline(always)]
    fn pool_list_iter_node_ids(&self) -> PoolVecIterNodeIds<T> {
        PoolVecIterNodeIds {
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

impl<T> ShallowClone for PoolVec<T> {
    fn shallow_clone(&self) -> Self {
        // Question: should this fully clone, or is a shallow copy
        // (and the aliasing it entails) OK?
        Self {
            first_node_id: self.first_node_id,
            len: self.len,
        }
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

        match len_remaining.cmp(&1) {
            Ordering::Greater => {
                // Get the current node
                let index = self.current_node_id.index;
                let node_ptr = unsafe { self.pool.nodes.offset(index as isize) } as *const T;

                // Advance the node pointer to the next node in the current page
                self.current_node_id = NodeId {
                    index: index + 1,
                    _phantom: PhantomData,
                };
                self.len_remaining = len_remaining - 1;

                Some(unsafe { &*node_ptr })
            }
            Ordering::Equal => {
                self.len_remaining = 0;

                // Don't advance the node pointer's node, because that might
                // advance past the end of the page!

                let index = self.current_node_id.index;
                let node_ptr = unsafe { self.pool.nodes.offset(index as isize) } as *const T;

                Some(unsafe { &*node_ptr })
            }
            Ordering::Less => {
                // len_remaining was 0
                None
            }
        }
    }
}

struct PoolVecIterMut<'a, T> {
    pool: &'a Pool,
    current_node_id: NodeId<T>,
    len_remaining: u32,
}

impl<'a, T> ExactSizeIterator for PoolVecIterMut<'a, T>
where
    T: 'a,
{
    fn len(&self) -> usize {
        self.len_remaining as usize
    }
}

impl<'a, T> Iterator for PoolVecIterMut<'a, T>
where
    T: 'a,
{
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        let len_remaining = self.len_remaining;

        match len_remaining.cmp(&1) {
            Ordering::Greater => {
                // Get the current node
                let index = self.current_node_id.index;
                let node_ptr = unsafe { self.pool.nodes.offset(index as isize) } as *mut T;

                // Advance the node pointer to the next node in the current page
                self.current_node_id = NodeId {
                    index: index + 1,
                    _phantom: PhantomData,
                };
                self.len_remaining = len_remaining - 1;

                Some(unsafe { &mut *node_ptr })
            }
            Ordering::Equal => {
                self.len_remaining = 0;

                // Don't advance the node pointer's node, because that might
                // advance past the end of the page!

                let index = self.current_node_id.index;
                let node_ptr = unsafe { self.pool.nodes.offset(index as isize) } as *mut T;

                Some(unsafe { &mut *node_ptr })
            }
            Ordering::Less => {
                // len_remaining was 0
                None
            }
        }
    }
}

struct PoolVecIterNodeIds<T> {
    current_node_id: NodeId<T>,
    len_remaining: u32,
}

impl<T> ExactSizeIterator for PoolVecIterNodeIds<T> {
    fn len(&self) -> usize {
        self.len_remaining as usize
    }
}

impl<T> Iterator for PoolVecIterNodeIds<T> {
    type Item = NodeId<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let len_remaining = self.len_remaining;

        match len_remaining.cmp(&1) {
            Ordering::Greater => {
                // Get the current node
                let current = self.current_node_id;
                let index = current.index;

                // Advance the node pointer to the next node in the current page
                self.current_node_id = NodeId {
                    index: index + 1,
                    _phantom: PhantomData,
                };
                self.len_remaining = len_remaining - 1;

                Some(current)
            }
            Ordering::Equal => {
                self.len_remaining = 0;

                // Don't advance the node pointer's node, because that might
                // advance past the end of the page!

                Some(self.current_node_id)
            }
            Ordering::Less => {
                // len_remaining was 0
                None
            }
        }
    }
}

#[test]
fn pool_vec_iter_test() {
    let expected_vec: Vec<usize> = vec![2, 4, 8, 16];

    let mut test_pool = Pool::with_capacity(1024);
    let pool_vec = PoolVec::new(expected_vec.clone().into_iter(), &mut test_pool);

    let current_vec: Vec<usize> = pool_vec.iter(&test_pool).copied().collect();

    assert_eq!(current_vec, expected_vec);
}
