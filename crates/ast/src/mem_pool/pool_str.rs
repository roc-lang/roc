use super::pool::{NodeId, Pool, NODE_BYTES};
use super::shallow_clone::ShallowClone;
use std::ffi::c_void;
use std::marker::PhantomData;
use std::mem::size_of;

/// A string containing at most 2^32 pool-allocated bytes.
#[derive(Debug, Copy, Clone)]
pub struct PoolStr {
    first_node_id: NodeId<()>,
    len: u32,
}

#[test]
fn pool_str_size() {
    assert_eq!(size_of::<PoolStr>(), 8);
}

impl PoolStr {
    pub fn new(string: &str, pool: &mut Pool) -> Self {
        debug_assert!(string.len() <= u32::MAX as usize);

        let chars_per_node = NODE_BYTES / size_of::<char>();

        let number_of_nodes = f64::ceil(string.len() as f64 / chars_per_node as f64) as u32;

        if number_of_nodes > 0 {
            let first_node_id = pool.reserve(number_of_nodes);
            let index = first_node_id.index as isize;
            let next_node_ptr = unsafe { pool.nodes.offset(index) } as *mut c_void;

            unsafe {
                libc::memcpy(
                    next_node_ptr,
                    string.as_ptr() as *const c_void,
                    string.len(),
                );
            }

            PoolStr {
                first_node_id,
                len: string.len() as u32,
            }
        } else {
            PoolStr {
                first_node_id: NodeId {
                    index: 0,
                    _phantom: PhantomData,
                },
                len: 0,
            }
        }
    }

    pub fn as_str(&self, pool: &Pool) -> &str {
        unsafe {
            let node_ptr = pool.nodes.offset(self.first_node_id.index as isize) as *const u8;

            let node_slice: &[u8] = std::slice::from_raw_parts(node_ptr, self.len as usize);

            std::str::from_utf8_unchecked(&node_slice[0..self.len as usize])
        }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self, pool: &Pool) -> usize {
        let contents = self.as_str(pool);

        contents.len()
    }

    pub fn is_empty(&self, pool: &Pool) -> bool {
        self.len(pool) == 0
    }
}

impl ShallowClone for PoolStr {
    fn shallow_clone(&self) -> Self {
        // Question: should this fully clone, or is a shallow copy
        // (and the aliasing it entails) OK?
        Self {
            first_node_id: self.first_node_id,
            len: self.len,
        }
    }
}
