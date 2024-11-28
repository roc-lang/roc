use soa::Slice;

use crate::sized_vec::SizedVec;
use core::marker::PhantomData;

/// Length and capacity are u16. (For u32, use BigVec.)
pub struct Vec<ArenaType, T> {
    start: u32,
    len: u16,
    capacity: u16,
    _phantom: PhantomData<(ArenaType, T)>,
}

/// Like Vec, but with u32 length and capacity instead of u16. That's as big as we go!
pub struct BigVec<ArenaType, T> {
    start: u32,
    len: u32,
    capacity: u32,
    _phantom: PhantomData<(ArenaType, T)>,
}

impl<ArenaType, T> Default for Vec<ArenaType, T> {
    fn default() -> Self {
        Self {
            start: 0,
            len: 0,
            capacity: 0,
            _phantom: PhantomData,
        }
    }
}

impl<ArenaType, T> Default for BigVec<ArenaType, T> {
    fn default() -> Self {
        Self {
            start: 0,
            len: 0,
            capacity: 0,
            _phantom: PhantomData,
        }
    }
}

impl<ArenaType, T> SizedVec<T, ArenaType, u16, 8> for Vec<ArenaType, T> {
    fn len(&self) -> u16 {
        self.len
    }

    fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn capacity(&self) -> u16 {
        self.capacity
    }
}

impl<ArenaType, T> Vec<ArenaType, T> {
    fn as_slice(&self) -> Slice<T> {
        Slice::new(self.start.into(), self.len().into())
    }
}

impl<ArenaType, T> SizedVec<T, ArenaType, u32, 8> for BigVec<ArenaType, T> {
    fn len(&self) -> u32 {
        self.len
    }

    fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn capacity(&self) -> u32 {
        self.capacity
    }
}

// impl<ArenaType, T> BigVec<ArenaType, T> {
//     fn as_slice(&self) -> Slice<T> {
//         let todo = todo!(); // TODO BigSlice maybe?
//                             // Slice::new(self.start.into(), self.len().into())
//     }
// }
