use std::borrow::Borrow;
use std::fmt;

use crate::util::id_type::{self, Count, Id};
use crate::util::id_vec::IdVec;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct SliceData<SliceInfo> {
    info: SliceInfo,

    /// Determines which slice of the `flat_data` buffer contains the items of this slice.
    ///
    /// If the *previous* slice has `slice_end_idx == a`, and this slice has `inputs_end_idx == b`,
    /// then the items of this slice are given by `flat_data[a..b]`.
    slice_end_idx: usize,
}

/// Conceptually represents a collection of the form `IdVec<SliceId, (SliceInfo, Vec<T>)>`.
///
/// The notional `Vec<T>` values are actually stored in a single contiguous buffer to reduce the
/// number of heap allocations.  Because these values are all different slices of the same
/// underlying buffer, we refer to each tuple `(SliceInfo, Vec<T>)` as a "slice" for the purposes of
/// this data structure's API.
///
/// The `SliceInfo` parameter should be regarded as optional, and for some purposes it is perfectly
/// fine (stylistically speaking) to set `SliceInfo = ()`.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FlatSlices<SliceId: Id, SliceInfo, T> {
    flat_data: Vec<T>,
    slices: IdVec<SliceId, SliceData<SliceInfo>>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Slice<'a, SliceInfo, T> {
    pub info: &'a SliceInfo,
    pub items: &'a [T],
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SliceMut<'a, SliceInfo, T> {
    pub info: &'a mut SliceInfo,
    pub items: &'a mut [T],
}

impl<SliceId: Id, SliceInfo, T> Default for FlatSlices<SliceId, SliceInfo, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<SliceId: Id, SliceInfo, T> FlatSlices<SliceId, SliceInfo, T> {
    pub fn new() -> Self {
        Self {
            flat_data: Vec::new(),
            slices: IdVec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.slices.len()
    }

    pub fn count(&self) -> Count<SliceId> {
        self.slices.count()
    }

    pub fn push_slice(&mut self, info: SliceInfo, slice: &[T]) -> SliceId
    where
        T: Clone,
    {
        self.flat_data.extend_from_slice(slice);
        self.slices.push(SliceData {
            info,
            slice_end_idx: self.flat_data.len(),
        })
    }

    fn data_range(&self, idx: SliceId) -> (usize, usize) {
        let start = match id_type::decrement(idx.clone()) {
            None => 0,
            Some(prev_idx) => self.slices[prev_idx].slice_end_idx,
        };
        let end = self.slices[idx].slice_end_idx;
        (start, end)
    }

    pub fn get<I: Borrow<SliceId>>(&self, idx: I) -> Slice<SliceInfo, T> {
        let (start, end) = self.data_range(idx.borrow().clone());
        Slice {
            info: &self.slices[idx].info,
            items: &self.flat_data[start..end],
        }
    }

    pub fn get_mut<I: Borrow<SliceId>>(&mut self, idx: I) -> SliceMut<SliceInfo, T> {
        let (start, end) = self.data_range(idx.borrow().clone());
        SliceMut {
            info: &mut self.slices[idx].info,
            items: &mut self.flat_data[start..end],
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (SliceId, Slice<SliceInfo, T>)> {
        self.count().iter().map(move |i| (i.clone(), self.get(i)))
    }
}

impl<SliceId: Id + fmt::Debug, SliceInfo: fmt::Debug, T: fmt::Debug> fmt::Debug
    for FlatSlices<SliceId, SliceInfo, T>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}
