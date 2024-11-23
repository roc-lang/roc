/// Push to a std::vec::Vec<T> and then return an index to that new element's
/// position in the Vec.
///
/// This is not a method on soa::Index because the `soa` is `no_std` by design.
/// Long-term, our arena-allocated vectors should have this as a method!
pub fn index_push_new<T>(vector: &mut Vec<T>, value: T) -> soa::Index<T> {
    let index = soa::Index::new(vector.len() as u32);

    vector.push(value);

    index
}

/// Extend a std::vec::Vec<T> and then return a slice to the new elements'
/// positions in the Vec.
///
/// This is not a method on soa::Slice because the `soa` is `no_std` by design.
/// Long-term, our arena-allocated vectors should have this as a method!
pub fn slice_extend_new<T>(
    vector: &mut Vec<T>,
    values: impl IntoIterator<Item = T>,
) -> soa::Slice<T> {
    let start = vector.len() as u32;

    vector.extend(values);

    let end = vector.len() as u32;

    soa::Slice::new(start, (end - start) as u16)
}
