use std::{
    borrow::Borrow,
    io::{self, Write},
};

use roc_collections::{MutMap, VecMap};

pub fn serialize_slice<T: Copy>(
    slice: &[T],
    writer: &mut impl Write,
    written: usize,
) -> io::Result<usize> {
    let alignment = std::mem::align_of::<T>();
    let padding_bytes = next_multiple_of(written, alignment) - written;

    for _ in 0..padding_bytes {
        writer.write_all(&[0])?;
    }

    let bytes_slice = unsafe { slice_as_bytes(slice) };
    writer.write_all(bytes_slice)?;

    Ok(written + padding_bytes + bytes_slice.len())
}

pub fn deserialize_slice<T: Copy>(bytes: &[u8], length: usize, mut offset: usize) -> (&[T], usize) {
    let alignment = std::mem::align_of::<T>();
    let size = std::mem::size_of::<T>();

    offset = next_multiple_of(offset, alignment);

    let byte_length = length * size;
    let byte_slice = &bytes[offset..][..byte_length];

    let slice = unsafe { std::slice::from_raw_parts(byte_slice.as_ptr() as *const T, length) };

    (slice, offset + byte_length)
}

// We Copy the data into a new, aligned Vec<T>
// Prevents panic in debug: `unsafe precondition(s) violated: slice::from_raw_parts requires the pointer to be aligned...`
pub fn deserialize_slice_safe<T: Copy>(bytes: &[u8], length: usize, mut offset: usize) -> (Vec<T>, usize) {
    let alignment = std::mem::align_of::<T>();
    let size = std::mem::size_of::<T>();

    // align the offset to avoid reading partial data
    offset = next_multiple_of(offset, alignment);

    let byte_length = length * size;
    let byte_slice = &bytes[offset..offset + byte_length];

    // A Vec<T> is guaranteed to have its backing buffer aligned correctly for T.
    let mut result = Vec::with_capacity(length);

    unsafe {
        // Get a pointer to the start of the Vec's buffer. This pointer is aligned.
        let dest_ptr = result.as_mut_ptr();

        // Get a pointer to the source data. This pointer may be unaligned.
        let src_ptr = byte_slice.as_ptr();

        // Copy the bytes from the (potentially unaligned) source to the
        // (guaranteed aligned) destination. `copy_nonoverlapping` handles
        // unaligned sources correctly.
        std::ptr::copy_nonoverlapping(src_ptr, dest_ptr as *mut u8, byte_length);

        // Tell the Vec that we have initialized `length` elements.
        // This is important, otherwise, the Vec thinks it's still empty.
        result.set_len(length);
    }

    (result, offset + byte_length)
}

pub fn deserialize_vec<T: Clone + Copy>(
    bytes: &[u8],
    length: usize,
    offset: usize,
) -> (Vec<T>, usize) {
    let (slice, offset) = deserialize_slice(bytes, length, offset);
    (slice.to_vec(), offset)
}

#[derive(Copy, Clone)]
struct VecSlice<T> {
    pub start: u32,
    pub length: u16,
    _marker: std::marker::PhantomData<T>,
}

impl<T> VecSlice<T> {
    const fn len(&self) -> usize {
        self.length as usize
    }

    fn indices(&self) -> std::ops::Range<usize> {
        self.start as usize..(self.start as usize + self.length as usize)
    }

    fn extend_new(vec: &mut Vec<T>, it: impl IntoIterator<Item = T>) -> Self {
        let start = vec.len();

        vec.extend(it);

        let end = vec.len();

        Self {
            start: start as u32,
            length: (end - start) as u16,
            _marker: Default::default(),
        }
    }
}

pub fn serialize_slice_of_slices<'a, T, U>(
    slice_of_slices: &[U],
    writer: &mut impl Write,
    written: usize,
) -> io::Result<usize>
where
    T: 'a + Copy,
    U: 'a + Borrow<[T]> + Sized,
{
    let mut item_buf: Vec<T> = Vec::new();
    let mut serialized_slices: Vec<VecSlice<T>> = Vec::new();

    for slice in slice_of_slices {
        let slice = VecSlice::extend_new(&mut item_buf, slice.borrow().iter().copied());
        serialized_slices.push(slice);
    }

    let written = serialize_slice(&serialized_slices, writer, written)?;
    serialize_slice(&item_buf, writer, written)
}

pub fn deserialize_slice_of_slices<T, Container>(
    bytes: &[u8],
    length: usize,
    offset: usize,
) -> (Vec<Container>, usize)
where
    T: Copy,
    Container: From<Vec<T>>,
{
    let (serialized_slices, offset) = deserialize_slice::<VecSlice<T>>(bytes, length, offset);

    let (vars_slice, offset) = {
        let total_items = serialized_slices.iter().map(|s| s.len()).sum();
        deserialize_slice::<T>(bytes, total_items, offset)
    };

    let mut slice_of_slices = Vec::with_capacity(length);
    for slice in serialized_slices {
        let deserialized_slice = &vars_slice[slice.indices()];
        slice_of_slices.push(deserialized_slice.to_vec().into())
    }

    (slice_of_slices, offset)
}

pub fn serialize_map<K: Clone, V: Clone, W: Write>(
    map: &MutMap<K, V>,
    ser_keys: fn(&[K], &mut W, usize) -> io::Result<usize>,
    ser_values: fn(&[V], &mut W, usize) -> io::Result<usize>,
    writer: &mut W,
    written: usize,
) -> io::Result<usize> {
    let keys = map.keys().cloned().collect::<Vec<_>>();
    let values = map.values().cloned().collect::<Vec<_>>();

    let written = ser_keys(keys.as_slice(), writer, written)?;
    let written = ser_values(values.as_slice(), writer, written)?;

    Ok(written)
}

#[allow(clippy::type_complexity)]
pub fn deserialize_map<K, V>(
    bytes: &[u8],
    deser_keys: fn(&[u8], usize, usize) -> (Vec<K>, usize),
    deser_values: fn(&[u8], usize, usize) -> (Vec<V>, usize),
    length: usize,
    offset: usize,
) -> (MutMap<K, V>, usize)
where
    K: Clone + std::hash::Hash + Eq,
    V: Clone,
{
    let (keys, offset) = deser_keys(bytes, length, offset);
    let (values, offset) = deser_values(bytes, length, offset);

    (
        MutMap::from_iter((keys.iter().cloned()).zip(values.iter().cloned())),
        offset,
    )
}

pub fn serialize_vec_map<K, V, W: Write>(
    map: &VecMap<K, V>,
    ser_keys: fn(&[K], &mut W, usize) -> io::Result<usize>,
    ser_values: fn(&[V], &mut W, usize) -> io::Result<usize>,
    writer: &mut W,
    written: usize,
) -> io::Result<usize> {
    let (keys, values) = map.unzip_slices();

    let written = ser_keys(keys, writer, written)?;
    let written = ser_values(values, writer, written)?;

    Ok(written)
}

#[allow(clippy::type_complexity)]
pub fn deserialize_vec_map<K, V>(
    bytes: &[u8],
    deser_keys: fn(&[u8], usize, usize) -> (Vec<K>, usize),
    deser_values: fn(&[u8], usize, usize) -> (Vec<V>, usize),
    length: usize,
    offset: usize,
) -> (VecMap<K, V>, usize)
where
    K: PartialEq,
{
    let (keys, offset) = deser_keys(bytes, length, offset);
    let (values, offset) = deser_values(bytes, length, offset);

    (unsafe { VecMap::zip(keys, values) }, offset)
}

unsafe fn slice_as_bytes<T>(slice: &[T]) -> &[u8] {
    let ptr = slice.as_ptr();
    let byte_length = std::mem::size_of_val(slice);

    std::slice::from_raw_parts(ptr as *const u8, byte_length)
}

// TODO check on https://github.com/rust-lang/rust/issues/88581 some time in the future
pub const fn next_multiple_of(lhs: usize, rhs: usize) -> usize {
    match lhs % rhs {
        0 => lhs,
        r => lhs + (rhs - r),
    }
}

#[cfg(test)]
mod test {
    use roc_collections::{MutMap, VecMap, VecSet};

    use super::{
        deserialize_map, deserialize_slice, deserialize_slice_of_slices, deserialize_vec,
        deserialize_vec_map, serialize_map, serialize_slice, serialize_slice_of_slices,
        serialize_vec_map,
    };

    #[test]
    fn serde_empty_slice() {
        let mut buf = vec![];
        serialize_slice(&[] as &[u8], &mut buf, 0).unwrap();
        assert!(buf.is_empty());

        let (out, size) = deserialize_slice::<u8>(&buf, 0, 0);
        assert!(out.is_empty());
        assert_eq!(size, 0);
    }

    #[test]
    fn serde_slice() {
        let input: &[u64] = &[15u64, 23, 37, 89];

        let mut buf = vec![];
        serialize_slice(input, &mut buf, 0).unwrap();
        assert!(!buf.is_empty());

        let (out, size) = deserialize_slice::<u64>(&buf, 4, 0);
        assert_eq!(out, input);
        assert_eq!(size, 4 * 8);
    }

    #[test]
    fn serde_vec() {
        let input: &[u64] = &[15u64, 23, 37, 89];

        let mut buf = vec![];
        serialize_slice(input, &mut buf, 0).unwrap();
        assert!(!buf.is_empty());

        let (out, size) = deserialize_vec::<u64>(&buf, 4, 0);
        assert_eq!(out, input);
        assert_eq!(size, buf.len());
    }

    #[test]
    fn serde_empty_slice_of_slices() {
        let input: &[&[u64]] = &[];

        let mut buf = vec![];
        serialize_slice_of_slices(input, &mut buf, 0).unwrap();
        assert!(buf.is_empty());

        let (out, size) = deserialize_slice_of_slices::<u64, Vec<_>>(&buf, 0, 0);
        assert!(out.is_empty());
        assert_eq!(size, 0);
    }

    #[test]
    fn serde_slice_of_slices() {
        let input: &[&[u64]] = &[&[15, 23, 47], &[61, 72], &[85, 91]];

        let mut buf = vec![];
        serialize_slice_of_slices(input, &mut buf, 0).unwrap();
        assert!(!buf.is_empty());

        let (out, size) = deserialize_slice_of_slices::<u64, Vec<_>>(&buf, 3, 0);
        assert_eq!(out, input);
        assert_eq!(size, buf.len());
    }

    #[test]
    fn serde_slice_of_slices_into_vec_set() {
        let input: &[&[u64]] = &[&[15, 23, 47], &[61, 72], &[85, 91]];

        let mut buf = vec![];
        serialize_slice_of_slices(input, &mut buf, 0).unwrap();
        assert!(!buf.is_empty());

        let (out, size) = deserialize_slice_of_slices::<u64, VecSet<_>>(&buf, 3, 0);
        assert_eq!(size, buf.len());

        let mut out = out.into_iter();
        assert_eq!(out.next().unwrap().into_vec(), &[15, 23, 47]);
        assert_eq!(out.next().unwrap().into_vec(), &[61, 72]);
        assert_eq!(out.next().unwrap().into_vec(), &[85, 91]);
        assert!(out.next().is_none());
    }

    #[test]
    fn serde_empty_map() {
        let input: MutMap<u64, u64> = Default::default();

        let mut buf = vec![];
        serialize_map(&input, serialize_slice, serialize_slice, &mut buf, 0).unwrap();
        assert!(buf.is_empty());

        let (out, size) = deserialize_map::<u64, u64>(&buf, deserialize_vec, deserialize_vec, 0, 0);
        assert!(out.is_empty());
        assert_eq!(size, 0);
    }

    #[test]
    fn serde_map() {
        let mut input: MutMap<u64, Vec<u64>> = Default::default();
        input.insert(51, vec![15, 23, 37]);
        input.insert(39, vec![17, 91, 43]);
        input.insert(82, vec![90, 35, 76]);

        let mut buf = vec![];
        serialize_map(
            &input,
            serialize_slice,
            serialize_slice_of_slices,
            &mut buf,
            0,
        )
        .unwrap();
        assert!(!buf.is_empty());

        let (out, size) = deserialize_map::<u64, Vec<u64>>(
            &buf,
            deserialize_vec,
            deserialize_slice_of_slices,
            3,
            0,
        );
        assert_eq!(out, input);
        assert_eq!(size, buf.len());
    }

    #[test]
    fn serde_empty_vec_map() {
        let input: VecMap<u64, u64> = Default::default();

        let mut buf = vec![];
        serialize_vec_map(&input, serialize_slice, serialize_slice, &mut buf, 0).unwrap();
        assert!(buf.is_empty());

        let (out, size) =
            deserialize_vec_map::<u64, u64>(&buf, deserialize_vec, deserialize_vec, 0, 0);
        assert!(out.is_empty());
        assert_eq!(size, 0);
    }

    #[test]
    fn serde_vec_map() {
        let mut input: VecMap<u64, Vec<u64>> = Default::default();
        input.insert(51, vec![15, 23, 37]);
        input.insert(39, vec![17, 91, 43]);
        input.insert(82, vec![90, 35, 76]);

        let mut buf = vec![];
        serialize_vec_map(
            &input,
            serialize_slice,
            serialize_slice_of_slices,
            &mut buf,
            0,
        )
        .unwrap();
        assert!(!buf.is_empty());

        let (out, size) = deserialize_vec_map::<u64, Vec<u64>>(
            &buf,
            deserialize_vec,
            deserialize_slice_of_slices,
            3,
            0,
        );
        assert_eq!(out.unzip_slices(), input.unzip_slices());
        assert_eq!(size, buf.len());
    }
}
