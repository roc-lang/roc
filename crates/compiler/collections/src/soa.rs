use std::usize;

pub struct Index<T> {
    index: u32,
    _marker: std::marker::PhantomData<T>,
}

impl<T> Eq for Index<T> {}

impl<T> PartialEq for Index<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> std::hash::Hash for Index<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

impl<T> Clone for Index<T> {
    fn clone(&self) -> Self {
        Self {
            index: self.index,
            _marker: self._marker,
        }
    }
}

impl<T> Copy for Index<T> {}

impl<T> std::fmt::Debug for Index<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Index({})", self.index)
    }
}

impl<T> Index<T> {
    pub const fn new(index: u32) -> Self {
        Self {
            index,
            _marker: std::marker::PhantomData,
        }
    }

    pub const fn index(self) -> usize {
        self.index as usize
    }

    pub fn push_new(vector: &mut Vec<T>, value: T) -> Index<T> {
        let index = Self::new(vector.len() as _);

        vector.push(value);

        index
    }

    pub const fn as_slice(self) -> Slice<T> {
        Slice::new(self.index, 1)
    }
}

#[derive(PartialEq, Eq)]
pub struct Slice<T> {
    length: u16,
    start: u32,
    _marker: std::marker::PhantomData<T>,
}

impl<T> Clone for Slice<T> {
    fn clone(&self) -> Self {
        Self {
            start: self.start,
            length: self.length,
            _marker: self._marker,
        }
    }
}

impl<T> Copy for Slice<T> {}

impl<T> std::fmt::Debug for Slice<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Slice(start = {}, length = {})", self.start, self.length)
    }
}

impl<T> Default for Slice<T> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<T> Slice<T> {
    pub const fn empty() -> Self {
        Self::new(0, 0)
    }

    pub const fn new(start: u32, length: u16) -> Self {
        Self {
            start,
            length,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn extend_new<I>(vector: &mut Vec<T>, values: I) -> Slice<T>
    where
        I: IntoIterator<Item = T>,
    {
        let start = vector.len() as u32;

        vector.extend(values);

        let end = vector.len() as u32;

        Self::new(start, (end - start) as u16)
    }

    pub const fn len(&self) -> usize {
        self.length as _
    }

    pub const fn start(&self) -> usize {
        self.start as _
    }

    pub const fn is_empty(&self) -> bool {
        self.length == 0
    }

    pub const fn indices(&self) -> std::ops::Range<usize> {
        self.start as usize..(self.start as usize + self.length as usize)
    }

    pub fn into_iter(&self) -> impl Iterator<Item = Index<T>> {
        self.indices().map(|i| Index::new(i as _))
    }
}

#[derive(PartialEq, Eq)]
pub struct EitherIndex<T, U> {
    index: u32,
    _marker: std::marker::PhantomData<(T, U)>,
}

impl<T, U> Clone for EitherIndex<T, U> {
    fn clone(&self) -> Self {
        Self {
            index: self.index,
            _marker: self._marker,
        }
    }
}

impl<T, U> Copy for EitherIndex<T, U> {}

impl<T, U> std::fmt::Debug for EitherIndex<T, U> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Index({})", self.index)
    }
}

impl<T, U> EitherIndex<T, U> {
    const MASK: u32 = 1 << 31;

    pub const fn from_left(input: Index<T>) -> Self {
        assert!(input.index & Self::MASK == 0);

        Self {
            index: input.index,
            _marker: std::marker::PhantomData,
        }
    }

    pub const fn from_right(input: Index<U>) -> Self {
        assert!(input.index & Self::MASK == 0);

        Self {
            index: input.index | Self::MASK,
            _marker: std::marker::PhantomData,
        }
    }

    pub const fn split(self) -> Result<Index<T>, Index<U>> {
        if self.index & Self::MASK == 0 {
            Ok(Index::new(self.index))
        } else {
            Err(Index::new(self.index ^ Self::MASK))
        }
    }
}
