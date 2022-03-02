#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Index<T> {
    index: u32,
    _marker: std::marker::PhantomData<T>,
}

impl<T> Index<T> {
    pub const fn new(index: u32) -> Self {
        Self {
            index,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn push_new(vector: &mut Vec<T>, value: T) -> Index<T> {
        let index = Self::new(vector.len() as _);

        vector.push(value);

        index
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Slice<T> {
    start: u32,
    length: u16,
    _marker: std::marker::PhantomData<T>,
}

impl<T> Default for Slice<T> {
    fn default() -> Self {
        Self {
            start: Default::default(),
            length: Default::default(),
            _marker: Default::default(),
        }
    }
}

impl<T> Slice<T> {
    pub const fn new(start: u32, length: u16) -> Self {
        Self {
            start,
            length,
            _marker: std::marker::PhantomData,
        }
    }

    pub const fn len(&self) -> usize {
        self.length as _
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
