use core::{
    fmt::{self, Formatter},
    marker::PhantomData,
};

use crate::soa_index::Index;

#[derive(PartialEq, Eq)]
pub struct EitherIndex<T, U> {
    index: u32,
    _marker: PhantomData<(T, U)>,
}

impl<T, U> Clone for EitherIndex<T, U> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T, U> Copy for EitherIndex<T, U> {}

impl<T, U> fmt::Debug for EitherIndex<T, U> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "EitherIndex({})", self.index)
    }
}

impl<T, U> EitherIndex<T, U> {
    const MASK: u32 = 1 << 31;

    pub const fn from_left(input: Index<T>) -> Self {
        assert!(input.index & Self::MASK == 0);

        Self {
            index: input.index,
            _marker: PhantomData,
        }
    }

    pub const fn from_right(input: Index<U>) -> Self {
        assert!(input.index & Self::MASK == 0);

        Self {
            index: input.index | Self::MASK,
            _marker: PhantomData,
        }
    }

    pub const fn split(self) -> Result<Index<T>, Index<U>> {
        if self.index & Self::MASK == 0 {
            Ok(Index {
                index: self.index,
                _marker: PhantomData,
            })
        } else {
            Err(Index {
                index: self.index ^ Self::MASK,
                _marker: PhantomData,
            })
        }
    }

    pub fn decrement_index(&mut self) {
        self.index = self.index.saturating_sub(1);
    }
}
