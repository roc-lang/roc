use core::{
    fmt::{self, Formatter},
    marker::PhantomData,
    ptr::NonNull,
};

use crate::soa_index::Index;

#[derive(PartialEq, Eq)]
pub struct EitherIndex<T, U> {
    index: u32,
    _marker: PhantomData<(T, U)>,

    #[cfg(debug_assertions)]
    pub(crate) array_start: Result<Option<NonNull<T>>, Option<NonNull<U>>>,
}

impl<T, U> Clone for EitherIndex<T, U> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T, U> Copy for EitherIndex<T, U> {}

impl<T, U> fmt::Debug for EitherIndex<T, U> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Index({})", self.index)
    }
}

impl<T, U> EitherIndex<T, U> {
    const MASK: u32 = 1 << 31;

    pub const fn from_left(input: Index<T>) -> Self {
        assert!(input.index & Self::MASK == 0);

        Self {
            index: input.index,
            _marker: PhantomData,

            #[cfg(debug_assertions)]
            array_start: Ok(input.array_start),
        }
    }

    pub const fn from_right(input: Index<U>) -> Self {
        assert!(input.index & Self::MASK == 0);

        Self {
            index: input.index | Self::MASK,
            _marker: std::marker::PhantomData,

            #[cfg(debug_assertions)]
            array_start: Err(input.array_start),
        }
    }

    pub const fn split(self) -> Result<Index<T>, Index<U>> {
        if self.index & Self::MASK == 0 {
            Ok(Index {
                index: self.index,
                _marker: PhantomData,

                #[cfg(debug_assertions)]
                array_start: {
                    match self.array_start {
                        Ok(array) => array,
                        Err(_) => {
                            panic!("Tried to split an EitherIndex that was created with from_right, but ended up with a mask that indicates it was created with from_left");
                        }
                    }
                },
            })
        } else {
            Err(Index {
                index: self.index ^ Self::MASK,
                _marker: PhantomData,

                #[cfg(debug_assertions)]
                array_start: {
                    match self.array_start {
                        Err(array) => array,
                        Ok(_) => {
                            panic!("Tried to split an EitherIndex that was created with from_left, but ended up with a mask that indicates it was created with from_right");
                        }
                    }
                },
            })
        }
    }

    pub fn decrement_index(&mut self) {
        self.index = self.index.saturating_sub(1);
    }
}
