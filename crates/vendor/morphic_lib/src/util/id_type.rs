use std::cmp::Ordering;

/// Abstracts over types which wrap numerical indices.
pub trait Id: Clone {
    /// The maximum `usize` representable by this type.
    const MAX_USIZE: usize;

    /// Convert from `usize` to `Self`, potentially silently wrapping if the `usize` is out of
    /// range.
    ///
    /// In debug builds, we may still perform a check.
    fn from_index_unchecked(idx: usize) -> Self;

    /// Check that a `usize` is in range for `Self`, panicking with an informative error message if
    /// not.
    fn assert_in_range(idx: usize);

    /// Convert from `usize` to `Self`, panicking if the `usize` is out of range.
    fn from_index_or_panic(idx: usize) -> Self {
        Self::assert_in_range(idx);
        Self::from_index_unchecked(idx)
    }

    /// Convert from `Self` to `usize`.  This should never fail.
    fn to_index(&self) -> usize;
}

macro_rules! id_type {
    ($(#[$annot:meta])* $id_vis:vis $name:ident($wrapped:ty); ) => {
        $(#[$annot])*
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        $id_vis struct $name($id_vis $wrapped);

        impl $crate::util::id_type::Id for $name {
            const MAX_USIZE: usize = <$wrapped>::MAX as usize;

            fn from_index_unchecked(idx: usize) -> Self {
                if cfg!(debug_assertions) {
                    <Self as $crate::util::id_type::Id>::assert_in_range(idx);
                }
                $name(idx as $wrapped)
            }

            fn assert_in_range(idx: usize) {
                if idx > Self::MAX_USIZE {
                    panic!(
                        "index {} overflows range of type '{}' (backed by {})",
                        idx,
                        stringify!(name),
                        stringify!($wrapped),
                    );
                }
            }

            fn to_index(&self) -> usize {
                self.0 as usize
            }
        }

        // Custom Debug impl avoid multi-line formatting when formatted with {:#?}
        impl ::std::fmt::Debug for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                write!(f, "{}({})", stringify!($name), self.0)
            }
        }

        impl ::std::cmp::PartialEq<$crate::util::id_type::Count<$name>> for $name {
            fn eq(&self, other: &$crate::util::id_type::Count<$name>) -> bool {
                self.eq(&other.0)
            }
        }

        impl ::std::cmp::PartialOrd<$crate::util::id_type::Count<$name>> for $name {
            fn partial_cmp(
                &self,
                other: &$crate::util::id_type::Count<$name>,
            ) -> Option<::std::cmp::Ordering> {
                self.partial_cmp(&other.0)
            }
        }
    }
}

/// `Count(x)` represents the range of ids `0..x`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Count<T>(pub T);

impl<T: PartialEq> PartialEq<T> for Count<T> {
    fn eq(&self, other: &T) -> bool {
        self.0.eq(other)
    }
}

impl<T: PartialOrd> PartialOrd<T> for Count<T> {
    fn partial_cmp(&self, other: &T) -> Option<Ordering> {
        self.0.partial_cmp(other)
    }
}

impl<T: Id> Count<T> {
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = T> {
        (0..self.0.to_index()).map(T::from_index_unchecked)
    }
}

pub fn decrement<T: Id>(id: T) -> Option<T> {
    match id.to_index() {
        0 => None,
        index => Some(T::from_index_unchecked(index - 1)),
    }
}
