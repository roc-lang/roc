macro_rules! bytes_id {
    (
        // Capturing attributes allows us to capture doc comments
        $(#[$annot_borrowed:meta])* $borrowed_vis:vis $borrowed:ident;
        $(#[$annot_owned:meta])* $owned_vis:vis $owned:ident;
    ) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        $(#[$annot_borrowed])*
        $borrowed_vis struct $borrowed<'a>($borrowed_vis &'a [u8]);

        // The stack-allocated array portion of a `SmallVec` shares a union with two `usize`s, so on
        // 64-bit platforms we can make the array up to 16 bytes long with no space penalty.
        #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        $(#[$annot_owned])*
        $owned_vis struct $owned($owned_vis ::smallvec::SmallVec<[u8; 16]>);

        impl $owned {
            fn borrowed<'a>(&'a self) -> $borrowed<'a> {
                $borrowed(&self.0)
            }
        }

        impl<'a> ::std::convert::From<$borrowed<'a>> for $owned {
            fn from(borrowed: $borrowed<'a>) -> Self {
                $owned(::smallvec::SmallVec::from_slice(&borrowed.0))
            }
        }
    }
}
