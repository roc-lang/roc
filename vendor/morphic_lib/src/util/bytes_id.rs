macro_rules! bytes_id {
    (
        // Capturing attributes allows us to capture doc comments
        $(#[$annot_borrowed:meta])* $borrowed_vis:vis $borrowed:ident;
        $(#[$annot_owned:meta])* $owned_vis:vis $owned:ident;
    ) => {
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        $(#[$annot_borrowed])*
        $borrowed_vis struct $borrowed<'a>($borrowed_vis &'a [u8]);

        // On 64-bit platforms we can store up to `23` bytes inline in a `SmallVec` with no space
        // penalty.
        #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        $(#[$annot_owned])*
        $owned_vis struct $owned($owned_vis ::smallvec::SmallVec<[u8; 23]>);

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

        impl<'a> ::std::fmt::Debug for $borrowed<'a> {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                // TODO: Consolidate this with render_api_ir.rs
                write!(f, "{}(\"", stringify!($borrowed))?;
                for &byte in self.0 {
                    write!(f, "{}", ::std::ascii::escape_default(byte))?;
                }
                write!(f, "\")")?;
                Ok(())
            }
        }

        impl ::std::fmt::Debug for $owned {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                // We intentionally use the name of $borrowed to render values of type $owned,
                // because only the borrowed version of each bytestring-based identifier type are
                // exposed/documented in the public API, and we use this debug formatting logic to
                // render public-facing error messages.
                write!(f, "{:?}", self.borrowed())
            }
        }
    }
}
