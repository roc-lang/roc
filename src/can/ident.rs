use crate::ident::UnqualifiedIdent;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleName(Box<str>);

/// An uncapitalized identifier, such as a field name or local variable
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Lowercase(Box<str>);

/// A capitalized identifier, such as a tag name or module name
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Uppercase(Box<str>);

impl Lowercase {
    pub fn from_unqualified_ident(ident: &UnqualifiedIdent<'_>) -> Self {
        Self(ident.as_str().into())
    }
}

impl Into<Box<str>> for Lowercase {
    fn into(self) -> Box<str> {
        self.0
    }
}

/// Rather than displaying as this:
///
/// Lowercase("foo")
///
/// ...instead display as this:
///
/// 'foo'
impl fmt::Debug for Lowercase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}'", self.0)
    }
}

/// Rather than displaying as this:
///
/// Uppercase("Foo")
///
/// ...instead display as this:
///
/// 'Foo'
impl fmt::Debug for Uppercase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}'", self.0)
    }
}
