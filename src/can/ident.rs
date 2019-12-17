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
    pub fn into_str(self) -> Box<str> {
        self.0
    }
}

impl Into<Box<str>> for Lowercase {
    fn into(self) -> Box<str> {
        self.0
    }
}

impl<'a> From<&'a str> for Lowercase {
    fn from(string: &'a str) -> Self {
        Self(string.into())
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

impl fmt::Display for Lowercase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
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

impl fmt::Display for Uppercase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}
