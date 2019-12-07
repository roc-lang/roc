use std::fmt::{self, Display, Formatter};

/// An unqualified identifier, possibly capitalized.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct UnqualifiedIdent<'a>(&'a str);

impl<'a> Into<&'a str> for UnqualifiedIdent<'a> {
    fn into(self) -> &'a str {
        self.0
    }
}

/// Rather than displaying as this:
///
/// UnqualifiedIdent("foo")
///
/// ...instead display as this:
///
/// 'foo'
impl<'a> fmt::Debug for UnqualifiedIdent<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}'", self.0)
    }
}

impl<'a> UnqualifiedIdent<'a> {
    pub fn new(name: &'a str) -> Self {
        // Unqualified idents must always start with a lowercase character.
        debug_assert!(name
            .chars()
            .next()
            .expect("UnqualifiedIdent was empty")
            .is_alphabetic());

        UnqualifiedIdent(name)
    }

    pub fn as_str(&'a self) -> &'a str {
        self.0
    }
}

/// An identifier, possibly fully-qualified with a module name
/// e.g. (Http.Request from http)
/// Parameterized on a phantom marker for whether it has been canonicalized
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Ident {
    Unqualified(Box<str>),
    Qualified(Box<str>, Box<str>),
}

impl Ident {
    pub fn new(module_parts: &[&str], name: &str) -> Self {
        if module_parts.is_empty() {
            Ident::Unqualified(name.into())
        } else {
            Ident::Qualified(module_parts.to_vec().join(".").into(), name.into())
        }
    }
    pub fn is_qualified(&self) -> bool {
        match self {
            Ident::Unqualified(_) => false,
            Ident::Qualified(_, _) => true,
        }
    }

    pub fn name(self) -> Box<str> {
        match self {
            Ident::Unqualified(name) => name,
            Ident::Qualified(_, name) => name,
        }
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Ident::Unqualified(name) => write!(f, "{}", name),
            Ident::Qualified(path, name) => write!(f, "{}.{}", path, name),
        }
    }
}
