use std::fmt::{self, Display, Formatter};

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
        debug_assert!(!name.is_empty());

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

    pub fn first_char(&self) -> char {
        let opt_first = match self {
            Ident::Unqualified(name) => name.chars().next(),
            Ident::Qualified(_, name) => name.chars().next(),
        };

        opt_first
            .unwrap_or_else(|| panic!("Attempted to get the first character of an empty Ident"))
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
