use std::fmt::{self, Display, Formatter};

/// A variant name, possibly fully-qualified with a module name
/// e.g. (Result.Ok)
/// Parameterized on a phantom marker for whether it has been canonicalized
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum VariantName {
    Unqualified(String),
    Qualified(String, String),
}

/// An identifier, possibly fully-qualified with a module name
/// e.g. (Http.Request from http)
/// Parameterized on a phantom marker for whether it has been canonicalized
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Ident {
    Unqualified(String),
    Qualified(String, String),
}

impl Ident {
    pub fn new(module_parts: &[&str], name: &str) -> Self {
        if module_parts.is_empty() {
            Ident::Unqualified(name.to_string())
        } else {
            Ident::Qualified(module_parts.to_vec().join("."), name.to_string())
        }
    }
    pub fn is_qualified(&self) -> bool {
        match &self {
            &Ident::Unqualified(_) => false,
            &Ident::Qualified(_, _) => true,
        }
    }

    pub fn name(self) -> String {
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

impl Display for VariantName {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            VariantName::Unqualified(name) => write!(f, "{}", name),
            VariantName::Qualified(path, name) => write!(f, "{}.{}", path, name),
        }
    }
}
