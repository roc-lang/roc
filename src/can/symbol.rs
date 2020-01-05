use crate::module::ModuleName;
use inlinable_string::InlinableString;
use std::fmt;

/// A globally unique identifier, used for both vars and tags.
/// It will be used directly in code gen.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(InlinableString);

/// Rather than displaying as this:
///
/// Symbol("Foo.bar")
///
/// ...instead display as this:
///
/// 'Foo.bar'
impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}'", self.0)
    }
}

impl Into<InlinableString> for Symbol {
    fn into(self) -> InlinableString {
        self.0
    }
}

impl From<InlinableString> for Symbol {
    fn from(string: InlinableString) -> Self {
        Symbol(string)
    }
}

impl From<&str> for Symbol {
    fn from(string: &str) -> Self {
        Symbol(string.into())
    }
}

impl Symbol {
    pub fn new(prefix: &str, name: &str) -> Symbol {
        Symbol(format!("{}{}", prefix, name).into())
    }

    pub fn from_parts(module_parts: &[&str], name: &str) -> Symbol {
        Symbol(if module_parts.is_empty() {
            name.into()
        } else {
            format!("{}.{}", module_parts.join("."), name).into()
        })
    }

    pub fn from_global_tag(tag_name: &str) -> Symbol {
        Symbol(tag_name.into())
    }

    pub fn from_private_tag(home: &str, tag_name: &str) -> Symbol {
        Symbol(format!("{}.{}", home, tag_name).into())
    }

    pub fn from_module<'a>(module_name: &'a ModuleName<'a>, ident: &'a &'a str) -> Symbol {
        Symbol(format!("{}.{}", module_name.as_str(), ident).into())
    }

    pub fn from_qualified_ident(module_name: Box<str>, ident: Box<str>) -> Symbol {
        Symbol(format!("{}.{}", module_name, ident).into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}
