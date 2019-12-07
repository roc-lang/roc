use crate::ident::UnqualifiedIdent;
use crate::module::ModuleName;
use std::fmt;

/// A globally unique identifier, used for both vars and tags.
/// It will be used directly in code gen.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(Box<str>);

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

    pub fn from_module<'a>(
        module_name: &'a ModuleName<'a>,
        ident: &'a UnqualifiedIdent<'a>,
    ) -> Symbol {
        Symbol(format!("{}.{}", module_name.as_str(), ident.as_str()).into())
    }

    pub fn from_qualified_ident(module_name: Box<str>, ident: Box<str>) -> Symbol {
        Symbol(format!("{}.{}", module_name, ident).into())
    }

    pub fn into_boxed_str(self) -> Box<str> {
        self.0
    }
}

impl Into<Box<str>> for Symbol {
    fn into(self) -> Box<str> {
        self.0
    }
}
