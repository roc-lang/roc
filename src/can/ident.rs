use crate::module::symbol::{Interns, ModuleId, Symbol};
use inlinable_string::InlinableString;
use std::fmt;

/// This could be uppercase or lowercase, qualified or unqualified.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Ident(InlinableString);

impl Ident {
    pub fn as_inline_str(&self) -> &InlinableString {
        &self.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleName(InlinableString);

/// An uncapitalized identifier, such as a field name or local variable
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Lowercase(InlinableString);

/// A capitalized identifier, such as a tag name or module name
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Uppercase(InlinableString);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TagName {
    /// Global tags have no module, but tend to be short strings (since they're
    /// never qualified), so we store them as inlinable strings.
    ///
    /// This is allows canonicalization to happen in parallel without locks.
    /// If global tags had a Symbol representation, then each module would have to
    /// deal with contention on a global mutex around translating global tag strings
    /// into integers. (Record field labels work the same way, for the same reason.)
    Global(Uppercase),

    /// Private tags are associated with a specific module, and as such use a
    /// Symbol just like all other module-specific identifiers.
    Private(Symbol),
}

impl TagName {
    pub fn to_string(self, interns: &Interns, home: ModuleId) -> InlinableString {
        match self {
            TagName::Global(uppercase) => uppercase.as_inline_str().clone(),
            TagName::Private(symbol) => symbol.fully_qualified(interns, home),
        }
    }
}

impl ModuleName {
    // NOTE: After adding one of these, go to `impl ModuleId` and
    // add a corresponding ModuleId to there!
    pub const FLOAT: &'static str = "Float";
    pub const BOOL: &'static str = "Bool";
    pub const INT: &'static str = "Int";
    pub const STR: &'static str = "Str";
    pub const LIST: &'static str = "List";
    pub const MAP: &'static str = "Map";
    pub const SET: &'static str = "Set";
    pub const NUM: &'static str = "Num";

    pub fn as_str(&self) -> &str {
        &*self.0
    }

    pub fn as_inline_str(&self) -> &InlinableString {
        &self.0
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl AsRef<str> for ModuleName {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl<'a> From<&'a str> for ModuleName {
    fn from(string: &'a str) -> Self {
        Self(string.into())
    }
}

impl From<Box<str>> for ModuleName {
    fn from(string: Box<str>) -> Self {
        Self((string.as_ref()).into())
    }
}

impl From<String> for ModuleName {
    fn from(string: String) -> Self {
        Self(string.into())
    }
}

impl From<InlinableString> for ModuleName {
    fn from(string: InlinableString) -> Self {
        Self(string)
    }
}

impl Into<InlinableString> for ModuleName {
    fn into(self) -> InlinableString {
        self.0
    }
}

impl<'a> Into<&'a InlinableString> for &'a ModuleName {
    fn into(self) -> &'a InlinableString {
        &self.0
    }
}

impl<'a> Into<Box<str>> for ModuleName {
    fn into(self) -> Box<str> {
        self.0.to_string().into()
    }
}

impl Uppercase {
    pub fn as_str(&self) -> &str {
        &*self.0
    }

    pub fn as_inline_str(&self) -> &InlinableString {
        &self.0
    }
}

impl<'a> From<&'a str> for Uppercase {
    fn from(string: &'a str) -> Self {
        Self(string.into())
    }
}

impl<'a> From<String> for Uppercase {
    fn from(string: String) -> Self {
        Self(string.into())
    }
}

impl Lowercase {
    pub fn as_str(&self) -> &str {
        &*self.0
    }
}

impl<'a> From<&'a str> for Lowercase {
    fn from(string: &'a str) -> Self {
        Self(string.into())
    }
}

impl<'a> From<String> for Lowercase {
    fn from(string: String) -> Self {
        Self(string.into())
    }
}

impl Into<InlinableString> for Lowercase {
    fn into(self) -> InlinableString {
        self.0
    }
}

impl AsRef<str> for Ident {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl<'a> From<&'a str> for Ident {
    fn from(string: &'a str) -> Self {
        Self(string.into())
    }
}

impl From<Box<str>> for Ident {
    fn from(string: Box<str>) -> Self {
        Self((string.as_ref()).into())
    }
}

impl From<String> for Ident {
    fn from(string: String) -> Self {
        Self(string.into())
    }
}

impl From<InlinableString> for Ident {
    fn from(string: InlinableString) -> Self {
        Self(string)
    }
}

impl Into<InlinableString> for Ident {
    fn into(self) -> InlinableString {
        self.0
    }
}

impl<'a> Into<&'a InlinableString> for &'a Ident {
    fn into(self) -> &'a InlinableString {
        &self.0
    }
}

impl<'a> Into<Box<str>> for Ident {
    fn into(self) -> Box<str> {
        self.0.to_string().into()
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
