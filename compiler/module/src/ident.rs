use crate::symbol::{Interns, ModuleId, Symbol};
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

pub struct QualifiedModuleName<'a> {
    pub opt_package: Option<&'a str>,
    pub module: ModuleName,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleName(InlinableString);

/// An uncapitalized identifier, such as a field name or local variable
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Lowercase(InlinableString);

/// A capitalized identifier, such as a tag name or module name
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Uppercase(roc_ident::IdentStr);

/// A string representing a foreign (linked-in) symbol
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct ForeignSymbol(InlinableString);

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

    /// Used to connect the closure size to the function it corresponds to
    Closure(Symbol),
}

impl TagName {
    pub fn as_string(&self, interns: &Interns, home: ModuleId) -> InlinableString {
        match self {
            TagName::Global(uppercase) => uppercase.as_inline_str().as_str().clone().into(),
            TagName::Private(symbol) => symbol.fully_qualified(interns, home),
            TagName::Closure(symbol) => symbol.fully_qualified(interns, home),
        }
    }
}

impl ModuleName {
    // NOTE: After adding one of these, go to `impl ModuleId` and
    // add a corresponding ModuleId to there!
    pub const APP: &'static str = "#UserApp"; // app modules have this hardcoded name
    pub const BOOL: &'static str = "Bool";
    pub const STR: &'static str = "Str";
    pub const NUM: &'static str = "Num";
    pub const LIST: &'static str = "List";
    pub const DICT: &'static str = "Dict";
    pub const SET: &'static str = "Set";
    pub const RESULT: &'static str = "Result";

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

impl From<ModuleName> for InlinableString {
    fn from(name: ModuleName) -> Self {
        name.0
    }
}

impl<'a> From<&'a ModuleName> for &'a InlinableString {
    fn from(name: &'a ModuleName) -> Self {
        &name.0
    }
}

impl From<ModuleName> for Box<str> {
    fn from(name: ModuleName) -> Self {
        name.0.to_string().into()
    }
}

impl ForeignSymbol {
    pub fn as_str(&self) -> &str {
        &*self.0
    }

    pub fn as_inline_str(&self) -> &InlinableString {
        &self.0
    }
}

impl<'a> From<&'a str> for ForeignSymbol {
    fn from(string: &'a str) -> Self {
        Self(string.into())
    }
}

impl<'a> From<String> for ForeignSymbol {
    fn from(string: String) -> Self {
        Self(string.into())
    }
}

impl Uppercase {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn as_inline_str(&self) -> &roc_ident::IdentStr {
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

impl From<Lowercase> for InlinableString {
    fn from(lowercase: Lowercase) -> Self {
        lowercase.0
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

impl From<Ident> for InlinableString {
    fn from(ident: Ident) -> Self {
        ident.0
    }
}

impl<'a> From<&'a Ident> for &'a InlinableString {
    fn from(ident: &'a Ident) -> Self {
        &ident.0
    }
}

impl From<Ident> for Box<str> {
    fn from(ident: Ident) -> Self {
        ident.0.to_string().into()
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
