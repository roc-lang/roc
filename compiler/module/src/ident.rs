pub use roc_ident::IdentStr;
use std::fmt;

/// This could be uppercase or lowercase, qualified or unqualified.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct Ident(pub IdentStr);

impl Ident {
    pub fn as_inline_str(&self) -> &IdentStr {
        &self.0
    }

    #[inline(always)]
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

pub struct QualifiedModuleName<'a> {
    pub opt_package: Option<&'a str>,
    pub module: ModuleName,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleName(IdentStr);

impl std::ops::Deref for ModuleName {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_str()
    }
}

/// An uncapitalized identifier, such as a field name or local variable
#[derive(Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Lowercase(IdentStr);

/// A capitalized identifier, such as a tag name or module name
#[derive(Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Uppercase(IdentStr);

/// A string representing a foreign (linked-in) symbol
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct ForeignSymbol(IdentStr);

pub type TagIdIntType = u16;

/// Tags have no module, but tend to be short strings (since they're
/// never qualified), so we store them as ident strings.
///
/// This is allows canonicalization to happen in parallel without locks.
/// If tags had a Symbol representation, then each module would have to
/// deal with contention on a global mutex around translating tag strings
/// into integers. (Record field labels work the same way, for the same reason.)
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TagName(pub Uppercase);

roc_error_macros::assert_sizeof_non_wasm!(TagName, 16);
roc_error_macros::assert_sizeof_wasm!(TagName, 8);

impl TagName {
    pub fn as_ident_str(&self) -> IdentStr {
        self.0.as_ident_str().clone()
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
    pub const BOX: &'static str = "Box";
    pub const ENCODE: &'static str = "Encode";
    pub const JSON: &'static str = "Json";

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn as_ident_str(&self) -> &IdentStr {
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

impl<'a> From<IdentStr> for ModuleName {
    fn from(string: IdentStr) -> Self {
        Self(string.as_str().into())
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

impl From<ModuleName> for Box<str> {
    fn from(name: ModuleName) -> Self {
        name.0.to_string().into()
    }
}

impl fmt::Display for ModuleName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl ForeignSymbol {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn as_inline_str(&self) -> &IdentStr {
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

    pub fn as_ident_str(&self) -> &IdentStr {
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
        self.0.as_str()
    }
}

impl<'a> From<&'a str> for Lowercase {
    fn from(string: &'a str) -> Self {
        Self(string.into())
    }
}

impl<'a> From<&'a Lowercase> for &'a str {
    fn from(lowercase: &'a Lowercase) -> Self {
        lowercase.as_str()
    }
}

impl<'a> From<String> for Lowercase {
    fn from(string: String) -> Self {
        Self(string.into())
    }
}

impl AsRef<str> for Ident {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        self.0.as_str()
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

impl From<IdentStr> for Ident {
    fn from(string: IdentStr) -> Self {
        Self(string)
    }
}

impl From<Ident> for IdentStr {
    fn from(ident: Ident) -> Self {
        ident.0
    }
}

impl<'a> From<&'a Ident> for &'a IdentStr {
    fn from(ident: &'a Ident) -> Self {
        &ident.0
    }
}

impl From<Ident> for Box<str> {
    fn from(ident: Ident) -> Self {
        ident.0.to_string().into()
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
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
