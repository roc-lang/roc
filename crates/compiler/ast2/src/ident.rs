use crate::EIdent;

/// A tag, for example. Must start with an uppercase letter
/// and then contain only letters and numbers afterwards - no dots allowed!
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct UppercaseIdent<'a>(&'a str);

impl<'a> From<&'a str> for UppercaseIdent<'a> {
    fn from(string: &'a str) -> Self {
        UppercaseIdent(string)
    }
}

impl<'a> From<UppercaseIdent<'a>> for &'a str {
    fn from(ident: UppercaseIdent<'a>) -> Self {
        ident.0
    }
}

impl<'a> From<&'a UppercaseIdent<'a>> for &'a str {
    fn from(ident: &'a UppercaseIdent<'a>) -> Self {
        ident.0
    }
}

/// The parser accepts all of these in any position where any one of them could
/// appear. This way, canonicalization can give more helpful error messages like
/// "you can't redefine this tag!" if you wrote `Foo = ...` or
/// "you can only define unqualified constants" if you wrote `Foo.bar = ...`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ident<'a> {
    /// Foo or Bar
    Tag(&'a str),
    /// @Foo or @Bar
    OpaqueRef(&'a str),
    /// foo or foo.bar or Foo.Bar.baz.qux
    Access {
        module_name: &'a str,
        parts: &'a [&'a str],
    },
    /// .foo { foo: 42 }
    RecordAccessorFunction(&'a str),
    /// .1 (1, 2, 3)
    TupleAccessorFunction(&'a str),
    /// .Foo or foo. or something like foo.Bar
    Malformed(&'a str, EIdent),
}

impl<'a> Ident<'a> {
    pub fn len(&self) -> usize {
        use self::Ident::*;

        match self {
            Tag(string) | OpaqueRef(string) => string.len(),
            Access { module_name, parts } => {
                let mut len = if module_name.is_empty() {
                    0
                } else {
                    module_name.len() + 1
                    // +1 for the dot
                };

                for part in parts.iter() {
                    len += part.len() + 1 // +1 for the dot
                }

                len - 1
            }
            RecordAccessorFunction(string) => string.len(),
            TupleAccessorFunction(string) => string.len(),
            Malformed(string, _) => string.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
