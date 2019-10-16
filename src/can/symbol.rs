use ident::VariantName;

/// A globally unique identifier, used for both vars and variants.
/// It will be used directly in code gen.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(Box<str>);

impl Symbol {
    pub fn new(prefix: &str, name: &str) -> Symbol {
        Symbol(format!("{}{}", prefix, name).into())
    }

    pub fn from_variant(variant_name: &VariantName, home: &str) -> Symbol {
        match &variant_name {
            &VariantName::Unqualified(ref name) => Symbol::new(home, name),

            &VariantName::Qualified(ref path, ref name) => Symbol::new(path, name),
        }
    }
}

impl Into<Box<str>> for Symbol {
    fn into(self) -> Box<str> {
        let Symbol(string) = self;

        string
    }
}
