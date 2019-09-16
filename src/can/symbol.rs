use ident::VariantName;

/// A globally unique identifier, used for both vars and variants.
/// It will be used directly in code gen.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(String);

impl Symbol {
    pub fn new(prefix: &str, name: &str) -> Symbol {
        Symbol(format!("{}{}", prefix, name))
    }

    pub fn from_variant(variant_name: &VariantName, home: &str) -> Symbol {
        match &variant_name {
            &VariantName::Unqualified(ref name) => Symbol::new(home, name),

            &VariantName::Qualified(ref path, ref name) => Symbol::new(path, name),
        }
    }
}

impl Into<String> for Symbol {
    fn into(self) -> String {
        let Symbol(string) = self;

        string
    }
}
