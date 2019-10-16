use ident::VariantName;

/// A globally unique identifier, used for both vars and variants.
/// It will be used directly in code gen.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol<'a>(&'a str);

impl<'a> Symbol<'a> {
    pub fn new(prefix: &'a str, name: &'a str) -> Symbol<'a> {
        Symbol(&format!("{}{}", prefix, name))
    }

    pub fn from_variant(variant_name: &'a VariantName, home: &'a str) -> Symbol<'a> {
        match &variant_name {
            &VariantName::Unqualified(ref name) => Symbol::new(home, name),

            &VariantName::Qualified(ref path, ref name) => Symbol::new(path, name),
        }
    }
}

impl<'a> Into<&'a str> for Symbol<'a> {
    fn into(self) -> &'a str {
        let Symbol(string) = self;

        string
    }
}
