

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    // Symbol(String),
    // Int,
    // Float,
    // Number,
    // TypeUnion(BTreeSet<Type>),
    // Function(Box<Type>, Box<Type>),
    Call(Box<Type>, Box<Type>),
}
