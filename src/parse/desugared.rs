#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    // Number Literals
    Float(&'a str),
    Int(&'a str),
    NonBase10Int {
        string: &'a str,
        base: Base,
        is_negative: bool,
    },

    // String Literals
    Str(&'a str),
    BlockStr(&'a [&'a str]),
    /// Look up exactly one field on a record, e.g. (expr).foo.
    Access(&'a Expr<'a>, UnqualifiedIdent<'a>),
    /// e.g. `.foo`
    AccessorFunction(UnqualifiedIdent<'a>),

    // Collection Literals
    List(Vec<'a, &'a Loc<Expr<'a>>>),
    Record(Vec<'a, Loc<AssignedField<'a, Expr<'a>>>>),

    // Lookups
    Var(&'a [&'a str], &'a str),

    // Tags
    GlobalTag(&'a str),
    PrivateTag(&'a str),

    // Pattern Matching
    Closure(&'a Vec<'a, Loc<Pattern<'a>>>, &'a Loc<Expr<'a>>),
    /// Multiple defs in a row
    Defs(Vec<'a, &'a Loc<Def<'a>>>, &'a Loc<Expr<'a>>),

    // Application
    /// To apply by name, do Apply(Var(...), ...)
    /// To apply a tag by name, do Apply(Tag(...), ...)
    Apply(&'a Loc<Expr<'a>>, Vec<'a, &'a Loc<Expr<'a>>>, CalledVia),

    // Conditionals
    Case(
        &'a Loc<Expr<'a>>,
        Vec<'a, &'a (Loc<Pattern<'a>>, Loc<Expr<'a>>)>,
    ),

    /// This is used only to avoid cloning when reordering expressions (e.g. in desugar()).
    /// It lets us take an (&Expr) and create a plain (Expr) from it.
    Nested(&'a Expr<'a>),

    // Problems
    MalformedIdent(&'a str),
    MalformedClosure,
    // Both operators were non-associative, e.g. (True == False == False).
    // We should tell the author to disambiguate by grouping them with parens.
    PrecedenceConflict(Loc<BinOp>, Loc<BinOp>, &'a Loc<Expr<'a>>),
}
