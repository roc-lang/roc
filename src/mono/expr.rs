use crate::mono::layout::Layout;
use crate::subs::Variable;
use inlinable_string::InlinableString;

#[derive(Clone, Debug, PartialEq)]
pub struct Proc<'a> {
    pub args: &'a [(Layout<'a>, InlinableString, Variable)],
    pub body: Expr<'a>,
    pub closes_over: Layout<'a>,
    pub ret_var: Variable,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    // Literals
    Int(i64),
    Float(f64),
    Str(&'a str),

    // Load/Store
    Load(InlinableString),
    Store(&'a [(InlinableString, Variable, Expr<'a>)], &'a Expr<'a>),

    // Functions
    FunctionPointer(InlinableString),
    CallByPointer(&'a Expr<'a>, &'a [Expr<'a>]),
    CallByName(InlinableString, &'a [Expr<'a>]),

    // Exactly two conditional branches, e.g. if/else
    Cond {
        // The left-hand side of the conditional comparison and the right-hand side.
        // These are stored separately because there are different machine instructions
        // for e.g. "compare float and jump" vs. "compare integer and jump"
        cond_lhs: &'a Expr<'a>,
        cond_rhs: &'a Expr<'a>,
        // What to do if the condition either passes or fails
        pass: &'a Expr<'a>,
        fail: &'a Expr<'a>,
        ret_var: Variable,
    },
    /// More than two conditional branches, e.g. a 3-way when-expression
    Branches {
        /// The left-hand side of the conditional. We compile this to LLVM once,
        /// then reuse it to test against each different compiled cond_rhs value.
        cond_lhs: &'a Expr<'a>,
        /// ( cond_rhs, pass, fail )
        branches: &'a [(Expr<'a>, Expr<'a>, Expr<'a>)],
        ret_var: Variable,
    },

    Struct(&'a [(InlinableString, Expr<'a>)]),

    RuntimeError(&'a str),
}
