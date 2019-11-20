use can::expr::Expr;
use can::expr::Expr::*;
use can::symbol::Symbol;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub enum ReferenceCount {
    Unique,
    Shared,
}

impl ReferenceCount {
    pub fn add(_a: &ReferenceCount, _b: &ReferenceCount) -> Self {
        Self::Shared
    }

    pub fn or(a: &ReferenceCount, b: &ReferenceCount) -> Self {
        match (a, b) {
            (Self::Unique, Self::Unique) => Self::Unique,
            _ => Self::Shared,
        }
    }
}

fn register(symbol: &Symbol, usage: &mut HashMap<Symbol, ReferenceCount>) -> () {
    use self::ReferenceCount::*;
    let value = match usage.get(symbol) {
        None => Unique,
        Some(_) => Shared,
    };

    usage.insert(symbol.clone(), value);

    dbg!(symbol.clone(), usage);
}

// actually, I think it would also work to do it as HashMap<Symbol, Variable>
//
// you get the same "if there's already an entry in the map, then this must be shared"
// but also, at the same time, you can now retroactively mark that other Variable as Shared because you know what it is - you got it right there out of the map
// and if there is no entry for that Symbol in the map, then cool - you insert your current Variable and move on assuming uniqueness until someone else later decides (or not) that you were actually Shared

pub fn sharing_analysis(expr: &Expr, usage: &mut HashMap<Symbol, ReferenceCount>) -> () {
    match expr {
        Var(_, symbol) | FunctionPointer(_, symbol) => {
            register(symbol, usage);
        }

        List(_, elements) => {
            for element in elements {
                sharing_analysis(&element.value, usage);
            }
        }

        Case(_, boxed_loc_expr, branches) => {
            sharing_analysis(&boxed_loc_expr.value, usage);

            for (pattern, branch) in branches {}
        }

        Defs(_, assignments, body) => {
            for (_pattern, value) in assignments {
                sharing_analysis(&value.value, usage);
            }

            sharing_analysis(&body.value, usage);
        }

        CallByName(symbol, arguments, _) => {
            register(symbol, usage);

            for argument in arguments {
                sharing_analysis(&argument.value, usage);
            }
        }

        CallPointer(function, arguments, _) => {
            sharing_analysis(function, usage);

            for argument in arguments {
                sharing_analysis(&argument.value, usage);
            }
        }

        Record(_, fields) => {
            for field in fields {
                sharing_analysis(&field.value.1.value, usage);
            }
        }

        Field(record, _) => {
            sharing_analysis(&record.value, usage);
        }
        Int(_) | Float(_) | Str(_) | BlockStr(_) | EmptyRecord | RuntimeError(_) => {}
    }
}

/*
 *
    // Literals
    Int(i64),
    Float(f64),
    Str(Box<str>),
    BlockStr(Box<str>),
    List(Variable, Vec<Located<Expr>>),

    // Lookups
    Var(Variable, Symbol),
    /// Works the same as Var, but has an important marking purpose.
    /// See 13623e3f5f65ea2d703cf155f16650c1e8246502 for the bug this fixed.
    FunctionPointer(Variable, Symbol),

    /// Look up exactly one field on a record, e.g. (expr).foo.
    /// Canonicalization will convert chains to single-access, e.g. foo.bar.baz to (foo.bar).baz.
    Field(Box<Located<Expr>>, Box<str>),

    // Pattern Matching
    /// Case is guaranteed to be exhaustive at this point. (If it wasn't, then
    /// a _ branch was added at the end that will throw a runtime error.)
    /// Also, `If` is desugared into `Case` matching on `False` and `_` at this point.
    Case(
        Variable,
        Box<Located<Expr>>,
        Vec<(Located<Pattern>, Located<Expr>)>,
    ),
    Defs(
        Variable,
        Vec<(Located<Pattern>, Located<Expr>)>,
        Box<Located<Expr>>,
    ),

    CallByName(Symbol, Vec<Located<Expr>>, CalledVia),
    CallPointer(Box<Expr>, Vec<Located<Expr>>, CalledVia),

    // Product Types
    Record(Variable, Vec<Located<(Box<str>, Located<Expr>)>>),
    EmptyRecord,

    // Compiles, but will crash if reached
    RuntimeError(RuntimeError),
*/
