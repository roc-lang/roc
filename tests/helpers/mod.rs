use roc::expr::{Expr, Pattern};
use roc::region::{Located, Region};
use std::hash::Hash;
use roc::collections::{MutMap};

pub fn loc_box<T>(val: T) -> Box<Located<T>> {
    Box::new(loc(val))
}

pub fn loc<T>(val: T) -> Located<T> {
    Located::new(val, empty_region())
}

pub fn empty_region() -> Region {
    Region {
        start_line: 0,
        start_col: 0,

        end_line: 0,
        end_col: 0,
    }
}


pub fn zero_loc<T>(located_val: Located<T>) -> Located<T> {
    loc(located_val.value)
}

/// Zero out the parse locations on everything in this Expr, so we can compare expected/actual without
/// having to account for that.
pub fn zero_loc_expr(expr: Expr) -> Expr {
    use roc::expr::Expr::*;

    match expr {
        Int(_) | Frac(_, _) | Approx(_) | EmptyStr | Str(_) | Char(_) | Var(_) | EmptyRecord => expr,
        InterpolatedStr(pairs, string) => InterpolatedStr(pairs.into_iter().map(|( prefix, ident )| ( prefix, zero_loc(ident))).collect(), string),
        Assign(assignments, loc_ret) => {
            let zeroed_assignments =
                assignments.into_iter().map(|( pattern, loc_expr )|
                    ( zero_loc_pattern(pattern), loc(zero_loc_expr(loc_expr.value)) )
                ).collect();

            Assign(zeroed_assignments, loc_box(zero_loc_expr((*loc_ret).value)))
        },
        CallByName(ident, args) => CallByName(ident, args.into_iter().map(|arg| loc(zero_loc_expr(arg.value))).collect()),
        Apply(fn_expr, args) => Apply(loc_box(zero_loc_expr((*fn_expr).value)), args.into_iter().map(|arg| loc(zero_loc_expr(arg.value))).collect()),
        Operator(left, op, right) => Operator(loc_box(zero_loc_expr((*left).value)), zero_loc(op), loc_box(zero_loc_expr((*right).value))),
        Closure(patterns, body) => Closure(patterns.into_iter().map(zero_loc).collect(), loc_box(zero_loc_expr((*body).value))),
        ApplyVariant(_, None) => expr,
        ApplyVariant(name, Some(args)) => ApplyVariant(name, Some(args.into_iter().map(|arg| loc(zero_loc_expr(arg.value))).collect())),
        If(condition, if_true, if_false) => If(loc_box(zero_loc_expr((*condition).value)), loc_box(zero_loc_expr((*if_true).value)), loc_box(zero_loc_expr((*if_false).value))),
        Case(condition, branches) =>
            Case(
                loc_box(zero_loc_expr((*condition).value)),
                branches.into_iter().map(|( pattern, loc_expr )| ( zero_loc_pattern(pattern), loc(zero_loc_expr(loc_expr.value)) )).collect()
            ),
    }
}

/// Zero out the parse locations on everything in this Pattern, so we can compare expected/actual without
/// having to account for that.
pub fn zero_loc_pattern(loc_pattern: Located<Pattern>) -> Located<Pattern> {
    use roc::expr::Pattern::*;

    let pattern = loc_pattern.value;

    match pattern {
        Identifier(_) | Integer(_) | Fraction(_, _) | ExactString(_) | EmptyRecordLiteral | Underscore => loc(pattern),
        Variant(loc_name, None) =>
            loc(Variant(loc(loc_name.value), None)),
        Variant(loc_name, Some(opt_located_patterns)) =>
            loc(Variant(loc(loc_name.value), Some(opt_located_patterns.into_iter().map(|loc_pat| zero_loc_pattern(loc_pat)).collect()))),
    }
}

#[allow(dead_code)] // For some reason rustc thinks this isn't used. It is, though, in test_canonicalize.rs
pub fn mut_map_from_pairs<K, V, I>(pairs: I) -> MutMap<K, V>
    where I: IntoIterator<Item=(K, V)>,
        K: Hash + Eq
    {
        let mut answer = MutMap::default();

        for (key, value) in pairs {
            answer.insert(key, value);
        }

        answer
    }