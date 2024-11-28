// This crate is only used in tests, so panic is fine
#![allow(clippy::panic)]

mod deindent;
mod help_can;
mod help_constrain;
mod help_parse;
mod help_solve;
mod help_specialize;

pub use deindent::trim_and_deindent;
pub use help_can::{CanExpr, CanExprOut};
pub use help_parse::ParseExpr;
pub use help_solve::{SolvedExpr, SolvedExprOut};
pub use help_specialize::{SpecializedExpr, SpecializedExprOut};

pub fn can_expr(input: &str) -> CanExprOut {
    CanExpr::default().can_expr(input)
}

pub fn solve_expr(input: &str) -> SolvedExprOut {
    SolvedExpr::default().solve_expr(input)
}
