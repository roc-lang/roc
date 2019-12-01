use crate::can::expr::Expr;
use crate::can::pattern::Pattern;
use crate::region::Located;
use im::Vector;

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub name: Option<Box<str>>,
    pub defs: Vector<(Located<Pattern>, Located<Expr>)>,
}
