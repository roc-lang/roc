use roc_module::symbol::Symbol;

use crate::layout::{InLayout, UnionLayout};

use super::Expr;

pub fn box_<'a>(symbol: &'a Symbol, element_layout: &'a InLayout<'a>) -> Expr<'a> {
    Expr::Tag {
        tag_layout: UnionLayout::NonNullableUnwrapped(std::slice::from_ref(element_layout)),
        tag_id: 0,
        arguments: std::slice::from_ref(symbol),
        reuse: None,
    }
}

pub fn unbox<'a>(symbol: Symbol, element_layout: &'a InLayout<'a>) -> Expr<'a> {
    Expr::UnionAtIndex {
        structure: symbol,
        tag_id: 0,
        union_layout: UnionLayout::NonNullableUnwrapped(std::slice::from_ref(element_layout)),
        index: 0,
    }
}

pub fn box_nullable<'a>(symbol: &'a Symbol, element_layout: &'a InLayout<'a>) -> Expr<'a> {
    Expr::Tag {
        tag_layout: UnionLayout::boxed_erased_value(element_layout),
        tag_id: 0,
        arguments: std::slice::from_ref(symbol),
        reuse: None,
    }
}

pub fn unbox_nullable<'a>(symbol: Symbol, element_layout: &'a InLayout<'a>) -> Expr<'a> {
    Expr::UnionAtIndex {
        structure: symbol,
        tag_id: 0,
        union_layout: UnionLayout::boxed_erased_value(element_layout),
        index: 0,
    }
}
