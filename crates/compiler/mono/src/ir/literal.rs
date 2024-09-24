use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_can::expr::IntValue;
use roc_error_macros::internal_error;
use roc_module::symbol::Symbol;
use roc_std::RocDec;

use crate::layout::{Builtin, InLayout, LayoutInterner, LayoutRepr, TLLayoutInterner};

use super::pattern::Pattern;

#[derive(Debug, Clone, Copy)]
pub enum IntOrFloatValue {
    Int(IntValue),
    Float(f64),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal<'a> {
    // Literals
    /// stored as raw bytes rather than a number to avoid an alignment bump
    Int([u8; 16]),
    /// stored as raw bytes rather than a number to avoid an alignment bump
    U128([u8; 16]),
    Float(f64),
    /// stored as raw bytes rather than a number to avoid an alignment bump
    Decimal([u8; 16]),
    Str(&'a str),
    /// Closed tag unions containing exactly two (0-arity) tags compile to Expr::Bool,
    /// so they can (at least potentially) be emitted as 1-bit machine bools.
    ///
    /// So [True, False] compiles to this, and so do [A, B] and [Foo, Bar].
    /// However, a union like [True, False, Other Int] would not.
    Bool(bool),
    /// Closed tag unions containing between 3 and 256 tags (all of 0 arity)
    /// compile to bytes, e.g. [Blue, Black, Red, Green, White]
    Byte(u8),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ListLiteralElement<'a> {
    Literal(Literal<'a>),
    Symbol(Symbol),
}

impl<'a> ListLiteralElement<'a> {
    pub fn to_symbol(&self) -> Option<Symbol> {
        match self {
            Self::Symbol(s) => Some(*s),
            _ => None,
        }
    }

    pub fn get_literal(&self) -> Option<Literal<'a>> {
        match self {
            Self::Literal(l) => Some(*l),
            _ => None,
        }
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Self::Literal(_))
    }
}

pub enum NumLiteral {
    Int([u8; 16], IntWidth),
    U128([u8; 16]),
    Float(f64, FloatWidth),
    Decimal([u8; 16]),
}

impl NumLiteral {
    pub fn to_expr_literal(&self) -> Literal<'static> {
        match *self {
            NumLiteral::Int(n, _) => Literal::Int(n),
            NumLiteral::U128(n) => Literal::U128(n),
            NumLiteral::Float(n, _) => Literal::Float(n),
            NumLiteral::Decimal(n) => Literal::Decimal(n),
        }
    }
    pub fn to_pattern(&self) -> Pattern<'static> {
        match *self {
            NumLiteral::Int(n, w) => Pattern::IntLiteral(n, w),
            NumLiteral::U128(n) => Pattern::IntLiteral(n, IntWidth::U128),
            NumLiteral::Float(n, w) => Pattern::FloatLiteral(f64::to_bits(n), w),
            NumLiteral::Decimal(n) => Pattern::DecimalLiteral(n),
        }
    }
}

pub fn make_num_literal<'a>(
    interner: &TLLayoutInterner<'a>,
    layout: InLayout<'a>,
    num_str: &str,
    num_value: IntOrFloatValue,
) -> NumLiteral {
    match interner.get_repr(layout) {
        LayoutRepr::Builtin(Builtin::Int(width)) => match num_value {
            IntOrFloatValue::Int(IntValue::I128(n)) => NumLiteral::Int(n, width),
            IntOrFloatValue::Int(IntValue::U128(n)) => NumLiteral::U128(n),
            IntOrFloatValue::Float(..) => {
                internal_error!("Frac value where int was expected, should have been a type error")
            }
        },
        LayoutRepr::Builtin(Builtin::Float(width)) => match num_value {
            IntOrFloatValue::Float(n) => NumLiteral::Float(n, width),
            IntOrFloatValue::Int(int_value) => match int_value {
                IntValue::I128(n) => NumLiteral::Float(i128::from_ne_bytes(n) as f64, width),
                IntValue::U128(n) => NumLiteral::Float(u128::from_ne_bytes(n) as f64, width),
            },
        },
        LayoutRepr::Builtin(Builtin::Decimal) => {
            let dec = match RocDec::from_str(num_str) {
                Some(d) => d,
                None => internal_error!(
                    "Invalid decimal for float literal = {}. This should be a type error!",
                    num_str
                ),
            };
            NumLiteral::Decimal(dec.to_ne_bytes())
        }
        layout => internal_error!(
            "Found a non-num layout where a number was expected: {:?}",
            layout
        ),
    }
}
