use bumpalo::collections::Vec;
use bumpalo::Bump;
use parse::ast::AssignedField;
use parse::ast::Spaceable;
use parse::blankspace::{space0, space0_before};
use parse::ident::lowercase_ident;
use parse::parser::{self, and, char, loc, optional, Parser};
use region::Located;

// Parse a record - generally one of these two:
//
// * Literal Value, e.g. { name: "foo", email: "blah@example.com" }
// * Type Annotation, e.g. { name: String, email: String }
// pub fn record<'a, P, S>(
//     val_parser: P,
//     min_indent: u16,
// ) -> impl Parser<'a, Vec<'a, Located<AssignedField<'a, S>>>>
// where
//     P: Parser<'a, Located<S>>,
//     P: 'a,
//     S: Spaceable<'a>,
//     S: 'a,
// {
//     collection!(
//         char('{'),
//         loc(record_field(val_parser, min_indent)),
//         char(','),
//         char('}'),
//         min_indent
//     )
// }

// fn record_field<'a, P, S>(val_parser: P, min_indent: u16) -> impl Parser<'a, AssignedField<'a, S>>
// where
//     P: Parser<'a, Located<S>>,
//     P: 'a,
//     S: Spaceable<'a>,
//     S: 'a,
// {
//     use parse::ast::AssignedField::*;
//     panic!("TODO");
// }
