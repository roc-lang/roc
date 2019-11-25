use bumpalo::collections::Vec;
use bumpalo::Bump;
use parse::ast::AssignedField;
use parse::ast::Spaceable;
use parse::blankspace::{space0, space0_before};
use parse::collection::collection;
use parse::ident::lowercase_ident;
use parse::parser::{self, and, char, loc, optional, skip_first, Parser};
use region::Located;

/// Parse a record - generally one of these two:
///
/// * Literal Value, e.g. { name: "foo", email: "blah@example.com" }
/// * Type Annotation, e.g. { name: String, email: String }
pub fn record<'a, P, S>(
    val_parser: P,
    min_indent: u16,
) -> impl Parser<'a, Vec<'a, Located<AssignedField<'a, S>>>>
where
    P: Parser<'a, Located<S>>,
    P: 'a,
    S: Spaceable<'a>,
    S: 'a,
{
    collection(
        char('{'),
        loc(record_field(val_parser, min_indent)),
        char(','),
        char('}'),
        min_indent,
    )
}

fn record_field<'a, P, S>(val_parser: P, min_indent: u16) -> impl Parser<'a, AssignedField<'a, S>>
where
    P: Parser<'a, Located<S>>,
    P: 'a,
    S: Spaceable<'a>,
    S: 'a,
{
    use parse::ast::AssignedField::*;

    parser::map_with_arena(
        and(
            // You must have a field name, e.g. "email"
            loc!(lowercase_ident()),
            and(
                space0(min_indent),
                // Having a value is optional; both `{ email }` and `{ email: blah }` work.
                // (This is true in both literals and types.)
                optional(skip_first(char(':'), space0_before(val_parser, min_indent))),
            ),
        ),
        |arena: &'a Bump, (loc_label, (spaces, opt_loc_val))| match opt_loc_val {
            Some(loc_val) => LabeledValue(loc_label, spaces, arena.alloc(loc_val)),
            // If no value was provided, record it as a Var.
            // Canonicalize will know what to do with a Var later.
            None => {
                if !spaces.is_empty() {
                    SpaceAfter(arena.alloc(LabelOnly(loc_label)), spaces)
                } else {
                    LabelOnly(loc_label)
                }
            }
        },
    )
}
