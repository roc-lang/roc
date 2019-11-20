use bumpalo::collections::Vec;
use parse::ast::AssignedField;
use parse::ast::Spaceable;
use parse::blankspace::{space0, space0_before};
use parse::collection::collection;
use parse::ident::unqualified_ident;
use parse::parser::{and, char, map_with_arena, optional, skip_first, Parser, State};
use region::{Located, Region};

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

/// For some reason, record() needs to use this instead of using the loc! macro directly.
#[inline(always)]
pub fn loc<'a, P, Val>(parser: P) -> impl Parser<'a, Located<Val>>
where
    P: Parser<'a, Val>,
{
    loc!(parser)
}

fn record_field<'a, P, S>(val_parser: P, min_indent: u16) -> impl Parser<'a, AssignedField<'a, S>>
where
    P: Parser<'a, Located<S>>,
    P: 'a,
    S: Spaceable<'a>,
    S: 'a,
{
    use parse::ast::AssignedField::*;

    map_with_arena(
        and(
            // You must have a field name, e.g. "email"
            loc!(unqualified_ident()),
            and(
                space0(min_indent),
                // Having a value is optional; both `{ email }` and `{ email: blah }` work.
                // (This is true in both literals and types.)
                optional(skip_first(char(':'), space0_before(val_parser, min_indent))),
            ),
        ),
        |arena, (loc_label, (spaces, opt_loc_val))| match opt_loc_val {
            Some(loc_val) => LabeledValue(loc_label, spaces, arena.alloc(loc_val)),
            // If no value was provided, record it as a Var.
            // Canonicalize will know what to do with a Var later.
            None => LabelOnly(loc_label, spaces),
        },
    )
}
