use crate::ir::DestructType;
use roc_collections::all::HumanIndex;
use roc_exhaustive::{
    is_useful, Context, Ctor, CtorName, Error, Guard, Literal, Pattern, RenderAs, TagId, Union,
};
use roc_module::ident::{TagIdIntType, TagName};
use roc_region::all::{Loc, Region};

use Pattern::*;

fn simplify(pattern: &crate::ir::Pattern) -> Pattern {
    use crate::ir::Pattern::*;

    match pattern {
        IntLiteral(v, _) => Literal(Literal::Int(*v)),
        FloatLiteral(v, _) => Literal(Literal::Float(*v)),
        DecimalLiteral(v) => Literal(Literal::Decimal(*v)),
        StrLiteral(v) => Literal(Literal::Str(v.clone())),

        // To make sure these are exhaustive, we have to "fake" a union here
        BitLiteral { value, union, .. } => {
            Ctor(union.clone(), TagId(*value as TagIdIntType), vec![])
        }
        EnumLiteral { tag_id, union, .. } => {
            Ctor(union.clone(), TagId(*tag_id as TagIdIntType), vec![])
        }

        Underscore => Anything,
        Identifier(_) => Anything,
        RecordDestructure(destructures, _) => {
            let tag_id = TagId(0);
            let mut patterns = std::vec::Vec::with_capacity(destructures.len());
            let mut field_names = std::vec::Vec::with_capacity(destructures.len());

            for destruct in destructures {
                field_names.push(destruct.label.clone());

                match &destruct.typ {
                    DestructType::Required(_) => patterns.push(Anything),
                    DestructType::Guard(guard) => patterns.push(simplify(guard)),
                }
            }

            let union = Union {
                render_as: RenderAs::Record(field_names),
                alternatives: vec![Ctor {
                    name: CtorName::Tag(TagName::Tag("#Record".into())),
                    tag_id,
                    arity: destructures.len(),
                }],
            };

            Ctor(union, tag_id, patterns)
        }

        NewtypeDestructure {
            arguments,
            tag_name,
        } => {
            let tag_id = 0;
            let simplified_args: std::vec::Vec<_> =
                arguments.iter().map(|v| simplify(&v.0)).collect();
            Ctor(
                Union::newtype_wrapper(CtorName::Tag(tag_name.clone()), arguments.len()),
                TagId(tag_id),
                simplified_args,
            )
        }

        AppliedTag {
            tag_id,
            arguments,
            union,
            ..
        } => {
            let simplified_args: std::vec::Vec<_> =
                arguments.iter().map(|v| simplify(&v.0)).collect();
            Ctor(union.clone(), TagId(*tag_id), simplified_args)
        }

        OpaqueUnwrap { opaque, argument } => {
            let (argument, _) = &(**argument);

            let tag_id = TagId(0);

            let union = Union {
                render_as: RenderAs::Opaque,
                alternatives: vec![Ctor {
                    name: CtorName::Opaque(*opaque),
                    tag_id,
                    arity: 1,
                }],
            };

            Ctor(union, tag_id, vec![simplify(argument)])
        }
    }
}

pub fn check(
    region: Region,
    patterns: &[(Loc<crate::ir::Pattern>, Guard)],
    context: Context,
) -> Result<(), Vec<Error>> {
    let mut errors = Vec::new();
    check_patterns(region, context, patterns, &mut errors);

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn check_patterns<'a>(
    region: Region,
    context: Context,
    patterns: &[(Loc<crate::ir::Pattern<'a>>, Guard)],
    errors: &mut Vec<Error>,
) {
    match to_nonredundant_rows(region, patterns) {
        Err(err) => errors.push(err),
        Ok(matrix) => {
            if let Err(err) = roc_exhaustive::check(region, context, matrix) {
                *errors = err;
            }
        }
    }
}

/// REDUNDANT PATTERNS

/// INVARIANT: Produces a list of rows where (forall row. length row == 1)
fn to_nonredundant_rows(
    overall_region: Region,
    patterns: &[(Loc<crate::ir::Pattern>, Guard)],
) -> Result<Vec<Vec<Pattern>>, Error> {
    let mut checked_rows = Vec::with_capacity(patterns.len());

    // If any of the branches has a guard, e.g.
    //
    // when x is
    //      y if y < 10 -> "foo"
    //      _ -> "bar"
    //
    // then we treat it as a pattern match on the pattern and a boolean, wrapped in the #Guard
    // constructor. We can use this special constructor name to generate better error messages.
    // This transformation of the pattern match only works because we only report exhaustiveness
    // errors: the Pattern created in this file is not used for code gen.
    //
    // when x is
    //      #Guard y True -> "foo"
    //      #Guard _ _    -> "bar"
    let any_has_guard = patterns.iter().any(|(_, guard)| guard == &Guard::HasGuard);

    for (loc_pat, guard) in patterns {
        let region = loc_pat.region;

        let next_row = if any_has_guard {
            let guard_pattern = match guard {
                Guard::HasGuard => Pattern::Literal(Literal::Bit(true)),
                Guard::NoGuard => Pattern::Anything,
            };

            let tag_id = TagId(0);

            let union = Union {
                render_as: RenderAs::Guard,
                alternatives: vec![Ctor {
                    tag_id,
                    name: CtorName::Tag(TagName::Tag("#Guard".into())),
                    arity: 2,
                }],
            };

            vec![Pattern::Ctor(
                union,
                tag_id,
                // NB: ordering the guard pattern first seems to be better at catching
                // non-exhaustive constructors in the second argument; see the paper to see if
                // there is a way to improve this in general.
                vec![guard_pattern, simplify(&loc_pat.value)],
            )]
        } else {
            vec![simplify(&loc_pat.value)]
        };

        if matches!(guard, Guard::HasGuard) || is_useful(checked_rows.clone(), next_row.clone()) {
            checked_rows.push(next_row);
        } else {
            return Err(Error::Redundant {
                overall_region,
                branch_region: region,
                index: HumanIndex::zero_based(checked_rows.len()),
            });
        }
    }

    Ok(checked_rows)
}
