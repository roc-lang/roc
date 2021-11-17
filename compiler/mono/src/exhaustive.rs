use crate::{ir::DestructType, layout::TagIdIntType};
use roc_collections::all::{Index, MutMap};
use roc_module::ident::{Lowercase, TagName};
use roc_region::all::{Located, Region};
use roc_std::RocDec;

use self::Pattern::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Union {
    pub alternatives: Vec<Ctor>,
    pub render_as: RenderAs,
}

impl Union {
    pub fn newtype_wrapper(tag_name: TagName, arity: usize) -> Self {
        let alternatives = vec![Ctor {
            name: tag_name,
            tag_id: TagId(0),
            arity,
        }];

        Union {
            alternatives,
            render_as: RenderAs::Tag,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RenderAs {
    Tag,
    Record(Vec<Lowercase>),
    Guard,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub struct TagId(pub TagIdIntType);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ctor {
    pub name: TagName,
    pub tag_id: TagId,
    pub arity: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Anything,
    Literal(Literal),
    Ctor(Union, TagId, std::vec::Vec<Pattern>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Int(i128),
    Bit(bool),
    Byte(u8),
    Float(u64),
    Decimal(RocDec),
    Str(Box<str>),
}

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
                    name: TagName::Global("#Record".into()),
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
                Union::newtype_wrapper(tag_name.clone(), arguments.len()),
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
    }
}

/// Error

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    Incomplete(Region, Context, Vec<Pattern>),
    Redundant {
        overall_region: Region,
        branch_region: Region,
        index: Index,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Context {
    BadArg,
    BadDestruct,
    BadCase,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Guard {
    HasGuard,
    NoGuard,
}

/// Check

pub fn check(
    region: Region,
    patterns: &[(Located<crate::ir::Pattern>, Guard)],
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

pub fn check_patterns<'a>(
    region: Region,
    context: Context,
    patterns: &[(Located<crate::ir::Pattern<'a>>, Guard)],
    errors: &mut Vec<Error>,
) {
    match to_nonredundant_rows(region, patterns) {
        Err(err) => errors.push(err),
        Ok(matrix) => {
            let bad_patterns = is_exhaustive(&matrix, 1);
            if !bad_patterns.is_empty() {
                // TODO i suspect this is like a concat in in practice? code below can panic
                // if this debug_assert! ever fails, the theory is disproven
                debug_assert!(
                    bad_patterns.iter().map(|v| v.len()).sum::<usize>() == bad_patterns.len()
                );
                let heads = bad_patterns.into_iter().map(|mut v| v.remove(0)).collect();
                errors.push(Error::Incomplete(region, context, heads));
            }
        }
    }
}

/// EXHAUSTIVE PATTERNS

/// INVARIANTS:
///
///   The initial rows "matrix" are all of length 1
///   The initial count of items per row "n" is also 1
///   The resulting rows are examples of missing patterns
fn is_exhaustive(matrix: &PatternMatrix, n: usize) -> PatternMatrix {
    if matrix.is_empty() {
        vec![std::iter::repeat(Anything).take(n).collect()]
    } else if n == 0 {
        vec![]
    } else {
        let ctors = collect_ctors(matrix);
        let num_seen = ctors.len();

        if num_seen == 0 {
            let new_matrix = matrix
                .iter()
                .filter_map(specialize_row_by_anything)
                .collect();
            let mut rest = is_exhaustive(&new_matrix, n - 1);

            for row in rest.iter_mut() {
                row.push(Anything);
            }

            rest
        } else {
            let alts = ctors.iter().next().unwrap().1;

            let alt_list = &alts.alternatives;
            let num_alts = alt_list.len();

            if num_seen < num_alts {
                let new_matrix = matrix
                    .iter()
                    .filter_map(specialize_row_by_anything)
                    .collect();
                let rest: Vec<Vec<Pattern>> = is_exhaustive(&new_matrix, n - 1);

                let last: _ = alt_list
                    .iter()
                    .filter_map(|r| is_missing(alts.clone(), &ctors, r));

                let mut result = Vec::new();

                for last_option in last {
                    for mut row in rest.clone() {
                        row.push(last_option.clone());

                        result.push(row);
                    }
                }

                result
            } else {
                let is_alt_exhaustive = |Ctor { arity, tag_id, .. }| {
                    let new_matrix = matrix
                        .iter()
                        .filter_map(|r| specialize_row_by_ctor(tag_id, arity, r))
                        .collect();
                    let rest: Vec<Vec<Pattern>> = is_exhaustive(&new_matrix, arity + n - 1);

                    let mut result = Vec::with_capacity(rest.len());
                    for row in rest {
                        result.push(recover_ctor(alts.clone(), tag_id, arity, row));
                    }

                    result
                };

                alt_list
                    .iter()
                    .cloned()
                    .map(is_alt_exhaustive)
                    .flatten()
                    .collect()
            }
        }
    }
}

fn is_missing<T>(union: Union, ctors: &MutMap<TagId, T>, ctor: &Ctor) -> Option<Pattern> {
    let Ctor { arity, tag_id, .. } = ctor;

    if ctors.contains_key(tag_id) {
        None
    } else {
        let anythings = std::iter::repeat(Anything).take(*arity).collect();
        Some(Pattern::Ctor(union, *tag_id, anythings))
    }
}

fn recover_ctor(
    union: Union,
    tag_id: TagId,
    arity: usize,
    mut patterns: Vec<Pattern>,
) -> Vec<Pattern> {
    let mut rest = patterns.split_off(arity);
    let args = patterns;

    rest.push(Ctor(union, tag_id, args));

    rest
}

/// REDUNDANT PATTERNS

/// INVARIANT: Produces a list of rows where (forall row. length row == 1)
fn to_nonredundant_rows(
    overall_region: Region,
    patterns: &[(Located<crate::ir::Pattern>, Guard)],
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
                    name: TagName::Global("#Guard".into()),
                    arity: 2,
                }],
            };

            vec![Pattern::Ctor(
                union,
                tag_id,
                vec![simplify(&loc_pat.value), guard_pattern],
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
                index: Index::zero_based(checked_rows.len()),
            });
        }
    }

    Ok(checked_rows)
}

/// Check if a new row "vector" is useful given previous rows "matrix"
fn is_useful(mut old_matrix: PatternMatrix, mut vector: Row) -> bool {
    let mut matrix = Vec::with_capacity(old_matrix.len());

    // this loop ping-pongs the rows between old_matrix and matrix
    'outer: loop {
        match vector.pop() {
            _ if old_matrix.is_empty() => {
                // No rows are the same as the new vector! The vector is useful!
                break true;
            }
            None => {
                // There is nothing left in the new vector, but we still have
                // rows that match the same things. This is not a useful vector!
                break false;
            }
            Some(first_pattern) => {
                // NOTE: if there are bugs in this code, look at the ordering of the row/matrix

                match first_pattern {
                    // keep checking rows that start with this Ctor or Anything
                    Ctor(_, id, args) => {
                        specialize_row_by_ctor2(id, args.len(), &mut old_matrix, &mut matrix);

                        std::mem::swap(&mut old_matrix, &mut matrix);

                        vector.extend(args);
                    }

                    Anything => {
                        // check if all alternatives appear in matrix
                        match is_complete(&old_matrix) {
                            Complete::No => {
                                // This Anything is useful because some Ctors are missing.
                                // But what if a previous row has an Anything?
                                // If so, this one is not useful.
                                for mut row in old_matrix.drain(..) {
                                    if let Some(Anything) = row.pop() {
                                        matrix.push(row);
                                    }
                                }

                                std::mem::swap(&mut old_matrix, &mut matrix);
                            }
                            Complete::Yes(alternatives) => {
                                // All Ctors are covered, so this Anything is not needed for any
                                // of those. But what if some of those Ctors have subpatterns
                                // that make them less general? If so, this actually is useful!
                                for alternative in alternatives {
                                    let Ctor { arity, tag_id, .. } = alternative;

                                    let mut old_matrix = old_matrix.clone();
                                    let mut matrix = vec![];
                                    specialize_row_by_ctor2(
                                        tag_id,
                                        arity,
                                        &mut old_matrix,
                                        &mut matrix,
                                    );

                                    let mut vector = vector.clone();
                                    vector.extend(std::iter::repeat(Anything).take(arity));

                                    if is_useful(matrix, vector) {
                                        break 'outer true;
                                    }
                                }

                                break false;
                            }
                        }
                    }

                    Literal(literal) => {
                        // keep checking rows that start with this Literal or Anything

                        for mut row in old_matrix.drain(..) {
                            let head = row.pop();
                            let patterns = row;

                            match head {
                                Some(Literal(lit)) => {
                                    if lit == literal {
                                        matrix.push(patterns);
                                    } else {
                                        // do nothing
                                    }
                                }
                                Some(Anything) => matrix.push(patterns),

                                Some(Ctor(_, _, _)) => panic!(
                                    r#"Compiler bug! After type checking, constructors and literals should never align in pattern match exhaustiveness checks."#
                                ),

                                None => panic!(
                                    "Compiler error! Empty matrices should not get specialized."
                                ),
                            }
                        }
                        std::mem::swap(&mut old_matrix, &mut matrix);
                    }
                }
            }
        }
    }
}

/// INVARIANT: (length row == N) ==> (length result == arity + N - 1)
fn specialize_row_by_ctor2(
    tag_id: TagId,
    arity: usize,
    old_matrix: &mut PatternMatrix,
    matrix: &mut PatternMatrix,
) {
    for mut row in old_matrix.drain(..) {
        let head = row.pop();
        let mut patterns = row;

        match head {
        Some(Ctor(_, id, args)) =>
            if id == tag_id {
                patterns.extend(args);
                matrix.push(patterns);
            } else {
                // do nothing 
            }
        Some(Anything) => {
            // TODO order!
            patterns.extend(std::iter::repeat(Anything).take(arity));
            matrix.push(patterns);
            }
        Some(Literal(_)) => panic!( "Compiler bug! After type checking, constructors and literal should never align in pattern match exhaustiveness checks."),
        None => panic!("Compiler error! Empty matrices should not get specialized."),
    }
    }
}

/// INVARIANT: (length row == N) ==> (length result == arity + N - 1)
fn specialize_row_by_ctor(tag_id: TagId, arity: usize, row: &Row) -> Option<Row> {
    let mut row = row.clone();

    let head = row.pop();
    let patterns = row;

    match head {
        Some(Ctor(_, id, args)) => {
            if id == tag_id {
                // TODO order!
                let mut new_patterns = Vec::new();
                new_patterns.extend(args);
                new_patterns.extend(patterns);
                Some(new_patterns)
            } else {
                None
            }
        }
        Some(Anything) => {
            // TODO order!
            let new_patterns = std::iter::repeat(Anything)
                .take(arity)
                .chain(patterns)
                .collect();
            Some(new_patterns)
        }
        Some(Literal(_)) => unreachable!(
            r#"Compiler bug! After type checking, a constructor can never align with a literal: that should be a type error!"#
        ),
        None => panic!("Compiler error! Empty matrices should not get specialized."),
    }
}

/// INVARIANT: (length row == N) ==> (length result == N-1)
fn specialize_row_by_anything(row: &Row) -> Option<Row> {
    let mut row = row.clone();

    match row.pop() {
        Some(Anything) => Some(row),
        _ => None,
    }
}

/// ALL CONSTRUCTORS ARE PRESENT?

pub enum Complete {
    Yes(Vec<Ctor>),
    No,
}

fn is_complete(matrix: &PatternMatrix) -> Complete {
    let ctors = collect_ctors(matrix);
    let length = ctors.len();
    let mut it = ctors.into_iter();

    match it.next() {
        None => Complete::No,
        Some((_, Union { alternatives, .. })) => {
            if length == alternatives.len() {
                Complete::Yes(alternatives)
            } else {
                Complete::No
            }
        }
    }
}

/// COLLECT CTORS

type RefPatternMatrix = [Vec<Pattern>];
type PatternMatrix = Vec<Vec<Pattern>>;
type Row = Vec<Pattern>;

fn collect_ctors(matrix: &RefPatternMatrix) -> MutMap<TagId, Union> {
    let mut ctors = MutMap::default();

    for row in matrix {
        if let Some(Ctor(union, id, _)) = row.get(row.len() - 1) {
            ctors.insert(*id, union.clone());
        }
    }

    ctors
}
