use roc_collections::all::MutMap;
use roc_module::ident::TagName;
use roc_region::all::{Located, Region};

use self::Pattern::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Union {
    pub alternatives: Vec<Ctor>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ctor {
    pub name: TagName,
    pub arity: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Anything,
    Literal(Literal),
    Ctor(Union, TagName, std::vec::Vec<Pattern>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Int(i64),
    Bit(bool),
    Byte(u8),
    Float(u64),
    Str(Box<str>),
}

fn simplify<'a>(pattern: &crate::expr::Pattern<'a>) -> Pattern {
    use crate::expr::Pattern::*;

    match pattern {
        IntLiteral(v) => Literal(Literal::Int(*v)),
        FloatLiteral(v) => Literal(Literal::Float(*v)),
        StrLiteral(v) => Literal(Literal::Str(v.clone())),

        // To make sure these are exhaustive, we have to "fake" a union here
        // TODO: use the hash or some other integer to discriminate between constructors
        BitLiteral(b) => {
            let union = Union {
                alternatives: vec![
                    Ctor {
                        name: TagName::Global("False".into()),
                        arity: 0,
                    },
                    Ctor {
                        name: TagName::Global("True".into()),
                        arity: 0,
                    },
                ],
            };

            Ctor(union, TagName::Global(format!("{}", b).into()), vec![])
        }
        EnumLiteral { tag_id, enum_size } => {
            let alternatives = (0..*enum_size)
                .map(|id| Ctor {
                    name: TagName::Global(format!("{}", id).into()),
                    arity: 0,
                })
                .collect();

            let union = Union { alternatives };
            Ctor(union, TagName::Global(format!("{}", tag_id).into()), vec![])
        }

        Underscore => Anything,
        Identifier(_) => Anything,
        RecordDestructure { .. } => {
            // TODO we must check the guard conditions!
            Anything
        }

        Shadowed(_region, _ident) => {
            // Treat as an Anything
            // code-gen will make a runtime error out of the branch
            Anything
        }
        UnsupportedPattern(_region) => {
            // Treat as an Anything
            // code-gen will make a runtime error out of the branch
            Anything
        }

        AppliedTag {
            tag_name,
            arguments,
            union,
            ..
        } => {
            let simplified_args: std::vec::Vec<_> =
                arguments.iter().map(|v| simplify(&v.0)).collect();
            Ctor(union.clone(), tag_name.clone(), simplified_args)
        }
    }
}

/// Error

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    Incomplete(Region, Context, Vec<Pattern>),
    Redundant(Region, Region, usize),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Context {
    BadArg,
    BadDestruct,
    BadCase,
}

/// Check

pub fn check<'a>(
    region: Region,
    patterns: &[Located<crate::expr::Pattern<'a>>],
) -> Result<(), Vec<Error>> {
    let mut errors = Vec::new();
    check_patterns(region, Context::BadArg, patterns, &mut errors);

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn check_patterns<'a>(
    region: Region,
    context: Context,
    patterns: &[Located<crate::expr::Pattern<'a>>],
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
                    .filter_map(|r| is_missing(alts.clone(), ctors.clone(), r));

                let mut result = Vec::new();

                for last_option in last {
                    for mut row in rest.clone() {
                        row.push(last_option.clone());

                        result.push(row);
                    }
                }

                result
            } else {
                let is_alt_exhaustive = |Ctor { name, arity }| {
                    let new_matrix = matrix
                        .iter()
                        .filter_map(|r| specialize_row_by_ctor(&name, arity, r))
                        .collect();
                    let rest: Vec<Vec<Pattern>> = is_exhaustive(&new_matrix, arity + n - 1);

                    let mut result = Vec::with_capacity(rest.len());
                    for row in rest {
                        result.push(recover_ctor(alts.clone(), name.clone(), arity, row));
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

fn is_missing<T>(union: Union, ctors: MutMap<TagName, T>, ctor: &Ctor) -> Option<Pattern> {
    let Ctor { name, arity, .. } = ctor;

    if ctors.contains_key(&name) {
        None
    } else {
        let anythings = std::iter::repeat(Anything).take(*arity).collect();
        Some(Pattern::Ctor(union, name.clone(), anythings))
    }
}

fn recover_ctor(
    union: Union,
    tag_name: TagName,
    arity: usize,
    mut patterns: Vec<Pattern>,
) -> Vec<Pattern> {
    let mut rest = patterns.split_off(arity);
    let args = patterns;

    rest.push(Ctor(union, tag_name, args));

    rest
}

/// REDUNDANT PATTERNS

/// INVARIANT: Produces a list of rows where (forall row. length row == 1)
fn to_nonredundant_rows<'a>(
    overall_region: Region,
    patterns: &[Located<crate::expr::Pattern<'a>>],
) -> Result<Vec<Vec<Pattern>>, Error> {
    let mut checked_rows = Vec::with_capacity(patterns.len());

    for loc_pat in patterns {
        let region = loc_pat.region;

        let next_row = vec![simplify(&loc_pat.value)];

        if is_useful(&checked_rows, &next_row) {
            checked_rows.push(next_row);
        } else {
            return Err(Error::Redundant(
                overall_region,
                region,
                checked_rows.len() + 1,
            ));
        }
    }

    Ok(checked_rows)
}

/// Check if a new row "vector" is useful given previous rows "matrix"
fn is_useful(matrix: &PatternMatrix, vector: &Row) -> bool {
    if matrix.is_empty() {
        // No rows are the same as the new vector! The vector is useful!
        true
    } else if vector.is_empty() {
        // There is nothing left in the new vector, but we still have
        // rows that match the same things. This is not a useful vector!
        false
    } else {
        let mut vector = vector.clone();
        let first_pattern = vector.remove(0);
        let patterns = vector;

        match first_pattern {
            // keep checking rows that start with this Ctor or Anything
            Ctor(_, name, args) => {
                let new_matrix: Vec<_> = matrix
                    .iter()
                    .filter_map(|r| specialize_row_by_ctor(&name, args.len(), r))
                    .collect();

                let mut new_row = Vec::new();
                new_row.extend(args);
                new_row.extend(patterns);

                is_useful(&new_matrix, &new_row)
            }

            Anything => {
                // check if all alts appear in matrix
                match is_complete(matrix) {
                    Complete::No => {
                        // This Anything is useful because some Ctors are missing.
                        // But what if a previous row has an Anything?
                        // If so, this one is not useful.
                        let new_matrix: Vec<_> = matrix
                            .iter()
                            .filter_map(|r| specialize_row_by_anything(r))
                            .collect();

                        is_useful(&new_matrix, &patterns)
                    }
                    Complete::Yes(alts) => {
                        // All Ctors are covered, so this Anything is not needed for any
                        // of those. But what if some of those Ctors have subpatterns
                        // that make them less general? If so, this actually is useful!
                        let is_useful_alt = |Ctor { name, arity, .. }| {
                            let new_matrix = matrix
                                .iter()
                                .filter_map(|r| specialize_row_by_ctor(&name, arity, r))
                                .collect();
                            let mut new_row: Vec<Pattern> =
                                std::iter::repeat(Anything).take(arity).collect::<Vec<_>>();

                            new_row.extend(patterns.clone());

                            is_useful(&new_matrix, &new_row)
                        };

                        alts.iter().cloned().any(is_useful_alt)
                    }
                }
            }

            Literal(literal) => {
                // keep checking rows that start with this Literal or Anything
                let new_matrix = matrix
                    .iter()
                    .filter_map(|r| specialize_row_by_literal(&literal, r))
                    .collect();
                is_useful(&new_matrix, &patterns)
            }
        }
    }
}

/// INVARIANT: (length row == N) ==> (length result == arity + N - 1)
fn specialize_row_by_ctor(tag_name: &TagName, arity: usize, row: &Row) -> Option<Row> {
    let mut row = row.clone();

    let head = row.pop();
    let patterns = row;

    match head {
        Some(Ctor(_,name, args)) =>
            if &name == tag_name {
                // TODO order!
                let mut new_patterns = Vec::new();
                new_patterns.extend(args);
                new_patterns.extend(patterns);
                Some(new_patterns)
            } else {
                None
            }
        Some(Anything) => {
            // TODO order!
            let new_patterns =
                    std::iter::repeat(Anything).take(arity).chain(patterns).collect();
            Some(new_patterns)
            }
        Some(Literal(_)) => panic!( "Compiler bug! After type checking, constructors and literal should never align in pattern match exhaustiveness checks."),
        None => panic!("Compiler error! Empty matrices should not get specialized."),
    }
}

/// INVARIANT: (length row == N) ==> (length result == N-1)
fn specialize_row_by_literal(literal: &Literal, row: &Row) -> Option<Row> {
    let mut row = row.clone();

    let head = row.pop();
    let patterns = row;

    match head {
        Some(Literal(lit)) => if &lit == literal { Some(patterns) } else{  None } ,
        Some(Anything) => Some(patterns),

      Some(Ctor(_,_,_)) => panic!( "Compiler bug! After type checking, constructors and literals should never align in pattern match exhaustiveness checks."),

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

    let mut it = ctors.values();

    match it.next() {
        None => Complete::No,
        Some(Union { alternatives, .. }) => {
            if ctors.len() == alternatives.len() {
                Complete::Yes(alternatives.to_vec())
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

fn collect_ctors(matrix: &RefPatternMatrix) -> MutMap<TagName, Union> {
    let mut ctors = MutMap::default();

    for row in matrix {
        if let Some(Ctor(union, name, _)) = row.get(row.len() - 1) {
            ctors.insert(name.clone(), union.clone());
        }
    }

    ctors
}
