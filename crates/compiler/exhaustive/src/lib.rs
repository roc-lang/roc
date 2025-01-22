//! Exhaustiveness checking, based on [Warnings for pattern matching](http://moscova.inria.fr/~maranget/papers/warn/warn.pdf)
//! (Luc Maranget, 2007).

use roc_collections::all::{HumanIndex, MutMap};
use roc_error_macros::internal_error;
use roc_module::{
    ident::{Lowercase, TagIdIntType, TagName},
    symbol::Symbol,
};
use roc_problem::Severity;
use roc_region::all::Region;

use self::Pattern::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Union {
    pub alternatives: Vec<Ctor>,
    pub render_as: RenderAs,
}

impl Union {
    pub fn newtype_wrapper(name: CtorName, arity: usize) -> Self {
        let alternatives = vec![Ctor {
            name,
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
    Opaque,
    Record {
        fields: Vec<Lowercase>,
        /// The first option is whether there is a spread,
        /// the second is whether the spread has a subpattern
        opt_spread: Box<Option<Option<RenderAs>>>,
    },
    Tuple,
    Guard,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub struct TagId(pub TagIdIntType);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CtorName {
    Tag(TagName),
    Opaque(Symbol),
}

impl CtorName {
    pub fn is_tag(&self, tag_name: &TagName) -> bool {
        match self {
            Self::Tag(test) => test == tag_name,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ctor {
    pub name: CtorName,
    pub tag_id: TagId,
    pub arity: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Anything,
    Literal(Literal),
    Ctor(Union, TagId, std::vec::Vec<Pattern>),
    List(ListArity, std::vec::Vec<Pattern>),
}

/// The arity of list pattern.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ListArity {
    /// A list pattern of an exact size.
    Exact(usize),
    /// A list pattern matching a variable size, where `Slice(before, after)` refers to the number
    /// of elements that must be present before and after the variable rest pattern, respectively.
    ///
    /// For example,
    ///   [..] => Slice(0, 0)
    ///   [A, .., B] => Slice(1, 1)
    ///   [A, B, ..] => Slice(2, 0)
    ///   [.., A, B] => Slice(0, 2)
    Slice(usize, usize),
}

impl ListArity {
    /// The trivially-exhaustive list pattern `[..]`
    const ANY: ListArity = ListArity::Slice(0, 0);

    pub fn min_len(&self) -> usize {
        match self {
            ListArity::Exact(n) => *n,
            ListArity::Slice(l, r) => l + r,
        }
    }

    /// Could this list pattern include list pattern arity `other`?
    fn covers_arities_of(&self, other: &Self) -> bool {
        self.covers_length(other.min_len())
    }

    pub fn covers_length(&self, length: usize) -> bool {
        match self {
            ListArity::Exact(l) => {
                // [_, _, _] can only cover [_, _, _]
                *l == length
            }
            ListArity::Slice(head, tail) => {
                // [_, _, .., _] can cover infinite arities >=3 , including
                // [_, _, .., _], [_, .., _, _], [_, _, .., _, _], [_, _, _, .., _, _], and so on
                head + tail <= length
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Int([u8; 16]),
    U128([u8; 16]),
    Bit(bool),
    Byte(u8),
    /// Stores the float bits
    Float(u64),
    Decimal([u8; 16]),
    Str(Box<str>),
}

/// Error

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    Incomplete(Region, Context, Vec<Pattern>),
    Redundant {
        overall_region: Region,
        branch_region: Region,
        index: HumanIndex,
    },
    Unmatchable {
        overall_region: Region,
        branch_region: Region,
        index: HumanIndex,
    },
}

impl Error {
    pub fn severity(&self) -> Severity {
        use Severity::*;
        match self {
            Error::Incomplete(..) => RuntimeError,
            Error::Redundant { .. } => Warning,
            Error::Unmatchable { .. } => Warning,
        }
    }

    pub fn region(&self) -> Region {
        match self {
            Error::Incomplete(region, _, _) => *region,
            Error::Redundant { branch_region, .. } => *branch_region,
            Error::Unmatchable { branch_region, .. } => *branch_region,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Context {
    BadArg,
    BadDestruct,
    BadCase,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Guard {
    HasGuard,
    NoGuard,
}

/// Check

pub fn check(
    region: Region,
    context: Context,
    matrix: Vec<Vec<Pattern>>,
) -> Result<(), Vec<Error>> {
    let mut errors = Vec::new();
    let bad_patterns = is_exhaustive(&matrix, 1);
    if !bad_patterns.is_empty() {
        // TODO i suspect this is like a concat in in practice? code below can panic
        // if this debug_assert! ever fails, the theory is disproven
        debug_assert!(bad_patterns.iter().map(|v| v.len()).sum::<usize>() == bad_patterns.len());
        let heads = bad_patterns.into_iter().map(|mut v| v.remove(0)).collect();
        errors.push(Error::Incomplete(region, context, heads));
        return Err(errors);
    }
    Ok(())
}

/// EXHAUSTIVE PATTERNS

/// INVARIANTS:
///
///   The initial rows "matrix" are all of length 1
///   The initial count of items per row "n" is also 1
///   The resulting rows are examples of missing patterns
fn is_exhaustive(matrix: &RefPatternMatrix, n: usize) -> PatternMatrix {
    let ctors = if matrix.is_empty() {
        return vec![std::iter::repeat(Anything).take(n).collect()];
    } else if n == 0 {
        return vec![];
    } else {
        collect_ctors(matrix)
    };

    match ctors {
        CollectedCtors::NonExhaustiveAny => {
            let new_matrix: Vec<_> = matrix
                .iter()
                .filter_map(|row| specialize_row_by_anything(row))
                .collect();
            let mut rest = is_exhaustive(&new_matrix, n - 1);

            for row in rest.iter_mut() {
                row.push(Anything);
            }

            rest
        }
        CollectedCtors::Ctors(ctors) => {
            debug_assert!(!ctors.is_empty());

            let num_seen = ctors.len();
            let alts = ctors.iter().next().unwrap().1;

            let alt_list = &alts.alternatives;
            let num_alts = alt_list.len();

            if num_seen < num_alts {
                let new_matrix: Vec<_> = matrix
                    .iter()
                    .filter_map(|row| specialize_row_by_anything(row))
                    .collect();
                let rest: Vec<Vec<Pattern>> = is_exhaustive(&new_matrix, n - 1);

                let last = alt_list
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
                    let new_matrix: Vec<_> = matrix
                        .iter()
                        .filter_map(|r| specialize_row_by_ctor(tag_id, arity, r.to_owned()))
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
                    .flat_map(is_alt_exhaustive)
                    .collect()
            }
        }
        CollectedCtors::NonExhaustiveList(alt_lists) => {
            let is_alt_exhaustive = |arity: ListArity| {
                let new_matrix: Vec<_> = matrix
                    .iter()
                    .filter_map(|row| specialize_row_by_list(arity, row.to_owned()))
                    .collect();

                let rest = is_exhaustive(&new_matrix, arity.min_len() + n - 1);

                rest.into_iter()
                    .map(move |row_not_covered| recover_list(arity, row_not_covered))
            };

            alt_lists.into_iter().flat_map(is_alt_exhaustive).collect()
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
    let args = patterns.split_off(patterns.len() - arity);
    let mut rest = patterns;

    rest.push(Ctor(union, tag_id, args));

    rest
}

fn recover_list(arity: ListArity, mut patterns: Vec<Pattern>) -> Vec<Pattern> {
    let list_elems = patterns.split_off(patterns.len() - arity.min_len());
    let mut rest = patterns;

    rest.push(List(arity, list_elems));

    rest
}

/// Check if a new row "vector" is useful given previous rows "matrix"
pub fn is_useful(mut old_matrix: PatternMatrix, mut vector: Row) -> bool {
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
                        specialize_matrix_by_ctor(id, args.len(), &mut old_matrix, &mut matrix);

                        std::mem::swap(&mut old_matrix, &mut matrix);

                        vector.extend(args);
                    }

                    List(arity, args) => {
                        // Check if there any specialized constructor of this list pattern
                        // that is useful.
                        let spec_list_ctors = build_list_ctors_covering_patterns(
                            arity,
                            filter_matrix_list_ctors(&old_matrix),
                        );
                        debug_assert!(!spec_list_ctors.is_empty());

                        if spec_list_ctors.len() == 1 {
                            specialize_matrix_by_list(
                                spec_list_ctors[0],
                                &mut old_matrix,
                                &mut matrix,
                            );

                            std::mem::swap(&mut old_matrix, &mut matrix);

                            vector.extend(args);
                        } else {
                            // TODO turn this into an iteration over the outer loop rather than bouncing
                            for list_ctor in spec_list_ctors {
                                let mut old_matrix = old_matrix.clone();
                                let mut spec_matrix = Vec::with_capacity(old_matrix.len());

                                specialize_matrix_by_list(
                                    list_ctor,
                                    &mut old_matrix,
                                    &mut spec_matrix,
                                );

                                let mut vector = vector.clone();
                                specialize_row_with_polymorphic_list(
                                    &mut vector,
                                    &args,
                                    arity,
                                    list_ctor,
                                );

                                if is_useful(spec_matrix, vector) {
                                    return true;
                                }
                            }

                            return false;
                        }
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
                                    specialize_matrix_by_ctor(
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

                                Some(List(..)) => internal_error!("After type checking, lists and literals should never align in exhaustiveness checking"),

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

// Specialize rows in the matrix that match a list's constructor(s).
//
// See the docs on [build_list_ctors_covering_patterns] for more information on how list
// constructors are built up.
fn specialize_matrix_by_list(
    spec_arity: ListArity,
    old_matrix: &mut PatternMatrix,
    spec_matrix: &mut PatternMatrix,
) {
    for row in old_matrix.drain(..) {
        if let Some(spec_row) = specialize_row_by_list(spec_arity, row) {
            spec_matrix.push(spec_row);
        }
    }
}

fn specialize_row_with_polymorphic_list(
    row: &mut Vec<Pattern>,
    list_element_patterns: &[Pattern],
    polymorphic_list_ctor: ListArity,
    specialized_list_ctor: ListArity,
) {
    let min_len = specialized_list_ctor.min_len();
    if list_element_patterns.len() > min_len {
        row.extend(list_element_patterns.iter().cloned());
    }

    let (patterns_before, patterns_after) = match polymorphic_list_ctor {
        ListArity::Slice(before, after) => (
            &list_element_patterns[..before],
            &list_element_patterns[list_element_patterns.len() - after..],
        ),
        ListArity::Exact(_) => (list_element_patterns, &[] as &[Pattern]),
    };

    let middle_any_patterns_needed =
        specialized_list_ctor.min_len() - polymorphic_list_ctor.min_len();
    let middle_patterns = std::iter::repeat(Anything).take(middle_any_patterns_needed);

    row.extend(
        (patterns_before.iter().cloned())
            .chain(middle_patterns)
            .chain(patterns_after.iter().cloned()),
    );
}

// Specialize a row that matches a list's constructor(s).
//
// See the docs on [build_list_ctors_covering_patterns] for more information on how list
// constructors are built up.
fn specialize_row_by_list(spec_arity: ListArity, mut row: Row) -> Option<Row> {
    let head = row.pop();
    let mut spec_patterns = row;

    match head {
        Some(List(this_arity, args)) => {
            if this_arity.covers_arities_of(&spec_arity) {
                // This pattern covers the constructor we are specializing, so add on the
                // specialized fields of this pattern relative to the given constructor.
                if spec_arity.min_len() != this_arity.min_len() {
                    // This list pattern covers the list we are specializing, so it must be
                    // a variable-length slice, i.e. of the form `[before, .., after]`.
                    //
                    // Hence, the list we're specializing for must have at least a larger minimum length.
                    // So we fill the middle part with enough wildcards to reach the length of
                    // list constructor we're specializing for.
                    debug_assert!(spec_arity.min_len() > this_arity.min_len());
                    match this_arity {
                        ListArity::Exact(_) => internal_error!("exact-sized lists cannot cover lists of other minimum length"),
                        ListArity::Slice(before, after) => {
                            let before = &args[..before];
                            let after = &args[this_arity.min_len() - after..];
                            let num_extra_wildcards = spec_arity.min_len() - this_arity.min_len();
                            let extra_wildcards = std::iter::repeat(&Anything).take(num_extra_wildcards);

                            let new_pats = (before.iter().chain(extra_wildcards).chain(after)).cloned();

                            spec_patterns.extend(new_pats);
                        }
                    }
                } else {
                    debug_assert_eq!(this_arity.min_len(), spec_arity.min_len());

                    spec_patterns.extend(args);
                }

                Some(spec_patterns)
            } else {
                None
            }
        }
        Some(Anything) => {
            // The specialized fields for a `Anything` pattern with a list constructor is just
            // `Anything` repeated for the number of times we want to see the list pattern.
            spec_patterns.extend(std::iter::repeat(Anything).take(spec_arity.min_len()));
            Some(spec_patterns)
        }
        Some(Ctor(..)) => internal_error!("After type checking, lists and constructors should never align in exhaustiveness checking"),
        Some(Literal(..)) => internal_error!("After type checking, lists and literals should never align in exhaustiveness checking"),
        None => internal_error!("Empty matrices should not get specialized"),
    }
}

/// INVARIANT: (length row == N) ==> (length result == arity + N - 1)
fn specialize_matrix_by_ctor(
    tag_id: TagId,
    arity: usize,
    old_matrix: &mut PatternMatrix,
    matrix: &mut PatternMatrix,
) {
    for row in old_matrix.drain(..) {
        if let Some(spec_row) = specialize_row_by_ctor(tag_id, arity, row) {
            matrix.push(spec_row);
        }
    }
}

/// INVARIANT: (length row == N) ==> (length result == arity + N - 1)
fn specialize_row_by_ctor(tag_id: TagId, arity: usize, mut row: Row) -> Option<Row> {
    let head = row.pop();
    let mut spec_patterns = row;

    match head {
        Some(Ctor(_, id, args)) => {
            if id == tag_id {
                spec_patterns.extend(args);
                Some(spec_patterns)
            } else {
                None
            }
        }
        Some(Anything) => {
            spec_patterns.extend(std::iter::repeat(Anything).take(arity));
            Some(spec_patterns)
        }
        Some(List(..)) => {
            internal_error!(r#"After type checking, a constructor can never align with a list"#)
        }
        Some(Literal(_)) => internal_error!(
            r#"After type checking, a constructor can never align with a literal: that should be a type error!"#
        ),
        None => internal_error!("Empty matrices should not get specialized."),
    }
}

/// INVARIANT: (length row == N) ==> (length result == N-1)
fn specialize_row_by_anything(row: &RefRow) -> Option<Row> {
    let mut row = row.to_vec();

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

fn is_complete(matrix: &RefPatternMatrix) -> Complete {
    let ctors = collect_ctors(matrix);
    match ctors {
        CollectedCtors::NonExhaustiveAny | CollectedCtors::NonExhaustiveList(_) => Complete::No,
        CollectedCtors::Ctors(ctors) => {
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
    }
}

/// COLLECT CTORS

type RefPatternMatrix = [Vec<Pattern>];
type PatternMatrix = Vec<Vec<Pattern>>;
type RefRow = [Pattern];
type Row = Vec<Pattern>;

enum CollectedCtors {
    NonExhaustiveAny,
    NonExhaustiveList(Vec<ListArity>),
    Ctors(MutMap<TagId, Union>),
}

fn collect_ctors(matrix: &RefPatternMatrix) -> CollectedCtors {
    if matrix.is_empty() {
        return CollectedCtors::NonExhaustiveAny;
    }

    let first_row = &matrix[0];

    if let Some(ctor) = first_row.last() {
        match ctor {
            Anything => CollectedCtors::NonExhaustiveAny,
            Pattern::Literal(_) => CollectedCtors::NonExhaustiveAny,
            List(_, _) => {
                let list_ctors = build_list_ctors_covering_patterns(
                    ListArity::ANY,
                    filter_matrix_list_ctors(matrix),
                );

                CollectedCtors::NonExhaustiveList(list_ctors)
            }
            Pattern::Ctor(_, _, _) => {
                let mut ctors = MutMap::default();

                for row in matrix {
                    if let Some(Ctor(union, id, _)) = row.last() {
                        ctors.insert(*id, union.clone());
                    }
                }

                CollectedCtors::Ctors(ctors)
            }
        }
    } else {
        CollectedCtors::NonExhaustiveAny
    }
}

/// Largely derived from Rust's list-pattern exhaustiveness checking algorithm: https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_build/thir/pattern/usefulness/index.html
/// Dual-licensed under MIT and Apache licenses.
/// Thank you, Rust contributors.
///
/// Calculates the list constructors that are covered by a given [slice constructor][ListArity::Slice],
/// relative to other list constructors matched by a series of patterns.
///
/// This is relevant for both exhaustiveness and redundancy checking; to understand the motivation,
/// let's start with the exhaustiveness checking case.
///
/// # Exhaustiveness Checking
///
/// All list constructors are exausted by the pattern [..], which actually represents the infinite
/// series of constructors
///   []
///   [_]
///   [_, _]
///   ...
///
/// But we don't need to enumerate that infinite series to check if a series of list patterns is exhaustive -
/// we only need to enumerate a finite number of constructors, up to the largest exact-size list
/// pattern not covered by the patterns, or the largest slice pattern covered by the patterns.
///
/// ## Exact-sized patterns
///
/// Say we have patterns
///   [_] -> ..
///   [_, _] -> ..
/// To exhaustiveness-check these patterns, we only need to build the subset of `[..]` constructors
///   []
///   [_]
///   [_, _]
///   [_, _, _, ..]
/// to cover all list constructors that may or may not be matched by the patterns (in this case
/// not, because `[]` is not matched, and the last constructor `[_, _, _, ..]` is not matched).
///
/// We include `[_, _, _, ..]` here because during exhaustiveness checking, we specialize list
/// patterns **by exact size**, not by ranges. That means that is we stopped enumerating the
/// constructors needed at `[_, _, ..]`, when specializing the list patterns against `[_, _, ..]`,
/// we would see that the last pattern `[_, _] -> ..` exhausts it.
///
/// So, in the presence of exact-size constructors, we want to include a slice constructor that is
/// larger than all other exact-size list pattern.
///
/// ## Slice patterns
///
/// Say we have patterns
///   [1] -> ..
///   [2, ..] -> ..
/// now it's enough to just build
///   []
///   [_, ..]
/// as possible constructors, since the last constructor `[_, ..]` will specialize both patterns to
///   [1] -> ..
///   [2] -> ..
/// and if these patterns are exhaustive w.r.t. their arguments (`1` and `2`, which they are not,
/// since number literals are not exhaustive), then the whole pattern must be exhaustive, since the
/// largest slice constructor `[_, ..]` will cover the remaining infinite number of list constructors.
///
/// You can see that this holds with slice constructors that match elements at their head and tail
/// as well:
///   [{}, ..] -> ..
///   [.., {}] -> ..
/// Here again it's enough to just build the constructors [] and [_, ..] to match against -
/// notice that above slices of arity `1`, the patterns above do not provide any more information,
/// since they match any additional elements at the tail and head, respectively.
///
/// So, if they are exhaustive at arity `1`, they must be exhaustive at any higher arity.
///
/// In fact, in this case, if we are matching against `List {}`, the second pattern redundant!
///
/// # Redundancy checking
///
/// Redundancy checking (in general, and for list patterns) is the same as exhaustiveness checking,
/// except that instead of checking whether `[..]` is covered by all patterns, we want to check if
/// the list constructor of a pattern introduces any more information than previous patterns we've
/// seen.
///
/// Let's say we're redundancy checking the pattern marked by `*`
///     [] -> ..
///     [_] -> ..
/// (*) [.., _] -> ..
///
/// The list constructors this pattern introduces are the infinite series [_], [_, _], ...
/// But the only ones relevant, relevant to the patterns we've already seen, are
///   [_]
///   [_, _]
/// (Notice that the enumeration algorithm is the same as for `[..]` in the presence of exact-size
/// slices, just that the starting size differs - due to the tail matched by this pattern)
///
/// During checking we'll see that the `[_, _]` pattern is not already covered, so `[.., _]` is in
/// fact not redundant.
///
/// On the other hand, suppose we have
///     [] -> ..
///     [_, ..] -> ..
/// (*) [.., _] -> ..
///
/// Again enumerating the relevant constructors of `[.., _]` relative to the other patterns, we find
/// them to be
///   []
///   [.., _]
/// the first is already matched by the first pattern `[] -> ..`, and the latter specialized to
/// `[_]`, which in fact is covered by the second pattern `[_, ..] -> ..`. So the pattern marked by (*)
/// is indeed redundant.
///
/// # All together
///
/// So the idea to cover the infinite # of list constructors enumerated by a [slice][ListArity::Slice],
/// while specializing to the constructors that the user has provided, is as follows:
///   - Build [exact][ListArity::Exact] constructor variants for everything up to the max slice
///     constructor size, L.
///   - Then, the infinite # of list constructors is covered by the [0..L) exact-size constructors, and
///     the last slice constructor, that covers size [L..∞).
///
/// If we might only see [exact][ListArity::Exact] constructors along the way, we want to pick the
/// max slice size L that is larger than all of those exact size constructors.
///
/// But for slice constructors, we can just pick the largest slice, since that will cover slices of
/// that size, and any larger size.
///
/// Putting that together, we calculate L via
///
///   L = max(max_exact_len + 1, max_prefix_len + max_suffix_len)
fn build_list_ctors_covering_patterns(
    list_arity: ListArity,
    list_pattern_arities: impl IntoIterator<Item = ListArity>,
) -> std::vec::Vec<ListArity> {
    match list_arity {
        ListArity::Exact(_) => {
            // Exact-size lists can only cover themselves..
            vec![list_arity]
        }
        ListArity::Slice(prefix_len, suffix_len) => {
            let min_len = prefix_len + suffix_len;

            let mut max_exact_len = 0;
            let mut max_prefix_len = prefix_len;
            let mut max_suffix_len = suffix_len;

            for arity in list_pattern_arities {
                match arity {
                    ListArity::Exact(n) => max_exact_len = max_exact_len.max(n),
                    ListArity::Slice(prefix, suffix) => {
                        max_prefix_len = max_prefix_len.max(prefix);
                        max_suffix_len = max_suffix_len.max(suffix);
                    }
                }
            }

            let (inf_cover_prefix, inf_cover_suffix) = {
                if max_exact_len + 1 >= max_prefix_len + max_suffix_len {
                    max_prefix_len = max_exact_len + 1 - max_suffix_len;
                }
                (max_prefix_len, max_suffix_len)
            };
            let l = inf_cover_prefix + inf_cover_suffix;

            let exact_size_lists = (min_len..l) // exclusive
                .map(ListArity::Exact);

            exact_size_lists
                .chain([ListArity::Slice(inf_cover_prefix, inf_cover_suffix)])
                .collect()
        }
    }
}

fn filter_matrix_list_ctors(matrix: &RefPatternMatrix) -> impl Iterator<Item = ListArity> + '_ {
    matrix.iter().filter_map(|ctor| match ctor.last() {
        Some(List(ar, _)) => Some(*ar),
        _ => None,
    })
}
