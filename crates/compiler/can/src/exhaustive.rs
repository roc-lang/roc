use crate::expr::{self, IntValue, WhenBranch};
use crate::pattern::DestructType;
use roc_collections::all::HumanIndex;
use roc_collections::VecMap;
use roc_error_macros::internal_error;
use roc_exhaustive::{
    is_useful, Ctor, CtorName, Error, Guard, ListArity, Literal, Pattern, RenderAs, TagId, Union,
};
use roc_module::ident::{Lowercase, TagIdIntType, TagName};
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::{
    Content, FlatType, GetSubsSlice, RedundantMark, SortedTagsIterator, Subs, SubsFmtContent,
    Variable,
};
use roc_types::types::{gather_tags_unsorted_iter, AliasKind};

pub use roc_exhaustive::Context as ExhaustiveContext;

pub const GUARD_CTOR: &str = "#Guard";
pub const NONEXHAUSIVE_CTOR: &str = "#Open";

pub struct ExhaustiveSummary {
    pub errors: Vec<Error>,
    pub exhaustive: bool,
    pub redundancies: Vec<RedundantMark>,
}

#[derive(Debug)]
pub struct TypeError;

/// Exhaustiveness-checks [sketched rows][SketchedRows] against an expected type.
///
/// Returns an error if the sketch has a type error, in which case exhautiveness checking will not
/// have been performed.
pub fn check(
    subs: &Subs,
    real_var: Variable,
    sketched_rows: SketchedRows,
    context: ExhaustiveContext,
) -> Result<ExhaustiveSummary, TypeError> {
    let overall_region = sketched_rows.overall_region;
    let mut all_errors = Vec::with_capacity(1);

    let NonRedundantSummary {
        non_redundant_rows,
        errors,
        redundancies,
    } = sketched_rows.reify_to_non_redundant(subs, real_var)?;
    all_errors.extend(errors);

    let exhaustive = match roc_exhaustive::check(overall_region, context, non_redundant_rows) {
        Ok(()) => true,
        Err(errors) => {
            all_errors.extend(errors);
            false
        }
    };

    Ok(ExhaustiveSummary {
        errors: all_errors,
        exhaustive,
        redundancies,
    })
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum SketchedPattern {
    Anything,
    Literal(Literal),
    /// A constructor whose expected union is not yet known.
    /// We'll know the whole union when reifying the sketched pattern against an expected case type.
    Ctor(TagName, Vec<SketchedPattern>),
    KnownCtor(Union, TagId, Vec<SketchedPattern>),
    List(ListArity, Vec<SketchedPattern>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum IndexCtor<'a> {
    /// Index an opaque type. There should be one argument.
    Opaque,
    /// Index a record type. The arguments are the types of the record fields.
    Record(&'a [Lowercase]),
    /// Index a tuple type.
    Tuple,
    /// Index a guard constructor. The arguments are a faux guard pattern, and then the real
    /// pattern being guarded. E.g. `A B if g` becomes Guard { [True, (A B)] }.
    Guard,
    /// Index a tag union with the given tag constructor.
    Tag(&'a TagName),
    /// Index a list type. The argument is the element type.
    List,
}

impl<'a> IndexCtor<'a> {
    fn of_union(un: &'a Union, tag_id: TagId) -> Self {
        let Union {
            alternatives,
            render_as,
        } = un;

        match render_as {
            RenderAs::Tag => {
                let tag_name = alternatives
                    .iter()
                    .find(|ctor| ctor.tag_id == tag_id)
                    .map(|Ctor { name, .. }| match name {
                        CtorName::Tag(tag) => tag,
                        CtorName::Opaque(_) => {
                            internal_error!("tag union should never have opaque alternative")
                        }
                    })
                    .expect("indexable tag ID must be known to alternatives");
                Self::Tag(tag_name)
            }
            RenderAs::Opaque => Self::Opaque,
            RenderAs::Record(fields) => Self::Record(fields),
            RenderAs::Tuple => Self::Tuple,
            RenderAs::Guard => Self::Guard,
        }
    }
}

/// Index a variable as a certain constructor, to get the expected argument types of that constructor.
fn index_var(
    subs: &Subs,
    mut var: Variable,
    ctor: IndexCtor,
    render_as: &RenderAs,
) -> Result<Vec<Variable>, TypeError> {
    if matches!(ctor, IndexCtor::Guard) {
        // `A B if g` becomes Guard { [True, (A B)] }, so the arguments are a bool, and the type
        // of the pattern.
        return Ok(vec![Variable::BOOL, var]);
    }
    loop {
        match subs.get_content_without_compacting(var) {
            Content::FlexVar(_)
            | Content::RigidVar(_)
            | Content::FlexAbleVar(_, _)
            | Content::RigidAbleVar(_, _)
            | Content::LambdaSet(_)
            | Content::ErasedLambda
            | Content::RangedNumber(..)
            | Content::Pure
            | Content::Effectful => return Err(TypeError),
            Content::Error => return Err(TypeError),
            Content::RecursionVar {
                structure,
                opt_name: _,
            } => {
                var = *structure;
            }
            Content::Structure(structure) => match structure {
                FlatType::Func(_, _, _, _) => return Err(TypeError),
                FlatType::EffectfulFunc => return Err(TypeError),
                FlatType::Apply(Symbol::LIST_LIST, args) => {
                    match (subs.get_subs_slice(*args), ctor) {
                        ([elem_var], IndexCtor::List) => {
                            return Ok(vec![*elem_var]);
                        }
                        _ => internal_error!("list types can only be indexed by list patterns"),
                    }
                }
                FlatType::Apply(..) => internal_error!("not an indexable constructor"),
                FlatType::Record(fields, ext) => {
                    let fields_order = match render_as {
                        RenderAs::Record(fields) => fields,
                        _ => internal_error!(
                            "record constructors must always be rendered as records"
                        ),
                    };
                    let iter = fields
                        .unsorted_iterator(subs, *ext)
                        .expect("should not have errors if performing exhautiveness checking");

                    let map: VecMap<_, _> = iter
                        .map(|(name, field)| (name, *field.as_inner()))
                        .collect();

                    let field_types = fields_order
                        .iter()
                        .map(|field| {
                            *map.get(&field)
                                .expect("field must be present during exhautiveness checking")
                        })
                        .collect();

                    return Ok(field_types);
                }
                FlatType::Tuple(elems, ext) => {
                    let elem_types = elems
                        .sorted_iterator(subs, *ext)
                        .map(|(_, elem)| elem)
                        .collect();

                    return Ok(elem_types);
                }
                FlatType::TagUnion(tags, ext) | FlatType::RecursiveTagUnion(_, tags, ext) => {
                    let tag_ctor = match ctor {
                        IndexCtor::Tag(name) => name,
                        _ => {
                            internal_error!("constructor in a tag union must be tag")
                        }
                    };
                    let mut iter = tags.unsorted_iterator(subs, *ext);
                    let opt_vars = iter.find_map(|(tag, vars)| {
                        if tag == tag_ctor {
                            Some(vars.to_vec())
                        } else {
                            None
                        }
                    });
                    let vars = opt_vars.expect("constructor must be known in the indexable type if we are exhautiveness checking");
                    return Ok(vars);
                }
                FlatType::FunctionOrTagUnion(tags, _, _) => {
                    let tag_ctor = match ctor {
                        IndexCtor::Tag(name) => name,
                        _ => {
                            internal_error!("constructor in a tag union must be tag")
                        }
                    };

                    let tags = subs.get_subs_slice(*tags);
                    debug_assert!(tags.contains(tag_ctor), "constructor must be known in the indexable type if we are exhautiveness checking");

                    return Ok(vec![]);
                }
                FlatType::EmptyRecord => {
                    debug_assert!(matches!(ctor, IndexCtor::Record(..)));
                    // If there are optional record fields we don't unify them, but we need to
                    // cover them. Since optional fields correspond to "any" patterns, we can pass
                    // through arbitrary types.
                    let num_fields = match render_as {
                        RenderAs::Record(fields) => fields.len(),
                        _ => internal_error!(
                            "record constructors must always be rendered as records"
                        ),
                    };
                    return Ok(std::iter::repeat(Variable::NULL).take(num_fields).collect());
                }
                FlatType::EmptyTagUnion => {
                    internal_error!("empty tag unions are not indexable")
                }
            },
            Content::Alias(_, _, var, AliasKind::Opaque) => {
                debug_assert!(matches!(ctor, IndexCtor::Opaque));
                return Ok(vec![*var]);
            }
            Content::Alias(_, _, inner, AliasKind::Structural) => {
                var = *inner;
            }
        }
    }
}

impl SketchedPattern {
    fn reify(self, subs: &Subs, real_var: Variable) -> Result<Pattern, TypeError> {
        match self {
            Self::Anything => Ok(Pattern::Anything),
            Self::Literal(lit) => Ok(Pattern::Literal(lit)),
            Self::KnownCtor(union, tag_id, patterns) => {
                let index_ctor = IndexCtor::of_union(&union, tag_id);
                let arg_vars = index_var(subs, real_var, index_ctor, &union.render_as)?;

                debug_assert!(arg_vars.len() == patterns.len());
                let args = (patterns.into_iter())
                    .zip(arg_vars)
                    .map(|(pat, var)| pat.reify(subs, var))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Pattern::Ctor(union, tag_id, args))
            }
            Self::Ctor(tag_name, patterns) => {
                let arg_vars =
                    index_var(subs, real_var, IndexCtor::Tag(&tag_name), &RenderAs::Tag)?;
                let (union, tag_id) = convert_tag(subs, real_var, &tag_name);

                debug_assert!(arg_vars.len() == patterns.len());
                let args = (patterns.into_iter())
                    .zip(arg_vars)
                    .map(|(pat, var)| pat.reify(subs, var))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Pattern::Ctor(union, tag_id, args))
            }
            Self::List(arity, patterns) => {
                let elem_var = index_var(subs, real_var, IndexCtor::List, &RenderAs::Tag)?[0];

                let patterns = patterns
                    .into_iter()
                    .map(|pat| pat.reify(subs, elem_var))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Pattern::List(arity, patterns))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct SketchedRow {
    patterns: Vec<SketchedPattern>,
    region: Region,
    guard: Guard,
    redundant_mark: RedundantMark,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SketchedRows {
    rows: Vec<SketchedRow>,
    overall_region: Region,
}

impl SketchedRows {
    fn reify_to_non_redundant(
        self,
        subs: &Subs,
        real_var: Variable,
    ) -> Result<NonRedundantSummary, TypeError> {
        to_nonredundant_rows(subs, real_var, self)
    }
}

fn sketch_pattern(pattern: &crate::pattern::Pattern) -> SketchedPattern {
    use crate::pattern::Pattern::*;
    use SketchedPattern as SP;

    match pattern {
        As(subpattern, _) => sketch_pattern(&subpattern.value),
        &NumLiteral(_, _, IntValue::I128(n), _) | &IntLiteral(_, _, _, IntValue::I128(n), _) => {
            SP::Literal(Literal::Int(n))
        }
        &NumLiteral(_, _, IntValue::U128(n), _) | &IntLiteral(_, _, _, IntValue::U128(n), _) => {
            SP::Literal(Literal::U128(n))
        }
        &FloatLiteral(_, _, _, f, _) => SP::Literal(Literal::Float(f64::to_bits(f))),
        StrLiteral(v) => SP::Literal(Literal::Str(v.clone())),
        &SingleQuote(_, _, c, _) => SP::Literal(Literal::Byte(c as u8)),
        RecordDestructure { destructs, .. } => {
            let tag_id = TagId(0);
            let mut patterns = std::vec::Vec::with_capacity(destructs.len());
            let mut field_names = std::vec::Vec::with_capacity(destructs.len());

            for Loc {
                value: destruct,
                region: _,
            } in destructs
            {
                field_names.push(destruct.label.clone());

                match &destruct.typ {
                    DestructType::Required | DestructType::Optional(..) => {
                        patterns.push(SP::Anything)
                    }
                    DestructType::Guard(_, guard) => patterns.push(sketch_pattern(&guard.value)),
                }
            }

            let union = Union {
                render_as: RenderAs::Record(field_names),
                alternatives: vec![Ctor {
                    name: CtorName::Tag(TagName("#Record".into())),
                    tag_id,
                    arity: destructs.len(),
                }],
            };

            SP::KnownCtor(union, tag_id, patterns)
        }

        TupleDestructure { destructs, .. } => {
            let tag_id = TagId(0);
            let mut patterns = std::vec::Vec::with_capacity(destructs.len());

            for Loc {
                value: destruct,
                region: _,
            } in destructs
            {
                patterns.push(sketch_pattern(&destruct.typ.1.value));
            }

            let union = Union {
                render_as: RenderAs::Tuple,
                alternatives: vec![Ctor {
                    name: CtorName::Tag(TagName("#Record".into())),
                    tag_id,
                    arity: destructs.len(),
                }],
            };

            SP::KnownCtor(union, tag_id, patterns)
        }

        List {
            patterns,
            list_var: _,
            elem_var: _,
        } => {
            let arity = patterns.arity();

            let sketched_elem_patterns = patterns
                .patterns
                .iter()
                .map(|p| sketch_pattern(&p.value))
                .collect();

            SP::List(arity, sketched_elem_patterns)
        }

        AppliedTag {
            tag_name,
            arguments,
            ..
        } => {
            let simplified_args: std::vec::Vec<_> = arguments
                .iter()
                .map(|(_, arg)| sketch_pattern(&arg.value))
                .collect();

            SP::Ctor(tag_name.clone(), simplified_args)
        }

        UnwrappedOpaque {
            opaque, argument, ..
        } => {
            let (_, argument) = &(**argument);

            let tag_id = TagId(0);

            let union = Union {
                render_as: RenderAs::Opaque,
                alternatives: vec![Ctor {
                    name: CtorName::Opaque(*opaque),
                    tag_id,
                    arity: 1,
                }],
            };

            SP::KnownCtor(union, tag_id, vec![sketch_pattern(&argument.value)])
        }

        // Treat this like a literal so we mark it as non-exhaustive
        MalformedPattern(..) => SP::Literal(Literal::Byte(1)),

        Underscore
        | Identifier(_)
        | AbilityMemberSpecialization { .. }
        | Shadowed(..)
        | OpaqueNotInScope(..)
        | UnsupportedPattern(..) => SP::Anything,
    }
}

pub fn sketch_when_branches(region: Region, patterns: &[expr::WhenBranch]) -> SketchedRows {
    let mut rows: Vec<SketchedRow> = Vec::with_capacity(patterns.len());

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
    let any_has_guard = patterns.iter().any(|branch| branch.guard.is_some());

    use SketchedPattern as SP;
    for WhenBranch {
        patterns,
        guard,
        value: _,
        redundant,
    } in patterns
    {
        let guard = if guard.is_some() {
            Guard::HasGuard
        } else {
            Guard::NoGuard
        };

        for loc_pat in patterns {
            // Decompose each pattern in the branch into its own row.

            let patterns = if any_has_guard {
                let guard_pattern = match guard {
                    Guard::HasGuard => SP::Literal(Literal::Bit(true)),
                    Guard::NoGuard => SP::Anything,
                };

                let tag_id = TagId(0);

                let union = Union {
                    render_as: RenderAs::Guard,
                    alternatives: vec![Ctor {
                        tag_id,
                        name: CtorName::Tag(TagName(GUARD_CTOR.into())),
                        arity: 2,
                    }],
                };

                vec![SP::KnownCtor(
                    union,
                    tag_id,
                    // NB: ordering the guard pattern first seems to be better at catching
                    // non-exhaustive constructors in the second argument; see the paper to see if
                    // there is a way to improve this in general.
                    vec![guard_pattern, sketch_pattern(&loc_pat.pattern.value)],
                )]
            } else {
                // Simple case
                vec![sketch_pattern(&loc_pat.pattern.value)]
            };

            let row = SketchedRow {
                patterns,
                region: loc_pat.pattern.region,
                guard,
                redundant_mark: *redundant,
            };
            rows.push(row);
        }
    }

    SketchedRows {
        rows,
        overall_region: region,
    }
}

pub fn sketch_pattern_to_rows(region: Region, pattern: &crate::pattern::Pattern) -> SketchedRows {
    let row = SketchedRow {
        patterns: vec![sketch_pattern(pattern)],
        region,
        // A single row cannot be redundant!
        redundant_mark: RedundantMark::known_non_redundant(),
        guard: Guard::NoGuard,
    };
    SketchedRows {
        rows: vec![row],
        overall_region: region,
    }
}

/// REDUNDANT PATTERNS

struct NonRedundantSummary {
    non_redundant_rows: Vec<Vec<Pattern>>,
    redundancies: Vec<RedundantMark>,
    errors: Vec<Error>,
}

/// INVARIANT: Produces a list of rows where (forall row. length row == 1)
fn to_nonredundant_rows(
    subs: &Subs,
    real_var: Variable,
    rows: SketchedRows,
) -> Result<NonRedundantSummary, TypeError> {
    let SketchedRows {
        rows,
        overall_region,
    } = rows;
    let mut checked_rows = Vec::with_capacity(rows.len());

    let mut redundancies = vec![];
    let mut errors = vec![];

    for (
        row_number,
        SketchedRow {
            patterns,
            guard,
            region,
            redundant_mark,
        },
    ) in rows.into_iter().enumerate()
    {
        let next_row: Vec<Pattern> = patterns
            .into_iter()
            .map(|pattern| pattern.reify(subs, real_var))
            .collect::<Result<_, _>>()?;

        let redundant_err = if !is_inhabited_row(&next_row) {
            Some(Error::Unmatchable {
                overall_region,
                branch_region: region,
                index: HumanIndex::zero_based(row_number),
            })
        } else if !(matches!(guard, Guard::HasGuard)
            || is_useful(checked_rows.clone(), next_row.clone()))
        {
            Some(Error::Redundant {
                overall_region,
                branch_region: region,
                index: HumanIndex::zero_based(row_number),
            })
        } else {
            None
        };

        match redundant_err {
            None => {
                checked_rows.push(next_row);
            }
            Some(err) => {
                redundancies.push(redundant_mark);
                errors.push(err);
            }
        }
    }

    Ok(NonRedundantSummary {
        non_redundant_rows: checked_rows,
        redundancies,
        errors,
    })
}

fn is_inhabited_row(patterns: &[Pattern]) -> bool {
    patterns.iter().any(is_inhabited_pattern)
}

fn is_inhabited_pattern(pat: &Pattern) -> bool {
    let mut stack = vec![pat];
    while let Some(pat) = stack.pop() {
        match pat {
            Pattern::Anything => {}
            Pattern::Literal(_) => {}
            Pattern::Ctor(union, id, pats) => {
                if !union.alternatives.iter().any(|alt| alt.tag_id == *id) {
                    // The tag ID was dropped from the union, which means that this tag ID is one
                    // that is not material to the union, and so is uninhabited!
                    return false;
                }
                stack.extend(pats);
            }
            Pattern::List(_, pats) => {
                // List is uninhabited if any element is uninhabited.
                stack.extend(pats);
            }
        }
    }
    true
}

fn convert_tag(subs: &Subs, whole_var: Variable, this_tag: &TagName) -> (Union, TagId) {
    let content = subs.get_content_without_compacting(whole_var);

    use {Content::*, FlatType::*};

    let (sorted_tags, ext) = match dealias_tag(subs, content) {
        Structure(TagUnion(tags, ext) | RecursiveTagUnion(_, tags, ext)) => {
            let (sorted_tags, ext) = tags.sorted_iterator_and_ext(subs, *ext);
            (sorted_tags, ext)
        }
        Structure(FunctionOrTagUnion(tags, _, ext)) => {
            let (ext_tags, ext) = gather_tags_unsorted_iter(subs, Default::default(), *ext)
                .unwrap_or_else(|_| {
                    internal_error!("Content is not a tag union: {:?}", subs.dbg(whole_var))
                });
            let mut all_tags: Vec<(TagName, &[Variable])> = Vec::with_capacity(tags.len());
            for tag in subs.get_subs_slice(*tags) {
                all_tags.push((tag.clone(), &[]));
            }
            for (tag, vars) in ext_tags {
                debug_assert!(vars.is_empty());
                all_tags.push((tag.clone(), &[]));
            }
            (Box::new(all_tags.into_iter()) as SortedTagsIterator, ext)
        }
        _ => internal_error!(
            "Content is not a tag union: {:?}",
            SubsFmtContent(content, subs)
        ),
    };

    let mut num_tags = sorted_tags.len();

    // DEVIATION: model openness by attaching a #Open constructor, that can never
    // be matched unless there's an `Anything` pattern.
    let opt_openness_tag = match subs.get_content_without_compacting(ext.var()) {
        FlexVar(_) | RigidVar(_) => {
            let openness_tag = TagName(NONEXHAUSIVE_CTOR.into());
            num_tags += 1;
            Some((openness_tag, &[] as _))
        }
        Structure(EmptyTagUnion) => None,
        // Anything else is erroneous and we ignore
        _ => None,
    };

    // High tag ID if we're out-of-bounds.
    let mut my_tag_id = TagId(num_tags as TagIdIntType);

    let mut alternatives = Vec::with_capacity(num_tags);
    let alternatives_iter = sorted_tags.into_iter().chain(opt_openness_tag);

    let mut index = 0;
    for (tag, args) in alternatives_iter {
        let is_inhabited = args.iter().all(|v| subs.is_inhabited(*v));
        if !is_inhabited {
            // This constructor is not material; we don't need to match over it!
            continue;
        }

        let tag_id = TagId(index as TagIdIntType);
        index += 1;

        if this_tag == &tag {
            my_tag_id = tag_id;
        }
        alternatives.push(Ctor {
            name: CtorName::Tag(tag),
            tag_id,
            arity: args.len(),
        });
    }

    let union = Union {
        alternatives,
        render_as: RenderAs::Tag,
    };

    (union, my_tag_id)
}

pub fn dealias_tag<'a>(subs: &'a Subs, content: &'a Content) -> &'a Content {
    use Content::*;
    let mut result = content;
    loop {
        match result {
            Alias(_, _, real_var, AliasKind::Structural)
            | RecursionVar {
                structure: real_var,
                ..
            } => result = subs.get_content_without_compacting(*real_var),
            _ => return result,
        }
    }
}
