use crate::ir::{substitute_in_exprs, Env, Expr, Procs, Stmt};
use crate::layout::{
    self, Builtin, InLayout, Layout, LayoutCache, LayoutInterner, LayoutProblem, LayoutRepr,
    TagIdIntType, UnionLayout, WrappedVariant,
};
use bumpalo::collections::Vec;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::{BumpMap, BumpMapDefault};
use roc_error_macros::internal_error;
use roc_exhaustive::{Ctor, CtorName, ListArity, RenderAs, TagId};
use roc_module::ident::{Lowercase, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_problem::can::{RuntimeError, ShadowKind};
use roc_types::subs::Variable;

use super::literal::{make_num_literal, IntOrFloatValue};
use super::{Call, CallType, Literal};

/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern<'a> {
    Identifier(Symbol),
    Underscore,
    As(Box<Pattern<'a>>, Symbol),
    IntLiteral([u8; 16], IntWidth),
    FloatLiteral(u64, FloatWidth),
    DecimalLiteral([u8; 16]),
    BitLiteral {
        value: bool,
        tag_name: TagName,
        union: roc_exhaustive::Union,
    },
    EnumLiteral {
        tag_id: u8,
        tag_name: TagName,
        union: roc_exhaustive::Union,
    },
    StrLiteral(Box<str>),

    RecordDestructure(Vec<'a, RecordDestruct<'a>>, &'a [InLayout<'a>]),
    TupleDestructure(Vec<'a, TupleDestruct<'a>>, &'a [InLayout<'a>]),
    NewtypeDestructure {
        tag_name: TagName,
        arguments: Vec<'a, (Pattern<'a>, InLayout<'a>)>,
    },
    AppliedTag {
        tag_name: TagName,
        tag_id: TagIdIntType,
        arguments: Vec<'a, (Pattern<'a>, InLayout<'a>)>,
        layout: UnionLayout<'a>,
        union: roc_exhaustive::Union,
    },
    Voided {
        tag_name: TagName,
    },
    OpaqueUnwrap {
        opaque: Symbol,
        argument: Box<(Pattern<'a>, InLayout<'a>)>,
    },
    List {
        arity: ListArity,
        list_layout: InLayout<'a>,
        element_layout: InLayout<'a>,
        elements: Vec<'a, Pattern<'a>>,
        opt_rest: Option<(usize, Option<Symbol>)>,
    },
}

impl<'a> Pattern<'a> {
    /// This pattern contains a pattern match on Void (i.e. [], the empty tag union)
    /// such branches are not reachable at runtime
    pub fn is_voided(&self) -> bool {
        let mut stack: std::vec::Vec<&Pattern> = vec![self];

        while let Some(pattern) = stack.pop() {
            match pattern {
                Pattern::Identifier(_)
                | Pattern::Underscore
                | Pattern::IntLiteral(_, _)
                | Pattern::FloatLiteral(_, _)
                | Pattern::DecimalLiteral(_)
                | Pattern::BitLiteral { .. }
                | Pattern::EnumLiteral { .. }
                | Pattern::StrLiteral(_) => { /* terminal */ }
                Pattern::As(subpattern, _) => stack.push(subpattern),
                Pattern::RecordDestructure(destructs, _) => {
                    for destruct in destructs {
                        match &destruct.typ {
                            DestructType::Required(_) => { /* do nothing */ }
                            DestructType::Guard(pattern) => {
                                stack.push(pattern);
                            }
                        }
                    }
                }
                Pattern::TupleDestructure(destructs, _) => {
                    for destruct in destructs {
                        stack.push(&destruct.pat);
                    }
                }
                Pattern::NewtypeDestructure { arguments, .. } => {
                    stack.extend(arguments.iter().map(|(t, _)| t))
                }
                Pattern::Voided { .. } => return true,
                Pattern::AppliedTag { arguments, .. } => {
                    stack.extend(arguments.iter().map(|(t, _)| t))
                }
                Pattern::OpaqueUnwrap { argument, .. } => stack.push(&argument.0),
                Pattern::List { elements, .. } => stack.extend(elements),
            }
        }

        false
    }

    pub fn collect_symbols(
        &self,
        layout: InLayout<'a>,
    ) -> impl Iterator<Item = (Symbol, InLayout<'a>)> + '_ {
        PatternBindingIter::One(self, layout)
    }
}

enum PatternBindingIter<'r, 'a> {
    Done,
    One(&'r Pattern<'a>, InLayout<'a>),
    Stack(std::vec::Vec<(PatternBindingWork<'r, 'a>, InLayout<'a>)>),
}

enum PatternBindingWork<'r, 'a> {
    Pat(&'r Pattern<'a>),
    #[allow(dead_code)]
    // Field will be used once todo is immplemented in next in impl<'r, 'a> Iterator for PatternBindingIter
    RecordDestruct(&'r DestructType<'a>),
}

impl<'r, 'a> Iterator for PatternBindingIter<'r, 'a> {
    type Item = (Symbol, InLayout<'a>);

    fn next(&mut self) -> Option<Self::Item> {
        use Pattern::*;
        use PatternBindingIter::*;
        use PatternBindingWork::*;
        match self {
            Done => None,
            One(pattern, layout) => {
                let layout = *layout;
                match pattern {
                    Identifier(symbol) => {
                        *self = Done;
                        (*symbol, layout).into()
                    }
                    Underscore => None,
                    As(pat, symbol) => {
                        *self = One(pat, layout);
                        (*symbol, layout).into()
                    }
                    RecordDestructure(destructs, _) => {
                        let stack = destructs
                            .iter()
                            .map(|destruct| (RecordDestruct(&destruct.typ), destruct.layout))
                            .rev()
                            .collect();
                        *self = Stack(stack);
                        self.next()
                    }
                    TupleDestructure(destructs, _) => {
                        let stack = destructs
                            .iter()
                            .map(|destruct| (Pat(&destruct.pat), destruct.layout))
                            .rev()
                            .collect();
                        *self = Stack(stack);
                        self.next()
                    }
                    NewtypeDestructure { arguments, .. } | AppliedTag { arguments, .. } => {
                        let stack = arguments.iter().map(|(p, l)| (Pat(p), *l)).rev().collect();
                        *self = Stack(stack);
                        self.next()
                    }
                    OpaqueUnwrap { argument, .. } => {
                        *self = One(&argument.0, layout);
                        self.next()
                    }
                    List {
                        element_layout,
                        elements,
                        opt_rest,
                        ..
                    } => {
                        let stack = elements
                            .iter()
                            .map(|p| (Pat(p), *element_layout))
                            .rev()
                            .collect();
                        *self = Stack(stack);

                        match opt_rest {
                            Some((_, Some(rest_sym))) => (*rest_sym, layout).into(),
                            _ => self.next(),
                        }
                    }
                    IntLiteral(_, _)
                    | FloatLiteral(_, _)
                    | DecimalLiteral(_)
                    | BitLiteral { .. }
                    | EnumLiteral { .. }
                    | StrLiteral(_)
                    | Voided { .. } => None,
                }
            }
            Stack(stack) => {
                while let Some((pat, layout)) = stack.pop() {
                    match pat {
                        Pat(pattern) => match pattern {
                            Identifier(symbol) => return (*symbol, layout).into(),
                            As(pat, symbol) => {
                                stack.push((Pat(pat), layout));
                                return (*symbol, layout).into();
                            }
                            RecordDestructure(destructs, _) => stack.extend(
                                destructs
                                    .iter()
                                    .map(|destruct| {
                                        (RecordDestruct(&destruct.typ), destruct.layout)
                                    })
                                    .rev(),
                            ),
                            TupleDestructure(destructs, _) => stack.extend(
                                destructs
                                    .iter()
                                    .map(|destruct| (Pat(&destruct.pat), destruct.layout))
                                    .rev(),
                            ),
                            NewtypeDestructure { arguments, .. } | AppliedTag { arguments, .. } => {
                                stack.extend(arguments.iter().map(|(p, l)| (Pat(p), *l)).rev())
                            }
                            OpaqueUnwrap { argument, .. } => {
                                stack.push((Pat(&argument.0), layout));
                            }
                            List {
                                element_layout,
                                elements,
                                opt_rest,
                                ..
                            } => {
                                stack.extend(
                                    elements.iter().map(|p| (Pat(p), *element_layout)).rev(),
                                );

                                if let Some((_, Some(rest_sym))) = opt_rest {
                                    return (*rest_sym, layout).into();
                                }
                            }
                            IntLiteral(_, _)
                            | FloatLiteral(_, _)
                            | DecimalLiteral(_)
                            | BitLiteral { .. }
                            | EnumLiteral { .. }
                            | Underscore
                            | StrLiteral(_)
                            | Voided { .. } => {}
                        },
                        PatternBindingWork::RecordDestruct(_) => todo!(),
                    }
                }

                *self = Done;
                None
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordDestruct<'a> {
    pub label: Lowercase,
    pub variable: Variable,
    pub layout: InLayout<'a>,
    pub typ: DestructType<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TupleDestruct<'a> {
    pub index: usize,
    pub variable: Variable,
    pub layout: InLayout<'a>,
    pub pat: Pattern<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DestructType<'a> {
    Required(Symbol),
    Guard(Pattern<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhenBranch<'a> {
    pub patterns: Vec<'a, Pattern<'a>>,
    pub value: Expr<'a>,
    pub guard: Option<Stmt<'a>>,
}

#[allow(clippy::type_complexity)]
pub fn from_can_pattern<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    can_pattern: &roc_can::pattern::Pattern,
) -> Result<
    (
        Pattern<'a>,
        Vec<'a, (Symbol, Variable, roc_can::expr::Expr)>,
    ),
    RuntimeError,
> {
    let mut assignments = Vec::new_in(env.arena);
    let pattern = from_can_pattern_help(env, procs, layout_cache, can_pattern, &mut assignments)?;

    Ok((pattern, assignments))
}

fn from_can_pattern_help<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    can_pattern: &roc_can::pattern::Pattern,
    assignments: &mut Vec<'a, (Symbol, Variable, roc_can::expr::Expr)>,
) -> Result<Pattern<'a>, RuntimeError> {
    use roc_can::pattern::Pattern::*;

    match can_pattern {
        Underscore => Ok(Pattern::Underscore),
        Identifier(symbol) => Ok(Pattern::Identifier(*symbol)),
        As(subpattern, symbol) => {
            let mono_subpattern =
                from_can_pattern_help(env, procs, layout_cache, &subpattern.value, assignments)?;

            Ok(Pattern::As(Box::new(mono_subpattern), *symbol))
        }
        AbilityMemberSpecialization { ident, .. } => Ok(Pattern::Identifier(*ident)),
        IntLiteral(var, _, int_str, int, _bound) => Ok(make_num_literal_pattern(
            env,
            layout_cache,
            *var,
            int_str,
            IntOrFloatValue::Int(*int),
        )),
        FloatLiteral(var, _, float_str, float, _bound) => Ok(make_num_literal_pattern(
            env,
            layout_cache,
            *var,
            float_str,
            IntOrFloatValue::Float(*float),
        )),
        StrLiteral(v) => Ok(Pattern::StrLiteral(v.clone())),
        SingleQuote(var, _, c, _) => {
            let layout = layout_cache.from_var(env.arena, *var, env.subs);
            match layout.map(|l| layout_cache.get_repr(l)) {
                Ok(LayoutRepr::Builtin(Builtin::Int(width))) => {
                    Ok(Pattern::IntLiteral((*c as i128).to_ne_bytes(), width))
                }
                o => internal_error!("an integer width was expected, but we found {:?}", o),
            }
        }
        Shadowed(region, ident, _new_symbol) => Err(RuntimeError::Shadowing {
            original_region: *region,
            shadow: ident.clone(),
            kind: ShadowKind::Variable,
        }),
        UnsupportedPattern(region) => Err(RuntimeError::UnsupportedPattern(*region)),
        MalformedPattern(_problem, region) => {
            // TODO preserve malformed problem information here?
            Err(RuntimeError::UnsupportedPattern(*region))
        }
        OpaqueNotInScope(loc_ident) => {
            // TODO(opaques) should be `RuntimeError::OpaqueNotDefined`
            Err(RuntimeError::UnsupportedPattern(loc_ident.region))
        }
        NumLiteral(var, num_str, num, _bound) => Ok(make_num_literal_pattern(
            env,
            layout_cache,
            *var,
            num_str,
            IntOrFloatValue::Int(*num),
        )),

        AppliedTag {
            whole_var,
            tag_name,
            arguments,
            ..
        } => {
            use crate::layout::UnionVariant::*;
            use roc_exhaustive::Union;

            let res_variant = {
                let mut layout_env =
                    layout::Env::from_components(layout_cache, env.subs, env.arena);
                crate::layout::union_sorted_tags(&mut layout_env, *whole_var).map_err(Into::into)
            };

            let variant = match res_variant {
                Ok(cached) => cached,
                Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                    return Err(RuntimeError::UnresolvedTypeVar)
                }
                Err(LayoutProblem::Erroneous) => return Err(RuntimeError::ErroneousType),
            };

            let result = match variant {
                Never => unreachable!(
                    "there is no pattern of type `[]`, union var {:?}",
                    *whole_var
                ),
                Unit => Pattern::EnumLiteral {
                    tag_id: 0,
                    tag_name: tag_name.clone(),
                    union: Union {
                        render_as: RenderAs::Tag,
                        alternatives: vec![Ctor {
                            tag_id: TagId(0),
                            name: CtorName::Tag(tag_name.clone()),
                            arity: 0,
                        }],
                    },
                },
                BoolUnion { ttrue, ffalse } => {
                    let (ttrue, ffalse) = (ttrue.expect_tag(), ffalse.expect_tag());
                    Pattern::BitLiteral {
                        value: tag_name == &ttrue,
                        tag_name: tag_name.clone(),
                        union: Union {
                            render_as: RenderAs::Tag,
                            alternatives: vec![
                                Ctor {
                                    tag_id: TagId(0),
                                    name: CtorName::Tag(ffalse),
                                    arity: 0,
                                },
                                Ctor {
                                    tag_id: TagId(1),
                                    name: CtorName::Tag(ttrue),
                                    arity: 0,
                                },
                            ],
                        },
                    }
                }
                ByteUnion(tag_names) => {
                    let tag_id = tag_names
                        .iter()
                        .position(|key| tag_name == key.expect_tag_ref())
                        .expect("tag must be in its own type");

                    let mut ctors = std::vec::Vec::with_capacity(tag_names.len());
                    for (i, tag_name) in tag_names.into_iter().enumerate() {
                        ctors.push(Ctor {
                            tag_id: TagId(i as _),
                            name: CtorName::Tag(tag_name.expect_tag()),
                            arity: 0,
                        })
                    }

                    let union = roc_exhaustive::Union {
                        render_as: RenderAs::Tag,
                        alternatives: ctors,
                    };

                    Pattern::EnumLiteral {
                        tag_id: tag_id as u8,
                        tag_name: tag_name.clone(),
                        union,
                    }
                }
                Newtype {
                    arguments: field_layouts,
                    ..
                } => {
                    let mut arguments = arguments.clone();

                    arguments.sort_by(|arg1, arg2| {
                        let size1 = layout_cache
                            .from_var(env.arena, arg1.0, env.subs)
                            .map(|x| layout_cache.interner.alignment_bytes(x))
                            .unwrap_or(0);

                        let size2 = layout_cache
                            .from_var(env.arena, arg2.0, env.subs)
                            .map(|x| layout_cache.interner.alignment_bytes(x))
                            .unwrap_or(0);

                        size2.cmp(&size1)
                    });

                    let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);
                    for ((_, loc_pat), layout) in arguments.iter().zip(field_layouts.iter()) {
                        mono_args.push((
                            from_can_pattern_help(
                                env,
                                procs,
                                layout_cache,
                                &loc_pat.value,
                                assignments,
                            )?,
                            *layout,
                        ));
                    }

                    Pattern::NewtypeDestructure {
                        tag_name: tag_name.clone(),
                        arguments: mono_args,
                    }
                }
                NewtypeByVoid {
                    data_tag_arguments,
                    data_tag_name,
                    ..
                } => {
                    let data_tag_name = data_tag_name.expect_tag();

                    if tag_name != &data_tag_name {
                        // this tag is not represented at runtime
                        Pattern::Voided {
                            tag_name: tag_name.clone(),
                        }
                    } else {
                        let mut arguments = arguments.clone();

                        arguments.sort_by(|arg1, arg2| {
                            let size1 = layout_cache
                                .from_var(env.arena, arg1.0, env.subs)
                                .map(|x| layout_cache.interner.alignment_bytes(x))
                                .unwrap_or(0);

                            let size2 = layout_cache
                                .from_var(env.arena, arg2.0, env.subs)
                                .map(|x| layout_cache.interner.alignment_bytes(x))
                                .unwrap_or(0);

                            size2.cmp(&size1)
                        });

                        let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);
                        let it = arguments.iter().zip(data_tag_arguments.iter());
                        for ((_, loc_pat), layout) in it {
                            mono_args.push((
                                from_can_pattern_help(
                                    env,
                                    procs,
                                    layout_cache,
                                    &loc_pat.value,
                                    assignments,
                                )?,
                                *layout,
                            ));
                        }

                        Pattern::NewtypeDestructure {
                            tag_name: tag_name.clone(),
                            arguments: mono_args,
                        }
                    }
                }

                Wrapped(variant) => {
                    let (tag_id, argument_layouts) = variant.tag_name_to_id(tag_name);
                    let number_of_tags = variant.number_of_tags();
                    let mut ctors = std::vec::Vec::with_capacity(number_of_tags);

                    let arguments = {
                        let mut temp = arguments.clone();

                        temp.sort_by(|arg1, arg2| {
                            let layout1 =
                                layout_cache.from_var(env.arena, arg1.0, env.subs).unwrap();
                            let layout2 =
                                layout_cache.from_var(env.arena, arg2.0, env.subs).unwrap();

                            let size1 = layout_cache.interner.alignment_bytes(layout1);
                            let size2 = layout_cache.interner.alignment_bytes(layout2);

                            size2.cmp(&size1)
                        });

                        temp
                    };

                    // we must derive the union layout from the whole_var, building it up
                    // from `layouts` would unroll recursive tag unions, and that leads to
                    // problems down the line because we hash layouts and an unrolled
                    // version is not the same as the minimal version.
                    let whole_var_layout = layout_cache.from_var(env.arena, *whole_var, env.subs);
                    let layout =
                        match whole_var_layout.map(|l| layout_cache.interner.chase_recursive(l)) {
                            Ok(LayoutRepr::Union(ul)) => ul,
                            _ => internal_error!(),
                        };

                    use WrappedVariant::*;
                    match variant {
                        NonRecursive {
                            sorted_tag_layouts: ref tags,
                        } => {
                            debug_assert!(tags.len() > 1);

                            for (i, (tag_name, args)) in tags.iter().enumerate() {
                                ctors.push(Ctor {
                                    tag_id: TagId(i as _),
                                    name: CtorName::Tag(tag_name.expect_tag_ref().clone()),
                                    arity: args.len(),
                                })
                            }

                            let union = roc_exhaustive::Union {
                                render_as: RenderAs::Tag,
                                alternatives: ctors,
                            };

                            let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);

                            debug_assert_eq!(
                                arguments.len(),
                                argument_layouts.len(),
                                "The {:?} tag got {} arguments, but its layout expects {}!",
                                tag_name,
                                arguments.len(),
                                argument_layouts.len(),
                            );
                            let it = argument_layouts.iter();

                            for ((_, loc_pat), layout) in arguments.iter().zip(it) {
                                mono_args.push((
                                    from_can_pattern_help(
                                        env,
                                        procs,
                                        layout_cache,
                                        &loc_pat.value,
                                        assignments,
                                    )?,
                                    *layout,
                                ));
                            }

                            Pattern::AppliedTag {
                                tag_name: tag_name.clone(),
                                tag_id: tag_id as _,
                                arguments: mono_args,
                                union,
                                layout,
                            }
                        }

                        Recursive {
                            sorted_tag_layouts: ref tags,
                        } => {
                            debug_assert!(tags.len() > 1);

                            for (i, (tag_name, args)) in tags.iter().enumerate() {
                                ctors.push(Ctor {
                                    tag_id: TagId(i as _),
                                    name: CtorName::Tag(tag_name.expect_tag_ref().clone()),
                                    arity: args.len(),
                                })
                            }

                            let union = roc_exhaustive::Union {
                                render_as: RenderAs::Tag,
                                alternatives: ctors,
                            };

                            let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);

                            debug_assert_eq!(arguments.len(), argument_layouts.len());
                            let it = argument_layouts.iter();

                            for ((_, loc_pat), layout) in arguments.iter().zip(it) {
                                mono_args.push((
                                    from_can_pattern_help(
                                        env,
                                        procs,
                                        layout_cache,
                                        &loc_pat.value,
                                        assignments,
                                    )?,
                                    *layout,
                                ));
                            }

                            Pattern::AppliedTag {
                                tag_name: tag_name.clone(),
                                tag_id: tag_id as _,
                                arguments: mono_args,
                                union,
                                layout,
                            }
                        }

                        NonNullableUnwrapped {
                            tag_name: w_tag_name,
                            fields,
                        } => {
                            debug_assert_eq!(w_tag_name.expect_tag_ref(), tag_name);

                            ctors.push(Ctor {
                                tag_id: TagId(0),
                                name: CtorName::Tag(tag_name.clone()),
                                arity: fields.len(),
                            });

                            let union = roc_exhaustive::Union {
                                render_as: RenderAs::Tag,
                                alternatives: ctors,
                            };

                            let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);

                            debug_assert_eq!(arguments.len(), argument_layouts.len());
                            let it = argument_layouts.iter();

                            for ((_, loc_pat), layout) in arguments.iter().zip(it) {
                                mono_args.push((
                                    from_can_pattern_help(
                                        env,
                                        procs,
                                        layout_cache,
                                        &loc_pat.value,
                                        assignments,
                                    )?,
                                    *layout,
                                ));
                            }

                            Pattern::AppliedTag {
                                tag_name: tag_name.clone(),
                                tag_id: tag_id as _,
                                arguments: mono_args,
                                union,
                                layout,
                            }
                        }

                        NullableWrapped {
                            sorted_tag_layouts: ref non_nulled_tags,
                            nullable_id,
                            nullable_name,
                        } => {
                            for id in 0..(non_nulled_tags.len() + 1) {
                                if id == nullable_id as usize {
                                    ctors.push(Ctor {
                                        tag_id: TagId(id as _),
                                        name: CtorName::Tag(nullable_name.expect_tag_ref().clone()),
                                        arity: 0,
                                    });
                                } else {
                                    let i = if id < nullable_id.into() { id } else { id - 1 };
                                    let (tag_name, args) = &non_nulled_tags[i];
                                    ctors.push(Ctor {
                                        tag_id: TagId(i as _),
                                        name: CtorName::Tag(tag_name.expect_tag_ref().clone()),
                                        arity: args.len(),
                                    });
                                }
                            }

                            let union = roc_exhaustive::Union {
                                render_as: RenderAs::Tag,
                                alternatives: ctors,
                            };

                            let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);

                            let it = if tag_name == nullable_name.expect_tag_ref() {
                                [].iter()
                            } else {
                                argument_layouts.iter()
                            };

                            for ((_, loc_pat), layout) in arguments.iter().zip(it) {
                                mono_args.push((
                                    from_can_pattern_help(
                                        env,
                                        procs,
                                        layout_cache,
                                        &loc_pat.value,
                                        assignments,
                                    )?,
                                    *layout,
                                ));
                            }

                            Pattern::AppliedTag {
                                tag_name: tag_name.clone(),
                                tag_id: tag_id as _,
                                arguments: mono_args,
                                union,
                                layout,
                            }
                        }

                        NullableUnwrapped {
                            other_fields,
                            nullable_id,
                            nullable_name,
                            other_name: _,
                        } => {
                            debug_assert!(!other_fields.is_empty());

                            ctors.push(Ctor {
                                tag_id: TagId(nullable_id as _),
                                name: CtorName::Tag(nullable_name.expect_tag_ref().clone()),
                                arity: 0,
                            });

                            ctors.push(Ctor {
                                tag_id: TagId(!nullable_id as _),
                                name: CtorName::Tag(nullable_name.expect_tag_ref().clone()),
                                arity: other_fields.len(),
                            });

                            let union = roc_exhaustive::Union {
                                render_as: RenderAs::Tag,
                                alternatives: ctors,
                            };

                            let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);

                            let it = if tag_name == nullable_name.expect_tag_ref() {
                                [].iter()
                            } else {
                                argument_layouts.iter()
                            };

                            for ((_, loc_pat), layout) in arguments.iter().zip(it) {
                                mono_args.push((
                                    from_can_pattern_help(
                                        env,
                                        procs,
                                        layout_cache,
                                        &loc_pat.value,
                                        assignments,
                                    )?,
                                    *layout,
                                ));
                            }

                            Pattern::AppliedTag {
                                tag_name: tag_name.clone(),
                                tag_id: tag_id as _,
                                arguments: mono_args,
                                union,
                                layout,
                            }
                        }
                    }
                }
            };

            Ok(result)
        }

        UnwrappedOpaque {
            opaque, argument, ..
        } => {
            let (arg_var, loc_arg_pattern) = &(**argument);
            let arg_layout = layout_cache
                .from_var(env.arena, *arg_var, env.subs)
                .unwrap();
            let mono_arg_pattern = from_can_pattern_help(
                env,
                procs,
                layout_cache,
                &loc_arg_pattern.value,
                assignments,
            )?;
            Ok(Pattern::OpaqueUnwrap {
                opaque: *opaque,
                argument: Box::new((mono_arg_pattern, arg_layout)),
            })
        }

        TupleDestructure {
            whole_var,
            destructs,
            ..
        } => {
            // sorted fields based on the type
            let sorted_elems = {
                let mut layout_env =
                    layout::Env::from_components(layout_cache, env.subs, env.arena);
                crate::layout::sort_tuple_elems(&mut layout_env, *whole_var)
                    .map_err(RuntimeError::from)?
            };

            // sorted fields based on the destruct
            let mut mono_destructs = Vec::with_capacity_in(destructs.len(), env.arena);
            let mut destructs_by_index = Vec::with_capacity_in(destructs.len(), env.arena);
            destructs_by_index.extend(destructs.iter().map(Some));

            let mut elem_layouts = Vec::with_capacity_in(sorted_elems.len(), env.arena);

            for (index, variable, res_layout) in sorted_elems.into_iter() {
                if index < destructs.len() {
                    // this elem is destructured by the pattern
                    mono_destructs.push(from_can_tuple_destruct(
                        env,
                        procs,
                        layout_cache,
                        &destructs[index].value,
                        res_layout,
                        assignments,
                    )?);
                } else {
                    // this elem is not destructured by the pattern
                    // put in an underscore
                    mono_destructs.push(TupleDestruct {
                        index,
                        variable,
                        layout: res_layout,
                        pat: Pattern::Underscore,
                    });
                }

                // the layout of this field is part of the layout of the record
                elem_layouts.push(res_layout);
            }

            Ok(Pattern::TupleDestructure(
                mono_destructs,
                elem_layouts.into_bump_slice(),
            ))
        }

        RecordDestructure {
            whole_var,
            destructs,
            opt_spread,
        } => {
            // sorted fields based on the type
            let sorted_fields = {
                let mut layout_env =
                    layout::Env::from_components(layout_cache, env.subs, env.arena);
                crate::layout::sort_record_fields(&mut layout_env, *whole_var)
                    .map_err(RuntimeError::from)?
            };

            // sorted fields based on the destruct
            let mut mono_destructs = Vec::with_capacity_in(destructs.len(), env.arena);
            let mut destructs_by_label = BumpMap::with_capacity_in(destructs.len(), env.arena);
            destructs_by_label.extend(destructs.iter().map(|x| (&x.value.label, x)));

            let mut field_layouts = Vec::with_capacity_in(sorted_fields.len(), env.arena);

            // next we step through both sequences of fields. The outer loop is the sequence based
            // on the type, since not all fields need to actually be destructured in the source
            // language.
            //
            // However in mono patterns, we do destruct all patterns (but use Underscore) when
            // in the source the field is not matche in the source language.
            //
            // Optional fields somewhat complicate the matter here

            for (label, variable, res_layout) in sorted_fields.into_iter() {
                match res_layout {
                    Ok(field_layout) => {
                        // the field is non-optional according to the type

                        match destructs_by_label.remove(&label) {
                            Some(destruct) => {
                                // this field is destructured by the pattern
                                mono_destructs.push(from_can_record_destruct(
                                    env,
                                    procs,
                                    layout_cache,
                                    &destruct.value,
                                    field_layout,
                                    assignments,
                                )?);
                            }
                            None => {
                                // this field is not destructured by the pattern
                                // put in an underscore
                                mono_destructs.push(RecordDestruct {
                                    label: label.clone(),
                                    variable,
                                    layout: field_layout,
                                    typ: DestructType::Guard(Pattern::Underscore),
                                });
                            }
                        }

                        // the layout of this field is part of the layout of the record
                        field_layouts.push(field_layout);
                    }
                    Err(field_layout) => {
                        // the field is optional according to the type
                        match destructs_by_label.remove(&label) {
                            Some(destruct) => {
                                // this field is destructured by the pattern
                                match &destruct.value.typ {
                                    roc_can::pattern::DestructType::Optional(_, loc_expr) => {
                                        // if we reach this stage, the optional field is not present
                                        // so we push the default assignment into the branch
                                        assignments.push((
                                            destruct.value.symbol,
                                            variable,
                                            loc_expr.value.clone(),
                                        ));
                                    }
                                    _ => unreachable!(
                                        "only optional destructs can be optional fields"
                                    ),
                                };
                            }
                            None => {
                                // this field is not destructured by the pattern
                                // put in an underscore
                                mono_destructs.push(RecordDestruct {
                                    label: label.clone(),
                                    variable,
                                    layout: field_layout,
                                    typ: DestructType::Guard(Pattern::Underscore),
                                });
                            }
                        }
                    }
                }
            }

            for (_, destruct) in destructs_by_label.drain() {
                // this destruct is not in the type, but is in the pattern
                // it must be an optional field, and we will use the default
                match &destruct.value.typ {
                    roc_can::pattern::DestructType::Optional(field_var, loc_expr) => {
                        assignments.push((
                            destruct.value.symbol,
                            // destruct.value.var,
                            *field_var,
                            loc_expr.value.clone(),
                        ));
                    }
                    _ => unreachable!("only optional destructs can be optional fields"),
                }
            }

            Ok(Pattern::RecordDestructure(
                mono_destructs,
                field_layouts.into_bump_slice(),
            ))
        }

        List {
            list_var,
            elem_var,
            patterns,
        } => {
            let list_layout = match layout_cache.from_var(env.arena, *list_var, env.subs) {
                Ok(lay) => lay,
                Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                    return Err(RuntimeError::UnresolvedTypeVar)
                }
                Err(LayoutProblem::Erroneous) => return Err(RuntimeError::ErroneousType),
            };

            let element_layout = match layout_cache.from_var(env.arena, *elem_var, env.subs) {
                Ok(lay) => lay,
                Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                    return Err(RuntimeError::UnresolvedTypeVar)
                }
                Err(LayoutProblem::Erroneous) => return Err(RuntimeError::ErroneousType),
            };

            let arity = patterns.arity();

            let mut mono_patterns = Vec::with_capacity_in(patterns.patterns.len(), env.arena);
            for loc_pat in patterns.patterns.iter() {
                let mono_pat =
                    from_can_pattern_help(env, procs, layout_cache, &loc_pat.value, assignments)?;
                mono_patterns.push(mono_pat);
            }

            Ok(Pattern::List {
                arity,
                list_layout,
                element_layout,
                elements: mono_patterns,
                opt_rest: patterns
                    .opt_rest
                    .map(|(i, name)| (i, name.map(|s| s.value))),
            })
        }
    }
}

fn make_num_literal_pattern<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &mut LayoutCache<'a>,
    variable: Variable,
    num_str: &str,
    num_value: IntOrFloatValue,
) -> Pattern<'a> {
    let layout = layout_cache
        .from_var(env.arena, variable, env.subs)
        .unwrap();
    let literal = make_num_literal(&layout_cache.interner, layout, num_str, num_value);
    literal.to_pattern()
}

fn from_can_record_destruct<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    can_rd: &roc_can::pattern::RecordDestruct,
    field_layout: InLayout<'a>,
    assignments: &mut Vec<'a, (Symbol, Variable, roc_can::expr::Expr)>,
) -> Result<RecordDestruct<'a>, RuntimeError> {
    Ok(RecordDestruct {
        label: can_rd.label.clone(),
        variable: can_rd.var,
        layout: field_layout,
        typ: match &can_rd.typ {
            roc_can::pattern::DestructType::Required => DestructType::Required(can_rd.symbol),
            roc_can::pattern::DestructType::Optional(_, _) => {
                // if we reach this stage, the optional field is present
                DestructType::Required(can_rd.symbol)
            }
            roc_can::pattern::DestructType::Guard(_, loc_pattern) => DestructType::Guard(
                from_can_pattern_help(env, procs, layout_cache, &loc_pattern.value, assignments)?,
            ),
        },
    })
}

fn from_can_tuple_destruct<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    can_rd: &roc_can::pattern::TupleDestruct,
    field_layout: InLayout<'a>,
    assignments: &mut Vec<'a, (Symbol, Variable, roc_can::expr::Expr)>,
) -> Result<TupleDestruct<'a>, RuntimeError> {
    Ok(TupleDestruct {
        index: can_rd.destruct_index,
        variable: can_rd.var,
        layout: field_layout,
        pat: from_can_pattern_help(env, procs, layout_cache, &can_rd.typ.1.value, assignments)?,
    })
}

#[allow(clippy::too_many_arguments)]
pub fn store_pattern<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    can_pat: &Pattern<'a>,
    outer_symbol: Symbol,
    stmt: Stmt<'a>,
) -> Stmt<'a> {
    match store_pattern_help(env, procs, layout_cache, can_pat, outer_symbol, stmt) {
        StorePattern::Productive(new) => new,
        StorePattern::NotProductive(new) => new,
    }
}

enum StorePattern<'a> {
    /// we bound new symbols
    Productive(Stmt<'a>),
    /// no new symbols were bound in this pattern
    NotProductive(Stmt<'a>),
}

/// It is crucial for correct RC insertion that we don't create dead variables!
#[allow(clippy::too_many_arguments)]
fn store_pattern_help<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    can_pat: &Pattern<'a>,
    outer_symbol: Symbol,
    mut stmt: Stmt<'a>,
) -> StorePattern<'a> {
    use Pattern::*;

    match can_pat {
        Identifier(symbol) => {
            substitute_in_exprs(env.arena, &mut stmt, *symbol, outer_symbol);
        }
        Underscore => {
            // do nothing
            return StorePattern::NotProductive(stmt);
        }
        As(subpattern, symbol) => {
            let stored_subpattern =
                store_pattern_help(env, procs, layout_cache, subpattern, outer_symbol, stmt);

            let mut stmt = match stored_subpattern {
                StorePattern::Productive(stmt) => stmt,
                StorePattern::NotProductive(stmt) => stmt,
            };

            substitute_in_exprs(env.arena, &mut stmt, *symbol, outer_symbol);

            return StorePattern::Productive(stmt);
        }
        IntLiteral(_, _)
        | FloatLiteral(_, _)
        | DecimalLiteral(_)
        | EnumLiteral { .. }
        | BitLiteral { .. }
        | StrLiteral(_) => {
            return StorePattern::NotProductive(stmt);
        }
        NewtypeDestructure { arguments, .. } => match arguments.as_slice() {
            [(pattern, _layout)] => {
                return store_pattern_help(env, procs, layout_cache, pattern, outer_symbol, stmt);
            }
            _ => {
                let mut fields = Vec::with_capacity_in(arguments.len(), env.arena);
                fields.extend(arguments.iter().map(|x| x.1));

                let layout = layout_cache
                    .put_in_direct_no_semantic(LayoutRepr::struct_(fields.into_bump_slice()));

                return store_newtype_pattern(
                    env,
                    procs,
                    layout_cache,
                    outer_symbol,
                    layout,
                    arguments,
                    stmt,
                );
            }
        },
        AppliedTag {
            arguments,
            layout,
            tag_id,
            ..
        } => {
            return store_tag_pattern(
                env,
                procs,
                layout_cache,
                outer_symbol,
                *layout,
                arguments,
                *tag_id,
                stmt,
            );
        }

        List {
            arity,
            list_layout,
            element_layout,
            elements,
            opt_rest,
        } => {
            return store_list_pattern(
                env,
                procs,
                layout_cache,
                outer_symbol,
                *arity,
                *list_layout,
                *element_layout,
                elements,
                opt_rest,
                stmt,
            )
        }

        Voided { .. } => {
            return StorePattern::NotProductive(stmt);
        }

        OpaqueUnwrap { argument, .. } => {
            let (pattern, _layout) = &**argument;
            return store_pattern_help(env, procs, layout_cache, pattern, outer_symbol, stmt);
        }

        RecordDestructure(destructs, [_single_field]) => {
            for destruct in destructs {
                match &destruct.typ {
                    DestructType::Required(symbol) => {
                        substitute_in_exprs(env.arena, &mut stmt, *symbol, outer_symbol);
                    }
                    DestructType::Guard(guard_pattern) => {
                        return store_pattern_help(
                            env,
                            procs,
                            layout_cache,
                            guard_pattern,
                            outer_symbol,
                            stmt,
                        );
                    }
                }
            }
        }
        RecordDestructure(destructs, sorted_fields) => {
            let mut is_productive = false;
            for (index, destruct) in destructs.iter().enumerate().rev() {
                match store_record_destruct(
                    env,
                    procs,
                    layout_cache,
                    destruct,
                    index as u64,
                    outer_symbol,
                    sorted_fields,
                    stmt,
                ) {
                    StorePattern::Productive(new) => {
                        is_productive = true;
                        stmt = new;
                    }
                    StorePattern::NotProductive(new) => {
                        stmt = new;
                    }
                }
            }

            if !is_productive {
                return StorePattern::NotProductive(stmt);
            }
        }

        TupleDestructure(destructs, [_single_field]) => {
            if let Some(destruct) = destructs.first() {
                return store_pattern_help(
                    env,
                    procs,
                    layout_cache,
                    &destruct.pat,
                    outer_symbol,
                    stmt,
                );
            }
        }
        TupleDestructure(destructs, sorted_fields) => {
            let mut is_productive = false;
            for (index, destruct) in destructs.iter().enumerate().rev() {
                match store_tuple_destruct(
                    env,
                    procs,
                    layout_cache,
                    destruct,
                    index as u64,
                    outer_symbol,
                    sorted_fields,
                    stmt,
                ) {
                    StorePattern::Productive(new) => {
                        is_productive = true;
                        stmt = new;
                    }
                    StorePattern::NotProductive(new) => {
                        stmt = new;
                    }
                }
            }

            if !is_productive {
                return StorePattern::NotProductive(stmt);
            }
        }
    }

    StorePattern::Productive(stmt)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ListIndex(
    /// Positive if we should index from the head, negative if we should index from the tail
    /// 0 is lst[0]
    /// -1 is lst[List.len lst - 1]
    i64,
);

impl ListIndex {
    pub fn from_pattern_index(index: usize, arity: ListArity) -> Self {
        match arity {
            ListArity::Exact(_) => Self(index as _),
            ListArity::Slice(head, tail) => {
                if index < head {
                    Self(index as _)
                } else {
                    // Slice(head=2, tail=5)
                    //
                    // s t ... w y z x q
                    // 0 1     2 3 4 5 6 index
                    //         0 1 2 3 4 (index - head)
                    //         5 4 3 2 1 (tail - (index - head))
                    Self(-((tail - (index - head)) as i64))
                }
            }
        }
    }
}

pub(crate) type Store<'a> = (Symbol, InLayout<'a>, Expr<'a>);

/// Builds the list index we should index into
#[must_use]
pub(crate) fn build_list_index_probe<'a>(
    env: &mut Env<'a, '_>,
    list_sym: Symbol,
    list_index: &ListIndex,
) -> (Symbol, impl DoubleEndedIterator<Item = Store<'a>>) {
    let list_index = list_index.0;
    let index_sym = env.unique_symbol();

    let (opt_len_store, opt_offset_store, index_store) = if list_index >= 0 {
        let index_expr = Expr::Literal(Literal::Int((list_index as i128).to_ne_bytes()));

        let index_store = (index_sym, Layout::U64, index_expr);

        (None, None, index_store)
    } else {
        let len_sym = env.unique_symbol();
        let len_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::ListLenU64,
                update_mode: env.next_update_mode_id(),
            },
            arguments: env.arena.alloc([list_sym]),
        });

        let offset = list_index.abs();
        let offset_sym = env.unique_symbol();
        let offset_expr = Expr::Literal(Literal::Int((offset as i128).to_ne_bytes()));

        let index_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::NumSub,
                update_mode: env.next_update_mode_id(),
            },
            arguments: env.arena.alloc([len_sym, offset_sym]),
        });

        let len_store = (len_sym, Layout::U64, len_expr);
        let offset_store = (offset_sym, Layout::U64, offset_expr);
        let index_store = (index_sym, Layout::U64, index_expr);

        (Some(len_store), Some(offset_store), index_store)
    };

    let stores = (opt_len_store.into_iter())
        .chain(opt_offset_store)
        .chain([index_store]);

    (index_sym, stores)
}

#[allow(clippy::too_many_arguments)]
fn store_list_pattern<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    list_sym: Symbol,
    list_arity: ListArity,
    list_layout: InLayout<'a>,
    element_layout: InLayout<'a>,
    elements: &[Pattern<'a>],
    opt_rest: &Option<(usize, Option<Symbol>)>,
    mut stmt: Stmt<'a>,
) -> StorePattern<'a> {
    use Pattern::*;

    let mut is_productive = false;

    for (index, element) in elements.iter().enumerate().rev() {
        let compute_element_load = |env: &mut Env<'a, '_>| {
            let list_index = ListIndex::from_pattern_index(index, list_arity);

            let (index_sym, needed_stores) = build_list_index_probe(env, list_sym, &list_index);

            let load = Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::ListGetUnsafe,
                    update_mode: env.next_update_mode_id(),
                },
                arguments: env.arena.alloc([list_sym, index_sym]),
            });

            (load, needed_stores)
        };

        let (store_loaded, needed_stores) = match element {
            Identifier(symbol) => {
                let (load, needed_stores) = compute_element_load(env);

                // store immediately in the given symbol
                (
                    Stmt::Let(*symbol, load, element_layout, env.arena.alloc(stmt)),
                    needed_stores,
                )
            }
            Underscore
            | IntLiteral(_, _)
            | FloatLiteral(_, _)
            | DecimalLiteral(_)
            | EnumLiteral { .. }
            | BitLiteral { .. }
            | StrLiteral(_) => {
                // ignore
                continue;
            }
            _ => {
                // store the field in a symbol, and continue matching on it
                let symbol = env.unique_symbol();

                // first recurse, continuing to unpack symbol
                match store_pattern_help(env, procs, layout_cache, element, symbol, stmt) {
                    StorePattern::Productive(new) => {
                        stmt = new;
                        let (load, needed_stores) = compute_element_load(env);

                        // only if we bind one of its (sub)fields to a used name should we
                        // extract the field
                        (
                            Stmt::Let(symbol, load, element_layout, env.arena.alloc(stmt)),
                            needed_stores,
                        )
                    }
                    StorePattern::NotProductive(new) => {
                        // do nothing
                        stmt = new;
                        continue;
                    }
                }
            }
        };

        is_productive = true;

        stmt = store_loaded;
        for (sym, lay, expr) in needed_stores.rev() {
            stmt = Stmt::Let(sym, expr, lay, env.arena.alloc(stmt));
        }
    }

    stmt = match store_list_rest(env, list_sym, list_arity, list_layout, opt_rest, stmt) {
        StorePattern::Productive(new) => {
            is_productive = true;
            new
        }
        StorePattern::NotProductive(new) => new,
    };

    if is_productive {
        StorePattern::Productive(stmt)
    } else {
        StorePattern::NotProductive(stmt)
    }
}

fn store_list_rest<'a>(
    env: &mut Env<'a, '_>,
    list_sym: Symbol,
    list_arity: ListArity,
    list_layout: InLayout<'a>,
    opt_rest: &Option<(usize, Option<Symbol>)>,
    mut stmt: Stmt<'a>,
) -> StorePattern<'a> {
    let mut is_productive = false;

    if let Some((index, Some(rest_sym))) = opt_rest {
        is_productive = true;

        let total_dropped = list_arity.min_len();
        let total_dropped_sym = env.unique_symbol();
        let total_dropped_expr = Expr::Literal(Literal::Int((total_dropped as u128).to_ne_bytes()));

        let list_len_sym = env.unique_symbol();
        let list_len_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                // Must use ListLenU64 here because we're using it with List.sublist,
                // which takes U64s for start and len.
                op: LowLevel::ListLenU64,
                update_mode: env.next_update_mode_id(),
            },
            arguments: env.arena.alloc([list_sym]),
        });

        let rest_len_sym = env.unique_symbol();
        let rest_len_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::NumSub,
                update_mode: env.next_update_mode_id(),
            },
            arguments: env.arena.alloc([list_len_sym, total_dropped_sym]),
        });

        let start_sym = env.unique_symbol();
        let start_expr = Expr::Literal(Literal::Int((*index as u128).to_ne_bytes()));

        let rest_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::ListSublist,
                update_mode: env.next_update_mode_id(),
            },
            arguments: env.arena.alloc([list_sym, start_sym, rest_len_sym]),
        });
        let needed_stores = [
            (total_dropped_sym, total_dropped_expr, Layout::U64),
            (list_len_sym, list_len_expr, Layout::U64),
            (rest_len_sym, rest_len_expr, Layout::U64),
            (start_sym, start_expr, Layout::U64),
            (*rest_sym, rest_expr, list_layout),
        ];
        for (sym, expr, lay) in needed_stores.into_iter().rev() {
            stmt = Stmt::Let(sym, expr, lay, env.arena.alloc(stmt));
        }
    }

    if is_productive {
        StorePattern::Productive(stmt)
    } else {
        StorePattern::NotProductive(stmt)
    }
}

#[allow(clippy::too_many_arguments)]
fn store_tag_pattern<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    structure: Symbol,
    union_layout: UnionLayout<'a>,
    arguments: &[(Pattern<'a>, InLayout<'a>)],
    tag_id: TagIdIntType,
    mut stmt: Stmt<'a>,
) -> StorePattern<'a> {
    use Pattern::*;

    let mut is_productive = false;

    for (index, (argument, arg_layout)) in arguments.iter().enumerate().rev() {
        let mut arg_layout = *arg_layout;

        if let LayoutRepr::RecursivePointer(_) = layout_cache.get_repr(arg_layout) {
            // TODO(recursive-layouts): fix after disjoint rec ptrs
            arg_layout = layout_cache.put_in_direct_no_semantic(LayoutRepr::Union(union_layout));
        }

        let load = Expr::UnionAtIndex {
            index: index as u64,
            structure,
            tag_id,
            union_layout,
        };

        match argument {
            Identifier(symbol) => {
                // store immediately in the given symbol
                stmt = Stmt::Let(*symbol, load, arg_layout, env.arena.alloc(stmt));
                is_productive = true;
            }
            Underscore => {
                // ignore
            }
            IntLiteral(_, _)
            | FloatLiteral(_, _)
            | DecimalLiteral(_)
            | EnumLiteral { .. }
            | BitLiteral { .. }
            | StrLiteral(_) => {}
            _ => {
                // store the field in a symbol, and continue matching on it
                let symbol = env.unique_symbol();

                // first recurse, continuing to unpack symbol
                match store_pattern_help(env, procs, layout_cache, argument, symbol, stmt) {
                    StorePattern::Productive(new) => {
                        is_productive = true;
                        stmt = new;
                        // only if we bind one of its (sub)fields to a used name should we
                        // extract the field
                        stmt = Stmt::Let(symbol, load, arg_layout, env.arena.alloc(stmt));
                    }
                    StorePattern::NotProductive(new) => {
                        // do nothing
                        stmt = new;
                    }
                }
            }
        }
    }

    if is_productive {
        StorePattern::Productive(stmt)
    } else {
        StorePattern::NotProductive(stmt)
    }
}

#[allow(clippy::too_many_arguments)]
fn store_newtype_pattern<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    structure: Symbol,
    layout: InLayout<'a>,
    arguments: &[(Pattern<'a>, InLayout<'a>)],
    mut stmt: Stmt<'a>,
) -> StorePattern<'a> {
    use Pattern::*;

    let mut arg_layouts = Vec::with_capacity_in(arguments.len(), env.arena);
    let mut is_productive = false;

    for (_, layout) in arguments {
        arg_layouts.push(*layout);
    }

    for (index, (argument, arg_layout)) in arguments.iter().enumerate().rev() {
        let mut arg_layout = *arg_layout;

        if let LayoutRepr::RecursivePointer(_) = layout_cache.get_repr(arg_layout) {
            arg_layout = layout;
        }

        let load = Expr::StructAtIndex {
            index: index as u64,
            field_layouts: arg_layouts.clone().into_bump_slice(),
            structure,
        };

        match argument {
            Identifier(symbol) => {
                stmt = Stmt::Let(*symbol, load, arg_layout, env.arena.alloc(stmt));
                is_productive = true;
            }
            Underscore => {
                // ignore
            }
            IntLiteral(_, _)
            | FloatLiteral(_, _)
            | DecimalLiteral(_)
            | EnumLiteral { .. }
            | BitLiteral { .. }
            | StrLiteral(_) => {}
            _ => {
                // store the field in a symbol, and continue matching on it
                let symbol = env.unique_symbol();

                // first recurse, continuing to unpack symbol
                match store_pattern_help(env, procs, layout_cache, argument, symbol, stmt) {
                    StorePattern::Productive(new) => {
                        is_productive = true;
                        stmt = new;
                        // only if we bind one of its (sub)fields to a used name should we
                        // extract the field
                        stmt = Stmt::Let(symbol, load, arg_layout, env.arena.alloc(stmt));
                    }
                    StorePattern::NotProductive(new) => {
                        // do nothing
                        stmt = new;
                    }
                }
            }
        }
    }

    if is_productive {
        StorePattern::Productive(stmt)
    } else {
        StorePattern::NotProductive(stmt)
    }
}

#[allow(clippy::too_many_arguments)]
fn store_tuple_destruct<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    destruct: &TupleDestruct<'a>,
    index: u64,
    outer_symbol: Symbol,
    sorted_fields: &'a [InLayout<'a>],
    mut stmt: Stmt<'a>,
) -> StorePattern<'a> {
    use Pattern::*;

    let load = Expr::StructAtIndex {
        index,
        field_layouts: sorted_fields,
        structure: outer_symbol,
    };

    match &destruct.pat {
        Identifier(symbol) => {
            stmt = Stmt::Let(*symbol, load, destruct.layout, env.arena.alloc(stmt));
        }
        Underscore => {
            // important that this is special-cased to do nothing: mono record patterns will extract all the
            // fields, but those not bound in the source code are guarded with the underscore
            // pattern. So given some record `{ x : a, y : b }`, a match
            //
            // { x } -> ...
            //
            // is actually
            //
            // { x, y: _ } -> ...
            //
            // internally. But `y` is never used, so we must make sure it't not stored/loaded.
            //
            // This also happens with tuples, so when matching a tuple `(a, b, c)`,
            // a pattern like `(x, y)` will be internally rewritten to `(x, y, _)`.
            return StorePattern::NotProductive(stmt);
        }
        IntLiteral(_, _)
        | FloatLiteral(_, _)
        | DecimalLiteral(_)
        | EnumLiteral { .. }
        | BitLiteral { .. }
        | StrLiteral(_) => {
            return StorePattern::NotProductive(stmt);
        }

        _ => {
            let symbol = env.unique_symbol();

            match store_pattern_help(env, procs, layout_cache, &destruct.pat, symbol, stmt) {
                StorePattern::Productive(new) => {
                    stmt = new;
                    stmt = Stmt::Let(symbol, load, destruct.layout, env.arena.alloc(stmt));
                }
                StorePattern::NotProductive(stmt) => return StorePattern::NotProductive(stmt),
            }
        }
    }

    StorePattern::Productive(stmt)
}

#[allow(clippy::too_many_arguments)]
fn store_record_destruct<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    destruct: &RecordDestruct<'a>,
    index: u64,
    outer_symbol: Symbol,
    sorted_fields: &'a [InLayout<'a>],
    mut stmt: Stmt<'a>,
) -> StorePattern<'a> {
    use Pattern::*;

    let load = Expr::StructAtIndex {
        index,
        field_layouts: sorted_fields,
        structure: outer_symbol,
    };

    match &destruct.typ {
        DestructType::Required(symbol) => {
            stmt = Stmt::Let(*symbol, load, destruct.layout, env.arena.alloc(stmt));
        }
        DestructType::Guard(guard_pattern) => match &guard_pattern {
            Identifier(symbol) => {
                stmt = Stmt::Let(*symbol, load, destruct.layout, env.arena.alloc(stmt));
            }
            Underscore => {
                // important that this is special-cased to do nothing: mono record patterns will extract all the
                // fields, but those not bound in the source code are guarded with the underscore
                // pattern. So given some record `{ x : a, y : b }`, a match
                //
                // { x } -> ...
                //
                // is actually
                //
                // { x, y: _ } -> ...
                //
                // internally. But `y` is never used, so we must make sure it't not stored/loaded.
                return StorePattern::NotProductive(stmt);
            }
            IntLiteral(_, _)
            | FloatLiteral(_, _)
            | DecimalLiteral(_)
            | EnumLiteral { .. }
            | BitLiteral { .. }
            | StrLiteral(_) => {
                return StorePattern::NotProductive(stmt);
            }

            _ => {
                let symbol = env.unique_symbol();

                match store_pattern_help(env, procs, layout_cache, guard_pattern, symbol, stmt) {
                    StorePattern::Productive(new) => {
                        stmt = new;
                        stmt = Stmt::Let(symbol, load, destruct.layout, env.arena.alloc(stmt));
                    }
                    StorePattern::NotProductive(stmt) => return StorePattern::NotProductive(stmt),
                }
            }
        },
    }

    StorePattern::Productive(stmt)
}
