// use crate::annotation::canonicalize_annotation;
// use crate::annotation::IntroducedVariables;
// use crate::env::Env;
// use crate::expr::Expr::{self, *};
// use crate::expr::{
//     canonicalize_expr, local_successors, references_from_call, references_from_local, Output,
//     Recursive,
// };
// use crate::pattern::{bindings_from_patterns, canonicalize_pattern, Pattern};
// use crate::procedure::References;
use crate::ast::{FunctionDef, Rigids, ValueDef};
use crate::expr::Env;
use crate::pattern::{
    symbols_and_variables_from_pattern, symbols_from_pattern, to_pattern_id, Pattern2, PatternId,
};
use crate::pool::{NodeId, Pool, PoolStr, PoolVec, ShallowClone};
use crate::scope::Scope;
use crate::types::{to_annotation2, Alias, Annotation2, References, Signature, Type2, TypeId};
use roc_can::expr::Output;
use roc_collections::all::{default_hasher, ImMap, ImSet, MutMap, MutSet, SendMap};
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_parse::ast;
use roc_parse::pattern::PatternType;
use roc_problem::can::{Problem, RuntimeError};
use roc_region::all::{Located, Region};
use roc_types::subs::{VarStore, Variable};
use std::collections::HashMap;
use std::fmt::Debug;
use ven_graph::{strongly_connected_components, topological_sort_into_groups};

#[derive(Debug)]
enum Def {
    AnnotationOnly {
        rigids: crate::ast::Rigids,
        annotation: TypeId,
    },
    Value(ValueDef),
    Function(FunctionDef),
}

impl ShallowClone for Def {
    fn shallow_clone(&self) -> Self {
        match self {
            Self::AnnotationOnly { rigids, annotation } => Self::AnnotationOnly {
                rigids: rigids.shallow_clone(),
                annotation: *annotation,
            },
            Self::Value(def) => Self::Value(def.shallow_clone()),
            Self::Function(def) => Self::Function(def.shallow_clone()),
        }
    }
}

/// A Def that has had patterns and type annnotations canonicalized,
/// but no Expr canonicalization has happened yet. Also, it has had spaces
/// and nesting resolved, and knows whether annotations are standalone or not.
#[derive(Debug)]
enum PendingDef<'a> {
    /// A standalone annotation with no body
    AnnotationOnly(
        &'a Located<ast::Pattern<'a>>,
        PatternId,
        &'a Located<ast::TypeAnnotation<'a>>,
    ),
    /// A body with no type annotation
    Body(
        &'a Located<ast::Pattern<'a>>,
        PatternId,
        &'a Located<ast::Expr<'a>>,
    ),
    /// A body with a type annotation
    TypedBody(
        &'a Located<ast::Pattern<'a>>,
        PatternId,
        &'a Located<ast::TypeAnnotation<'a>>,
        &'a Located<ast::Expr<'a>>,
    ),

    /// A type alias, e.g. `Ints : List Int`
    Alias {
        name: Located<Symbol>,
        vars: Vec<Located<Lowercase>>,
        ann: &'a Located<ast::TypeAnnotation<'a>>,
    },

    /// An invalid alias, that is ignored in the rest of the pipeline
    /// e.g. a shadowed alias, or a definition like `MyAlias 1 : Int`
    /// with an incorrect pattern
    InvalidAlias,
}

fn to_pending_def<'a>(
    env: &mut Env<'a>,
    def: &'a ast::Def<'a>,
    scope: &mut Scope,
    pattern_type: PatternType,
) -> Option<(Output, PendingDef<'a>)> {
    use roc_parse::ast::Def::*;

    match def {
        Annotation(loc_pattern, loc_ann) => {
            // This takes care of checking for shadowing and adding idents to scope.
            let (output, loc_can_pattern) = crate::pattern::to_pattern_id(
                env,
                scope,
                pattern_type,
                &loc_pattern.value,
                loc_pattern.region,
            );

            Some((
                output,
                PendingDef::AnnotationOnly(loc_pattern, loc_can_pattern, loc_ann),
            ))
        }
        Body(loc_pattern, loc_expr) => {
            // This takes care of checking for shadowing and adding idents to scope.
            let (output, loc_can_pattern) = crate::pattern::to_pattern_id(
                env,
                scope,
                pattern_type,
                &loc_pattern.value,
                loc_pattern.region,
            );

            Some((
                output,
                PendingDef::Body(loc_pattern, loc_can_pattern, loc_expr),
            ))
        }

        AnnotatedBody {
            ann_pattern,
            ann_type,
            comment: _,
            body_pattern,
            body_expr,
        } => {
            if ann_pattern.value.equivalent(&body_pattern.value) {
                // NOTE: Pick the body pattern, picking the annotation one is
                // incorrect in the presence of optional record fields!
                //
                // { x, y } : { x : Int, y ? Bool }*
                // { x, y ? False } = rec
                Some(pending_typed_body(
                    env,
                    body_pattern,
                    ann_type,
                    body_expr,
                    scope,
                    pattern_type,
                ))
            } else {
                // the pattern of the annotation does not match the pattern of the body direc
                env.problem(Problem::SignatureDefMismatch {
                    annotation_pattern: ann_pattern.region,
                    def_pattern: body_pattern.region,
                });

                // TODO: Should we instead build some PendingDef::InvalidAnnotatedBody ? This would
                // remove the `Option` on this function (and be probably more reliable for further
                // problem/error reporting)
                None
            }
        }

        roc_parse::ast::Def::Alias { name, vars, ann } => {
            let region = Region::span_across(&name.region, &ann.region);

            match scope.introduce(
                name.value.into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                region,
            ) {
                Ok(symbol) => {
                    let mut can_rigids: Vec<Located<Lowercase>> = Vec::with_capacity(vars.len());

                    for loc_var in vars.iter() {
                        match loc_var.value {
                            ast::Pattern::Identifier(name)
                                if name.chars().next().unwrap().is_lowercase() =>
                            {
                                let lowercase = Lowercase::from(name);
                                can_rigids.push(Located {
                                    value: lowercase,
                                    region: loc_var.region,
                                });
                            }
                            _ => {
                                // any other pattern in this position is a syntax error.
                                env.problem(Problem::InvalidAliasRigid {
                                    alias_name: symbol,
                                    region: loc_var.region,
                                });

                                return Some((Output::default(), PendingDef::InvalidAlias));
                            }
                        }
                    }

                    Some((
                        Output::default(),
                        PendingDef::Alias {
                            name: Located {
                                region: name.region,
                                value: symbol,
                            },
                            vars: can_rigids,
                            ann,
                        },
                    ))
                }

                Err((original_region, loc_shadowed_symbol)) => {
                    env.problem(Problem::ShadowingInAnnotation {
                        original_region,
                        shadow: loc_shadowed_symbol,
                    });

                    Some((Output::default(), PendingDef::InvalidAlias))
                }
            }
        }

        SpaceBefore(sub_def, _) | SpaceAfter(sub_def, _) | Nested(sub_def) => {
            to_pending_def(env, sub_def, scope, pattern_type)
        }

        NotYetImplemented(s) => todo!("{}", s),
    }
}

fn pending_typed_body<'a>(
    env: &mut Env<'a>,
    loc_pattern: &'a Located<ast::Pattern<'a>>,
    loc_ann: &'a Located<ast::TypeAnnotation<'a>>,
    loc_expr: &'a Located<ast::Expr<'a>>,
    scope: &mut Scope,
    pattern_type: PatternType,
) -> (Output, PendingDef<'a>) {
    // This takes care of checking for shadowing and adding idents to scope.
    let (output, loc_can_pattern) = to_pattern_id(
        env,
        scope,
        pattern_type,
        &loc_pattern.value,
        loc_pattern.region,
    );

    (
        output,
        PendingDef::TypedBody(loc_pattern, loc_can_pattern, loc_ann, loc_expr),
    )
}

// TODO trim down these arguments!
#[allow(clippy::too_many_arguments)]
#[allow(clippy::cognitive_complexity)]
fn canonicalize_pending_def<'a>(
    env: &mut Env<'a>,
    pending_def: PendingDef<'a>,
    mut output: Output,
    scope: &mut Scope,
    can_defs_by_symbol: &mut MutMap<Symbol, Def>,
    refs_by_symbol: &mut MutMap<Symbol, (Region, References)>,
    aliases: &mut SendMap<Symbol, Alias>,
) -> Output {
    use PendingDef::*;

    // Make types for the body expr, even if we won't end up having a body.
    let expr_var = env.var_store.fresh();

    match pending_def {
        AnnotationOnly(_, loc_can_pattern, loc_ann) => {
            // annotation sans body cannot introduce new rigids that are visible in other annotations
            // but the rigids can show up in type error messages, so still register them

            match to_annotation2(env, scope, &loc_ann.value, loc_ann.region) {
                Annotation2::Erroneous(_) => todo!(),
                Annotation2::Annotation {
                    named_rigids,
                    unnamed_rigids,
                    symbols,
                    signature,
                } => {
                    // Record all the annotation's references in output.references.lookups

                    for symbol in symbols {
                        output.references.lookups.insert(symbol);
                        output.references.referenced_aliases.insert(symbol);
                    }

                    let rigids = {
                        let named = PoolVec::with_capacity(named_rigids.len() as u32, env.pool);
                        let unnamed = PoolVec::with_capacity(unnamed_rigids.len() as u32, env.pool);

                        Rigids { named, unnamed }
                    };

                    let annotation = match signature {
                        Signature::Value { annotation } => annotation,
                        Signature::Function {
                            arguments,
                            closure_type_id,
                            return_type_id,
                        } => Type2::Function(arguments, closure_type_id, return_type_id),
                        Signature::FunctionWithAliases { annotation, .. } => annotation,
                    };
                    let annotation = env.add(annotation, loc_ann.region);

                    let def = Def::AnnotationOnly { rigids, annotation };

                    for symbol in symbols_from_pattern(env.pool, env.pool.get(loc_can_pattern)) {
                        can_defs_by_symbol.insert(symbol, def.shallow_clone());
                    }

                    output
                }
            }
        }
        _ => todo!(),
    }
}
