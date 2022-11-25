#![allow(clippy::all)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
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
use roc_collections::all::{default_hasher, ImMap, MutMap, MutSet, SendMap};
use roc_error_macros::{internal_error, todo_abilities};
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_parse::ast::{self, CommentOrNewline, Defs, TypeDef, TypeHeader, ValueDef as AstValueDef};
use roc_parse::pattern::PatternType;
use roc_problem::can::{Problem, RuntimeError, ShadowKind};
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::AliasKind;
use std::collections::HashMap;
use std::fmt::Debug;
use ven_graph::{strongly_connected_components, topological_sort_into_groups};

use crate::{
    lang::{
        core::{
            expr::{expr2::Expr2, expr_to_expr2::expr_to_expr2, output::Output},
            fun_def::FunctionDef,
            pattern::{self, symbols_from_pattern, to_pattern_id, Pattern2, PatternId},
            types::{to_annotation2, Alias, Annotation2, Signature, Type2, TypeId},
            val_def::ValueDef,
        },
        env::Env,
        rigids::Rigids,
        scope::Scope,
    },
    mem_pool::{
        pool::{NodeId, Pool},
        pool_vec::PoolVec,
        shallow_clone::ShallowClone,
    },
};

#[derive(Debug)]
pub enum Def {
    AnnotationOnly { rigids: Rigids, annotation: TypeId },
    Value(ValueDef),
    Function(FunctionDef),
}

impl Def {
    pub fn symbols(&self, pool: &Pool) -> MutSet<Symbol> {
        let mut output = MutSet::default();

        match self {
            Def::AnnotationOnly { .. } => todo!("lost pattern information here ... "),
            Def::Value(value_def) => match value_def {
                ValueDef::WithAnnotation { pattern_id, .. }
                | ValueDef::NoAnnotation { pattern_id, .. } => {
                    let pattern2 = &pool[*pattern_id];
                    output.extend(symbols_from_pattern(pool, pattern2));
                }
            },
            Def::Function(function_def) => match function_def {
                FunctionDef::NoAnnotation { name, .. }
                | FunctionDef::WithAnnotation { name, .. } => {
                    output.insert(*name);
                }
            },
        }

        output
    }
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
pub enum PendingDef<'a> {
    /// A standalone annotation with no body
    AnnotationOnly(
        &'a Loc<ast::Pattern<'a>>,
        PatternId,
        &'a Loc<ast::TypeAnnotation<'a>>,
    ),
    /// A body with no type annotation
    Body(&'a Loc<ast::Pattern<'a>>, PatternId, &'a Loc<ast::Expr<'a>>),
    /// A body with a type annotation
    TypedBody(
        &'a Loc<ast::Pattern<'a>>,
        PatternId,
        &'a Loc<ast::TypeAnnotation<'a>>,
        &'a Loc<ast::Expr<'a>>,
    ),

    /// A type alias, e.g. `Ints : List Int`
    Alias {
        name: Loc<Symbol>,
        vars: Vec<Loc<Lowercase>>,
        ann: &'a Loc<ast::TypeAnnotation<'a>>,
    },

    /// An invalid alias, that is ignored in the rest of the pipeline
    /// e.g. a shadowed alias, or a definition like `MyAlias 1 : Int`
    /// with an incorrect pattern
    InvalidAlias,
}

pub enum AstDef<'a> {
    Type(TypeDef<'a>),
    Value(AstValueDef<'a>),

    // Blank Space (e.g. comments, spaces, newlines) before or after a def.
    // We preserve this for the formatter; canonicalization ignores it.
    SpaceBefore(&'a AstDef<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a AstDef<'a>, &'a [CommentOrNewline<'a>]),

    NotYetImplemented(&'static str),
}

fn to_pending_def<'a>(
    env: &mut Env<'a>,
    def: &'a AstDef<'a>,
    scope: &mut Scope,
    pattern_type: PatternType,
) -> Option<(Output, PendingDef<'a>)> {
    use AstDef::*;

    match def {
        Value(AstValueDef::Annotation(loc_pattern, loc_ann)) => {
            // This takes care of checking for shadowing and adding idents to scope.
            let (output, loc_can_pattern) = pattern::to_pattern_id(
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
        Value(AstValueDef::Body(loc_pattern, loc_expr)) => {
            // This takes care of checking for shadowing and adding idents to scope.
            let (output, loc_can_pattern) = pattern::to_pattern_id(
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

        Value(AstValueDef::AnnotatedBody {
            ann_pattern,
            ann_type,
            comment: _,
            body_pattern,
            body_expr,
        }) => {
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

        Type(TypeDef::Alias {
            header: TypeHeader { name, vars },
            ann,
        }) => {
            let region = Region::span_across(&name.region, &ann.region);

            match scope.introduce(
                name.value.into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                region,
            ) {
                Ok(symbol) => {
                    let mut can_rigids: Vec<Loc<Lowercase>> = Vec::with_capacity(vars.len());

                    for loc_var in vars.iter() {
                        match loc_var.value {
                            ast::Pattern::Identifier(name)
                                if name.chars().next().unwrap().is_lowercase() =>
                            {
                                let lowercase = Lowercase::from(name);
                                can_rigids.push(Loc {
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
                            name: Loc {
                                region: name.region,
                                value: symbol,
                            },
                            vars: can_rigids,
                            ann,
                        },
                    ))
                }

                Err((original_region, loc_shadowed_symbol)) => {
                    env.problem(Problem::Shadowing {
                        original_region,
                        shadow: loc_shadowed_symbol,
                        kind: ShadowKind::Variable,
                    });

                    Some((Output::default(), PendingDef::InvalidAlias))
                }
            }
        }

        Type(TypeDef::Opaque { .. }) => internal_error!("opaques not implemented"),
        Type(TypeDef::Ability { .. }) => todo_abilities!(),

        Value(AstValueDef::Dbg { .. }) => todo!(),
        Value(AstValueDef::Expect { .. }) => todo!(),
        Value(AstValueDef::ExpectFx { .. }) => todo!(),

        SpaceBefore(sub_def, _) | SpaceAfter(sub_def, _) => {
            to_pending_def(env, sub_def, scope, pattern_type)
        }

        NotYetImplemented(s) => todo!("{}", s),
    }
}

fn pending_typed_body<'a>(
    env: &mut Env<'a>,
    loc_pattern: &'a Loc<ast::Pattern<'a>>,
    loc_ann: &'a Loc<ast::TypeAnnotation<'a>>,
    loc_expr: &'a Loc<ast::Expr<'a>>,
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

fn from_pending_alias<'a>(
    env: &mut Env<'a>,
    scope: &mut Scope,
    name: Loc<Symbol>,
    vars: Vec<Loc<Lowercase>>,
    ann: &'a Loc<ast::TypeAnnotation<'a>>,
    mut output: Output,
) -> Output {
    let symbol = name.value;

    match to_annotation2(env, scope, &ann.value, ann.region) {
        Annotation2::Erroneous => todo!(),
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

            for loc_lowercase in vars {
                if !named_rigids.contains_key(&loc_lowercase.value) {
                    env.problem(Problem::PhantomTypeArgument {
                        typ: symbol,
                        variable_region: loc_lowercase.region,
                        variable_name: loc_lowercase.value.clone(),
                        alias_kind: AliasKind::Structural,
                    });
                }
            }

            let rigids = Rigids::new(named_rigids, unnamed_rigids, env.pool);

            let annotation = match signature {
                Signature::Value { annotation } => annotation,
                Signature::Function {
                    arguments,
                    closure_type_id,
                    return_type_id,
                } => Type2::Function(arguments, closure_type_id, return_type_id),
                Signature::FunctionWithAliases { annotation, .. } => annotation,
            };

            if annotation.contains_symbol(env.pool, symbol) {
                // the alias is recursive. If it's a tag union, we attempt to fix this
                if let Type2::TagUnion(tags, ext) = annotation {
                    // re-canonicalize the alias with the alias already in scope
                    let rec_var = env.var_store.fresh();
                    let rec_type_union = Type2::RecursiveTagUnion(rec_var, tags, ext);

                    // NOTE this only looks at the symbol, and just assumes that the
                    // recursion is not polymorphic
                    rec_type_union.substitute_alias(env.pool, symbol, Type2::Variable(rec_var));

                    let annotation_id = env.add(rec_type_union, ann.region);
                    let named = rigids.named(env.pool);

                    scope.add_alias(env.pool, symbol, named, annotation_id);
                } else {
                    env.problem(Problem::CyclicAlias(
                        symbol,
                        name.region,
                        vec![],
                        AliasKind::Structural,
                    ));
                    return output;
                }
            } else {
                let annotation_id = env.add(annotation, ann.region);
                let named = rigids.named(env.pool);

                scope.add_alias(env.pool, symbol, named, annotation_id);
            }

            output
        }
    }
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
    aliases: &mut MutMap<Symbol, Alias>,
) -> Output {
    use PendingDef::*;

    // Make types for the body expr, even if we won't end up having a body.
    let expr_var = env.var_store.fresh();

    match pending_def {
        AnnotationOnly(_, loc_can_pattern, loc_ann) => {
            // annotation sans body cannot introduce new rigids that are visible in other annotations
            // but the rigids can show up in type error messages, so still register them

            match to_annotation2(env, scope, &loc_ann.value, loc_ann.region) {
                Annotation2::Erroneous => todo!(),
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

                    let rigids = Rigids::new(named_rigids, unnamed_rigids, env.pool);

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

        PendingDef::Alias { name, ann, vars } => {
            from_pending_alias(env, scope, name, vars, ann, output)
        }

        InvalidAlias => {
            // invalid aliases (shadowed, incorrect patterns )
            todo!()
        }

        TypedBody(loc_pattern, loc_can_pattern, loc_ann, loc_expr) => {
            match to_annotation2(env, scope, &loc_ann.value, loc_ann.region) {
                Annotation2::Erroneous => todo!(),
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

                    // Ensure rigid type vars and their names are known in the output.
                    for (name, &var) in named_rigids.iter() {
                        output.introduced_variables.insert_named(name.clone(), var);
                    }
                    let rigids = Rigids::new(named_rigids, unnamed_rigids, env.pool);

                    // bookkeeping for tail-call detection. If we're assigning to an
                    // identifier (e.g. `f = \x -> ...`), then this symbol can be tail-called.
                    let outer_identifier = env.tailcallable_symbol;

                    if let Pattern2::Identifier(ref defined_symbol) = env.pool[loc_can_pattern] {
                        env.tailcallable_symbol = Some(*defined_symbol);
                    };

                    // register the name of this closure, to make sure the closure won't capture it's own name
                    if let (Pattern2::Identifier(ref defined_symbol), &ast::Expr::Closure(_, _)) =
                        (&env.pool[loc_can_pattern], &loc_expr.value)
                    {
                        env.closure_name_symbol = Some(*defined_symbol);
                    };

                    let (loc_can_expr, can_output) =
                        expr_to_expr2(env, scope, &loc_expr.value, loc_expr.region);

                    output.references.union_mut(can_output.references.clone());

                    // reset the tailcallable_symbol
                    env.tailcallable_symbol = outer_identifier;

                    // First, make sure we are actually assigning an identifier instead of (for example) a tag.
                    //
                    // If we're assigning (UserId userId) = ... then this is certainly not a closure declaration,
                    // which also implies it's not a self tail call!
                    //
                    // Only defs of the form (foo = ...) can be closure declarations or self tail calls.
                    match loc_can_expr {
                        Expr2::Closure {
                            args: closure_args,
                            body_id,
                            extra,
                            uniq_symbol: closure_symbol,
                            ..
                        } => {
                            let symbol = match env.pool[loc_can_pattern] {
                                Pattern2::Identifier(ref s) => *s,
                                _ => todo!(
                                    r"this is an error; functions must be bound with an identifier pattern!"
                                ),
                            };

                            // Since everywhere in the code it'll be referred to by its defined name,
                            // remove its generated name from the closure map.
                            let references =
                                env.closures.remove(&closure_symbol).unwrap_or_else(|| {
                            panic!( r"Tried to remove symbol {:?} from procedures, but it was not found: {:?}", closure_symbol, env.closures)
                            });

                            // TODO should we re-insert this function into env.closures?
                            env.closures.insert(symbol, references);

                            // Recursion doesn't count as referencing. (If it did, all recursive functions
                            // would result in circular def errors!)
                            refs_by_symbol.entry(symbol).and_modify(|(_, refs)| {
                                refs.lookups.remove(&symbol);
                            });

                            // Functions' references don't count in defs.
                            // See 3d5a2560057d7f25813112dfa5309956c0f9e6a9 and its
                            // parent commit for the bug this fixed!
                            let refs = References::new();

                            let arguments: PoolVec<(NodeId<Type2>, PatternId)> =
                                PoolVec::with_capacity(closure_args.len() as u32, env.pool);

                            let return_type: TypeId;

                            let annotation = match signature {
                                Signature::Value { .. } => {
                                    todo!("type annotation says 0 arguments, but it's a function")
                                }
                                Signature::Function {
                                    arguments: type_arguments,
                                    closure_type_id,
                                    return_type_id,
                                }
                                | Signature::FunctionWithAliases {
                                    arguments: type_arguments,
                                    closure_type_id,
                                    return_type_id,
                                    ..
                                } => {
                                    if arguments.len() != type_arguments.len() {
                                        panic!("argument number mismatch");
                                    }

                                    let it: Vec<_> = closure_args
                                        .iter(env.pool)
                                        .map(|(x, y)| (*x, *y))
                                        .zip(
                                            type_arguments
                                                .iter(env.pool)
                                                .map(|t| t.shallow_clone()),
                                        )
                                        .collect();

                                    for (node_id, ((_, pattern_id), typ)) in
                                        arguments.iter_node_ids().zip(it.into_iter())
                                    {
                                        let typ = env.pool.add(typ);
                                        env.pool[node_id] = (typ, pattern_id);
                                    }

                                    return_type = return_type_id;
                                }
                            };

                            let function_def = FunctionDef::WithAnnotation {
                                name: symbol,
                                arguments,
                                rigids: env.pool.add(rigids),
                                return_type,
                                body_id,
                            };

                            let def = Def::Function(function_def);

                            can_defs_by_symbol.insert(symbol, def);

                            output
                        }

                        _ => {
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

                            let value_def = ValueDef::WithAnnotation {
                                pattern_id: loc_can_pattern,
                                expr_id: env.pool.add(loc_can_expr),
                                type_id: annotation,
                                rigids,
                                expr_var: env.var_store.fresh(),
                            };

                            let def = Def::Value(value_def);

                            for symbol in
                                symbols_from_pattern(env.pool, env.pool.get(loc_can_pattern))
                            {
                                can_defs_by_symbol.insert(symbol, def.shallow_clone());
                            }

                            for (_, (symbol, region)) in scope.idents() {
                                // if !vars_by_symbol.contains_key(&symbol) {
                                //     continue;
                                // }
                                let refs = can_output.references.clone();
                                refs_by_symbol.insert(*symbol, (*region, refs));
                            }

                            output
                        }
                    }
                }
            }
        }

        Body(loc_pattern, loc_can_pattern, loc_expr) => {
            // bookkeeping for tail-call detection. If we're assigning to an
            // identifier (e.g. `f = \x -> ...`), then this symbol can be tail-called.
            let outer_identifier = env.tailcallable_symbol;

            if let Pattern2::Identifier(ref defined_symbol) = env.pool[loc_can_pattern] {
                env.tailcallable_symbol = Some(*defined_symbol);
            };

            // register the name of this closure, to make sure the closure won't capture it's own name
            if let (Pattern2::Identifier(ref defined_symbol), &ast::Expr::Closure(_, _)) =
                (&env.pool[loc_can_pattern], &loc_expr.value)
            {
                env.closure_name_symbol = Some(*defined_symbol);
            };

            let (loc_can_expr, can_output) =
                expr_to_expr2(env, scope, &loc_expr.value, loc_expr.region);

            output.references.union_mut(can_output.references.clone());

            // reset the tailcallable_symbol
            env.tailcallable_symbol = outer_identifier;

            // First, make sure we are actually assigning an identifier instead of (for example) a tag.
            //
            // If we're assigning (UserId userId) = ... then this is certainly not a closure declaration,
            // which also implies it's not a self tail call!
            //
            // Only defs of the form (foo = ...) can be closure declarations or self tail calls.
            match loc_can_expr {
                Expr2::Closure {
                    args: closure_args,
                    body_id,
                    extra,
                    uniq_symbol: closure_symbol,
                    ..
                } => {
                    let symbol = match env.pool[loc_can_pattern] {
                        Pattern2::Identifier(ref s) => *s,
                        _ => todo!(
                            r"this is an error; functions must be bound with an identifier pattern!"
                        ),
                    };

                    // Since everywhere in the code it'll be referred to by its defined name,
                    // remove its generated name from the closure map.
                    let references =
                        env.closures.remove(&closure_symbol).unwrap_or_else(|| {
                            panic!( r"Tried to remove symbol {:?} from procedures, but it was not found: {:?}", closure_symbol, env.closures)
                        });

                    // TODO should we re-insert this function into env.closures?
                    env.closures.insert(symbol, references);

                    // Recursion doesn't count as referencing. (If it did, all recursive functions
                    // would result in circular def errors!)
                    refs_by_symbol.entry(symbol).and_modify(|(_, refs)| {
                        refs.lookups.remove(&symbol);
                    });

                    // Functions' references don't count in defs.
                    // See 3d5a2560057d7f25813112dfa5309956c0f9e6a9 and its
                    // parent commit for the bug this fixed!
                    let refs = References::new();

                    let arguments: PoolVec<(Variable, PatternId)> =
                        PoolVec::with_capacity(closure_args.len() as u32, env.pool);

                    let it: Vec<_> = closure_args.iter(env.pool).map(|(x, y)| (*x, *y)).collect();

                    for (node_id, (_, pattern_id)) in arguments.iter_node_ids().zip(it.into_iter())
                    {
                        env.pool[node_id] = (env.var_store.fresh(), pattern_id);
                    }

                    let function_def = FunctionDef::NoAnnotation {
                        name: symbol,
                        arguments,
                        return_var: env.var_store.fresh(),
                        body_id,
                    };

                    let def = Def::Function(function_def);

                    can_defs_by_symbol.insert(symbol, def);

                    output
                }

                _ => {
                    let value_def = ValueDef::NoAnnotation {
                        pattern_id: loc_can_pattern,
                        expr_id: env.pool.add(loc_can_expr),
                        expr_var: env.var_store.fresh(),
                    };

                    let def = Def::Value(value_def);

                    for symbol in symbols_from_pattern(env.pool, env.pool.get(loc_can_pattern)) {
                        can_defs_by_symbol.insert(symbol, def.shallow_clone());
                    }

                    for (_, (symbol, region)) in scope.idents() {
                        // if !vars_by_symbol.contains_key(&symbol) {
                        //     continue;
                        // }
                        let refs = can_output.references.clone();
                        refs_by_symbol.insert(*symbol, (*region, refs));
                    }

                    output
                }
            }
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct References {
    pub bound_symbols: MutSet<Symbol>,
    pub lookups: MutSet<Symbol>,
    pub referenced_aliases: MutSet<Symbol>,
    pub calls: MutSet<Symbol>,
}

impl References {
    pub fn new() -> References {
        Self::default()
    }

    pub fn union_mut(&mut self, other: References) {
        self.lookups.extend(other.lookups);
        self.calls.extend(other.calls);
        self.bound_symbols.extend(other.bound_symbols);
        self.referenced_aliases.extend(other.referenced_aliases);
    }

    pub fn has_lookup(&self, symbol: Symbol) -> bool {
        self.lookups.contains(&symbol)
    }
}

#[derive(Debug)]
pub struct CanDefs {
    pub refs_by_symbol: MutMap<Symbol, (Region, References)>,
    pub can_defs_by_symbol: MutMap<Symbol, Def>,
    pub aliases: MutMap<Symbol, Alias>,
}

#[inline(always)]
pub fn canonicalize_defs<'a>(
    env: &mut Env<'a>,
    mut output: Output,
    original_scope: &Scope,
    loc_defs: &'a Defs<'a>,
    pattern_type: PatternType,
) -> (CanDefs, Scope, Output, MutMap<Symbol, Region>) {
    // Canonicalizing defs while detecting shadowing involves a multi-step process:
    //
    // 1. Go through each of the patterns.
    // 2. For each identifier pattern, get the scope.symbol() for the ident. (That symbol will use the home module for its module.)
    // 3. If that symbol is already in scope, then we're about to shadow it. Error!
    // 4. Otherwise, add it to the scope immediately, so we can detect shadowing within the same
    //    pattern (e.g. (Foo a a) = ...)
    // 5. Add this canonicalized pattern and its corresponding ast::Expr to pending_exprs.
    // 5. Once every pattern has been processed and added to scope, go back and canonicalize the exprs from
    //    pending_exprs, this time building up a canonical def for each one.
    //
    // This way, whenever any expr is doing lookups, it knows everything that's in scope -
    // even defs that appear after it in the source.
    //
    // This naturally handles recursion too, because a given expr which refers
    // to itself won't be processed until after its def has been added to scope.

    // Record both the original and final idents from the scope,
    // so we can diff them while detecting unused defs.
    let mut scope = original_scope.shallow_clone();
    let num_defs = loc_defs.len();
    let mut refs_by_symbol = MutMap::default();
    let mut can_defs_by_symbol = HashMap::with_capacity_and_hasher(num_defs, default_hasher());
    let mut pending = Vec::with_capacity(num_defs); // TODO bump allocate this!

    // Canonicalize all the patterns, record shadowing problems, and store
    // the ast::Expr values in pending_exprs for further canonicalization
    // once we've finished assembling the entire scope.
    for loc_def in loc_defs.defs() {
        let def = match loc_def {
            Ok(type_def) => AstDef::Type(type_def.clone()),
            Err(value_def) => AstDef::Value(value_def.clone()),
        };

        match to_pending_def(env, env.arena.alloc(def), &mut scope, pattern_type) {
            None => (),
            Some((new_output, pending_def)) => {
                // store the top-level defs, used to ensure that closures won't capture them
                if let PatternType::TopLevelDef = pattern_type {
                    match &pending_def {
                        PendingDef::AnnotationOnly(_, loc_can_pattern, _)
                        | PendingDef::Body(_, loc_can_pattern, _)
                        | PendingDef::TypedBody(_, loc_can_pattern, _, _) => {
                            env.top_level_symbols.extend(symbols_from_pattern(
                                env.pool,
                                env.pool.get(*loc_can_pattern),
                            ))
                        }
                        PendingDef::Alias { .. } | PendingDef::InvalidAlias => {}
                    }
                }
                // Record the ast::Expr for later. We'll do another pass through these
                // once we have the entire scope assembled. If we were to canonicalize
                // the exprs right now, they wouldn't have symbols in scope from defs
                // that get would have gotten added later in the defs list!
                pending.push(pending_def);
                output.union(new_output);
            }
        }
    }

    if cfg!(debug_assertions) {
        env.home.register_debug_idents(&env.ident_ids);
    }

    // TODO what to do here? aliases are already in the scope!
    let mut aliases = MutMap::default();
    let mut value_defs = Vec::new();

    for pending_def in pending.into_iter() {
        match pending_def {
            PendingDef::Alias { name, vars, ann } => {
                output = from_pending_alias(env, &mut scope, name, vars, ann, output);
            }
            other => value_defs.push(other),
        }
    }

    // TODO
    // correct_mutual_recursive_type_alias(env, &mut aliases, var_store);

    // Now that we have the scope completely assembled, and shadowing resolved,
    // we're ready to canonicalize any body exprs.
    for pending_def in value_defs.into_iter() {
        output = canonicalize_pending_def(
            env,
            pending_def,
            output,
            &mut scope,
            &mut can_defs_by_symbol,
            &mut refs_by_symbol,
            &mut aliases,
        );

        // TODO we should do something with these references; they include
        // things like type annotations.
    }

    // Determine which idents we introduced in the course of this process.
    let mut symbols_introduced = MutMap::default();

    for (symbol, region) in scope.symbols() {
        if !original_scope.contains_symbol(symbol) {
            symbols_introduced.insert(symbol, region);
        }
    }

    // This returns both the defs info as well as the new scope.
    //
    // We have to return the new scope because we added defs to it
    // (and those lookups shouldn't fail later, e.g. when canonicalizing
    // the return expr), but we didn't want to mutate the original scope
    // directly because we wanted to keep a clone of it around to diff
    // when looking for unused idents.
    //
    // We have to return the scope separately from the defs, because the
    // defs need to get moved later.
    (
        CanDefs {
            refs_by_symbol,
            can_defs_by_symbol,
            aliases,
        },
        scope,
        output,
        symbols_introduced,
    )
}

// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
pub enum Declaration {
    Declare(Def),
    DeclareRec(Vec<Def>),
    Builtin(Def),
    InvalidCycle(Vec<Symbol>, Vec<(Region /* pattern */, Region /* expr */)>),
}

impl Declaration {
    pub fn def_count(&self) -> usize {
        use Declaration::*;
        match self {
            Declare(_) => 1,
            DeclareRec(defs) => defs.len(),
            InvalidCycle(_, _) => 0,
            Builtin(_) => 0,
        }
    }
}

#[inline(always)]
pub fn sort_can_defs(
    env: &mut Env<'_>,
    defs: CanDefs,
    mut output: Output,
) -> (Result<Vec<Declaration>, RuntimeError>, Output) {
    let CanDefs {
        refs_by_symbol,
        can_defs_by_symbol,
        aliases,
    } = defs;

    // for (symbol, alias) in aliases.into_iter() {
    //     output.aliases.insert(symbol, alias);
    // }

    // Determine the full set of references by traversing the graph.
    let mut visited_symbols = MutSet::default();
    let returned_lookups = MutSet::clone(&output.references.lookups);

    // Start with the return expression's referenced locals. They're the only ones that count!
    //
    // If I have two defs which reference each other, but neither of them is referenced
    // in the return expression, I don't want either of them (or their references) to end up
    // in the final output.references. They were unused, and so were their references!
    //
    // The reason we need a graph here is so we don't overlook transitive dependencies.
    // For example, if I have `a = b + 1` and the def returns `a + 1`, then the
    // def as a whole references both `a` *and* `b`, even though it doesn't
    // directly mention `b` - because `a` depends on `b`. If we didn't traverse a graph here,
    // we'd erroneously give a warning that `b` was unused since it wasn't directly referenced.
    for symbol in returned_lookups.into_iter() {
        // We only care about local symbols in this analysis.
        if symbol.module_id() == env.home {
            // Traverse the graph and look up *all* the references for this local symbol.
            let refs =
                references_from_local(symbol, &mut visited_symbols, &refs_by_symbol, &env.closures);

            output.references.union_mut(refs);
        }
    }

    for symbol in output.references.calls.clone() {
        // Traverse the graph and look up *all* the references for this call.
        // Reuse the same visited_symbols as before; if we already visited it,
        // we won't learn anything new from visiting it again!
        let refs =
            references_from_call(symbol, &mut visited_symbols, &refs_by_symbol, &env.closures);

        output.references.union_mut(refs);
    }

    let mut defined_symbols: Vec<Symbol> = Vec::new();
    let mut defined_symbols_set: MutSet<Symbol> = MutSet::default();

    for symbol in can_defs_by_symbol.keys().into_iter() {
        defined_symbols.push(*symbol);
        defined_symbols_set.insert(*symbol);
    }

    // Use topological sort to reorder the defs based on their dependencies to one another.
    // This way, during code gen, no def will refer to a value that hasn't been initialized yet.
    // As a bonus, the topological sort also reveals any cycles between the defs, allowing
    // us to give a CircularAssignment error for invalid (mutual) recursion, and a `DeclareRec` for mutually
    // recursive definitions.

    // All successors that occur in the body of a symbol.
    let all_successors_without_self = |symbol: &Symbol| -> MutSet<Symbol> {
        // This may not be in refs_by_symbol. For example, the `f` in `f x` here:
        //
        // f = \z -> z
        //
        // (\x ->
        //     a = f x
        //     x
        // )
        //
        // It's not part of the current defs (the one with `a = f x`); rather,
        // it's in the enclosing scope. It's still referenced though, so successors
        // will receive it as an argument!
        match refs_by_symbol.get(symbol) {
            Some((_, references)) => {
                // We can only sort the symbols at the current level. That is safe because
                // symbols defined at higher levels cannot refer to symbols at lower levels.
                // Therefore they can never form a cycle!
                //
                // In the above example, `f` cannot reference `a`, and in the closure
                // a call to `f` cannot cycle back to `a`.
                let mut loc_succ = local_successors(&references, &env.closures);

                // if the current symbol is a closure, peek into its body
                if let Some(References { lookups, .. }) = env.closures.get(symbol) {
                    let home = env.home;

                    for lookup in lookups {
                        if lookup != symbol && lookup.module_id() == home {
                            // DO NOT register a self-call behind a lambda!
                            //
                            // We allow `boom = \_ -> boom {}`, but not `x = x`
                            loc_succ.insert(*lookup);
                        }
                    }
                }

                // remove anything that is not defined in the current block
                loc_succ.retain(|key| defined_symbols_set.contains(key));

                loc_succ
            }
            None => MutSet::default(),
        }
    };

    // All successors that occur in the body of a symbol, including the symbol itself
    // This is required to determine whether a symbol is recursive. Recursive symbols
    // (that are not faulty) always need a DeclareRec, even if there is just one symbol in the
    // group
    let mut all_successors_with_self = |symbol: &Symbol| -> MutSet<Symbol> {
        // This may not be in refs_by_symbol. For example, the `f` in `f x` here:
        //
        // f = \z -> z
        //
        // (\x ->
        //     a = f x
        //     x
        // )
        //
        // It's not part of the current defs (the one with `a = f x`); rather,
        // it's in the enclosing scope. It's still referenced though, so successors
        // will receive it as an argument!
        match refs_by_symbol.get(symbol) {
            Some((_, references)) => {
                // We can only sort the symbols at the current level. That is safe because
                // symbols defined at higher levels cannot refer to symbols at lower levels.
                // Therefore they can never form a cycle!
                //
                // In the above example, `f` cannot reference `a`, and in the closure
                // a call to `f` cannot cycle back to `a`.
                let mut loc_succ = local_successors(&references, &env.closures);

                // if the current symbol is a closure, peek into its body
                if let Some(References { lookups, .. }) = env.closures.get(symbol) {
                    for lookup in lookups {
                        loc_succ.insert(*lookup);
                    }
                }

                // remove anything that is not defined in the current block
                loc_succ.retain(|key| defined_symbols_set.contains(key));

                loc_succ
            }
            None => MutSet::default(),
        }
    };

    // If a symbol is a direct successor of itself, there is an invalid cycle.
    // The difference with the function above is that this one does not look behind lambdas,
    // but does consider direct self-recursion.
    let direct_successors = |symbol: &Symbol| -> MutSet<Symbol> {
        match refs_by_symbol.get(symbol) {
            Some((_, references)) => {
                let mut loc_succ = local_successors(&references, &env.closures);

                // NOTE: if the symbol is a closure we DONT look into its body

                // remove anything that is not defined in the current block
                loc_succ.retain(|key| defined_symbols_set.contains(key));

                // NOTE: direct recursion does matter here: `x = x` is invalid recursion!

                loc_succ
            }
            None => MutSet::default(),
        }
    };

    // TODO also do the same `addDirects` check elm/compiler does, so we can
    // report an error if a recursive definition can't possibly terminate!
    match ven_graph::topological_sort_into_groups(
        defined_symbols.as_slice(),
        all_successors_without_self,
    ) {
        Ok(groups) => {
            let mut declarations = Vec::new();

            // groups are in reversed order
            let mut can_defs_by_symbol = can_defs_by_symbol;
            let cdbs = &mut can_defs_by_symbol;
            for group in groups.into_iter().rev() {
                group_to_declaration(
                    &group,
                    &env.closures,
                    &mut all_successors_with_self,
                    cdbs,
                    &mut declarations,
                );
            }

            (Ok(declarations), output)
        }
        Err((mut groups, nodes_in_cycle)) => {
            let mut declarations = Vec::new();
            let problems = Vec::new();

            // nodes_in_cycle are symbols that form a syntactic cycle. That isn't always a problem,
            // and in general it's impossible to decide whether it is. So we use a crude heuristic:
            //
            // Definitions where the cycle occurs behind a lambda are OK
            //
            // boom = \_ -> boom {}
            //
            // But otherwise we report an error, e.g.
            //
            // foo = if b then foo else bar

            for cycle in strongly_connected_components(&nodes_in_cycle, all_successors_without_self)
            {
                // check whether the cycle is faulty, which is when it has
                // a direct successor in the current cycle. This catches things like:
                //
                // x = x
                //
                // or
                //
                // p = q
                // q = p
                let is_invalid_cycle = match cycle.get(0) {
                    Some(symbol) => {
                        let mut succs = direct_successors(symbol);

                        succs.retain(|key| cycle.contains(key));

                        !succs.is_empty()
                    }
                    None => false,
                };

                if is_invalid_cycle {
                    // We want to show the entire cycle in the error message, so expand it out.
                    let mut loc_symbols = Vec::new();

                    for symbol in cycle {
                        match refs_by_symbol.get(&symbol) {
                            None => unreachable!(
                                r#"Symbol `{:?}` not found in refs_by_symbol! refs_by_symbol was: {:?}"#,
                                symbol, refs_by_symbol
                            ),
                            Some((region, _)) => {
                                loc_symbols.push(Loc::at(*region, symbol));
                            }
                        }
                    }

                    // TODO we don't store those regions any more!
                    // let regions = Vec::with_capacity(can_defs_by_symbol.len());
                    // for def in can_defs_by_symbol.values() {
                    //     regions.push((def.loc_pattern.region, def.loc_expr.region));
                    // }
                    //
                    // // Sort them by line number to make the report more helpful.
                    // loc_symbols.sort();
                    // regions.sort();

                    // let symbols_in_cycle: Vec<Symbol> =
                    //     loc_symbols.into_iter().map(|s| s.value).collect();
                    //
                    // problems.push(Problem::RuntimeError(RuntimeError::CircularDef(
                    //     symbols_in_cycle.clone(),
                    //     regions.clone(),
                    // )));
                    //
                    // declarations.push(Declaration::InvalidCycle(symbols_in_cycle, regions));
                    panic!("Invalid Cycle");
                } else {
                    // slightly inefficient, because we know this becomes exactly one DeclareRec already
                    groups.push(cycle);
                }
            }

            // now we have a collection of groups whose dependencies are not cyclic.
            // They are however not yet topologically sorted. Here we have to get a bit
            // creative to get all the definitions in the correct sorted order.

            let mut group_ids = Vec::with_capacity(groups.len());
            let mut symbol_to_group_index = MutMap::default();
            for (i, group) in groups.iter().enumerate() {
                for symbol in group {
                    symbol_to_group_index.insert(*symbol, i);
                }

                group_ids.push(i);
            }

            let successors_of_group = |group_id: &usize| {
                let mut result = MutSet::default();

                // for each symbol in this group
                for symbol in &groups[*group_id] {
                    // find its successors
                    for succ in all_successors_without_self(symbol) {
                        // and add its group to the result
                        result.insert(symbol_to_group_index[&succ]);
                    }
                }

                // don't introduce any cycles to self
                result.remove(group_id);

                result
            };

            match ven_graph::topological_sort_into_groups(&group_ids, successors_of_group) {
                Ok(sorted_group_ids) => {
                    let mut can_defs_by_symbol = can_defs_by_symbol;
                    let cdbs = &mut can_defs_by_symbol;
                    for sorted_group in sorted_group_ids.iter().rev() {
                        for group_id in sorted_group.iter().rev() {
                            let group = &groups[*group_id];

                            group_to_declaration(
                                group,
                                &env.closures,
                                &mut all_successors_with_self,
                                cdbs,
                                &mut declarations,
                            );
                        }
                    }
                }
                Err(_) => unreachable!("there should be no cycles now!"),
            }

            for problem in problems {
                env.problem(problem);
            }

            (Ok(declarations), output)
        }
    }
}

pub fn references_from_local<'a, T>(
    defined_symbol: Symbol,
    visited: &'a mut MutSet<Symbol>,
    refs_by_def: &'a MutMap<Symbol, (T, References)>,
    closures: &'a MutMap<Symbol, References>,
) -> References
where
    T: Debug,
{
    let mut answer: References = References::new();

    match refs_by_def.get(&defined_symbol) {
        Some((_, refs)) => {
            visited.insert(defined_symbol);

            for local in refs.lookups.iter() {
                if !visited.contains(&local) {
                    let other_refs: References =
                        references_from_local(*local, visited, refs_by_def, closures);

                    answer.union_mut(other_refs);
                }

                answer.lookups.insert(*local);
            }

            for call in refs.calls.iter() {
                if !visited.contains(&call) {
                    let other_refs = references_from_call(*call, visited, refs_by_def, closures);

                    answer.union_mut(other_refs);
                }

                answer.calls.insert(*call);
            }

            answer
        }
        None => answer,
    }
}

pub fn references_from_call<'a, T>(
    call_symbol: Symbol,
    visited: &'a mut MutSet<Symbol>,
    refs_by_def: &'a MutMap<Symbol, (T, References)>,
    closures: &'a MutMap<Symbol, References>,
) -> References
where
    T: Debug,
{
    match closures.get(&call_symbol) {
        Some(references) => {
            let mut answer = references.clone();

            visited.insert(call_symbol);

            for closed_over_local in references.lookups.iter() {
                if !visited.contains(&closed_over_local) {
                    let other_refs =
                        references_from_local(*closed_over_local, visited, refs_by_def, closures);

                    answer.union_mut(other_refs);
                }

                answer.lookups.insert(*closed_over_local);
            }

            for call in references.calls.iter() {
                if !visited.contains(&call) {
                    let other_refs = references_from_call(*call, visited, refs_by_def, closures);

                    answer.union_mut(other_refs);
                }

                answer.calls.insert(*call);
            }

            answer
        }
        None => {
            // If the call symbol was not in the closure map, that means we're calling a non-function and
            // will get a type mismatch later. For now, assume no references as a result of the "call."
            References::new()
        }
    }
}

fn local_successors(
    references: &References,
    closures: &MutMap<Symbol, References>,
) -> MutSet<Symbol> {
    let mut answer = references.lookups.clone();

    for call_symbol in references.calls.iter() {
        answer.extend(call_successors(*call_symbol, closures));
    }

    answer
}

fn call_successors<'a>(
    call_symbol: Symbol,
    closures: &'a MutMap<Symbol, References>,
) -> MutSet<Symbol> {
    let mut answer = MutSet::default();
    let mut seen = MutSet::default();
    let mut queue = vec![call_symbol];

    while let Some(symbol) = queue.pop() {
        if seen.contains(&symbol) {
            continue;
        }

        if let Some(references) = closures.get(&symbol) {
            answer.extend(references.lookups.iter().copied());
            queue.extend(references.calls.iter().copied());

            seen.insert(symbol);
        }
    }

    answer
}

fn group_to_declaration(
    group: &[Symbol],
    closures: &MutMap<Symbol, References>,
    successors: &mut dyn FnMut(&Symbol) -> MutSet<Symbol>,
    can_defs_by_symbol: &mut MutMap<Symbol, Def>,
    declarations: &mut Vec<Declaration>,
) {
    use Declaration::*;

    // We want only successors in the current group, otherwise definitions get duplicated
    let filtered_successors = |symbol: &Symbol| -> MutSet<Symbol> {
        let mut result = successors(symbol);

        result.retain(|key| group.contains(key));
        result
    };

    // TODO fix this
    // Patterns like
    //
    // { x, y } = someDef
    //
    // Can bind multiple symbols. When not incorrectly recursive (which is guaranteed in this function),
    // normally `someDef` would be inserted twice. We use the region of the pattern as a unique key
    // for a definition, so every definition is only inserted (thus typechecked and emitted) once
    // let mut seen_pattern_regions: MutSet<Region> = MutSet::default();

    for cycle in strongly_connected_components(&group, filtered_successors) {
        if cycle.len() == 1 {
            let symbol = &cycle[0];

            if let Some(can_def) = can_defs_by_symbol.remove(&symbol) {
                // Determine recursivity of closures that are not tail-recursive

                let is_recursive = successors(&symbol).contains(&symbol);

                if is_recursive {
                    declarations.push(DeclareRec(vec![can_def]));
                } else {
                    declarations.push(Declare(can_def));
                }
            }
        } else {
            let mut can_defs = Vec::new();

            // Topological sort gives us the reverse of the sorting we want!
            for symbol in cycle.into_iter().rev() {
                if let Some(can_def) = can_defs_by_symbol.remove(&symbol) {
                    can_defs.push(can_def);
                }
            }

            declarations.push(DeclareRec(can_defs));
        }
    }
}
