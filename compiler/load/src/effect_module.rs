use roc_can::def::Declaration;
use roc_can::pattern::Pattern;
use roc_collections::all::SendMap;
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::{Located, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::Type;

pub fn build_effect_builtins(
    env: &mut roc_can::env::Env,
    scope: &mut roc_can::scope::Scope,
    effect_symbol: Symbol,
    var_store: &mut VarStore,
    exposed_vars_by_symbol: &mut Vec<(Symbol, Variable)>,
    declarations: &mut Vec<Declaration>,
) {
    // Effect.after : Effect a, (a -> Effect b) -> Effect b
    {
        let (symbol, def) = build_effect_after(
            env,
            scope,
            effect_symbol,
            TagName::Private(effect_symbol),
            var_store,
        );

        exposed_vars_by_symbol.push((symbol, def.expr_var));
        declarations.push(Declaration::Declare(def));
    }

    // Effect.map : Effect a, (a -> b) -> Effect b
    {
        let (symbol, def) = build_effect_map(
            env,
            scope,
            effect_symbol,
            TagName::Private(effect_symbol),
            var_store,
        );

        exposed_vars_by_symbol.push((symbol, def.expr_var));
        declarations.push(Declaration::Declare(def));
    }

    // Effect.always : a -> Effect a
    {
        let (symbol, def) = build_effect_always(
            env,
            scope,
            effect_symbol,
            TagName::Private(effect_symbol),
            var_store,
        );

        exposed_vars_by_symbol.push((symbol, def.expr_var));
        declarations.push(Declaration::Declare(def));
    }
}

fn build_effect_always(
    env: &mut roc_can::env::Env,
    scope: &mut roc_can::scope::Scope,
    effect_symbol: Symbol,
    effect_tag_name: TagName,
    var_store: &mut VarStore,
) -> (Symbol, roc_can::def::Def) {
    use roc_can::expr::Expr;
    use roc_can::expr::Recursive;

    // Effect.always = \value -> @Effect \{} -> value

    let value_symbol = {
        scope
            .introduce(
                "effect_always_value".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    let inner_closure_symbol = {
        scope
            .introduce(
                "effect_always_inner".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    let always_symbol = {
        scope
            .introduce(
                "always".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    // \{} -> value
    let const_closure = {
        let arguments = vec![(
            var_store.fresh(),
            Located::at_zero(empty_record_pattern(var_store)),
        )];

        let body = Expr::Var(value_symbol);

        Expr::Closure {
            function_type: var_store.fresh(),
            closure_type: var_store.fresh(),
            closure_ext_var: var_store.fresh(),
            return_type: var_store.fresh(),
            name: inner_closure_symbol,
            captured_symbols: vec![(value_symbol, var_store.fresh())],
            recursive: Recursive::NotRecursive,
            arguments,
            loc_body: Box::new(Located::at_zero(body)),
        }
    };

    // \value -> @Effect \{} -> value
    let (function_var, always_closure) = {
        // `@Effect \{} -> value`
        let body = Expr::Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: effect_tag_name.clone(),
            arguments: vec![(var_store.fresh(), Located::at_zero(const_closure))],
        };

        let arguments = vec![(
            var_store.fresh(),
            Located::at_zero(Pattern::Identifier(value_symbol)),
        )];

        let function_var = var_store.fresh();
        let closure = Expr::Closure {
            function_type: function_var,
            closure_type: var_store.fresh(),
            closure_ext_var: var_store.fresh(),
            return_type: var_store.fresh(),
            name: always_symbol,
            captured_symbols: Vec::new(),
            recursive: Recursive::NotRecursive,
            arguments,
            loc_body: Box::new(Located::at_zero(body)),
        };

        (function_var, closure)
    };

    use roc_can::annotation::IntroducedVariables;

    let mut introduced_variables = IntroducedVariables::default();

    let signature = {
        // Effect.always : a -> Effect a
        let var_a = var_store.fresh();

        introduced_variables.insert_named("a".into(), var_a);

        let effect_a = {
            let actual =
                build_effect_actual(effect_tag_name.clone(), Type::Variable(var_a), var_store);

            Type::Alias(
                effect_symbol,
                vec![("a".into(), Type::Variable(var_a))],
                Box::new(actual),
            )
        };

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(closure_var);

        Type::Function(
            vec![Type::Variable(var_a)],
            Box::new(Type::Variable(closure_var)),
            Box::new(effect_a),
        )
    };

    let def_annotation = roc_can::def::Annotation {
        signature,
        introduced_variables,
        aliases: SendMap::default(),
        region: Region::zero(),
    };

    let pattern = Pattern::Identifier(always_symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(always_symbol, function_var);
    let def = roc_can::def::Def {
        loc_pattern: Located::at_zero(pattern),
        loc_expr: Located::at_zero(always_closure),
        expr_var: function_var,
        pattern_vars,
        annotation: Some(def_annotation),
    };

    (always_symbol, def)
}

fn build_effect_map(
    env: &mut roc_can::env::Env,
    scope: &mut roc_can::scope::Scope,
    effect_symbol: Symbol,
    effect_tag_name: TagName,
    var_store: &mut VarStore,
) -> (Symbol, roc_can::def::Def) {
    use roc_can::expr::Expr;
    use roc_can::expr::Recursive;
    use roc_module::operator::CalledVia;

    // Effect.map = \@Effect thunk, mapper -> @Effect \{} -> mapper (thunk {})

    let thunk_symbol = {
        scope
            .introduce(
                "effect_map_thunk".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    let mapper_symbol = {
        scope
            .introduce(
                "effect_map_mapper".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    let map_symbol = {
        scope
            .introduce(
                "map".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    // `thunk {}`
    let force_thunk_call = {
        let boxed = (
            var_store.fresh(),
            Located::at_zero(Expr::Var(thunk_symbol)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![(var_store.fresh(), Located::at_zero(Expr::EmptyRecord))];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    // `toEffect (thunk {})`
    let mapper_call = {
        let boxed = (
            var_store.fresh(),
            Located::at_zero(Expr::Var(mapper_symbol)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![(var_store.fresh(), Located::at_zero(force_thunk_call))];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    let inner_closure_symbol = {
        scope
            .introduce(
                "effect_map_inner".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    // \{} -> mapper (thunk {})
    let inner_closure = {
        let arguments = vec![(
            var_store.fresh(),
            Located::at_zero(empty_record_pattern(var_store)),
        )];

        Expr::Closure {
            function_type: var_store.fresh(),
            closure_type: var_store.fresh(),
            closure_ext_var: var_store.fresh(),
            return_type: var_store.fresh(),
            name: inner_closure_symbol,
            captured_symbols: vec![
                (thunk_symbol, var_store.fresh()),
                (mapper_symbol, var_store.fresh()),
            ],
            recursive: Recursive::NotRecursive,
            arguments,
            loc_body: Box::new(Located::at_zero(mapper_call)),
        }
    };

    let arguments = vec![
        (
            var_store.fresh(),
            Located::at_zero(Pattern::AppliedTag {
                whole_var: var_store.fresh(),
                ext_var: var_store.fresh(),
                tag_name: effect_tag_name.clone(),
                arguments: vec![(
                    var_store.fresh(),
                    Located::at_zero(Pattern::Identifier(thunk_symbol)),
                )],
            }),
        ),
        (
            var_store.fresh(),
            Located::at_zero(Pattern::Identifier(mapper_symbol)),
        ),
    ];

    // `@Effect \{} -> (mapper (thunk {}))`
    let body = Expr::Tag {
        variant_var: var_store.fresh(),
        ext_var: var_store.fresh(),
        name: effect_tag_name.clone(),
        arguments: vec![(var_store.fresh(), Located::at_zero(inner_closure))],
    };

    let function_var = var_store.fresh();
    let map_closure = Expr::Closure {
        function_type: function_var,
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
        return_type: var_store.fresh(),
        name: map_symbol,
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments,
        loc_body: Box::new(Located::at_zero(body)),
    };

    use roc_can::annotation::IntroducedVariables;

    let mut introduced_variables = IntroducedVariables::default();

    let signature = {
        // Effect.map : Effect a, (a -> b) -> Effect b
        let var_a = var_store.fresh();
        let var_b = var_store.fresh();

        introduced_variables.insert_named("a".into(), var_a);
        introduced_variables.insert_named("b".into(), var_b);

        let effect_a = {
            let actual =
                build_effect_actual(effect_tag_name.clone(), Type::Variable(var_a), var_store);

            Type::Alias(
                effect_symbol,
                vec![("a".into(), Type::Variable(var_a))],
                Box::new(actual),
            )
        };

        let effect_b = {
            let actual = build_effect_actual(effect_tag_name, Type::Variable(var_b), var_store);

            Type::Alias(
                effect_symbol,
                vec![("b".into(), Type::Variable(var_b))],
                Box::new(actual),
            )
        };

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(closure_var);
        let a_to_b = {
            Type::Function(
                vec![Type::Variable(var_a)],
                Box::new(Type::Variable(closure_var)),
                Box::new(Type::Variable(var_b)),
            )
        };

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(closure_var);
        Type::Function(
            vec![effect_a, a_to_b],
            Box::new(Type::Variable(closure_var)),
            Box::new(effect_b),
        )
    };

    let def_annotation = roc_can::def::Annotation {
        signature,
        introduced_variables,
        aliases: SendMap::default(),
        region: Region::zero(),
    };

    let pattern = Pattern::Identifier(map_symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(map_symbol, function_var);
    let def = roc_can::def::Def {
        loc_pattern: Located::at_zero(pattern),
        loc_expr: Located::at_zero(map_closure),
        expr_var: function_var,
        pattern_vars,
        annotation: Some(def_annotation),
    };

    (map_symbol, def)
}

fn build_effect_after(
    env: &mut roc_can::env::Env,
    scope: &mut roc_can::scope::Scope,
    effect_symbol: Symbol,
    effect_tag_name: TagName,
    var_store: &mut VarStore,
) -> (Symbol, roc_can::def::Def) {
    use roc_can::expr::Expr;
    use roc_can::expr::Recursive;
    use roc_module::operator::CalledVia;

    // Effect.after = \@Effect effect, toEffect -> toEffect (effect {})

    let thunk_symbol = {
        scope
            .introduce(
                "effect_after_thunk".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    let to_effect_symbol = {
        scope
            .introduce(
                "effect_after_toEffect".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    let after_symbol = {
        scope
            .introduce(
                "after".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    // `thunk {}`
    let force_thunk_call = {
        let boxed = (
            var_store.fresh(),
            Located::at_zero(Expr::Var(thunk_symbol)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![(var_store.fresh(), Located::at_zero(Expr::EmptyRecord))];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    // `toEffect (thunk {})`
    let to_effect_call = {
        let boxed = (
            var_store.fresh(),
            Located::at_zero(Expr::Var(to_effect_symbol)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![(var_store.fresh(), Located::at_zero(force_thunk_call))];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    let arguments = vec![
        (
            var_store.fresh(),
            Located::at_zero(Pattern::AppliedTag {
                whole_var: var_store.fresh(),
                ext_var: var_store.fresh(),
                tag_name: effect_tag_name.clone(),
                arguments: vec![(
                    var_store.fresh(),
                    Located::at_zero(Pattern::Identifier(thunk_symbol)),
                )],
            }),
        ),
        (
            var_store.fresh(),
            Located::at_zero(Pattern::Identifier(to_effect_symbol)),
        ),
    ];

    let function_var = var_store.fresh();
    let after_closure = Expr::Closure {
        function_type: function_var,
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
        return_type: var_store.fresh(),
        name: after_symbol,
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments,
        loc_body: Box::new(Located::at_zero(to_effect_call)),
    };

    use roc_can::annotation::IntroducedVariables;

    let mut introduced_variables = IntroducedVariables::default();

    let signature = {
        let var_a = var_store.fresh();
        let var_b = var_store.fresh();

        introduced_variables.insert_named("a".into(), var_a);
        introduced_variables.insert_named("b".into(), var_b);

        let effect_a = {
            let actual =
                build_effect_actual(effect_tag_name.clone(), Type::Variable(var_a), var_store);

            Type::Alias(
                effect_symbol,
                vec![("a".into(), Type::Variable(var_a))],
                Box::new(actual),
            )
        };

        let effect_b = {
            let actual = build_effect_actual(effect_tag_name, Type::Variable(var_b), var_store);

            Type::Alias(
                effect_symbol,
                vec![("b".into(), Type::Variable(var_b))],
                Box::new(actual),
            )
        };

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(closure_var);
        let a_to_effect_b = Type::Function(
            vec![Type::Variable(var_a)],
            Box::new(Type::Variable(closure_var)),
            Box::new(effect_b.clone()),
        );

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(closure_var);
        Type::Function(
            vec![effect_a, a_to_effect_b],
            Box::new(Type::Variable(closure_var)),
            Box::new(effect_b),
        )
    };

    let def_annotation = roc_can::def::Annotation {
        signature,
        introduced_variables,
        aliases: SendMap::default(),
        region: Region::zero(),
    };

    let pattern = Pattern::Identifier(after_symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(after_symbol, function_var);
    let def = roc_can::def::Def {
        loc_pattern: Located::at_zero(pattern),
        loc_expr: Located::at_zero(after_closure),
        expr_var: function_var,
        pattern_vars,
        annotation: Some(def_annotation),
    };

    (after_symbol, def)
}

pub fn build_host_exposed_def(
    env: &mut roc_can::env::Env,
    scope: &mut roc_can::scope::Scope,
    symbol: Symbol,
    ident: &str,
    effect_tag_name: TagName,
    var_store: &mut VarStore,
    annotation: roc_can::annotation::Annotation,
) -> roc_can::def::Def {
    let expr_var = var_store.fresh();
    let pattern = Pattern::Identifier(symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(symbol, expr_var);

    use roc_can::expr::Expr;
    use roc_can::expr::Recursive;

    let mut arguments: Vec<(Variable, Located<Pattern>)> = Vec::new();
    let mut linked_symbol_arguments: Vec<(Variable, Expr)> = Vec::new();
    let mut captured_symbols: Vec<(Symbol, Variable)> = Vec::new();

    let def_body = {
        match annotation.typ.shallow_dealias() {
            Type::Function(args, _, _) => {
                for i in 0..args.len() {
                    let name = format!("closure_arg_{}_{}", ident, i);

                    let arg_symbol = {
                        let ident = name.clone().into();
                        scope
                            .introduce(
                                ident,
                                &env.exposed_ident_ids,
                                &mut env.ident_ids,
                                Region::zero(),
                            )
                            .unwrap()
                    };

                    let arg_var = var_store.fresh();

                    arguments.push((arg_var, Located::at_zero(Pattern::Identifier(arg_symbol))));

                    captured_symbols.push((arg_symbol, arg_var));
                    linked_symbol_arguments.push((arg_var, Expr::Var(arg_symbol)));
                }

                let foreign_symbol_name = format!("roc_fx_{}", ident);
                let low_level_call = Expr::ForeignCall {
                    foreign_symbol: foreign_symbol_name.into(),
                    args: linked_symbol_arguments,
                    ret_var: var_store.fresh(),
                };

                let effect_closure_symbol = {
                    let name = format!("effect_closure_{}", ident);

                    let ident = name.into();
                    scope
                        .introduce(
                            ident,
                            &env.exposed_ident_ids,
                            &mut env.ident_ids,
                            Region::zero(),
                        )
                        .unwrap()
                };

                let effect_closure = Expr::Closure {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    closure_ext_var: var_store.fresh(),
                    return_type: var_store.fresh(),
                    name: effect_closure_symbol,
                    captured_symbols,
                    recursive: Recursive::NotRecursive,
                    arguments: vec![(
                        var_store.fresh(),
                        Located::at_zero(empty_record_pattern(var_store)),
                    )],
                    loc_body: Box::new(Located::at_zero(low_level_call)),
                };

                let body = Expr::Tag {
                    variant_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    name: effect_tag_name,
                    arguments: vec![(var_store.fresh(), Located::at_zero(effect_closure))],
                };

                Expr::Closure {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    closure_ext_var: var_store.fresh(),
                    return_type: var_store.fresh(),
                    name: symbol,
                    captured_symbols: std::vec::Vec::new(),
                    recursive: Recursive::NotRecursive,
                    arguments,
                    loc_body: Box::new(Located::at_zero(body)),
                }
            }
            _ => {
                // not a function

                let foreign_symbol_name = format!("roc_fx_{}", ident);
                let low_level_call = Expr::ForeignCall {
                    foreign_symbol: foreign_symbol_name.into(),
                    args: linked_symbol_arguments,
                    ret_var: var_store.fresh(),
                };

                let effect_closure_symbol = {
                    let name = format!("effect_closure_{}", ident);

                    let ident = name.into();
                    scope
                        .introduce(
                            ident,
                            &env.exposed_ident_ids,
                            &mut env.ident_ids,
                            Region::zero(),
                        )
                        .unwrap()
                };

                let empty_record_pattern = Pattern::RecordDestructure {
                    whole_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    destructs: vec![],
                };

                let effect_closure = Expr::Closure {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    closure_ext_var: var_store.fresh(),
                    return_type: var_store.fresh(),
                    name: effect_closure_symbol,
                    captured_symbols,
                    recursive: Recursive::NotRecursive,
                    arguments: vec![(var_store.fresh(), Located::at_zero(empty_record_pattern))],
                    loc_body: Box::new(Located::at_zero(low_level_call)),
                };

                Expr::Tag {
                    variant_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    name: effect_tag_name,
                    arguments: vec![(var_store.fresh(), Located::at_zero(effect_closure))],
                }
            }
        }
    };

    let def_annotation = roc_can::def::Annotation {
        signature: annotation.typ,
        introduced_variables: annotation.introduced_variables,
        aliases: annotation.aliases,
        region: Region::zero(),
    };

    roc_can::def::Def {
        loc_pattern: Located::at_zero(pattern),
        loc_expr: Located::at_zero(def_body),
        expr_var,
        pattern_vars,
        annotation: Some(def_annotation),
    }
}

pub fn build_effect_actual(
    effect_tag_name: TagName,
    a_type: Type,
    var_store: &mut VarStore,
) -> Type {
    let closure_var = var_store.fresh();

    Type::TagUnion(
        vec![(
            effect_tag_name,
            vec![Type::Function(
                vec![Type::EmptyRec],
                Box::new(Type::Variable(closure_var)),
                Box::new(a_type),
            )],
        )],
        Box::new(Type::EmptyTagUnion),
    )
}

#[inline(always)]
fn empty_record_pattern(var_store: &mut VarStore) -> Pattern {
    Pattern::RecordDestructure {
        whole_var: var_store.fresh(),
        ext_var: var_store.fresh(),
        destructs: vec![],
    }
}
