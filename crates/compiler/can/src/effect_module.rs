use crate::annotation::IntroducedVariables;
use crate::def::Def;
use crate::expr::{AnnotatedMark, ClosureData, Declarations, Expr, Recursive, WhenBranchPattern};
use crate::pattern::Pattern;
use crate::scope::Scope;
use roc_collections::{SendMap, VecMap, VecSet};
use roc_module::called_via::CalledVia;
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::{ExhaustiveMark, RedundantMark, VarStore, Variable};
use roc_types::types::{AliasKind, LambdaSet, OptAbleType, OptAbleVar, Type, TypeExtension};

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct HostedGeneratedFunctions {
    pub(crate) after: bool,
    pub(crate) map: bool,
    pub(crate) always: bool,
    pub(crate) loop_: bool,
    pub(crate) forever: bool,
}

/// the Effects alias & associated functions
///
/// A platform can define an Effect type in its header. It can have an arbitrary name
/// (e.g. Task, IO), but we'll call it an Effect in general.
///
/// From that name, we generate an effect module, an effect alias, and some functions.
///
/// The effect alias is implemented as
///
///  Effect a := {} -> a
///
/// For this alias we implement the functions specified in HostedGeneratedFunctions with the
/// standard implementation.
pub(crate) fn build_effect_builtins(
    scope: &mut Scope,
    effect_symbol: Symbol,
    var_store: &mut VarStore,
    exposed_symbols: &mut VecSet<Symbol>,
    declarations: &mut Declarations,
    generated_functions: HostedGeneratedFunctions,
) {
    macro_rules! helper {
        ($f:expr) => {{
            let (symbol, def) = $f(scope, effect_symbol, var_store);

            // make the outside world know this symbol exists
            exposed_symbols.insert(symbol);

            def
        }};
    }

    if generated_functions.after {
        let def = helper!(build_effect_after);
        declarations.push_def(def);
    }

    // Effect.map : Effect a, (a -> b) -> Effect b
    if generated_functions.map {
        let def = helper!(build_effect_map);
        declarations.push_def(def);
    }

    // Effect.always : a -> Effect a
    if generated_functions.always {
        let def = helper!(build_effect_always);
        declarations.push_def(def);
    }

    // Effect.forever : Effect a -> Effect b
    if generated_functions.forever {
        let def = helper!(build_effect_forever);
        declarations.push_def(def);
    }

    // Effect.loop : a, (a -> Effect [Step a, Done b]) -> Effect b
    if generated_functions.loop_ {
        let def = helper!(build_effect_loop);
        declarations.push_def(def);
    }

    // Useful when working on functions in this module. By default symbols that we named do now
    // show up with their name. We have to register them like below to make the names show up in
    // debug prints
    if false {
        scope.register_debug_idents();
    }
}

macro_rules! new_symbol {
    ($scope:expr, $name:expr) => {{
        $scope.introduce($name.into(), Region::zero()).unwrap()
    }};
}

fn build_effect_always(
    scope: &mut Scope,
    effect_symbol: Symbol,
    var_store: &mut VarStore,
) -> (Symbol, Def) {
    // Effect.always = \value -> @Effect \{} -> value

    let value_symbol = {
        scope
            .introduce("effect_always_value".into(), Region::zero())
            .unwrap()
    };

    let inner_closure_symbol = {
        scope
            .introduce("effect_always_inner".into(), Region::zero())
            .unwrap()
    };

    let always_symbol = { scope.introduce("always".into(), Region::zero()).unwrap() };

    // \{} -> value
    let const_closure = {
        let arguments = vec![(
            var_store.fresh(),
            AnnotatedMark::new(var_store),
            Loc::at_zero(empty_record_pattern(var_store)),
        )];

        let value_var = var_store.fresh();
        let body = Expr::Var(value_symbol, value_var);

        Expr::Closure(ClosureData {
            function_type: var_store.fresh(),
            closure_type: var_store.fresh(),
            return_type: var_store.fresh(),
            name: inner_closure_symbol,
            captured_symbols: vec![(value_symbol, value_var)],
            recursive: Recursive::NotRecursive,
            arguments,
            loc_body: Box::new(Loc::at_zero(body)),
        })
    };

    // \value -> @Effect \{} -> value
    let (function_var, always_closure) = {
        // `@Effect \{} -> value`
        let (specialized_def_type, type_arguments, lambda_set_variables) =
            build_fresh_opaque_variables(var_store);
        let body = Expr::OpaqueRef {
            opaque_var: var_store.fresh(),
            name: effect_symbol,
            argument: Box::new((var_store.fresh(), Loc::at_zero(const_closure))),
            specialized_def_type,
            type_arguments,
            lambda_set_variables,
        };

        let arguments = vec![(
            var_store.fresh(),
            AnnotatedMark::new(var_store),
            Loc::at_zero(Pattern::Identifier(value_symbol)),
        )];

        let function_var = var_store.fresh();
        let closure = Expr::Closure(ClosureData {
            function_type: function_var,
            closure_type: var_store.fresh(),
            return_type: var_store.fresh(),
            name: always_symbol,
            captured_symbols: Vec::new(),
            recursive: Recursive::NotRecursive,
            arguments,
            loc_body: Box::new(Loc::at_zero(body)),
        });

        (function_var, closure)
    };

    let mut introduced_variables = IntroducedVariables::default();

    let signature = {
        // Effect.always : a -> Effect a
        let var_a = var_store.fresh();
        introduced_variables.insert_named("a".into(), Loc::at_zero(var_a));

        let effect_a = build_effect_opaque(
            effect_symbol,
            var_a,
            Type::Variable(var_a),
            var_store,
            &mut introduced_variables,
        );

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(Loc::at_zero(closure_var));

        Type::Function(
            vec![Type::Variable(var_a)],
            Box::new(Type::Variable(closure_var)),
            Box::new(effect_a),
        )
    };

    let def_annotation = crate::def::Annotation {
        signature,
        introduced_variables,
        aliases: VecMap::default(),
        region: Region::zero(),
    };

    let pattern = Pattern::Identifier(always_symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(always_symbol, function_var);
    let def = Def {
        loc_pattern: Loc::at_zero(pattern),
        loc_expr: Loc::at_zero(always_closure),
        expr_var: function_var,
        pattern_vars,
        annotation: Some(def_annotation),
    };

    (always_symbol, def)
}

fn build_effect_map(
    scope: &mut Scope,
    effect_symbol: Symbol,
    var_store: &mut VarStore,
) -> (Symbol, Def) {
    // Effect.map = \@Effect thunk, mapper -> @Effect \{} -> mapper (thunk {})

    let thunk_symbol = {
        scope
            .introduce("effect_map_thunk".into(), Region::zero())
            .unwrap()
    };
    let thunk_var = var_store.fresh();

    let mapper_symbol = {
        scope
            .introduce("effect_map_mapper".into(), Region::zero())
            .unwrap()
    };
    let mapper_var = var_store.fresh();

    let map_symbol = { scope.introduce("map".into(), Region::zero()).unwrap() };

    // `thunk {}`
    let force_thunk_call = {
        let boxed = (
            thunk_var,
            Loc::at_zero(Expr::Var(thunk_symbol, thunk_var)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![(var_store.fresh(), Loc::at_zero(Expr::EmptyRecord))];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    // `toEffect (thunk {})`
    let mapper_call = {
        let boxed = (
            mapper_var,
            Loc::at_zero(Expr::Var(mapper_symbol, mapper_var)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![(var_store.fresh(), Loc::at_zero(force_thunk_call))];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    let inner_closure_symbol = {
        scope
            .introduce("effect_map_inner".into(), Region::zero())
            .unwrap()
    };

    // \{} -> mapper (thunk {})
    let inner_closure = {
        let arguments = vec![(
            var_store.fresh(),
            AnnotatedMark::new(var_store),
            Loc::at_zero(empty_record_pattern(var_store)),
        )];

        Expr::Closure(ClosureData {
            function_type: var_store.fresh(),
            closure_type: var_store.fresh(),
            return_type: var_store.fresh(),
            name: inner_closure_symbol,
            captured_symbols: vec![
                (thunk_symbol, var_store.fresh()),
                (mapper_symbol, var_store.fresh()),
            ],
            recursive: Recursive::NotRecursive,
            arguments,
            loc_body: Box::new(Loc::at_zero(mapper_call)),
        })
    };

    // \@Effect thunk, mapper
    let (specialized_def_type, type_arguments, lambda_set_variables) =
        build_fresh_opaque_variables(var_store);
    let arguments = vec![
        (
            var_store.fresh(),
            AnnotatedMark::new(var_store),
            Loc::at_zero(Pattern::UnwrappedOpaque {
                opaque: effect_symbol,
                whole_var: var_store.fresh(),
                argument: Box::new((
                    var_store.fresh(),
                    Loc::at_zero(Pattern::Identifier(thunk_symbol)),
                )),
                specialized_def_type,
                type_arguments,
                lambda_set_variables,
            }),
        ),
        (
            var_store.fresh(),
            AnnotatedMark::new(var_store),
            Loc::at_zero(Pattern::Identifier(mapper_symbol)),
        ),
    ];

    // `@Effect \{} -> (mapper (thunk {}))`
    let (specialized_def_type, type_arguments, lambda_set_variables) =
        build_fresh_opaque_variables(var_store);
    let body = Expr::OpaqueRef {
        opaque_var: var_store.fresh(),
        name: effect_symbol,
        argument: Box::new((var_store.fresh(), Loc::at_zero(inner_closure))),
        specialized_def_type,
        type_arguments,
        lambda_set_variables,
    };

    let function_var = var_store.fresh();
    let map_closure = Expr::Closure(ClosureData {
        function_type: function_var,
        closure_type: var_store.fresh(),
        return_type: var_store.fresh(),
        name: map_symbol,
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments,
        loc_body: Box::new(Loc::at_zero(body)),
    });

    let mut introduced_variables = IntroducedVariables::default();

    let signature = {
        // Effect.map : Effect a, (a -> b) -> Effect b
        let var_a = var_store.fresh();
        let var_b = var_store.fresh();

        introduced_variables.insert_named("a".into(), Loc::at_zero(var_a));
        introduced_variables.insert_named("b".into(), Loc::at_zero(var_b));

        let effect_a = build_effect_opaque(
            effect_symbol,
            var_a,
            Type::Variable(var_a),
            var_store,
            &mut introduced_variables,
        );

        let effect_b = build_effect_opaque(
            effect_symbol,
            var_b,
            Type::Variable(var_b),
            var_store,
            &mut introduced_variables,
        );

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(Loc::at_zero(closure_var));
        let a_to_b = {
            Type::Function(
                vec![Type::Variable(var_a)],
                Box::new(Type::Variable(closure_var)),
                Box::new(Type::Variable(var_b)),
            )
        };

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(Loc::at_zero(closure_var));
        Type::Function(
            vec![effect_a, a_to_b],
            Box::new(Type::Variable(closure_var)),
            Box::new(effect_b),
        )
    };

    let def_annotation = crate::def::Annotation {
        signature,
        introduced_variables,
        aliases: VecMap::default(),
        region: Region::zero(),
    };

    let pattern = Pattern::Identifier(map_symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(map_symbol, function_var);
    let def = Def {
        loc_pattern: Loc::at_zero(pattern),
        loc_expr: Loc::at_zero(map_closure),
        expr_var: function_var,
        pattern_vars,
        annotation: Some(def_annotation),
    };

    (map_symbol, def)
}

fn force_thunk(expr: Expr, thunk_var: Variable, var_store: &mut VarStore) -> Expr {
    let boxed = (
        thunk_var,
        Loc::at_zero(expr),
        var_store.fresh(),
        var_store.fresh(),
    );

    let arguments = vec![(var_store.fresh(), Loc::at_zero(Expr::EmptyRecord))];
    Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
}

fn build_effect_after(
    scope: &mut Scope,
    effect_opaque_symbol: Symbol,
    var_store: &mut VarStore,
) -> (Symbol, Def) {
    //    Effect.after = \@Effect effect, toEffect ->
    //        @Effect \{} ->
    //            when toEffect (effect {}) is
    //            @Effect thunk -> thunk {}

    let thunk_symbol = new_symbol!(scope, "effect_after_thunk");

    let effect_symbol = new_symbol!(scope, "effect_after_effect");
    let to_effect_symbol = new_symbol!(scope, "effect_after_toEffect");
    let after_symbol = new_symbol!(scope, "after");
    let outer_closure_symbol = new_symbol!(scope, "effect_after_inner");

    // `effect {}`
    let force_effect_var = var_store.fresh();
    let force_effect_call = force_thunk(
        Expr::Var(effect_symbol, force_effect_var),
        force_effect_var,
        var_store,
    );

    // `toEffect (effect {})`
    let to_effect_var = var_store.fresh();
    let to_effect_call = {
        let boxed = (
            to_effect_var,
            Loc::at_zero(Expr::Var(to_effect_symbol, to_effect_var)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![(var_store.fresh(), Loc::at_zero(force_effect_call))];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    // let @Effect thunk = toEffect (effect {}) in thunk {}
    let let_effect_thunk = {
        // `thunk {}`
        let force_inner_thunk_var = var_store.fresh();
        let force_inner_thunk_call = force_thunk(
            Expr::Var(thunk_symbol, force_inner_thunk_var),
            force_inner_thunk_var,
            var_store,
        );

        let (specialized_def_type, type_arguments, lambda_set_variables) =
            build_fresh_opaque_variables(var_store);

        let pattern = Pattern::UnwrappedOpaque {
            whole_var: var_store.fresh(),
            opaque: effect_opaque_symbol,
            argument: Box::new((
                var_store.fresh(),
                Loc::at_zero(Pattern::Identifier(thunk_symbol)),
            )),
            specialized_def_type,
            type_arguments,
            lambda_set_variables,
        };
        let pattern = WhenBranchPattern {
            pattern: Loc::at_zero(pattern),
            degenerate: false,
        };

        let branches = vec![crate::expr::WhenBranch {
            guard: None,
            value: Loc::at_zero(force_inner_thunk_call),
            patterns: vec![pattern],
            redundant: RedundantMark::new(var_store),
        }];

        Expr::When {
            cond_var: var_store.fresh(),
            branches_cond_var: var_store.fresh(),
            expr_var: var_store.fresh(),
            region: Region::zero(),
            loc_cond: Box::new(Loc::at_zero(to_effect_call)),
            branches,
            exhaustive: ExhaustiveMark::new(var_store),
        }
    };

    // @Effect \{} -> when toEffect (effect {}) is @Effect thunk -> thunk {}
    let outer_effect = wrap_in_effect_thunk(
        let_effect_thunk,
        effect_opaque_symbol,
        outer_closure_symbol,
        vec![effect_symbol, to_effect_symbol],
        var_store,
    );

    scope.register_debug_idents();

    let (specialized_def_type, type_arguments, lambda_set_variables) =
        build_fresh_opaque_variables(var_store);

    let arguments = vec![
        (
            var_store.fresh(),
            AnnotatedMark::new(var_store),
            Loc::at_zero(Pattern::UnwrappedOpaque {
                opaque: effect_opaque_symbol,
                whole_var: var_store.fresh(),
                argument: Box::new((
                    var_store.fresh(),
                    Loc::at_zero(Pattern::Identifier(effect_symbol)),
                )),
                specialized_def_type,
                type_arguments,
                lambda_set_variables,
            }),
        ),
        (
            var_store.fresh(),
            AnnotatedMark::new(var_store),
            Loc::at_zero(Pattern::Identifier(to_effect_symbol)),
        ),
    ];

    let function_var = var_store.fresh();
    let after_closure = Expr::Closure(ClosureData {
        function_type: function_var,
        closure_type: var_store.fresh(),
        return_type: var_store.fresh(),
        name: after_symbol,
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments,
        loc_body: Box::new(Loc::at_zero(outer_effect)),
    });

    let mut introduced_variables = IntroducedVariables::default();

    let signature = {
        let var_a = var_store.fresh();
        let var_b = var_store.fresh();

        introduced_variables.insert_named("a".into(), Loc::at_zero(var_a));
        introduced_variables.insert_named("b".into(), Loc::at_zero(var_b));

        let effect_a = build_effect_opaque(
            effect_opaque_symbol,
            var_a,
            Type::Variable(var_a),
            var_store,
            &mut introduced_variables,
        );

        let effect_b1 = build_effect_opaque(
            effect_opaque_symbol,
            var_b,
            Type::Variable(var_b),
            var_store,
            &mut introduced_variables,
        );

        // we need a second b2 to give it a unique lambda set variable
        let effect_b2 = build_effect_opaque(
            effect_opaque_symbol,
            var_b,
            Type::Variable(var_b),
            var_store,
            &mut introduced_variables,
        );

        let closure_var = var_store.fresh();
        introduced_variables.insert_lambda_set(closure_var);
        let a_to_effect_b = Type::Function(
            vec![Type::Variable(var_a)],
            Box::new(Type::Variable(closure_var)),
            Box::new(effect_b1),
        );

        let closure_var = var_store.fresh();
        introduced_variables.insert_lambda_set(closure_var);
        Type::Function(
            vec![effect_a, a_to_effect_b],
            Box::new(Type::Variable(closure_var)),
            Box::new(effect_b2),
        )
    };

    let def_annotation = crate::def::Annotation {
        signature,
        introduced_variables,
        aliases: VecMap::default(),
        region: Region::zero(),
    };

    let pattern = Pattern::Identifier(after_symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(after_symbol, function_var);
    let def = Def {
        loc_pattern: Loc::at_zero(pattern),
        loc_expr: Loc::at_zero(after_closure),
        expr_var: function_var,
        pattern_vars,
        annotation: Some(def_annotation),
    };

    (after_symbol, def)
}

/// turn `value` into `@Effect \{} -> value`
fn wrap_in_effect_thunk(
    body: Expr,
    effect_symbol: Symbol,
    closure_name: Symbol,
    captured_symbols: Vec<Symbol>,
    var_store: &mut VarStore,
) -> Expr {
    let captured_symbols: Vec<_> = captured_symbols
        .into_iter()
        .map(|x| (x, var_store.fresh()))
        .collect();

    // \{} -> body
    let const_closure = {
        let arguments = vec![(
            var_store.fresh(),
            AnnotatedMark::new(var_store),
            Loc::at_zero(empty_record_pattern(var_store)),
        )];

        Expr::Closure(ClosureData {
            function_type: var_store.fresh(),
            closure_type: var_store.fresh(),
            return_type: var_store.fresh(),
            name: closure_name,
            // captured_symbols: vec![(value_symbol, var_store.fresh())],
            captured_symbols,
            recursive: Recursive::NotRecursive,
            arguments,
            loc_body: Box::new(Loc::at_zero(body)),
        })
    };

    // `@Effect \{} -> value`
    let (specialized_def_type, type_arguments, lambda_set_variables) =
        build_fresh_opaque_variables(var_store);
    Expr::OpaqueRef {
        opaque_var: var_store.fresh(),
        name: effect_symbol,
        argument: Box::new((var_store.fresh(), Loc::at_zero(const_closure))),
        specialized_def_type,
        type_arguments,
        lambda_set_variables,
    }
}

/// given `effect : Effect a`, unwrap the thunk and force it, giving a value of type `a`
fn force_effect(
    effect: Expr,
    effect_symbol: Symbol,
    thunk_symbol: Symbol,
    var_store: &mut VarStore,
) -> Expr {
    let whole_var = var_store.fresh();

    let thunk_var = var_store.fresh();

    let (specialized_def_type, type_arguments, lambda_set_variables) =
        build_fresh_opaque_variables(var_store);
    let pattern = Pattern::UnwrappedOpaque {
        whole_var,
        opaque: effect_symbol,
        argument: Box::new((thunk_var, Loc::at_zero(Pattern::Identifier(thunk_symbol)))),
        specialized_def_type,
        type_arguments,
        lambda_set_variables,
    };

    let pattern_vars = SendMap::default();
    // pattern_vars.insert(thunk_symbol, thunk_var);

    let def = Def {
        loc_pattern: Loc::at_zero(pattern),
        loc_expr: Loc::at_zero(effect),
        expr_var: var_store.fresh(),
        pattern_vars,
        annotation: None,
    };

    let ret_var = var_store.fresh();

    let force_thunk_call = {
        let thunk_var = var_store.fresh();
        let boxed = (
            thunk_var,
            Loc::at_zero(Expr::Var(thunk_symbol, thunk_var)),
            var_store.fresh(),
            ret_var,
        );

        let arguments = vec![(var_store.fresh(), Loc::at_zero(Expr::EmptyRecord))];
        let call = Expr::Call(Box::new(boxed), arguments, CalledVia::Space);

        Loc::at_zero(call)
    };

    Expr::LetNonRec(Box::new(def), Box::new(force_thunk_call))
}

fn build_effect_forever(
    scope: &mut Scope,
    effect_symbol: Symbol,
    var_store: &mut VarStore,
) -> (Symbol, Def) {
    // morally
    //
    //  Effect.forever = \effect -> Effect.after effect (\_ -> Effect.forever effect)
    //
    // Here we inline the `Effect.after`, and get
    //
    //  Effect.forever : Effect a -> Effect b
    //  Effect.forever = \effect ->
    //      @Effect \{} ->
    //          @Effect thunk1 = effect
    //          _ = thunk1 {}
    //          @Effect thunk2 = Effect.forever effect
    //          thunk2 {}
    //
    // We then rely on our defunctionalization to turn this into a tail-recursive loop.
    // First the `@Effect` wrapper melts away
    //
    //  Effect.forever : ({} -> a) -> ({} -> b)
    //  Effect.forever = \effect ->
    //      \{} ->
    //          thunk1 = effect
    //          _ = thunk1 {}
    //          thunk2 = Effect.forever effect
    //          thunk2 {}
    //
    // Then we defunctionalize
    //
    //  foreverInner = \{}, { effect } ->
    //      thunk1 = effect
    //      _ = thunk1 {}
    //      thunk2 = Effect.forever effect
    //      thunk2 {}
    //
    //  Effect.forever : [C foreverInner { effect : T }]
    //  Effect.forever = \effect ->
    //      C { effect }
    //
    // And we have to adjust the call
    //
    //  foreverInner = \{}, { effect } ->
    //      thunk1 = effect
    //      _ = thunk1 {}
    //      thunk2 = Effect.forever effect
    //      when thunk2 is
    //          C env -> foreverInner {} env.effect
    //
    // Making `foreverInner` perfectly tail-call optimizable

    let forever_symbol = { scope.introduce("forever".into(), Region::zero()).unwrap() };

    let effect = { scope.introduce("effect".into(), Region::zero()).unwrap() };

    let body = build_effect_forever_body(scope, effect_symbol, forever_symbol, effect, var_store);

    let arguments = vec![(
        var_store.fresh(),
        AnnotatedMark::new(var_store),
        Loc::at_zero(Pattern::Identifier(effect)),
    )];

    let function_var = var_store.fresh();
    let after_closure = Expr::Closure(ClosureData {
        function_type: var_store.fresh(),
        closure_type: var_store.fresh(),
        return_type: var_store.fresh(),
        name: forever_symbol,
        captured_symbols: Vec::new(),
        recursive: Recursive::Recursive,
        arguments,
        loc_body: Box::new(Loc::at_zero(body)),
    });

    let mut introduced_variables = IntroducedVariables::default();

    let signature = {
        let var_a = var_store.fresh();
        let var_b = var_store.fresh();

        introduced_variables.insert_named("a".into(), Loc::at_zero(var_a));
        introduced_variables.insert_named("b".into(), Loc::at_zero(var_b));

        let effect_a = build_effect_opaque(
            effect_symbol,
            var_a,
            Type::Variable(var_a),
            var_store,
            &mut introduced_variables,
        );

        let effect_b = build_effect_opaque(
            effect_symbol,
            var_b,
            Type::Variable(var_b),
            var_store,
            &mut introduced_variables,
        );

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(Loc::at_zero(closure_var));

        Type::Function(
            vec![effect_a],
            Box::new(Type::Variable(closure_var)),
            Box::new(effect_b),
        )
    };

    let def_annotation = crate::def::Annotation {
        signature,
        introduced_variables,
        aliases: VecMap::default(),
        region: Region::zero(),
    };

    let pattern = Pattern::Identifier(forever_symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(forever_symbol, function_var);
    let def = Def {
        loc_pattern: Loc::at_zero(pattern),
        loc_expr: Loc::at_zero(after_closure),
        expr_var: function_var,
        pattern_vars,
        annotation: Some(def_annotation),
    };

    (forever_symbol, def)
}

fn build_effect_forever_body(
    scope: &mut Scope,
    effect_symbol: Symbol,
    forever_symbol: Symbol,
    effect: Symbol,
    var_store: &mut VarStore,
) -> Expr {
    let closure_name = {
        scope
            .introduce("forever_inner".into(), Region::zero())
            .unwrap()
    };

    let inner_body =
        build_effect_forever_inner_body(scope, effect_symbol, forever_symbol, effect, var_store);

    let captured_symbols = vec![effect];
    wrap_in_effect_thunk(
        inner_body,
        effect_symbol,
        closure_name,
        captured_symbols,
        var_store,
    )
}

fn build_effect_forever_inner_body(
    scope: &mut Scope,
    effect_symbol: Symbol,
    forever_symbol: Symbol,
    effect: Symbol,
    var_store: &mut VarStore,
) -> Expr {
    let thunk1_var = var_store.fresh();
    let thunk1_symbol = { scope.introduce("thunk1".into(), Region::zero()).unwrap() };

    let thunk2_symbol = { scope.introduce("thunk2".into(), Region::zero()).unwrap() };

    // @Effect thunk1 = effect
    let thunk_from_effect = {
        let whole_var = var_store.fresh();

        let thunk_var = var_store.fresh();

        let (specialized_def_type, type_arguments, lambda_set_variables) =
            build_fresh_opaque_variables(var_store);
        let pattern = Pattern::UnwrappedOpaque {
            whole_var,
            opaque: effect_symbol,
            argument: Box::new((thunk_var, Loc::at_zero(Pattern::Identifier(thunk1_symbol)))),
            specialized_def_type,
            type_arguments,
            lambda_set_variables,
        };

        let pattern_vars = SendMap::default();

        Def {
            loc_pattern: Loc::at_zero(pattern),
            loc_expr: Loc::at_zero(Expr::Var(effect, var_store.fresh())),
            expr_var: var_store.fresh(),
            pattern_vars,
            annotation: None,
        }
    };

    // thunk1 {}
    let force_thunk_call = {
        let ret_var = var_store.fresh();
        let boxed = (
            thunk1_var,
            Loc::at_zero(Expr::Var(thunk1_symbol, thunk1_var)),
            var_store.fresh(),
            ret_var,
        );

        let arguments = vec![(var_store.fresh(), Loc::at_zero(Expr::EmptyRecord))];
        let call = Expr::Call(Box::new(boxed), arguments, CalledVia::Space);

        Loc::at_zero(call)
    };

    // _ = thunk1 {}
    let force_thunk1 = Def {
        loc_pattern: Loc::at_zero(Pattern::Underscore),
        loc_expr: force_thunk_call,
        expr_var: var_store.fresh(),
        pattern_vars: Default::default(),
        annotation: None,
    };

    // recursive call `forever effect`
    let forever_effect = {
        let boxed = (
            var_store.fresh(),
            Loc::at_zero(Expr::Var(forever_symbol, var_store.fresh())),
            var_store.fresh(),
            var_store.fresh(),
        );

        let effect_var = var_store.fresh();
        let arguments = vec![(effect_var, Loc::at_zero(Expr::Var(effect, effect_var)))];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    // ```
    // @Effect thunk2 = forever effect
    // thunk2 {}
    // ```
    let force_thunk2 = Loc::at_zero(force_effect(
        forever_effect,
        effect_symbol,
        thunk2_symbol,
        var_store,
    ));

    Expr::LetNonRec(
        Box::new(thunk_from_effect),
        Box::new(Loc::at_zero(Expr::LetNonRec(
            Box::new(force_thunk1),
            Box::new(force_thunk2),
        ))),
    )
}

fn build_effect_loop(
    scope: &mut Scope,
    effect_symbol: Symbol,
    var_store: &mut VarStore,
) -> (Symbol, Def) {
    let loop_symbol = new_symbol!(scope, "loop");
    let state_symbol = new_symbol!(scope, "state");
    let step_symbol = new_symbol!(scope, "step");

    let body = build_effect_loop_body(
        scope,
        effect_symbol,
        loop_symbol,
        state_symbol,
        step_symbol,
        var_store,
    );

    let arguments = vec![
        (
            var_store.fresh(),
            AnnotatedMark::new(var_store),
            Loc::at_zero(Pattern::Identifier(state_symbol)),
        ),
        (
            var_store.fresh(),
            AnnotatedMark::new(var_store),
            Loc::at_zero(Pattern::Identifier(step_symbol)),
        ),
    ];

    let function_var = var_store.fresh();
    let after_closure = Expr::Closure(ClosureData {
        function_type: var_store.fresh(),
        closure_type: var_store.fresh(),
        return_type: var_store.fresh(),
        name: loop_symbol,
        captured_symbols: Vec::new(),
        recursive: Recursive::Recursive,
        arguments,
        loc_body: Box::new(Loc::at_zero(body)),
    });

    let mut introduced_variables = IntroducedVariables::default();

    let signature = {
        let var_a = var_store.fresh();
        let var_b = var_store.fresh();

        introduced_variables.insert_named("a".into(), Loc::at_zero(var_a));
        introduced_variables.insert_named("b".into(), Loc::at_zero(var_b));

        let effect_b = build_effect_opaque(
            effect_symbol,
            var_b,
            Type::Variable(var_b),
            var_store,
            &mut introduced_variables,
        );

        let state_type = {
            let step_tag_name = TagName("Step".into());
            let done_tag_name = TagName("Done".into());

            Type::TagUnion(
                vec![
                    (step_tag_name, vec![Type::Variable(var_a)]),
                    (done_tag_name, vec![Type::Variable(var_b)]),
                ],
                TypeExtension::Closed,
            )
        };

        let effect_state_type = {
            let closure_var = var_store.fresh();
            introduced_variables.insert_wildcard(Loc::at_zero(closure_var));

            let actual = Type::Function(
                vec![Type::EmptyRec],
                Box::new(Type::Variable(closure_var)),
                Box::new(state_type.clone()),
            );

            Type::Alias {
                symbol: effect_symbol,
                type_arguments: vec![OptAbleType::unbound(state_type)],
                lambda_set_variables: vec![roc_types::types::LambdaSet(Type::Variable(
                    closure_var,
                ))],
                infer_ext_in_output_types: vec![],
                actual: Box::new(actual),
                kind: AliasKind::Opaque,
            }
        };

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(Loc::at_zero(closure_var));

        let step_type = Type::Function(
            vec![Type::Variable(var_a)],
            Box::new(Type::Variable(closure_var)),
            Box::new(effect_state_type),
        );

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(Loc::at_zero(closure_var));

        Type::Function(
            vec![Type::Variable(var_a), step_type],
            Box::new(Type::Variable(closure_var)),
            Box::new(effect_b),
        )
    };

    let def_annotation = crate::def::Annotation {
        signature,
        introduced_variables,
        aliases: VecMap::default(),
        region: Region::zero(),
    };

    let pattern = Pattern::Identifier(loop_symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(loop_symbol, function_var);
    let def = Def {
        loc_pattern: Loc::at_zero(pattern),
        loc_expr: Loc::at_zero(after_closure),
        expr_var: function_var,
        pattern_vars,
        annotation: Some(def_annotation),
    };

    (loop_symbol, def)
}

fn build_effect_loop_body(
    scope: &mut Scope,
    effect_symbol: Symbol,
    loop_symbol: Symbol,
    state_symbol: Symbol,
    step_symbol: Symbol,
    var_store: &mut VarStore,
) -> Expr {
    let closure_name = {
        scope
            .introduce("loop_inner".into(), Region::zero())
            .unwrap()
    };

    let inner_body = build_effect_loop_inner_body(
        scope,
        effect_symbol,
        loop_symbol,
        state_symbol,
        step_symbol,
        var_store,
    );

    let captured_symbols = vec![state_symbol, step_symbol];
    wrap_in_effect_thunk(
        inner_body,
        effect_symbol,
        closure_name,
        captured_symbols,
        var_store,
    )
}

fn applied_tag_pattern(
    tag_name: TagName,
    argument_symbols: &[Symbol],
    var_store: &mut VarStore,
) -> Pattern {
    let arguments = argument_symbols
        .iter()
        .map(|s| {
            let pattern = Pattern::Identifier(*s);

            (var_store.fresh(), Loc::at_zero(pattern))
        })
        .collect();

    Pattern::AppliedTag {
        ext_var: var_store.fresh(),
        whole_var: var_store.fresh(),
        tag_name,
        arguments,
    }
}

fn build_effect_loop_inner_body(
    scope: &mut Scope,
    effect_symbol: Symbol,
    loop_symbol: Symbol,
    state_symbol: Symbol,
    step_symbol: Symbol,
    var_store: &mut VarStore,
) -> Expr {
    let thunk1_symbol = new_symbol!(scope, "thunk3");
    let thunk2_symbol = new_symbol!(scope, "thunk4");

    let new_state_symbol = new_symbol!(scope, "newState");
    let done_symbol = new_symbol!(scope, "done");

    // Effect thunk1 = step state
    let thunk_from_effect = {
        let whole_var = var_store.fresh();

        let thunk_var = var_store.fresh();

        let (specialized_def_type, type_arguments, lambda_set_variables) =
            build_fresh_opaque_variables(var_store);
        let pattern = Pattern::UnwrappedOpaque {
            whole_var,
            opaque: effect_symbol,
            argument: Box::new((thunk_var, Loc::at_zero(Pattern::Identifier(thunk1_symbol)))),
            specialized_def_type,
            type_arguments,
            lambda_set_variables,
        };

        let pattern_vars = SendMap::default();

        // `step state`
        let rhs = {
            let step_var = var_store.fresh();
            let boxed = (
                step_var,
                Loc::at_zero(Expr::Var(step_symbol, step_var)),
                var_store.fresh(),
                var_store.fresh(),
            );

            let state_var = var_store.fresh();
            let arguments = vec![(state_var, Loc::at_zero(Expr::Var(state_symbol, state_var)))];
            Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
        };

        Def {
            loc_pattern: Loc::at_zero(pattern),
            loc_expr: Loc::at_zero(rhs),
            expr_var: var_store.fresh(),
            pattern_vars,
            annotation: None,
        }
    };

    // thunk1 {}
    let force_thunk_call = {
        let thunk1_var = var_store.fresh();
        let ret_var = var_store.fresh();
        let boxed = (
            thunk1_var,
            Loc::at_zero(Expr::Var(thunk1_symbol, thunk1_var)),
            var_store.fresh(),
            ret_var,
        );

        let arguments = vec![(var_store.fresh(), Loc::at_zero(Expr::EmptyRecord))];
        let call = Expr::Call(Box::new(boxed), arguments, CalledVia::Space);

        Loc::at_zero(call)
    };

    // recursive call `loop newState step`
    let loop_new_state_step = {
        let loop_var = var_store.fresh();
        let boxed = (
            loop_var,
            Loc::at_zero(Expr::Var(loop_symbol, loop_var)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let new_state_var = var_store.fresh();
        let step_var = var_store.fresh();
        let arguments = vec![
            (
                new_state_var,
                Loc::at_zero(Expr::Var(new_state_symbol, new_state_var)),
            ),
            (step_var, Loc::at_zero(Expr::Var(step_symbol, step_var))),
        ];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    // ```
    // @Effect thunk2 = loop effect
    // thunk2 {}
    // ```
    let force_thunk2 = force_effect(loop_new_state_step, effect_symbol, thunk2_symbol, var_store);

    let step_branch = {
        let step_tag_name = TagName("Step".into());

        let step_pattern = applied_tag_pattern(step_tag_name, &[new_state_symbol], var_store);
        let step_pattern = WhenBranchPattern {
            pattern: Loc::at_zero(step_pattern),
            degenerate: false,
        };

        crate::expr::WhenBranch {
            patterns: vec![step_pattern],
            value: Loc::at_zero(force_thunk2),
            guard: None,
            redundant: RedundantMark::new(var_store),
        }
    };

    let done_branch = {
        let done_tag_name = TagName("Done".into());
        let done_pattern = applied_tag_pattern(done_tag_name, &[done_symbol], var_store);
        let done_pattern = WhenBranchPattern {
            pattern: Loc::at_zero(done_pattern),
            degenerate: false,
        };

        crate::expr::WhenBranch {
            patterns: vec![done_pattern],
            value: Loc::at_zero(Expr::Var(done_symbol, var_store.fresh())),
            guard: None,
            redundant: RedundantMark::new(var_store),
        }
    };

    let branches = vec![step_branch, done_branch];

    let match_on_force_thunk1 = Expr::When {
        cond_var: var_store.fresh(),
        expr_var: var_store.fresh(),
        region: Region::zero(),
        loc_cond: Box::new(force_thunk_call),
        branches,
        branches_cond_var: var_store.fresh(),
        exhaustive: ExhaustiveMark::new(var_store),
    };

    Expr::LetNonRec(
        Box::new(thunk_from_effect),
        Box::new(Loc::at_zero(match_on_force_thunk1)),
    )
}

pub fn build_host_exposed_def(
    scope: &mut Scope,
    symbol: Symbol,
    ident: &str,
    effect_symbol: Symbol,
    var_store: &mut VarStore,
    annotation: crate::annotation::Annotation,
) -> Def {
    let expr_var = var_store.fresh();
    let pattern = Pattern::Identifier(symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(symbol, expr_var);

    let mut arguments: Vec<(Variable, AnnotatedMark, Loc<Pattern>)> = Vec::new();
    let mut linked_symbol_arguments: Vec<(Variable, Expr)> = Vec::new();
    let mut captured_symbols: Vec<(Symbol, Variable)> = Vec::new();

    let crate::annotation::Annotation {
        introduced_variables,
        typ,
        aliases,
        ..
    } = annotation;

    let def_body = {
        match typ.shallow_structural_dealias() {
            Type::Function(args, _, _) => {
                for i in 0..args.len() {
                    let name = format!("closure_arg_{ident}_{i}");

                    let arg_symbol = {
                        let ident = name.clone().into();
                        scope.introduce(ident, Region::zero()).unwrap()
                    };

                    let arg_var = var_store.fresh();

                    arguments.push((
                        arg_var,
                        AnnotatedMark::new(var_store),
                        Loc::at_zero(Pattern::Identifier(arg_symbol)),
                    ));

                    captured_symbols.push((arg_symbol, arg_var));
                    linked_symbol_arguments.push((arg_var, Expr::Var(arg_symbol, arg_var)));
                }

                let foreign_symbol_name = format!("roc_fx_{ident}");
                let low_level_call = Expr::ForeignCall {
                    foreign_symbol: foreign_symbol_name.into(),
                    args: linked_symbol_arguments,
                    ret_var: var_store.fresh(),
                };

                let effect_closure_symbol = {
                    let name = format!("effect_closure_{ident}");

                    let ident = name.into();
                    scope.introduce(ident, Region::zero()).unwrap()
                };

                let effect_closure = Expr::Closure(ClosureData {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    return_type: var_store.fresh(),
                    name: effect_closure_symbol,
                    captured_symbols,
                    recursive: Recursive::NotRecursive,
                    arguments: vec![(
                        var_store.fresh(),
                        AnnotatedMark::new(var_store),
                        Loc::at_zero(empty_record_pattern(var_store)),
                    )],
                    loc_body: Box::new(Loc::at_zero(low_level_call)),
                });

                let (specialized_def_type, type_arguments, lambda_set_variables) =
                    build_fresh_opaque_variables(var_store);
                let body = Expr::OpaqueRef {
                    opaque_var: var_store.fresh(),
                    name: effect_symbol,
                    argument: Box::new((var_store.fresh(), Loc::at_zero(effect_closure))),
                    specialized_def_type,
                    type_arguments,
                    lambda_set_variables,
                };

                Expr::Closure(ClosureData {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    return_type: var_store.fresh(),
                    name: symbol,
                    captured_symbols: std::vec::Vec::new(),
                    recursive: Recursive::NotRecursive,
                    arguments,
                    loc_body: Box::new(Loc::at_zero(body)),
                })
            }
            _ => {
                // not a function

                let foreign_symbol_name = format!("roc_fx_{ident}");
                let low_level_call = Expr::ForeignCall {
                    foreign_symbol: foreign_symbol_name.into(),
                    args: linked_symbol_arguments,
                    ret_var: var_store.fresh(),
                };

                let effect_closure_symbol = {
                    let name = format!("effect_closure_{ident}");

                    let ident = name.into();
                    scope.introduce(ident, Region::zero()).unwrap()
                };

                let empty_record_pattern = Pattern::RecordDestructure {
                    whole_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    destructs: vec![],
                };

                let effect_closure = Expr::Closure(ClosureData {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    return_type: var_store.fresh(),
                    name: effect_closure_symbol,
                    captured_symbols,
                    recursive: Recursive::NotRecursive,
                    arguments: vec![(
                        var_store.fresh(),
                        AnnotatedMark::new(var_store),
                        Loc::at_zero(empty_record_pattern),
                    )],
                    loc_body: Box::new(Loc::at_zero(low_level_call)),
                });

                let (specialized_def_type, type_arguments, lambda_set_variables) =
                    build_fresh_opaque_variables(var_store);
                Expr::OpaqueRef {
                    opaque_var: var_store.fresh(),
                    name: effect_symbol,
                    argument: Box::new((var_store.fresh(), Loc::at_zero(effect_closure))),
                    specialized_def_type,
                    type_arguments,
                    lambda_set_variables,
                }
            }
        }
    };

    let def_annotation = crate::def::Annotation {
        signature: typ,
        introduced_variables,
        aliases,
        region: Region::zero(),
    };

    Def {
        loc_pattern: Loc::at_zero(pattern),
        loc_expr: Loc::at_zero(def_body),
        expr_var,
        pattern_vars,
        annotation: Some(def_annotation),
    }
}

pub fn build_effect_actual(a_type: Type, var_store: &mut VarStore) -> Type {
    let closure_var = var_store.fresh();

    Type::Function(
        vec![Type::EmptyRec],
        Box::new(Type::Variable(closure_var)),
        Box::new(a_type),
    )
}

/// Effect a := {} -> a
fn build_effect_opaque(
    effect_symbol: Symbol,
    a_var: Variable,
    a_type: Type,
    var_store: &mut VarStore,
    introduced_variables: &mut IntroducedVariables,
) -> Type {
    let closure_var = var_store.fresh();
    // introduced_variables.insert_wildcard(Loc::at_zero(closure_var));
    introduced_variables.insert_lambda_set(closure_var);

    let actual = Type::Function(
        vec![Type::EmptyRec],
        Box::new(Type::Variable(closure_var)),
        Box::new(a_type),
    );

    Type::Alias {
        symbol: effect_symbol,
        type_arguments: vec![OptAbleType::unbound(Type::Variable(a_var))],
        lambda_set_variables: vec![roc_types::types::LambdaSet(Type::Variable(closure_var))],
        infer_ext_in_output_types: vec![],
        actual: Box::new(actual),
        kind: AliasKind::Opaque,
    }
}

fn build_fresh_opaque_variables(
    var_store: &mut VarStore,
) -> (Box<Type>, Vec<OptAbleVar>, Vec<LambdaSet>) {
    let closure_var = var_store.fresh();

    // NB: if there are bugs, check whether not introducing variables is a problem!
    // introduced_variables.insert_wildcard(Loc::at_zero(closure_var));

    let a_var = var_store.fresh();
    let actual = Type::Function(
        vec![Type::EmptyRec],
        Box::new(Type::Variable(closure_var)),
        Box::new(Type::Variable(a_var)),
    );
    let type_arguments = vec![OptAbleVar {
        var: a_var,
        opt_abilities: None,
    }];
    let lambda_set_variables = vec![roc_types::types::LambdaSet(Type::Variable(closure_var))];

    (Box::new(actual), type_arguments, lambda_set_variables)
}

#[inline(always)]
fn empty_record_pattern(var_store: &mut VarStore) -> Pattern {
    Pattern::RecordDestructure {
        whole_var: var_store.fresh(),
        ext_var: var_store.fresh(),
        destructs: vec![],
    }
}
