use roc_can::annotation::IntroducedVariables;
use roc_can::def::{Declaration, Def};
use roc_can::env::Env;
use roc_can::expr::{ClosureData, Expr, Recursive};
use roc_can::pattern::Pattern;
use roc_can::scope::Scope;
use roc_collections::all::{MutSet, SendMap};
use roc_module::called_via::CalledVia;
use roc_module::ident::TagName;
use roc_module::symbol::Symbol;
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::Type;

/// Functions that are always implemented for Effect
type Builder = for<'r, 's, 't0, 't1> fn(
    &'r mut Env<'s>,
    &'t0 mut Scope,
    Symbol,
    TagName,
    &'t1 mut VarStore,
) -> (Symbol, Def);

pub const BUILTIN_EFFECT_FUNCTIONS: &[(&str, Builder)] = &[
    // Effect.after : Effect a, (a -> Effect b) -> Effect b
    ("after", build_effect_after),
    // Effect.map : Effect a, (a -> b) -> Effect b
    ("map", build_effect_map),
    // Effect.always : a -> Effect a
    ("always", build_effect_always),
    // Effect.forever : Effect a -> Effect b
    ("forever", build_effect_forever),
    // Effect.loop : a, (a -> Effect [ Step a, Done b ]) -> Effect b
    ("loop", build_effect_loop),
];

const RECURSIVE_BUILTIN_EFFECT_FUNCTIONS: &[&str] = &["forever", "loop"];

// the Effects alias & associated functions
//
// A platform can define an Effect type in its header. It can have an arbitrary name
// (e.g. Task, IO), but we'll call it an Effect in general.
//
// From that name, we generate an effect module, an effect alias, and some functions.
//
// The effect alias is implemented as
//
//  Effect a : [ @Effect ({} -> a) ]
//
// For this alias we implement the functions defined in BUILTIN_EFFECT_FUNCTIONS with the
// standard implementation.

pub fn build_effect_builtins(
    env: &mut Env,
    scope: &mut Scope,
    effect_symbol: Symbol,
    var_store: &mut VarStore,
    exposed_symbols: &mut MutSet<Symbol>,
    declarations: &mut Vec<Declaration>,
) {
    for (name, f) in BUILTIN_EFFECT_FUNCTIONS.iter() {
        let (symbol, def) = f(
            env,
            scope,
            effect_symbol,
            TagName::Private(effect_symbol),
            var_store,
        );

        exposed_symbols.insert(symbol);

        let is_recursive = RECURSIVE_BUILTIN_EFFECT_FUNCTIONS.iter().any(|n| n == name);
        if is_recursive {
            declarations.push(Declaration::DeclareRec(vec![def]));
        } else {
            declarations.push(Declaration::Declare(def));
        }
    }

    // Useful when working on functions in this module. By default symbols that we named do now
    // show up with their name. We have to register them like below to make the names show up in
    // debug prints
    if false {
        env.home.register_debug_idents(&env.ident_ids);
    }
}

macro_rules! new_symbol {
    ($scope:expr, $env:expr, $name:expr) => {{
        $scope
            .introduce(
                $name.into(),
                &$env.exposed_ident_ids,
                &mut $env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    }};
}

fn build_effect_always(
    env: &mut Env,
    scope: &mut Scope,
    effect_symbol: Symbol,
    effect_tag_name: TagName,
    var_store: &mut VarStore,
) -> (Symbol, Def) {
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
            Loc::at_zero(empty_record_pattern(var_store)),
        )];

        let body = Expr::Var(value_symbol);

        Expr::Closure(ClosureData {
            function_type: var_store.fresh(),
            closure_type: var_store.fresh(),
            closure_ext_var: var_store.fresh(),
            return_type: var_store.fresh(),
            name: inner_closure_symbol,
            captured_symbols: vec![(value_symbol, var_store.fresh())],
            recursive: Recursive::NotRecursive,
            arguments,
            loc_body: Box::new(Loc::at_zero(body)),
        })
    };

    // \value -> @Effect \{} -> value
    let (function_var, always_closure) = {
        // `@Effect \{} -> value`
        let body = Expr::Tag {
            variant_var: var_store.fresh(),
            ext_var: var_store.fresh(),
            name: effect_tag_name.clone(),
            arguments: vec![(var_store.fresh(), Loc::at_zero(const_closure))],
        };

        let arguments = vec![(
            var_store.fresh(),
            Loc::at_zero(Pattern::Identifier(value_symbol)),
        )];

        let function_var = var_store.fresh();
        let closure = Expr::Closure(ClosureData {
            function_type: function_var,
            closure_type: var_store.fresh(),
            closure_ext_var: var_store.fresh(),
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
        introduced_variables.insert_named("a".into(), var_a);

        let effect_a = build_effect_alias(
            effect_symbol,
            effect_tag_name,
            "a",
            var_a,
            Type::Variable(var_a),
            var_store,
            &mut introduced_variables,
        );

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
    env: &mut Env,
    scope: &mut Scope,
    effect_symbol: Symbol,
    effect_tag_name: TagName,
    var_store: &mut VarStore,
) -> (Symbol, Def) {
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
            Loc::at_zero(Expr::Var(thunk_symbol)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![(var_store.fresh(), Loc::at_zero(Expr::EmptyRecord))];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    // `toEffect (thunk {})`
    let mapper_call = {
        let boxed = (
            var_store.fresh(),
            Loc::at_zero(Expr::Var(mapper_symbol)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![(var_store.fresh(), Loc::at_zero(force_thunk_call))];
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
            Loc::at_zero(empty_record_pattern(var_store)),
        )];

        Expr::Closure(ClosureData {
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
            loc_body: Box::new(Loc::at_zero(mapper_call)),
        })
    };

    let arguments = vec![
        (
            var_store.fresh(),
            Loc::at_zero(Pattern::AppliedTag {
                whole_var: var_store.fresh(),
                ext_var: var_store.fresh(),
                tag_name: effect_tag_name.clone(),
                arguments: vec![(
                    var_store.fresh(),
                    Loc::at_zero(Pattern::Identifier(thunk_symbol)),
                )],
            }),
        ),
        (
            var_store.fresh(),
            Loc::at_zero(Pattern::Identifier(mapper_symbol)),
        ),
    ];

    // `@Effect \{} -> (mapper (thunk {}))`
    let body = Expr::Tag {
        variant_var: var_store.fresh(),
        ext_var: var_store.fresh(),
        name: effect_tag_name.clone(),
        arguments: vec![(var_store.fresh(), Loc::at_zero(inner_closure))],
    };

    let function_var = var_store.fresh();
    let map_closure = Expr::Closure(ClosureData {
        function_type: function_var,
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
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

        introduced_variables.insert_named("a".into(), var_a);
        introduced_variables.insert_named("b".into(), var_b);

        let effect_a = build_effect_alias(
            effect_symbol,
            effect_tag_name.clone(),
            "a",
            var_a,
            Type::Variable(var_a),
            var_store,
            &mut introduced_variables,
        );

        let effect_b = build_effect_alias(
            effect_symbol,
            effect_tag_name,
            "b",
            var_b,
            Type::Variable(var_b),
            var_store,
            &mut introduced_variables,
        );

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
    let def = Def {
        loc_pattern: Loc::at_zero(pattern),
        loc_expr: Loc::at_zero(map_closure),
        expr_var: function_var,
        pattern_vars,
        annotation: Some(def_annotation),
    };

    (map_symbol, def)
}

fn build_effect_after(
    env: &mut Env,
    scope: &mut Scope,
    effect_symbol: Symbol,
    effect_tag_name: TagName,
    var_store: &mut VarStore,
) -> (Symbol, Def) {
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
            Loc::at_zero(Expr::Var(thunk_symbol)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![(var_store.fresh(), Loc::at_zero(Expr::EmptyRecord))];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    // `toEffect (thunk {})`
    let to_effect_call = {
        let boxed = (
            var_store.fresh(),
            Loc::at_zero(Expr::Var(to_effect_symbol)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![(var_store.fresh(), Loc::at_zero(force_thunk_call))];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    let arguments = vec![
        (
            var_store.fresh(),
            Loc::at_zero(Pattern::AppliedTag {
                whole_var: var_store.fresh(),
                ext_var: var_store.fresh(),
                tag_name: effect_tag_name.clone(),
                arguments: vec![(
                    var_store.fresh(),
                    Loc::at_zero(Pattern::Identifier(thunk_symbol)),
                )],
            }),
        ),
        (
            var_store.fresh(),
            Loc::at_zero(Pattern::Identifier(to_effect_symbol)),
        ),
    ];

    let function_var = var_store.fresh();
    let after_closure = Expr::Closure(ClosureData {
        function_type: function_var,
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
        return_type: var_store.fresh(),
        name: after_symbol,
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments,
        loc_body: Box::new(Loc::at_zero(to_effect_call)),
    });

    let mut introduced_variables = IntroducedVariables::default();

    let signature = {
        let var_a = var_store.fresh();
        let var_b = var_store.fresh();

        introduced_variables.insert_named("a".into(), var_a);
        introduced_variables.insert_named("b".into(), var_b);

        let effect_a = build_effect_alias(
            effect_symbol,
            effect_tag_name.clone(),
            "a",
            var_a,
            Type::Variable(var_a),
            var_store,
            &mut introduced_variables,
        );

        let effect_b = build_effect_alias(
            effect_symbol,
            effect_tag_name,
            "b",
            var_b,
            Type::Variable(var_b),
            var_store,
            &mut introduced_variables,
        );

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
    effect_tag_name: TagName,
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
            Loc::at_zero(empty_record_pattern(var_store)),
        )];

        Expr::Closure(ClosureData {
            function_type: var_store.fresh(),
            closure_type: var_store.fresh(),
            closure_ext_var: var_store.fresh(),
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
    Expr::Tag {
        variant_var: var_store.fresh(),
        ext_var: var_store.fresh(),
        name: effect_tag_name,
        arguments: vec![(var_store.fresh(), Loc::at_zero(const_closure))],
    }
}

/// given `effect : Effect a`, unwrap the thunk and force it, giving a value of type `a`
fn force_effect(
    effect: Expr,
    effect_tag_name: TagName,
    thunk_symbol: Symbol,
    var_store: &mut VarStore,
) -> Expr {
    let whole_var = var_store.fresh();
    let ext_var = var_store.fresh();

    let thunk_var = var_store.fresh();

    let pattern = Pattern::AppliedTag {
        ext_var,
        whole_var,
        tag_name: effect_tag_name,
        arguments: vec![(thunk_var, Loc::at_zero(Pattern::Identifier(thunk_symbol)))],
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
        let boxed = (
            var_store.fresh(),
            Loc::at_zero(Expr::Var(thunk_symbol)),
            var_store.fresh(),
            ret_var,
        );

        let arguments = vec![(var_store.fresh(), Loc::at_zero(Expr::EmptyRecord))];
        let call = Expr::Call(Box::new(boxed), arguments, CalledVia::Space);

        Loc::at_zero(call)
    };

    Expr::LetNonRec(Box::new(def), Box::new(force_thunk_call), var_store.fresh())
}

fn build_effect_forever(
    env: &mut Env,
    scope: &mut Scope,
    effect_symbol: Symbol,
    effect_tag_name: TagName,
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
    //  Effect.forever : [ C foreverInner { effect : T } ]
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

    let forever_symbol = {
        scope
            .introduce(
                "forever".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    let effect = {
        scope
            .introduce(
                "effect".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    let body = build_effect_forever_body(
        env,
        scope,
        effect_tag_name.clone(),
        forever_symbol,
        effect,
        var_store,
    );

    let arguments = vec![(var_store.fresh(), Loc::at_zero(Pattern::Identifier(effect)))];

    let function_var = var_store.fresh();
    let after_closure = Expr::Closure(ClosureData {
        function_type: var_store.fresh(),
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
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

        introduced_variables.insert_named("a".into(), var_a);
        introduced_variables.insert_named("b".into(), var_b);

        let effect_a = build_effect_alias(
            effect_symbol,
            effect_tag_name.clone(),
            "a",
            var_a,
            Type::Variable(var_a),
            var_store,
            &mut introduced_variables,
        );

        let effect_b = build_effect_alias(
            effect_symbol,
            effect_tag_name,
            "b",
            var_b,
            Type::Variable(var_b),
            var_store,
            &mut introduced_variables,
        );

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(closure_var);

        Type::Function(
            vec![effect_a],
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
    env: &mut Env,
    scope: &mut Scope,
    effect_tag_name: TagName,
    forever_symbol: Symbol,
    effect: Symbol,
    var_store: &mut VarStore,
) -> Expr {
    let closure_name = {
        scope
            .introduce(
                "forever_inner".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    let inner_body = build_effect_forever_inner_body(
        env,
        scope,
        effect_tag_name.clone(),
        forever_symbol,
        effect,
        var_store,
    );

    let captured_symbols = vec![effect];
    wrap_in_effect_thunk(
        inner_body,
        effect_tag_name,
        closure_name,
        captured_symbols,
        var_store,
    )
}

fn build_effect_forever_inner_body(
    env: &mut Env,
    scope: &mut Scope,
    effect_tag_name: TagName,
    forever_symbol: Symbol,
    effect: Symbol,
    var_store: &mut VarStore,
) -> Expr {
    let thunk1_symbol = {
        scope
            .introduce(
                "thunk1".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    let thunk2_symbol = {
        scope
            .introduce(
                "thunk2".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    // Effect thunk1 = effect
    let thunk_from_effect = {
        let whole_var = var_store.fresh();
        let ext_var = var_store.fresh();

        let thunk_var = var_store.fresh();

        let pattern = Pattern::AppliedTag {
            ext_var,
            whole_var,
            tag_name: effect_tag_name.clone(),
            arguments: vec![(thunk_var, Loc::at_zero(Pattern::Identifier(thunk1_symbol)))],
        };

        let pattern_vars = SendMap::default();

        Def {
            loc_pattern: Loc::at_zero(pattern),
            loc_expr: Loc::at_zero(Expr::Var(effect)),
            expr_var: var_store.fresh(),
            pattern_vars,
            annotation: None,
        }
    };

    // thunk1 {}
    let force_thunk_call = {
        let ret_var = var_store.fresh();
        let boxed = (
            var_store.fresh(),
            Loc::at_zero(Expr::Var(thunk1_symbol)),
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
            Loc::at_zero(Expr::Var(forever_symbol)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![(var_store.fresh(), Loc::at_zero(Expr::Var(effect)))];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    // ```
    // Effect thunk2 = forever effect
    // thunk2 {}
    // ```
    let force_thunk2 = Loc::at_zero(force_effect(
        forever_effect,
        effect_tag_name,
        thunk2_symbol,
        var_store,
    ));

    Expr::LetNonRec(
        Box::new(thunk_from_effect),
        Box::new(Loc::at_zero(Expr::LetNonRec(
            Box::new(force_thunk1),
            Box::new(force_thunk2),
            var_store.fresh(),
        ))),
        var_store.fresh(),
    )
}

fn build_effect_loop(
    env: &mut Env,
    scope: &mut Scope,
    effect_symbol: Symbol,
    effect_tag_name: TagName,
    var_store: &mut VarStore,
) -> (Symbol, Def) {
    let loop_symbol = new_symbol!(scope, env, "loop");
    let state_symbol = new_symbol!(scope, env, "state");
    let step_symbol = new_symbol!(scope, env, "step");

    let body = build_effect_loop_body(
        env,
        scope,
        effect_tag_name.clone(),
        loop_symbol,
        state_symbol,
        step_symbol,
        var_store,
    );

    let arguments = vec![
        (
            var_store.fresh(),
            Loc::at_zero(Pattern::Identifier(state_symbol)),
        ),
        (
            var_store.fresh(),
            Loc::at_zero(Pattern::Identifier(step_symbol)),
        ),
    ];

    let function_var = var_store.fresh();
    let after_closure = Expr::Closure(ClosureData {
        function_type: var_store.fresh(),
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
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

        introduced_variables.insert_named("a".into(), var_a);
        introduced_variables.insert_named("b".into(), var_b);

        let effect_b = build_effect_alias(
            effect_symbol,
            effect_tag_name.clone(),
            "b",
            var_b,
            Type::Variable(var_b),
            var_store,
            &mut introduced_variables,
        );

        let state_type = {
            let step_tag_name = TagName::Global("Step".into());
            let done_tag_name = TagName::Global("Done".into());

            Type::TagUnion(
                vec![
                    (step_tag_name, vec![Type::Variable(var_a)]),
                    (done_tag_name, vec![Type::Variable(var_b)]),
                ],
                Box::new(Type::EmptyTagUnion),
            )
        };

        let effect_state_type = {
            let closure_var = var_store.fresh();
            introduced_variables.insert_wildcard(closure_var);

            let actual = {
                Type::TagUnion(
                    vec![(
                        effect_tag_name,
                        vec![Type::Function(
                            vec![Type::EmptyRec],
                            Box::new(Type::Variable(closure_var)),
                            Box::new(state_type.clone()),
                        )],
                    )],
                    Box::new(Type::EmptyTagUnion),
                )
            };

            Type::Alias {
                symbol: effect_symbol,
                type_arguments: vec![("a".into(), state_type)],
                lambda_set_variables: vec![roc_types::types::LambdaSet(Type::Variable(
                    closure_var,
                ))],
                actual: Box::new(actual),
            }
        };

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(closure_var);

        let step_type = Type::Function(
            vec![Type::Variable(var_a)],
            Box::new(Type::Variable(closure_var)),
            Box::new(effect_state_type),
        );

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(closure_var);

        Type::Function(
            vec![Type::Variable(var_a), step_type],
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
    env: &mut Env,
    scope: &mut Scope,
    effect_tag_name: TagName,
    loop_symbol: Symbol,
    state_symbol: Symbol,
    step_symbol: Symbol,
    var_store: &mut VarStore,
) -> Expr {
    let closure_name = {
        scope
            .introduce(
                "loop_inner".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    let inner_body = build_effect_loop_inner_body(
        env,
        scope,
        effect_tag_name.clone(),
        loop_symbol,
        state_symbol,
        step_symbol,
        var_store,
    );

    let captured_symbols = vec![state_symbol, step_symbol];
    wrap_in_effect_thunk(
        inner_body,
        effect_tag_name,
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
    env: &mut Env,
    scope: &mut Scope,
    effect_tag_name: TagName,
    loop_symbol: Symbol,
    state_symbol: Symbol,
    step_symbol: Symbol,
    var_store: &mut VarStore,
) -> Expr {
    let thunk1_symbol = new_symbol!(scope, env, "thunk3");
    let thunk2_symbol = new_symbol!(scope, env, "thunk4");

    let new_state_symbol = new_symbol!(scope, env, "newState");
    let done_symbol = new_symbol!(scope, env, "done");

    // Effect thunk1 = step state
    let thunk_from_effect = {
        let whole_var = var_store.fresh();
        let ext_var = var_store.fresh();

        let thunk_var = var_store.fresh();

        let pattern = Pattern::AppliedTag {
            ext_var,
            whole_var,
            tag_name: effect_tag_name.clone(),
            arguments: vec![(thunk_var, Loc::at_zero(Pattern::Identifier(thunk1_symbol)))],
        };

        let pattern_vars = SendMap::default();

        // `step state`
        let rhs = {
            let boxed = (
                var_store.fresh(),
                Loc::at_zero(Expr::Var(step_symbol)),
                var_store.fresh(),
                var_store.fresh(),
            );

            let arguments = vec![(var_store.fresh(), Loc::at_zero(Expr::Var(state_symbol)))];
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
        let ret_var = var_store.fresh();
        let boxed = (
            var_store.fresh(),
            Loc::at_zero(Expr::Var(thunk1_symbol)),
            var_store.fresh(),
            ret_var,
        );

        let arguments = vec![(var_store.fresh(), Loc::at_zero(Expr::EmptyRecord))];
        let call = Expr::Call(Box::new(boxed), arguments, CalledVia::Space);

        Loc::at_zero(call)
    };

    // recursive call `loop newState step`
    let loop_new_state_step = {
        let boxed = (
            var_store.fresh(),
            Loc::at_zero(Expr::Var(loop_symbol)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![
            (var_store.fresh(), Loc::at_zero(Expr::Var(new_state_symbol))),
            (var_store.fresh(), Loc::at_zero(Expr::Var(step_symbol))),
        ];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    // ```
    // Effect thunk2 = loop effect
    // thunk2 {}
    // ```
    let force_thunk2 = force_effect(
        loop_new_state_step,
        effect_tag_name,
        thunk2_symbol,
        var_store,
    );

    let step_branch = {
        let step_tag_name = TagName::Global("Step".into());

        let step_pattern = applied_tag_pattern(step_tag_name, &[new_state_symbol], var_store);

        roc_can::expr::WhenBranch {
            patterns: vec![Loc::at_zero(step_pattern)],
            value: Loc::at_zero(force_thunk2),
            guard: None,
        }
    };

    let done_branch = {
        let done_tag_name = TagName::Global("Done".into());
        let done_pattern = applied_tag_pattern(done_tag_name, &[done_symbol], var_store);

        roc_can::expr::WhenBranch {
            patterns: vec![Loc::at_zero(done_pattern)],
            value: Loc::at_zero(Expr::Var(done_symbol)),
            guard: None,
        }
    };

    let branches = vec![step_branch, done_branch];

    let match_on_force_thunk1 = Expr::When {
        cond_var: var_store.fresh(),
        expr_var: var_store.fresh(),
        region: Region::zero(),
        loc_cond: Box::new(force_thunk_call),
        branches,
    };

    Expr::LetNonRec(
        Box::new(thunk_from_effect),
        Box::new(Loc::at_zero(match_on_force_thunk1)),
        var_store.fresh(),
    )
}

pub fn build_host_exposed_def(
    env: &mut Env,
    scope: &mut Scope,
    symbol: Symbol,
    ident: &str,
    effect_tag_name: TagName,
    var_store: &mut VarStore,
    annotation: roc_can::annotation::Annotation,
) -> Def {
    let expr_var = var_store.fresh();
    let pattern = Pattern::Identifier(symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(symbol, expr_var);

    let mut arguments: Vec<(Variable, Loc<Pattern>)> = Vec::new();
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

                    arguments.push((arg_var, Loc::at_zero(Pattern::Identifier(arg_symbol))));

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

                let effect_closure = Expr::Closure(ClosureData {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    closure_ext_var: var_store.fresh(),
                    return_type: var_store.fresh(),
                    name: effect_closure_symbol,
                    captured_symbols,
                    recursive: Recursive::NotRecursive,
                    arguments: vec![(
                        var_store.fresh(),
                        Loc::at_zero(empty_record_pattern(var_store)),
                    )],
                    loc_body: Box::new(Loc::at_zero(low_level_call)),
                });

                let body = Expr::Tag {
                    variant_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    name: effect_tag_name,
                    arguments: vec![(var_store.fresh(), Loc::at_zero(effect_closure))],
                };

                Expr::Closure(ClosureData {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    closure_ext_var: var_store.fresh(),
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

                let effect_closure = Expr::Closure(ClosureData {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    closure_ext_var: var_store.fresh(),
                    return_type: var_store.fresh(),
                    name: effect_closure_symbol,
                    captured_symbols,
                    recursive: Recursive::NotRecursive,
                    arguments: vec![(var_store.fresh(), Loc::at_zero(empty_record_pattern))],
                    loc_body: Box::new(Loc::at_zero(low_level_call)),
                });

                Expr::Tag {
                    variant_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    name: effect_tag_name,
                    arguments: vec![(var_store.fresh(), Loc::at_zero(effect_closure))],
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

    Def {
        loc_pattern: Loc::at_zero(pattern),
        loc_expr: Loc::at_zero(def_body),
        expr_var,
        pattern_vars,
        annotation: Some(def_annotation),
    }
}

fn build_effect_alias(
    effect_symbol: Symbol,
    effect_tag_name: TagName,
    a_name: &str,
    a_var: Variable,
    a_type: Type,
    var_store: &mut VarStore,
    introduced_variables: &mut IntroducedVariables,
) -> Type {
    let closure_var = var_store.fresh();
    introduced_variables.insert_wildcard(closure_var);

    let actual = {
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
    };

    Type::Alias {
        symbol: effect_symbol,
        type_arguments: vec![(a_name.into(), Type::Variable(a_var))],
        lambda_set_variables: vec![roc_types::types::LambdaSet(Type::Variable(closure_var))],
        actual: Box::new(actual),
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
