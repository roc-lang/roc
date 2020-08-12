#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc_mono;

mod helpers;

// Test monomorphization
#[cfg(test)]
mod test_mono {
    // NOTE because the Show instance of module names is different in --release mode,
    // these tests would all fail. In the future, when we do interesting optimizations,
    // we'll likely want some tests for --release too.
    #[cfg(not(debug_assertions))]
    fn compiles_to_ir(_src: &str, _expected: &str) {
        // just do nothing
    }

    #[cfg(debug_assertions)]
    fn compiles_to_ir(src: &str, expected: &str) {
        use crate::helpers::{can_expr, infer_expr, CanExprOut};
        use bumpalo::Bump;
        use roc_mono::layout::LayoutCache;
        use roc_types::subs::Subs;

        let arena = Bump::new();
        let CanExprOut {
            loc_expr,
            var_store,
            var,
            constraint,
            home,
            mut interns,
            ..
        } = can_expr(src);

        let subs = Subs::new(var_store.into());
        let mut unify_problems = Vec::new();
        let (_content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        // Compile and add all the Procs before adding main
        let mut procs = roc_mono::ir::Procs::default();
        let mut ident_ids = interns.all_ident_ids.remove(&home).unwrap();

        // Put this module's ident_ids back in the interns
        interns.all_ident_ids.insert(home, ident_ids.clone());

        // Populate Procs and Subs, and get the low-level Expr from the canonical Expr
        let mut mono_problems = Vec::new();
        let mut mono_env = roc_mono::ir::Env {
            arena: &arena,
            subs: &mut subs,
            problems: &mut mono_problems,
            home,
            ident_ids: &mut ident_ids,
        };

        let mut layout_cache = LayoutCache::default();
        let ir_expr =
            roc_mono::ir::from_can(&mut mono_env, loc_expr.value, &mut procs, &mut layout_cache);

        // let mono_expr = Expr::new(&mut mono_env, loc_expr.value, &mut procs);
        let procs = roc_mono::ir::specialize_all(&mut mono_env, procs, &mut LayoutCache::default());

        // apply inc/dec
        let stmt = mono_env.arena.alloc(ir_expr);
        let ir_expr = roc_mono::inc_dec::visit_declaration(mono_env.arena, stmt);

        assert_eq!(
            procs.runtime_errors,
            roc_collections::all::MutMap::default()
        );

        let mut procs_string = procs
            .get_specialized_procs(mono_env.arena)
            .values()
            .map(|proc| proc.to_pretty(200))
            .collect::<Vec<_>>();

        procs_string.push(ir_expr.to_pretty(200));

        let result = procs_string.join("\n");

        let the_same = result == expected;

        if !the_same {
            println!("{}", result);

            let expected_lines = expected.split("\n").collect::<Vec<&str>>();
            let result_lines = result.split("\n").collect::<Vec<&str>>();

            assert_eq!(expected_lines, result_lines);
        }
    }

    #[test]
    fn ir_int_literal() {
        compiles_to_ir(
            r#"
            5
            "#,
            indoc!(
                r#"
                let Test.0 = 5i64;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn ir_assignment() {
        compiles_to_ir(
            r#"
            x = 5

            x
            "#,
            indoc!(
                r#"
                let Test.0 = 5i64;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    #[test]
    fn ir_when_maybe() {
        compiles_to_ir(
            r#"
            when Just 3 is
                Just n -> n
                Nothing -> 0
            "#,
            indoc!(
                r#"
                let Test.9 = 0i64;
                let Test.10 = 3i64;
                let Test.1 = Just Test.9 Test.10;
                let Test.5 = true;
                let Test.7 = Index 0 Test.1;
                let Test.6 = 0i64;
                let Test.8 = lowlevel Eq Test.6 Test.7;
                let Test.4 = lowlevel And Test.8 Test.5;
                if Test.4 then
                    let Test.0 = Index 1 Test.1;
                    ret Test.0;
                else
                    let Test.3 = 0i64;
                    ret Test.3;
                "#
            ),
        )
    }

    #[test]
    fn ir_when_these() {
        compiles_to_ir(
            r#"
            when These 1 2 is
                This x -> x
                That y -> y
                These x _ -> x
            "#,
            indoc!(
                r#"
                let Test.7 = 1i64;
                let Test.8 = 1i64;
                let Test.9 = 2i64;
                let Test.3 = These Test.7 Test.8 Test.9;
                switch Test.3:
                    case 2:
                        let Test.0 = Index 1 Test.3;
                        ret Test.0;

                    case 0:
                        let Test.1 = Index 1 Test.3;
                        ret Test.1;

                    default:
                        let Test.2 = Index 1 Test.3;
                        ret Test.2;

                "#
            ),
        )
    }

    #[test]
    fn ir_when_record() {
        compiles_to_ir(
            r#"
            when { x: 1, y: 3.14 } is
                { x } -> x
            "#,
            indoc!(
                r#"
                let Test.4 = 1i64;
                let Test.5 = 3.14f64;
                let Test.1 = Struct {Test.4, Test.5};
                let Test.0 = Index 0 Test.1;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn ir_plus() {
        compiles_to_ir(
            r#"
            1 + 2
            "#,
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.3 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.3;

                let Test.1 = 1i64;
                let Test.2 = 2i64;
                let Test.0 = CallByName Num.14 Test.1 Test.2;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn ir_round() {
        compiles_to_ir(
            r#"
            Num.round 3.6
            "#,
            indoc!(
                r#"
                procedure Num.36 (#Attr.2):
                    let Test.2 = lowlevel NumRound #Attr.2;
                    ret Test.2;

                let Test.1 = 3.6f64;
                let Test.0 = CallByName Num.36 Test.1;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn ir_when_idiv() {
        compiles_to_ir(
            r#"
            when 1000 // 10 is
                Ok val -> val
                Err _ -> -1
            "#,
            indoc!(
                r#"
                procedure Num.32 (#Attr.2, #Attr.3):
                    let Test.18 = 0i64;
                    let Test.14 = lowlevel NotEq #Attr.3 Test.18;
                    if Test.14 then
                        let Test.16 = 1i64;
                        let Test.17 = lowlevel NumDivUnchecked #Attr.2 #Attr.3;
                        let Test.15 = Ok Test.16 Test.17;
                        ret Test.15;
                    else
                        let Test.12 = 0i64;
                        let Test.13 = Struct {};
                        let Test.11 = Err Test.12 Test.13;
                        ret Test.11;

                let Test.9 = 1000i64;
                let Test.10 = 10i64;
                let Test.1 = CallByName Num.32 Test.9 Test.10;
                let Test.5 = true;
                let Test.7 = Index 0 Test.1;
                let Test.6 = 1i64;
                let Test.8 = lowlevel Eq Test.6 Test.7;
                let Test.4 = lowlevel And Test.8 Test.5;
                if Test.4 then
                    let Test.0 = Index 1 Test.1;
                    ret Test.0;
                else
                    let Test.3 = -1i64;
                    ret Test.3;
            "#
            ),
        )
    }

    #[test]
    fn ir_two_defs() {
        compiles_to_ir(
            r#"
            x = 3
            y = 4

            x + y
            "#,
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.3 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.3;

                let Test.1 = 4i64;
                let Test.0 = 3i64;
                let Test.2 = CallByName Num.14 Test.0 Test.1;
                ret Test.2;
                "#
            ),
        )
    }

    #[test]
    fn ir_when_just() {
        compiles_to_ir(
            r#"
            x : [ Nothing, Just Int ]
            x = Just 41

            when x is
                Just v -> v + 0x1
                Nothing -> 0x1
            "#,
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.12 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.12;

                let Test.10 = 0i64;
                let Test.11 = 41i64;
                let Test.0 = Just Test.10 Test.11;
                let Test.6 = true;
                let Test.8 = Index 0 Test.0;
                let Test.7 = 0i64;
                let Test.9 = lowlevel Eq Test.7 Test.8;
                let Test.5 = lowlevel And Test.9 Test.6;
                if Test.5 then
                    let Test.1 = Index 1 Test.0;
                    let Test.3 = 1i64;
                    let Test.2 = CallByName Num.14 Test.1 Test.3;
                    ret Test.2;
                else
                    let Test.4 = 1i64;
                    ret Test.4;
                "#
            ),
        )
    }

    #[test]
    fn one_element_tag() {
        compiles_to_ir(
            r#"
            x : [ Pair Int ]
            x = Pair 2

            x
            "#,
            indoc!(
                r#"
                let Test.2 = 2i64;
                let Test.0 = Struct {Test.2};
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn guard_pattern_true() {
        compiles_to_ir(
            r#"
            when 2 is
                2 if False -> 42
                _ -> 0
            "#,
            indoc!(
                r#"
                let Test.0 = 2i64;
                let Test.6 = true;
                let Test.7 = 2i64;
                let Test.10 = lowlevel Eq Test.7 Test.0;
                let Test.8 = lowlevel And Test.10 Test.6;
                let Test.3 = false;
                jump Test.2 Test.3;
                joinpoint Test.2 Test.9:
                    let Test.5 = lowlevel And Test.9 Test.8;
                    if Test.5 then
                        let Test.1 = 42i64;
                        ret Test.1;
                    else
                        let Test.4 = 0i64;
                        ret Test.4;
               "#
            ),
        )
    }

    #[test]
    fn when_on_record() {
        compiles_to_ir(
            r#"
            when { x: 0x2 } is
                { x } -> x + 3
            "#,
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.5 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.5;

                let Test.4 = 2i64;
                let Test.1 = Struct {Test.4};
                let Test.0 = Index 0 Test.1;
                let Test.3 = 3i64;
                let Test.2 = CallByName Num.14 Test.0 Test.3;
                ret Test.2;
                "#
            ),
        )
    }

    #[test]
    fn when_nested_maybe() {
        compiles_to_ir(
            r#"
            Maybe a : [ Nothing, Just a ]

            x : Maybe (Maybe Int)
            x = Just (Just 41)

            when x is
                Just (Just v) -> v + 0x1
                _ -> 0x1
            "#,
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.20 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.20;

                let Test.16 = 0i64;
                let Test.18 = 0i64;
                let Test.19 = 41i64;
                let Test.17 = Just Test.18 Test.19;
                let Test.1 = Just Test.16 Test.17;
                let Test.8 = true;
                let Test.10 = Index 0 Test.1;
                let Test.9 = 0i64;
                let Test.15 = lowlevel Eq Test.9 Test.10;
                let Test.13 = lowlevel And Test.15 Test.8;
                let Test.12 = Index 0 Test.1;
                let Test.11 = 0i64;
                let Test.14 = lowlevel Eq Test.11 Test.12;
                let Test.7 = lowlevel And Test.14 Test.13;
                if Test.7 then
                    let Test.5 = Index 1 Test.1;
                    let Test.2 = Index 1 Test.5;
                    let Test.4 = 1i64;
                    let Test.3 = CallByName Num.14 Test.2 Test.4;
                    ret Test.3;
                else
                    let Test.6 = 1i64;
                    ret Test.6;
                "#
            ),
        )
    }

    #[test]
    fn when_on_two_values() {
        compiles_to_ir(
            r#"
            when Pair 2 3 is
                Pair 4 3 -> 9
                Pair a b -> a + b
            "#,
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.16 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.16;

                let Test.14 = 2i64;
                let Test.15 = 3i64;
                let Test.2 = Struct {Test.14, Test.15};
                let Test.6 = true;
                let Test.7 = 4i64;
                let Test.8 = Index 0 Test.2;
                let Test.13 = lowlevel Eq Test.7 Test.8;
                let Test.11 = lowlevel And Test.13 Test.6;
                let Test.9 = 3i64;
                let Test.10 = Index 1 Test.2;
                let Test.12 = lowlevel Eq Test.9 Test.10;
                let Test.5 = lowlevel And Test.12 Test.11;
                if Test.5 then
                    let Test.3 = 9i64;
                    ret Test.3;
                else
                    let Test.0 = Index 0 Test.2;
                    let Test.1 = Index 1 Test.2;
                    let Test.4 = CallByName Num.14 Test.0 Test.1;
                    ret Test.4;
                "#
            ),
        )
    }

    #[test]
    fn list_append_closure() {
        compiles_to_ir(
            r#"
            myFunction = \l -> List.append l 42

            myFunction [ 1, 2 ]
            "#,
            indoc!(
                r#"
                procedure List.5 (#Attr.2, #Attr.3):
                    let Test.9 = lowlevel ListAppend #Attr.2 #Attr.3;
                    ret Test.9;

                procedure Test.0 (Test.2):
                    let Test.8 = 42i64;
                    let Test.7 = CallByName List.5 Test.2 Test.8;
                    ret Test.7;

                let Test.5 = 1i64;
                let Test.6 = 2i64;
                let Test.4 = Array [Test.5, Test.6];
                let Test.3 = CallByName Test.0 Test.4;
                dec Test.4;
                ret Test.3;
                "#
            ),
        )
    }

    #[test]
    fn list_append() {
        compiles_to_ir(
            r#"
            List.append [1] 2
            "#,
            indoc!(
                r#"
                procedure List.5 (#Attr.2, #Attr.3):
                    let Test.4 = lowlevel ListAppend #Attr.2 #Attr.3;
                    ret Test.4;

                let Test.3 = 1i64;
                let Test.1 = Array [Test.3];
                let Test.2 = 2i64;
                let Test.0 = CallByName List.5 Test.1 Test.2;
                dec Test.1;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn list_len() {
        compiles_to_ir(
            r#"
            x = [1,2,3]
            y = [ 1.0 ]

            List.len x + List.len y
            "#,
            indoc!(
                r#"
                procedure List.7 (#Attr.2):
                    let Test.9 = lowlevel ListLen #Attr.2;
                    ret Test.9;

                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.11 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.11;

                let Test.8 = 1f64;
                let Test.1 = Array [Test.8];
                let Test.5 = 1i64;
                let Test.6 = 2i64;
                let Test.7 = 3i64;
                let Test.0 = Array [Test.5, Test.6, Test.7];
                let Test.3 = CallByName List.7 Test.0;
                dec Test.0;
                let Test.4 = CallByName List.7 Test.1;
                dec Test.1;
                let Test.2 = CallByName Num.14 Test.3 Test.4;
                ret Test.2;
                "#
            ),
        )
    }

    #[test]
    fn when_joinpoint() {
        compiles_to_ir(
            r#"
            x : [ Red, White, Blue ]
            x = Blue

            y =
                when x is
                    Red -> 1
                    White -> 2
                    Blue -> 3

            y
            "#,
            indoc!(
                r#"
                let Test.0 = 0u8;
                switch Test.0:
                    case 1:
                        let Test.4 = 1i64;
                        jump Test.3 Test.4;

                    case 2:
                        let Test.5 = 2i64;
                        jump Test.3 Test.5;

                    default:
                        let Test.6 = 3i64;
                        jump Test.3 Test.6;

                joinpoint Test.3 Test.1:
                    ret Test.1;
                "#
            ),
        )
    }

    #[test]
    fn simple_if() {
        compiles_to_ir(
            r#"
            if True then
                1
            else
                2
            "#,
            indoc!(
                r#"
                let Test.1 = true;
                if Test.1 then
                    let Test.2 = 1i64;
                    ret Test.2;
                else
                    let Test.0 = 2i64;
                    ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn if_multi_branch() {
        compiles_to_ir(
            r#"
            if True then
                1
            else if False then
                2
            else
                3
            "#,
            indoc!(
                r#"
                let Test.3 = true;
                if Test.3 then
                    let Test.4 = 1i64;
                    ret Test.4;
                else
                    let Test.1 = false;
                    if Test.1 then
                        let Test.2 = 2i64;
                        ret Test.2;
                    else
                        let Test.0 = 3i64;
                        ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn when_on_result() {
        compiles_to_ir(
            r#"
            x : Result Int Int
            x = Ok 2

            y =
                when x is
                    Ok 3 -> 1
                    Ok _ -> 2
                    Err _ -> 3
            y
            "#,
            indoc!(
                r#"
                let Test.17 = 1i64;
                let Test.18 = 2i64;
                let Test.0 = Ok Test.17 Test.18;
                let Test.13 = true;
                let Test.15 = Index 0 Test.0;
                let Test.14 = 1i64;
                let Test.16 = lowlevel Eq Test.14 Test.15;
                let Test.12 = lowlevel And Test.16 Test.13;
                if Test.12 then
                    let Test.8 = true;
                    let Test.9 = 3i64;
                    let Test.10 = Index 0 Test.0;
                    let Test.11 = lowlevel Eq Test.9 Test.10;
                    let Test.7 = lowlevel And Test.11 Test.8;
                    if Test.7 then
                        let Test.4 = 1i64;
                        jump Test.3 Test.4;
                    else
                        let Test.5 = 2i64;
                        jump Test.3 Test.5;
                else
                    let Test.6 = 3i64;
                    jump Test.3 Test.6;
                joinpoint Test.3 Test.1:
                    ret Test.1;
                "#
            ),
        )
    }

    #[test]
    fn let_with_record_pattern() {
        compiles_to_ir(
            r#"
            { x } = { x: 0x2, y: 3.14 }

            x
            "#,
            indoc!(
                r#"
                let Test.4 = 2i64;
                let Test.5 = 3.14f64;
                let Test.3 = Struct {Test.4, Test.5};
                let Test.0 = Index 0 Test.3;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn let_with_record_pattern_list() {
        compiles_to_ir(
            r#"
            { x } = { x: [ 1, 3, 4 ], y: 3.14 }

            x
            "#,
            indoc!(
                r#"
                let Test.6 = 1i64;
                let Test.7 = 3i64;
                let Test.8 = 4i64;
                let Test.4 = Array [Test.6, Test.7, Test.8];
                let Test.5 = 3.14f64;
                let Test.3 = Struct {Test.4, Test.5};
                let Test.0 = Index 0 Test.3;
                inc Test.0;
                dec Test.3;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn if_guard_bind_variable_false() {
        compiles_to_ir(
            indoc!(
                r#"
                when 10 is
                    x if x == 5 -> 0
                    _ -> 42
                "#
            ),
            indoc!(
                r#"
                procedure Bool.5 (#Attr.2, #Attr.3):
                    let Test.10 = lowlevel Eq #Attr.2 #Attr.3;
                    ret Test.10;

                let Test.1 = 10i64;
                let Test.8 = true;
                let Test.5 = 5i64;
                let Test.4 = CallByName Bool.5 Test.1 Test.5;
                jump Test.3 Test.4;
                joinpoint Test.3 Test.9:
                    let Test.7 = lowlevel And Test.9 Test.8;
                    if Test.7 then
                        let Test.2 = 0i64;
                        ret Test.2;
                    else
                        let Test.6 = 42i64;
                        ret Test.6;
                "#
            ),
        )
    }

    #[test]
    fn alias_variable() {
        compiles_to_ir(
            indoc!(
                r#"
                x = 5
                y = x

                3
                "#
            ),
            indoc!(
                r#"
                let Test.0 = 5i64;
                ret Test.0;
                "#
            ),
        );

        compiles_to_ir(
            indoc!(
                r#"
                x = 5
                y = x

                y
                "#
            ),
            indoc!(
                r#"
                let Test.0 = 5i64;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn branch_store_variable() {
        compiles_to_ir(
            indoc!(
                r#"
                when 0 is
                    1 -> 12
                    a -> a
                "#
            ),
            indoc!(
                r#"
                let Test.1 = 0i64;
                let Test.5 = true;
                let Test.6 = 1i64;
                let Test.7 = lowlevel Eq Test.6 Test.1;
                let Test.4 = lowlevel And Test.7 Test.5;
                if Test.4 then
                    let Test.2 = 12i64;
                    ret Test.2;
                else
                    ret Test.1;
                "#
            ),
        )
    }

    #[test]
    fn list_pass_to_function() {
        compiles_to_ir(
            indoc!(
                r#"
                x : List Int
                x = [1,2,3]

                id : List Int -> List Int
                id = \y -> List.set y 0 0

                id x
                "#
            ),
            indoc!(
                r#"
                procedure List.4 (#Attr.2, #Attr.3, #Attr.4):
                    let Test.14 = lowlevel ListLen #Attr.2;
                    let Test.12 = lowlevel NumLt #Attr.3 Test.14;
                    if Test.12 then
                        let Test.13 = lowlevel ListSet #Attr.2 #Attr.3 #Attr.4;
                        ret Test.13;
                    else
                        ret #Attr.2;

                procedure Test.1 (Test.3):
                    let Test.9 = 0i64;
                    let Test.10 = 0i64;
                    let Test.8 = CallByName List.4 Test.3 Test.9 Test.10;
                    ret Test.8;

                let Test.5 = 1i64;
                let Test.6 = 2i64;
                let Test.7 = 3i64;
                let Test.0 = Array [Test.5, Test.6, Test.7];
                let Test.4 = CallByName Test.1 Test.0;
                dec Test.0;
                ret Test.4;
                "#
            ),
        )
    }

    #[test]
    fn record_optional_field_let_no_use_default() {
        compiles_to_ir(
            indoc!(
                r#"
                f = \r ->
                    { x ? 10, y } = r
                    x + y


                f { x: 4, y: 9 }
                "#
            ),
            indoc!(
                r#"
                procedure Test.0 (Test.4):
                    let Test.2 = Index 0 Test.4;
                    let Test.3 = Index 1 Test.4;
                    let Test.11 = CallByName Num.14 Test.2 Test.3;
                    jump Test.10 Test.11;
                    joinpoint Test.10 Test.9:
                        ret Test.9;

                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.12 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.12;

                let Test.7 = 4i64;
                let Test.8 = 9i64;
                let Test.6 = Struct {Test.7, Test.8};
                let Test.5 = CallByName Test.0 Test.6;
                ret Test.5;
                "#
            ),
        )
    }

    #[test]
    fn record_optional_field_let_use_default() {
        compiles_to_ir(
            indoc!(
                r#"
                f = \r ->
                    { x ? 10, y } = r
                    x + y


                f { y: 9 }
                "#
            ),
            indoc!(
                r#"
                procedure Test.0 (Test.4):
                    let Test.2 = 10i64;
                    let Test.3 = Index 1 Test.4;
                    let Test.10 = CallByName Num.14 Test.2 Test.3;
                    jump Test.9 Test.10;
                    joinpoint Test.9 Test.8:
                        ret Test.8;

                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.11 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.11;

                let Test.7 = 9i64;
                let Test.6 = Struct {Test.7};
                let Test.5 = CallByName Test.0 Test.6;
                ret Test.5;
                "#
            ),
        )
    }

    #[test]
    fn record_optional_field_function_no_use_default() {
        compiles_to_ir(
            indoc!(
                r#"
                f = \{ x ? 10, y } -> x + y


                f { x: 4, y: 9 }
                "#
            ),
            indoc!(
                r#"
                procedure Test.0 (Test.4):
                    let Test.2 = Index 0 Test.4;
                    let Test.3 = Index 1 Test.4;
                    let Test.11 = CallByName Num.14 Test.2 Test.3;
                    jump Test.10 Test.11;
                    joinpoint Test.10 Test.9:
                        ret Test.9;

                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.12 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.12;

                let Test.7 = 4i64;
                let Test.8 = 9i64;
                let Test.6 = Struct {Test.7, Test.8};
                let Test.5 = CallByName Test.0 Test.6;
                ret Test.5;
                "#
            ),
        )
    }

    #[test]
    fn record_optional_field_function_use_default() {
        compiles_to_ir(
            indoc!(
                r#"
                f = \{ x ? 10, y } -> x + y


                f { y: 9 }
                "#
            ),
            indoc!(
                r#"
                procedure Test.0 (Test.4):
                    let Test.2 = 10i64;
                    let Test.3 = Index 1 Test.4;
                    let Test.10 = CallByName Num.14 Test.2 Test.3;
                    jump Test.9 Test.10;
                    joinpoint Test.9 Test.8:
                        ret Test.8;

                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.11 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.11;

                let Test.7 = 9i64;
                let Test.6 = Struct {Test.7};
                let Test.5 = CallByName Test.0 Test.6;
                ret Test.5;
                "#
            ),
        )
    }

    #[allow(dead_code)]
    fn quicksort_help() {
        crate::helpers::with_larger_debug_stack(|| {
            compiles_to_ir(
                indoc!(
                    r#"
                quicksortHelp : List (Num a), Int, Int -> List (Num a)
                quicksortHelp = \list, low, high ->
                    if low < high then
                        (Pair partitionIndex partitioned) = Pair 0 []

                        partitioned
                            |> quicksortHelp low (partitionIndex - 1)
                            |> quicksortHelp (partitionIndex + 1) high
                    else
                        list

                quicksortHelp [] 0 0
                "#
                ),
                indoc!(
                    r#"
                "#
                ),
            )
        })
    }

    #[allow(dead_code)]
    fn quicksort_partition_help() {
        crate::helpers::with_larger_debug_stack(|| {
            compiles_to_ir(
                indoc!(
                    r#"
                partitionHelp : Int, Int, List (Num a), Int, (Num a) -> [ Pair Int (List (Num a)) ]
                partitionHelp = \i, j, list, high, pivot ->
                    if j < high then
                        when List.get list j is
                            Ok value ->
                                if value <= pivot then
                                    partitionHelp (i + 1) (j + 1) (swap (i + 1) j list) high pivot
                                else
                                    partitionHelp i (j + 1) list high pivot

                            Err _ ->
                                Pair i list
                    else
                        Pair i list



                partitionHelp 0 0 [] 0 0
                "#
                ),
                indoc!(
                    r#"
                "#
                ),
            )
        })
    }

    #[allow(dead_code)]
    fn quicksort_full() {
        crate::helpers::with_larger_debug_stack(|| {
            compiles_to_ir(
                indoc!(
                    r#"
                quicksort = \originalList ->
                    quicksortHelp : List (Num a), Int, Int -> List (Num a)
                    quicksortHelp = \list, low, high ->
                        if low < high then
                            (Pair partitionIndex partitioned) = partition low high list

                            partitioned
                                |> quicksortHelp low (partitionIndex - 1)
                                |> quicksortHelp (partitionIndex + 1) high
                        else
                            list


                    swap : Int, Int, List a -> List a
                    swap = \i, j, list ->
                        when Pair (List.get list i) (List.get list j) is
                            Pair (Ok atI) (Ok atJ) ->
                                list
                                    |> List.set i atJ
                                    |> List.set j atI

                            _ ->
                                []

                    partition : Int, Int, List (Num a) -> [ Pair Int (List (Num a)) ]
                    partition = \low, high, initialList ->
                        when List.get initialList high is
                            Ok pivot ->
                                when partitionHelp (low - 1) low initialList high pivot is
                                    Pair newI newList ->
                                        Pair (newI + 1) (swap (newI + 1) high newList)

                            Err _ ->
                                Pair (low - 1) initialList


                    partitionHelp : Int, Int, List (Num a), Int, (Num a) -> [ Pair Int (List (Num a)) ]
                    partitionHelp = \i, j, list, high, pivot ->
                        if j < high then
                            when List.get list j is
                                Ok value ->
                                    if value <= pivot then
                                        partitionHelp (i + 1) (j + 1) (swap (i + 1) j list) high pivot
                                    else
                                        partitionHelp i (j + 1) list high pivot

                                Err _ ->
                                    Pair i list
                        else
                            Pair i list



                    n = List.len originalList
                    quicksortHelp originalList 0 (n - 1)

                quicksort [1,2,3]
                "#
                ),
                indoc!(
                    r#"
                "#
                ),
            )
        })
    }

    #[test]
    fn factorial() {
        compiles_to_ir(
            r#"
            factorial = \n, accum ->
                when n is
                    0 -> 
                        accum

                    _ -> 
                        factorial (n - 1) (n * accum)

            factorial 10 1
            "#,
            indoc!(
                r#"
                procedure Test.0 (Test.2, Test.3):
                    jump Test.20 Test.2 Test.3;
                    joinpoint Test.20 Test.2 Test.3:
                        let Test.17 = true;
                        let Test.18 = 0i64;
                        let Test.19 = lowlevel Eq Test.18 Test.2;
                        let Test.16 = lowlevel And Test.19 Test.17;
                        if Test.16 then
                            ret Test.3;
                        else
                            let Test.13 = 1i64;
                            let Test.9 = CallByName Num.15 Test.2 Test.13;
                            let Test.10 = CallByName Num.16 Test.2 Test.3;
                            jump Test.20 Test.9 Test.10;

                procedure Num.15 (#Attr.2, #Attr.3):
                    let Test.14 = lowlevel NumSub #Attr.2 #Attr.3;
                    ret Test.14;

                procedure Num.16 (#Attr.2, #Attr.3):
                    let Test.11 = lowlevel NumMul #Attr.2 #Attr.3;
                    ret Test.11;

                let Test.5 = 10i64;
                let Test.6 = 1i64;
                let Test.4 = CallByName Test.0 Test.5 Test.6;
                ret Test.4;
                "#
            ),
        )
    }
}
