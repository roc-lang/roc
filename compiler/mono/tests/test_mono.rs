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
    fn ir_if() {
        compiles_to_ir(
            r#"
            if True then 1 else 2
            "#,
            indoc!(
                r#"
                let Test.3 = true;
                if Test.3 then
                    let Test.1 = 1i64;
                    jump Test.2 Test.1;
                else
                    let Test.1 = 2i64;
                    jump Test.2 Test.1;
                joinpoint Test.2 Test.0:
                    ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn ir_when_enum() {
        compiles_to_ir(
            r#"
            when Blue is
                Red -> 1
                White -> 2
                Blue -> 3
            "#,
            indoc!(
                r#"
                let Test.1 = 0u8;
                switch Test.1:
                    case 1:
                        let Test.3 = 1i64;
                        jump Test.2 Test.3;

                    case 2:
                        let Test.4 = 2i64;
                        jump Test.2 Test.4;

                    default:
                        let Test.5 = 3i64;
                        jump Test.2 Test.5;

                joinpoint Test.2 Test.0:
                    ret Test.0;
                "#
            ),
        )
    }

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
                let Test.11 = 0i64;
                let Test.12 = 3i64;
                let Test.2 = Just Test.11 Test.12;
                let Test.7 = true;
                let Test.9 = Index 0 Test.2;
                let Test.8 = 0i64;
                let Test.10 = lowlevel Eq Test.8 Test.9;
                let Test.6 = lowlevel And Test.10 Test.7;
                if Test.6 then
                    let Test.0 = Index 1 Test.2;
                    jump Test.3 Test.0;
                else
                    let Test.5 = 0i64;
                    jump Test.3 Test.5;
                joinpoint Test.3 Test.1:
                    ret Test.1;
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
                let Test.9 = 1i64;
                let Test.10 = 1i64;
                let Test.11 = 2i64;
                let Test.4 = These Test.9 Test.10 Test.11;
                switch Test.4:
                    case 2:
                        let Test.0 = Index 1 Test.4;
                        jump Test.5 Test.0;

                    case 0:
                        let Test.1 = Index 1 Test.4;
                        jump Test.5 Test.1;

                    default:
                        let Test.2 = Index 1 Test.4;
                        jump Test.5 Test.2;

                joinpoint Test.5 Test.3:
                    ret Test.3;
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
                let Test.6 = 1i64;
                let Test.7 = 3.14f64;
                let Test.2 = Struct {Test.6, Test.7};
                let Test.0 = Index 0 Test.2;
                jump Test.3 Test.0;
                joinpoint Test.3 Test.1:
                    ret Test.1;
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
                    let Test.21 = 0i64;
                    let Test.18 = lowlevel NotEq #Attr.3 Test.21;
                    if Test.18 then
                        let Test.19 = 1i64;
                        let Test.20 = lowlevel NumDivUnchecked #Attr.2 #Attr.3;
                        let Test.14 = Ok Test.19 Test.20;
                        jump Test.15 Test.14;
                    else
                        let Test.16 = 0i64;
                        let Test.17 = Struct {};
                        let Test.14 = Err Test.16 Test.17;
                        jump Test.15 Test.14;
                    joinpoint Test.15 Test.13:
                        ret Test.13;

                let Test.11 = 1000i64;
                let Test.12 = 10i64;
                let Test.2 = CallByName Num.32 Test.11 Test.12;
                let Test.7 = true;
                let Test.9 = Index 0 Test.2;
                let Test.8 = 1i64;
                let Test.10 = lowlevel Eq Test.8 Test.9;
                let Test.6 = lowlevel And Test.10 Test.7;
                if Test.6 then
                    let Test.0 = Index 1 Test.2;
                    jump Test.3 Test.0;
                else
                    let Test.5 = -1i64;
                    jump Test.3 Test.5;
                joinpoint Test.3 Test.1:
                    ret Test.1;
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
                    let Test.14 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.14;

                let Test.12 = 0i64;
                let Test.13 = 41i64;
                let Test.0 = Just Test.12 Test.13;
                let Test.8 = true;
                let Test.10 = Index 0 Test.0;
                let Test.9 = 0i64;
                let Test.11 = lowlevel Eq Test.9 Test.10;
                let Test.7 = lowlevel And Test.11 Test.8;
                if Test.7 then
                    let Test.1 = Index 1 Test.0;
                    let Test.5 = 1i64;
                    let Test.4 = CallByName Num.14 Test.1 Test.5;
                    jump Test.3 Test.4;
                else
                    let Test.6 = 1i64;
                    jump Test.3 Test.6;
                joinpoint Test.3 Test.2:
                    ret Test.2;
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
    fn join_points() {
        compiles_to_ir(
            r#"
            x =
                if True then 1 else 2

            x
            "#,
            indoc!(
                r#"
                let Test.4 = true;
                if Test.4 then
                    let Test.2 = 1i64;
                    jump Test.3 Test.2;
                else
                    let Test.2 = 2i64;
                    jump Test.3 Test.2;
                joinpoint Test.3 Test.0:
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
                let Test.1 = 2i64;
                let Test.8 = true;
                let Test.9 = 2i64;
                let Test.12 = lowlevel Eq Test.9 Test.1;
                let Test.10 = lowlevel And Test.12 Test.8;
                let Test.5 = false;
                jump Test.4 Test.5;
                joinpoint Test.4 Test.11:
                    let Test.7 = lowlevel And Test.11 Test.10;
                    if Test.7 then
                        let Test.3 = 42i64;
                        jump Test.2 Test.3;
                    else
                        let Test.6 = 0i64;
                        jump Test.2 Test.6;
                joinpoint Test.2 Test.0:
                    ret Test.0;
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
                    let Test.7 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.7;

                let Test.6 = 2i64;
                let Test.2 = Struct {Test.6};
                let Test.0 = Index 0 Test.2;
                let Test.5 = 3i64;
                let Test.4 = CallByName Num.14 Test.0 Test.5;
                jump Test.3 Test.4;
                joinpoint Test.3 Test.1:
                    ret Test.1;
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
                    let Test.22 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.22;

                let Test.18 = 0i64;
                let Test.20 = 0i64;
                let Test.21 = 41i64;
                let Test.19 = Just Test.20 Test.21;
                let Test.1 = Just Test.18 Test.19;
                let Test.10 = true;
                let Test.12 = Index 0 Test.1;
                let Test.11 = 0i64;
                let Test.17 = lowlevel Eq Test.11 Test.12;
                let Test.15 = lowlevel And Test.17 Test.10;
                let Test.14 = Index 0 Test.1;
                let Test.13 = 0i64;
                let Test.16 = lowlevel Eq Test.13 Test.14;
                let Test.9 = lowlevel And Test.16 Test.15;
                if Test.9 then
                    let Test.7 = Index 1 Test.1;
                    let Test.2 = Index 1 Test.7;
                    let Test.6 = 1i64;
                    let Test.5 = CallByName Num.14 Test.2 Test.6;
                    jump Test.4 Test.5;
                else
                    let Test.8 = 1i64;
                    jump Test.4 Test.8;
                joinpoint Test.4 Test.3:
                    ret Test.3;
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
                        let Test.18 = lowlevel NumAdd #Attr.2 #Attr.3;
                        ret Test.18;

                    let Test.16 = 2i64;
                    let Test.17 = 3i64;
                    let Test.3 = Struct {Test.16, Test.17};
                    let Test.8 = true;
                    let Test.9 = 4i64;
                    let Test.10 = Index 0 Test.3;
                    let Test.15 = lowlevel Eq Test.9 Test.10;
                    let Test.13 = lowlevel And Test.15 Test.8;
                    let Test.11 = 3i64;
                    let Test.12 = Index 1 Test.3;
                    let Test.14 = lowlevel Eq Test.11 Test.12;
                    let Test.7 = lowlevel And Test.14 Test.13;
                    if Test.7 then
                        let Test.5 = 9i64;
                        jump Test.4 Test.5;
                    else
                        let Test.0 = Index 0 Test.3;
                        let Test.1 = Index 1 Test.3;
                        let Test.6 = CallByName Num.14 Test.0 Test.1;
                        jump Test.4 Test.6;
                    joinpoint Test.4 Test.2:
                        ret Test.2;
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
                    let Test.10 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.10;

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
                let Test.3 = true;
                if Test.3 then
                    let Test.1 = 1i64;
                    jump Test.2 Test.1;
                else
                    let Test.1 = 2i64;
                    jump Test.2 Test.1;
                joinpoint Test.2 Test.0:
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
                let Test.6 = true;
                if Test.6 then
                    let Test.1 = 1i64;
                    jump Test.2 Test.1;
                else
                    let Test.5 = false;
                    if Test.5 then
                        let Test.3 = 2i64;
                        jump Test.4 Test.3;
                    else
                        let Test.3 = 3i64;
                        jump Test.4 Test.3;
                    joinpoint Test.4 Test.1:
                        jump Test.2 Test.1;
                joinpoint Test.2 Test.0:
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
                    let Test.12 = lowlevel Eq #Attr.2 #Attr.3;
                    ret Test.12;

                let Test.2 = 10i64;
                let Test.10 = true;
                let Test.7 = 5i64;
                let Test.6 = CallByName Bool.5 Test.2 Test.7;
                jump Test.5 Test.6;
                joinpoint Test.5 Test.11:
                    let Test.9 = lowlevel And Test.11 Test.10;
                    if Test.9 then
                        let Test.4 = 0i64;
                        jump Test.3 Test.4;
                    else
                        let Test.8 = 42i64;
                        jump Test.3 Test.8;
                joinpoint Test.3 Test.1:
                    ret Test.1;
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
                let Test.2 = 0i64;
                let Test.7 = true;
                let Test.8 = 1i64;
                let Test.9 = lowlevel Eq Test.8 Test.2;
                let Test.6 = lowlevel And Test.9 Test.7;
                if Test.6 then
                    let Test.4 = 12i64;
                    jump Test.3 Test.4;
                else
                    jump Test.3 Test.2;
                joinpoint Test.3 Test.1:
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
                    let Test.15 = lowlevel ListLen #Attr.2;
                    let Test.14 = lowlevel NumLt #Attr.3 Test.15;
                    if Test.14 then
                        let Test.12 = lowlevel ListSet #Attr.2 #Attr.3 #Attr.4;
                        jump Test.13 Test.12;
                    else
                        jump Test.13 #Attr.2;
                    joinpoint Test.13 Test.11:
                        ret Test.11;

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
}
