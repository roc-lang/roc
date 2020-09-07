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

        assert_eq!(
            procs.runtime_errors,
            roc_collections::all::MutMap::default()
        );

        let (procs, param_map) = procs.get_specialized_procs_help(mono_env.arena);

        // apply inc/dec
        let stmt = mono_env.arena.alloc(ir_expr);
        let ir_expr = roc_mono::inc_dec::visit_declaration(mono_env.arena, param_map, stmt);

        let mut procs_string = procs
            .values()
            .map(|proc| proc.to_pretty(200))
            .collect::<Vec<_>>();

        procs_string.sort();
        procs_string.push(ir_expr.to_pretty(200));

        let result = procs_string.join("\n");

        let the_same = result == expected;

        if !the_same {
            println!("{}", result);

            let expected_lines = expected.split("\n").collect::<Vec<&str>>();
            let result_lines = result.split("\n").collect::<Vec<&str>>();

            assert_eq!(expected_lines, result_lines);
            //assert_eq!(0, 1);
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
                let Test.6 = 0i64;
                let Test.7 = Index 0 Test.1;
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
                let Test.6 = 1i64;
                let Test.7 = Index 0 Test.1;
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
                let Test.7 = 0i64;
                let Test.8 = Index 0 Test.0;
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
            main = \{} ->
                when 2 is
                    2 if False -> 42
                    _ -> 0

            main {}
            "#,
            indoc!(
                r#"
                procedure Test.0 (Test.2):
                    let Test.5 = 2i64;
                    let Test.11 = true;
                    let Test.12 = 2i64;
                    let Test.15 = lowlevel Eq Test.12 Test.5;
                    let Test.13 = lowlevel And Test.15 Test.11;
                    let Test.8 = false;
                    jump Test.7 Test.8;
                    joinpoint Test.7 Test.14:
                        let Test.10 = lowlevel And Test.14 Test.13;
                        if Test.10 then
                            let Test.6 = 42i64;
                            ret Test.6;
                        else
                            let Test.9 = 0i64;
                            ret Test.9;

                let Test.4 = Struct {};
                let Test.3 = CallByName Test.0 Test.4;
                ret Test.3;
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
                    let Test.21 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.21;

                let Test.17 = 0i64;
                let Test.19 = 0i64;
                let Test.20 = 41i64;
                let Test.18 = Just Test.19 Test.20;
                let Test.1 = Just Test.17 Test.18;
                let Test.8 = true;
                let Test.10 = 0i64;
                let Test.9 = Index 1 Test.1;
                let Test.11 = Index 0 Test.9;
                let Test.16 = lowlevel Eq Test.10 Test.11;
                let Test.14 = lowlevel And Test.16 Test.8;
                let Test.12 = 0i64;
                let Test.13 = Index 0 Test.1;
                let Test.15 = lowlevel Eq Test.12 Test.13;
                let Test.7 = lowlevel And Test.15 Test.14;
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
                let Test.8 = 4i64;
                let Test.7 = Index 0 Test.2;
                let Test.13 = lowlevel Eq Test.8 Test.7;
                let Test.11 = lowlevel And Test.13 Test.6;
                let Test.10 = 3i64;
                let Test.9 = Index 1 Test.2;
                let Test.12 = lowlevel Eq Test.10 Test.9;
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
                ret Test.3;
                "#
            ),
        )
    }

    #[test]
    fn list_append() {
        // TODO this leaks at the moment
        // ListAppend needs to decrement its arguments
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
                    let Test.10 = lowlevel ListLen #Attr.2;
                    ret Test.10;

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
            main = \{} -> 
                x : [ Red, White, Blue ]
                x = Blue

                y =
                    when x is
                        Red -> 1
                        White -> 2
                        Blue -> 3

                y

            main {}
            "#,
            indoc!(
                r#"
                procedure Test.0 (Test.4):
                    let Test.2 = 0u8;
                    switch Test.2:
                        case 1:
                            let Test.9 = 1i64;
                            jump Test.8 Test.9;
                    
                        case 2:
                            let Test.10 = 2i64;
                            jump Test.8 Test.10;
                    
                        default:
                            let Test.11 = 3i64;
                            jump Test.8 Test.11;
                    
                    joinpoint Test.8 Test.3:
                        ret Test.3;

                let Test.6 = Struct {};
                let Test.5 = CallByName Test.0 Test.6;
                ret Test.5;
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
            main = \{} -> 
                x : Result Int Int
                x = Ok 2

                y =
                    when x is
                        Ok 3 -> 1
                        Ok _ -> 2
                        Err _ -> 3
                y

            main {}
            "#,
            indoc!(
                r#"
                procedure Test.0 (Test.4):
                    let Test.22 = 1i64;
                    let Test.23 = 2i64;
                    let Test.2 = Ok Test.22 Test.23;
                    let Test.18 = true;
                    let Test.19 = 1i64;
                    let Test.20 = Index 0 Test.2;
                    let Test.21 = lowlevel Eq Test.19 Test.20;
                    let Test.17 = lowlevel And Test.21 Test.18;
                    if Test.17 then
                        let Test.13 = true;
                        let Test.15 = 3i64;
                        let Test.14 = Index 1 Test.2;
                        let Test.16 = lowlevel Eq Test.15 Test.14;
                        let Test.12 = lowlevel And Test.16 Test.13;
                        if Test.12 then
                            let Test.9 = 1i64;
                            jump Test.8 Test.9;
                        else
                            let Test.10 = 2i64;
                            jump Test.8 Test.10;
                    else
                        let Test.11 = 3i64;
                        jump Test.8 Test.11;
                    joinpoint Test.8 Test.3:
                        ret Test.3;

                let Test.6 = Struct {};
                let Test.5 = CallByName Test.0 Test.6;
                ret Test.5;
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
                main = \{} -> 
                    when 10 is
                        x if x == 5 -> 0
                        _ -> 42

                main {}
                "#
            ),
            indoc!(
                r#"
                procedure Bool.5 (#Attr.2, #Attr.3):
                    let Test.11 = lowlevel Eq #Attr.2 #Attr.3;
                    ret Test.11;

                procedure Test.0 (Test.3):
                    let Test.6 = 10i64;
                    let Test.14 = true;
                    let Test.10 = 5i64;
                    let Test.9 = CallByName Bool.5 Test.6 Test.10;
                    jump Test.8 Test.9;
                    joinpoint Test.8 Test.15:
                        let Test.13 = lowlevel And Test.15 Test.14;
                        if Test.13 then
                            let Test.7 = 0i64;
                            ret Test.7;
                        else
                            let Test.12 = 42i64;
                            ret Test.12;

                let Test.5 = Struct {};
                let Test.4 = CallByName Test.0 Test.5;
                ret Test.4;
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
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.10 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.10;

                procedure Test.0 (Test.2):
                    let Test.3 = Index 0 Test.2;
                    let Test.4 = Index 1 Test.2;
                    let Test.9 = CallByName Num.14 Test.3 Test.4;
                    ret Test.9;

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
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.9 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.9;

                procedure Test.0 (Test.2):
                    let Test.3 = 10i64;
                    let Test.4 = Index 1 Test.2;
                    let Test.8 = CallByName Num.14 Test.3 Test.4;
                    ret Test.8;

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
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.10 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.10;

                procedure Test.0 (Test.4):
                    let Test.2 = Index 0 Test.4;
                    let Test.3 = Index 1 Test.4;
                    let Test.9 = CallByName Num.14 Test.2 Test.3;
                    ret Test.9;

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
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.9 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.9;

                procedure Test.0 (Test.4):
                    let Test.2 = 10i64;
                    let Test.3 = Index 1 Test.4;
                    let Test.8 = CallByName Num.14 Test.2 Test.3;
                    ret Test.8;

                let Test.7 = 9i64;
                let Test.6 = Struct {Test.7};
                let Test.5 = CallByName Test.0 Test.6;
                ret Test.5;
                "#
            ),
        )
    }

    #[ignore]
    #[test]
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

    #[test]
    fn quicksort_swap() {
        crate::helpers::with_larger_debug_stack(|| {
            compiles_to_ir(
                indoc!(
                    r#"
                    swap = \list ->
                        when Pair (List.get list 0) (List.get list 0) is
                            Pair (Ok atI) (Ok atJ) ->
                                list

                            _ ->
                                []
                    swap [ 1, 2 ]
                "#
                ),
                indoc!(
                    r#"
                    procedure List.3 (#Attr.2, #Attr.3):
                        let Test.35 = lowlevel ListLen #Attr.2;
                        let Test.31 = lowlevel NumLt #Attr.3 Test.35;
                        if Test.31 then
                            let Test.33 = 1i64;
                            let Test.34 = lowlevel ListGetUnsafe #Attr.2 #Attr.3;
                            let Test.32 = Ok Test.33 Test.34;
                            ret Test.32;
                        else
                            let Test.29 = 0i64;
                            let Test.30 = Struct {};
                            let Test.28 = Err Test.29 Test.30;
                            ret Test.28;

                    procedure Test.0 (Test.2):
                        let Test.36 = 0i64;
                        let Test.25 = CallByName List.3 Test.2 Test.36;
                        let Test.27 = 0i64;
                        let Test.26 = CallByName List.3 Test.2 Test.27;
                        let Test.9 = Struct {Test.25, Test.26};
                        let Test.15 = true;
                        let Test.17 = 1i64;
                        let Test.16 = Index 0 Test.9;
                        let Test.18 = Index 0 Test.16;
                        let Test.24 = lowlevel Eq Test.17 Test.18;
                        let Test.22 = lowlevel And Test.24 Test.15;
                        let Test.20 = 1i64;
                        let Test.19 = Index 1 Test.9;
                        let Test.21 = Index 0 Test.19;
                        let Test.23 = lowlevel Eq Test.20 Test.21;
                        let Test.14 = lowlevel And Test.23 Test.22;
                        if Test.14 then
                            let Test.12 = Index 0 Test.9;
                            let Test.3 = Index 1 Test.12;
                            let Test.11 = Index 1 Test.9;
                            let Test.4 = Index 1 Test.11;
                            inc Test.2;
                            ret Test.2;
                        else
                            let Test.13 = Array [];
                            ret Test.13;

                    let Test.7 = 1i64;
                    let Test.8 = 2i64;
                    let Test.6 = Array [Test.7, Test.8];
                    let Test.5 = CallByName Test.0 Test.6;
                    dec Test.6;
                    ret Test.5;
                "#
                ),
            )
        })
    }

    #[ignore]
    #[test]
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

    #[ignore]
    #[test]
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
                procedure Num.15 (#Attr.2, #Attr.3):
                    let Test.13 = lowlevel NumSub #Attr.2 #Attr.3;
                    ret Test.13;

                procedure Num.16 (#Attr.2, #Attr.3):
                    let Test.11 = lowlevel NumMul #Attr.2 #Attr.3;
                    ret Test.11;

                procedure Test.0 (Test.2, Test.3):
                    jump Test.18 Test.2 Test.3;
                    joinpoint Test.18 Test.2 Test.3:
                        let Test.15 = true;
                        let Test.16 = 0i64;
                        let Test.17 = lowlevel Eq Test.16 Test.2;
                        let Test.14 = lowlevel And Test.17 Test.15;
                        if Test.14 then
                            ret Test.3;
                        else
                            let Test.12 = 1i64;
                            let Test.9 = CallByName Num.15 Test.2 Test.12;
                            let Test.10 = CallByName Num.16 Test.2 Test.3;
                            jump Test.18 Test.9 Test.10;

                let Test.5 = 10i64;
                let Test.6 = 1i64;
                let Test.4 = CallByName Test.0 Test.5 Test.6;
                ret Test.4;
                "#
            ),
        )
    }

    #[test]
    #[ignore]
    fn is_nil() {
        compiles_to_ir(
            r#"
            ConsList a : [ Cons a (ConsList a), Nil ]

            isNil : ConsList a -> Bool
            isNil = \list ->
                when list is 
                    Nil -> True
                    Cons _ _ -> False

            isNil (Cons 0x2 Nil) 
            "#,
            indoc!(
                r#"
                procedure Test.1 (Test.3):
                    let Test.13 = true;
                    let Test.15 = Index 0 Test.3;
                    let Test.14 = 1i64;
                    let Test.16 = lowlevel Eq Test.14 Test.15;
                    let Test.12 = lowlevel And Test.16 Test.13;
                    if Test.12 then
                        let Test.10 = true;
                        ret Test.10;
                    else
                        let Test.11 = false;
                        ret Test.11;

                let Test.6 = 0i64;
                let Test.7 = 2i64;
                let Test.9 = 1i64;
                let Test.8 = Nil Test.9;
                let Test.5 = Cons Test.6 Test.7 Test.8;
                let Test.4 = CallByName Test.1 Test.5;
                ret Test.4;
                "#
            ),
        )
    }

    #[test]
    #[ignore]
    fn has_none() {
        compiles_to_ir(
            r#"
            Maybe a : [ Just a, Nothing ]
            ConsList a : [ Cons a (ConsList a), Nil ]

            hasNone : ConsList (Maybe a) -> Bool
            hasNone = \list ->
                when list is 
                    Nil -> False
                    Cons Nothing _ -> True
                    Cons (Just _) xs -> hasNone xs

            hasNone (Cons (Just 3) Nil) 
            "#,
            indoc!(
                r#"
                procedure Test.1 (Test.3):
                    let Test.13 = true;
                    let Test.15 = Index 0 Test.3;
                    let Test.14 = 1i64;
                    let Test.16 = lowlevel Eq Test.14 Test.15;
                    let Test.12 = lowlevel And Test.16 Test.13;
                    if Test.12 then
                        let Test.10 = true;
                        ret Test.10;
                    else
                        let Test.11 = false;
                        ret Test.11;

                let Test.6 = 0i64;
                let Test.7 = 2i64;
                let Test.9 = 1i64;
                let Test.8 = Nil Test.9;
                let Test.5 = Cons Test.6 Test.7 Test.8;
                let Test.4 = CallByName Test.1 Test.5;
                ret Test.4;
                "#
            ),
        )
    }

    #[test]
    fn mk_pair_of() {
        compiles_to_ir(
            r#"
            mkPairOf = \x -> Pair x x

            mkPairOf [1,2,3]
            "#,
            indoc!(
                r#"
                procedure Test.0 (Test.2):
                    inc Test.2;
                    let Test.8 = Struct {Test.2, Test.2};
                    ret Test.8;

                let Test.5 = 1i64;
                let Test.6 = 2i64;
                let Test.7 = 3i64;
                let Test.4 = Array [Test.5, Test.6, Test.7];
                let Test.3 = CallByName Test.0 Test.4;
                ret Test.3;
                "#
            ),
        )
    }

    #[test]
    fn fst() {
        compiles_to_ir(
            r#"
            fst = \x, y -> x 

            fst [1,2,3] [3,2,1]
            "#,
            indoc!(
                r#"
                procedure Test.0 (Test.2, Test.3):
                    inc Test.2;
                    ret Test.2;

                let Test.10 = 1i64;
                let Test.11 = 2i64;
                let Test.12 = 3i64;
                let Test.5 = Array [Test.10, Test.11, Test.12];
                let Test.7 = 3i64;
                let Test.8 = 2i64;
                let Test.9 = 1i64;
                let Test.6 = Array [Test.7, Test.8, Test.9];
                let Test.4 = CallByName Test.0 Test.5 Test.6;
                dec Test.6;
                dec Test.5;
                ret Test.4;
                "#
            ),
        )
    }

    #[test]
    fn list_cannot_update_inplace() {
        compiles_to_ir(
            indoc!(
                r#"
                x : List Int
                x = [1,2,3]

                add : List Int -> List Int
                add = \y -> List.set y 0 0
                
                List.len (add x) + List.len x
                "#
            ),
            indoc!(
                r#"
                procedure List.4 (#Attr.2, #Attr.3, #Attr.4):
                    let Test.18 = lowlevel ListLen #Attr.2;
                    let Test.16 = lowlevel NumLt #Attr.3 Test.18;
                    if Test.16 then
                        let Test.17 = lowlevel ListSet #Attr.2 #Attr.3 #Attr.4;
                        ret Test.17;
                    else
                        ret #Attr.2;

                procedure List.7 (#Attr.2):
                    let Test.11 = lowlevel ListLen #Attr.2;
                    ret Test.11;

                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.19 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.19;

                procedure Test.1 (Test.3):
                    let Test.13 = 0i64;
                    let Test.14 = 0i64;
                    let Test.12 = CallByName List.4 Test.3 Test.13 Test.14;
                    ret Test.12;

                let Test.8 = 1i64;
                let Test.9 = 2i64;
                let Test.10 = 3i64;
                let Test.0 = Array [Test.8, Test.9, Test.10];
                inc Test.0;
                let Test.7 = CallByName Test.1 Test.0;
                let Test.5 = CallByName List.7 Test.7;
                dec Test.7;
                let Test.6 = CallByName List.7 Test.0;
                dec Test.0;
                let Test.4 = CallByName Num.14 Test.5 Test.6;
                ret Test.4;
                "#
            ),
        )
    }

    #[test]
    fn list_get() {
        compiles_to_ir(
            indoc!(
                r#"
                main = \{} -> 
                    List.get [1,2,3] 0

                main {}
                "#
            ),
            indoc!(
                r#"
                procedure List.3 (#Attr.2, #Attr.3):
                    let Test.15 = lowlevel ListLen #Attr.2;
                    let Test.11 = lowlevel NumLt #Attr.3 Test.15;
                    if Test.11 then
                        let Test.13 = 1i64;
                        let Test.14 = lowlevel ListGetUnsafe #Attr.2 #Attr.3;
                        let Test.12 = Ok Test.13 Test.14;
                        ret Test.12;
                    else
                        let Test.9 = 0i64;
                        let Test.10 = Struct {};
                        let Test.8 = Err Test.9 Test.10;
                        ret Test.8;

                procedure Test.0 (Test.2):
                    let Test.16 = 1i64;
                    let Test.17 = 2i64;
                    let Test.18 = 3i64;
                    let Test.6 = Array [Test.16, Test.17, Test.18];
                    let Test.7 = 0i64;
                    let Test.5 = CallByName List.3 Test.6 Test.7;
                    dec Test.6;
                    ret Test.5;

                let Test.4 = Struct {};
                let Test.3 = CallByName Test.0 Test.4;
                ret Test.3;
                "#
            ),
        )
    }

    #[test]
    fn peano() {
        compiles_to_ir(
            indoc!(
                r#"
                Peano : [ S Peano, Z ]

                three : Peano
                three = S (S (S Z))

                three
                "#
            ),
            indoc!(
                r#"
                let Test.3 = 0i64;
                let Test.5 = 0i64;
                let Test.7 = 0i64;
                let Test.9 = 1i64;
                let Test.8 = Z Test.9;
                let Test.6 = S Test.7 Test.8;
                let Test.4 = S Test.5 Test.6;
                let Test.1 = S Test.3 Test.4;
                ret Test.1;
                "#
            ),
        )
    }

    #[test]
    fn peano1() {
        compiles_to_ir(
            indoc!(
                r#"
                Peano : [ S Peano, Z ]

                three : Peano
                three = S (S (S Z))

                when three is
                    Z -> 0
                    S _ -> 1
                "#
            ),
            indoc!(
                r#"
                let Test.9 = 0i64;
                let Test.11 = 0i64;
                let Test.13 = 0i64;
                let Test.15 = 1i64;
                let Test.14 = Z Test.15;
                let Test.12 = S Test.13 Test.14;
                let Test.10 = S Test.11 Test.12;
                let Test.1 = S Test.9 Test.10;
                let Test.5 = true;
                let Test.6 = 1i64;
                let Test.7 = Index 0 Test.1;
                let Test.8 = lowlevel Eq Test.6 Test.7;
                let Test.4 = lowlevel And Test.8 Test.5;
                if Test.4 then
                    let Test.2 = 0i64;
                    ret Test.2;
                else
                    let Test.3 = 1i64;
                    ret Test.3;
                "#
            ),
        )
    }

    #[test]
    fn peano2() {
        compiles_to_ir(
            indoc!(
                r#"
                Peano : [ S Peano, Z ]

                three : Peano
                three = S (S (S Z))

                when three is
                    S (S _) -> 1
                    S (_) -> 0
                    Z -> 0
                "#
            ),
            indoc!(
                r#"
                let Test.17 = 0i64;
                let Test.19 = 0i64;
                let Test.21 = 0i64;
                let Test.23 = 1i64;
                let Test.22 = Z Test.23;
                let Test.20 = S Test.21 Test.22;
                let Test.18 = S Test.19 Test.20;
                let Test.1 = S Test.17 Test.18;
                let Test.13 = true;
                let Test.14 = 0i64;
                let Test.15 = Index 0 Test.1;
                let Test.16 = lowlevel Eq Test.14 Test.15;
                let Test.12 = lowlevel And Test.16 Test.13;
                if Test.12 then
                    let Test.7 = true;
                    let Test.9 = 0i64;
                    let Test.8 = Index 1 Test.1;
                    let Test.10 = Index 0 Test.8;
                    let Test.11 = lowlevel Eq Test.9 Test.10;
                    let Test.6 = lowlevel And Test.11 Test.7;
                    if Test.6 then
                        let Test.3 = Index 1 Test.1;
                        let Test.2 = 1i64;
                        ret Test.2;
                    else
                        let Test.4 = 0i64;
                        ret Test.4;
                else
                    let Test.5 = 0i64;
                    ret Test.5;
                "#
            ),
        )
    }

    #[test]
    fn optional_when() {
        compiles_to_ir(
            indoc!(
                r#"
                f = \r ->
                    when r is
                        { x: Blue, y ? 3 } -> y
                        { x: Red, y ? 5 } -> y

                a = f { x: Blue, y: 7 } 
                b = f { x: Blue }
                c = f { x: Red, y: 11 } 
                d = f { x: Red }

                a * b * c * d
                "#
            ),
            indoc!(
                r#"
                procedure Num.16 (#Attr.2, #Attr.3):
                    let Test.38 = lowlevel NumMul #Attr.2 #Attr.3;
                    ret Test.38;

                procedure Test.0 (Test.6):
                    let Test.27 = true;
                    let Test.29 = false;
                    let Test.28 = Index 0 Test.6;
                    let Test.30 = lowlevel Eq Test.29 Test.28;
                    let Test.26 = lowlevel And Test.30 Test.27;
                    if Test.26 then
                        let Test.8 = 3i64;
                        ret Test.8;
                    else
                        let Test.10 = 5i64;
                        ret Test.10;

                procedure Test.0 (Test.6):
                    let Test.34 = true;
                    let Test.36 = false;
                    let Test.35 = Index 0 Test.6;
                    let Test.37 = lowlevel Eq Test.36 Test.35;
                    let Test.33 = lowlevel And Test.37 Test.34;
                    if Test.33 then
                        let Test.8 = Index 1 Test.6;
                        ret Test.8;
                    else
                        let Test.10 = Index 1 Test.6;
                        ret Test.10;

                let Test.22 = true;
                let Test.23 = 11i64;
                let Test.21 = Struct {Test.22, Test.23};
                let Test.3 = CallByName Test.0 Test.21;
                let Test.20 = true;
                let Test.19 = Struct {Test.20};
                let Test.4 = CallByName Test.0 Test.19;
                let Test.17 = false;
                let Test.18 = 7i64;
                let Test.16 = Struct {Test.17, Test.18};
                let Test.1 = CallByName Test.0 Test.16;
                let Test.15 = false;
                let Test.14 = Struct {Test.15};
                let Test.2 = CallByName Test.0 Test.14;
                let Test.13 = CallByName Num.16 Test.1 Test.2;
                let Test.12 = CallByName Num.16 Test.13 Test.3;
                let Test.11 = CallByName Num.16 Test.12 Test.4;
                ret Test.11;
                "#
            ),
        )
    }

    #[test]
    fn nested_pattern_match() {
        compiles_to_ir(
            indoc!(
                r#"
                Maybe a : [ Nothing, Just a ]

                x : Maybe (Maybe Int)
                x = Just (Just 41)

                when x is
                    Just (Just v) -> v + 0x1
                    _ -> 0x1
                "#
            ),
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.21 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.21;

                let Test.17 = 0i64;
                let Test.19 = 0i64;
                let Test.20 = 41i64;
                let Test.18 = Just Test.19 Test.20;
                let Test.1 = Just Test.17 Test.18;
                let Test.8 = true;
                let Test.10 = 0i64;
                let Test.9 = Index 1 Test.1;
                let Test.11 = Index 0 Test.9;
                let Test.16 = lowlevel Eq Test.10 Test.11;
                let Test.14 = lowlevel And Test.16 Test.8;
                let Test.12 = 0i64;
                let Test.13 = Index 0 Test.1;
                let Test.15 = lowlevel Eq Test.12 Test.13;
                let Test.7 = lowlevel And Test.15 Test.14;
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
}
