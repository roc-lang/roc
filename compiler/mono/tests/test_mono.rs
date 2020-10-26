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
    use roc_collections::all::MutMap;
    use roc_module::symbol::Symbol;
    use roc_mono::ir::Proc;
    use roc_mono::layout::Layout;

    fn promote_expr_to_module(src: &str) -> String {
        let mut buffer = String::from("app Test provides [ main ] imports []\n\nmain =\n");

        for line in src.lines() {
            // indent the body!
            buffer.push_str("    ");
            buffer.push_str(line);
            buffer.push('\n');
        }

        buffer
    }

    fn compiles_to_ir(src: &str, expected: &str) {
        use bumpalo::Bump;
        use std::path::{Path, PathBuf};

        let arena = &Bump::new();

        // let stdlib = roc_builtins::unique::uniq_stdlib();
        let stdlib = roc_builtins::std::standard_stdlib();
        let filename = PathBuf::from("Test.roc");
        let src_dir = Path::new("fake/test/path");

        let module_src;
        let temp;
        if src.starts_with("app") {
            // this is already a module
            module_src = src;
        } else {
            // this is an expression, promote it to a module
            temp = promote_expr_to_module(src);
            module_src = &temp;
        }

        let exposed_types = MutMap::default();
        let loaded = roc_load::file::load_and_monomorphize_from_str(
            arena,
            filename,
            &module_src,
            stdlib,
            src_dir,
            exposed_types,
        );

        let loaded = loaded.expect("failed to load module");

        use roc_load::file::MonomorphizedModule;
        let MonomorphizedModule {
            can_problems,
            type_problems,
            mono_problems,
            procedures,
            exposed_to_host,
            ..
        } = loaded;

        if !can_problems.is_empty() {
            println!("Ignoring {} canonicalization problems", can_problems.len());
        }

        assert!(type_problems.is_empty());
        assert!(mono_problems.is_empty());

        debug_assert_eq!(exposed_to_host.len(), 1);

        let main_fn_symbol = exposed_to_host.keys().copied().nth(0).unwrap();

        verify_procedures(expected, procedures, main_fn_symbol);
    }

    #[cfg(debug_assertions)]
    fn verify_procedures(
        expected: &str,
        procedures: MutMap<(Symbol, Layout<'_>), Proc<'_>>,
        main_fn_symbol: Symbol,
    ) {
        let index = procedures
            .keys()
            .position(|(s, _)| *s == main_fn_symbol)
            .unwrap();

        let mut procs_string = procedures
            .values()
            .map(|proc| proc.to_pretty(200))
            .collect::<Vec<_>>();

        let main_fn = procs_string.swap_remove(index);

        procs_string.sort();
        procs_string.push(main_fn);

        let result = procs_string.join("\n");

        let the_same = result == expected;

        if !the_same {
            println!("{}", result);

            let expected_lines = expected.split("\n").collect::<Vec<&str>>();
            let result_lines = result.split("\n").collect::<Vec<&str>>();

            assert_eq!(expected_lines, result_lines);
            assert_eq!(0, 1);
        }
    }

    // NOTE because the Show instance of module names is different in --release mode,
    // these tests would all fail. In the future, when we do interesting optimizations,
    // we'll likely want some tests for --release too.
    #[cfg(not(debug_assertions))]
    fn verify_procedures(
        _expected: &str,
        _procedures: MutMap<(Symbol, Layout<'_>), Proc<'_>>,
        _main_fn_symbol: Symbol,
    ) {
        // Do nothing
    }

    #[test]
    fn ir_int_literal() {
        compiles_to_ir(
            r#"
            5
            "#,
            indoc!(
                r#"
                procedure Test.0 ():
                    let Test.1 = 5i64;
                    ret Test.1;
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
                procedure Test.0 ():
                    let Test.1 = 5i64;
                    ret Test.1;
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
                procedure Test.0 ():
                    let Test.10 = 0i64;
                    let Test.11 = 3i64;
                    let Test.2 = Just Test.10 Test.11;
                    let Test.6 = true;
                    let Test.7 = 0i64;
                    let Test.8 = Index 0 Test.2;
                    let Test.9 = lowlevel Eq Test.7 Test.8;
                    let Test.5 = lowlevel And Test.9 Test.6;
                    if Test.5 then
                        let Test.1 = Index 1 Test.2;
                        ret Test.1;
                    else
                        let Test.4 = 0i64;
                        ret Test.4;
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
                procedure Test.0 ():
                    let Test.8 = 1i64;
                    let Test.9 = 1i64;
                    let Test.10 = 2i64;
                    let Test.4 = These Test.8 Test.9 Test.10;
                    switch Test.4:
                        case 2:
                            let Test.1 = Index 1 Test.4;
                            ret Test.1;
                    
                        case 0:
                            let Test.2 = Index 1 Test.4;
                            ret Test.2;
                    
                        default:
                            let Test.3 = Index 1 Test.4;
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
                procedure Test.0 ():
                    let Test.5 = 1i64;
                    let Test.6 = 3.14f64;
                    let Test.2 = Struct {Test.5, Test.6};
                    let Test.1 = Index 0 Test.2;
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
                    let Test.4 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.4;

                procedure Test.0 ():
                    let Test.2 = 1i64;
                    let Test.3 = 2i64;
                    let Test.1 = CallByName Num.14 Test.2 Test.3;
                    ret Test.1;
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
                    let Test.3 = lowlevel NumRound #Attr.2;
                    ret Test.3;

                procedure Test.0 ():
                    let Test.2 = 3.6f64;
                    let Test.1 = CallByName Num.36 Test.2;
                    ret Test.1;
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
                let Test.19 = 0i64;
                let Test.15 = lowlevel NotEq #Attr.3 Test.19;
                if Test.15 then
                    let Test.17 = 1i64;
                    let Test.18 = lowlevel NumDivUnchecked #Attr.2 #Attr.3;
                    let Test.16 = Ok Test.17 Test.18;
                    ret Test.16;
                else
                    let Test.13 = 0i64;
                    let Test.14 = Struct {};
                    let Test.12 = Err Test.13 Test.14;
                    ret Test.12;

            procedure Test.0 ():
                let Test.10 = 1000i64;
                let Test.11 = 10i64;
                let Test.2 = CallByName Num.32 Test.10 Test.11;
                let Test.6 = true;
                let Test.7 = 1i64;
                let Test.8 = Index 0 Test.2;
                let Test.9 = lowlevel Eq Test.7 Test.8;
                let Test.5 = lowlevel And Test.9 Test.6;
                if Test.5 then
                    let Test.1 = Index 1 Test.2;
                    ret Test.1;
                else
                    let Test.4 = -1i64;
                    ret Test.4;
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
                    let Test.4 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.4;

                procedure Test.0 ():
                    let Test.1 = 3i64;
                    let Test.2 = 4i64;
                    let Test.3 = CallByName Num.14 Test.1 Test.2;
                    ret Test.3;
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
                    let Test.5 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.5;

                procedure Test.0 ():
                    let Test.12 = 0i64;
                    let Test.13 = 41i64;
                    let Test.1 = Just Test.12 Test.13;
                    let Test.8 = true;
                    let Test.9 = 0i64;
                    let Test.10 = Index 0 Test.1;
                    let Test.11 = lowlevel Eq Test.9 Test.10;
                    let Test.7 = lowlevel And Test.11 Test.8;
                    if Test.7 then
                        let Test.2 = Index 1 Test.1;
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
    fn one_element_tag() {
        compiles_to_ir(
            r#"
            x : [ Pair Int ]
            x = Pair 2

            x
            "#,
            indoc!(
                r#"
                procedure Test.0 ():
                    let Test.3 = 2i64;
                    let Test.1 = Struct {Test.3};
                    ret Test.1;
                "#
            ),
        )
    }

    #[test]
    fn guard_pattern_true() {
        compiles_to_ir(
            r#"
            wrapper = \{} ->
                when 2 is
                    2 if False -> 42
                    _ -> 0

            wrapper {}
            "#,
            indoc!(
                r#"
                procedure Test.1 (Test.2):
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

                procedure Test.0 ():
                    let Test.4 = Struct {};
                    let Test.3 = CallByName Test.1 Test.4;
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

                procedure Test.0 ():
                    let Test.6 = 2i64;
                    let Test.2 = Struct {Test.6};
                    let Test.1 = Index 0 Test.2;
                    let Test.4 = 3i64;
                    let Test.3 = CallByName Num.14 Test.1 Test.4;
                    ret Test.3;
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
                    let Test.6 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.6;

                procedure Test.0 ():
                    let Test.19 = 0i64;
                    let Test.21 = 0i64;
                    let Test.22 = 41i64;
                    let Test.20 = Just Test.21 Test.22;
                    let Test.2 = Just Test.19 Test.20;
                    let Test.10 = true;
                    let Test.12 = 0i64;
                    let Test.11 = Index 1 Test.2;
                    let Test.13 = Index 0 Test.11;
                    let Test.18 = lowlevel Eq Test.12 Test.13;
                    let Test.16 = lowlevel And Test.18 Test.10;
                    let Test.14 = 0i64;
                    let Test.15 = Index 0 Test.2;
                    let Test.17 = lowlevel Eq Test.14 Test.15;
                    let Test.9 = lowlevel And Test.17 Test.16;
                    if Test.9 then
                        let Test.7 = Index 1 Test.2;
                        let Test.3 = Index 1 Test.7;
                        let Test.5 = 1i64;
                        let Test.4 = CallByName Num.14 Test.3 Test.5;
                        ret Test.4;
                    else
                        let Test.8 = 1i64;
                        ret Test.8;
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
                    let Test.6 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.6;

                procedure Test.0 ():
                    let Test.16 = 2i64;
                    let Test.17 = 3i64;
                    let Test.3 = Struct {Test.16, Test.17};
                    let Test.8 = true;
                    let Test.10 = 4i64;
                    let Test.9 = Index 0 Test.3;
                    let Test.15 = lowlevel Eq Test.10 Test.9;
                    let Test.13 = lowlevel And Test.15 Test.8;
                    let Test.12 = 3i64;
                    let Test.11 = Index 1 Test.3;
                    let Test.14 = lowlevel Eq Test.12 Test.11;
                    let Test.7 = lowlevel And Test.14 Test.13;
                    if Test.7 then
                        let Test.4 = 9i64;
                        ret Test.4;
                    else
                        let Test.1 = Index 0 Test.3;
                        let Test.2 = Index 1 Test.3;
                        let Test.5 = CallByName Num.14 Test.1 Test.2;
                        ret Test.5;
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
                    let Test.7 = lowlevel ListAppend #Attr.2 #Attr.3;
                    ret Test.7;

                procedure Test.1 (Test.2):
                    let Test.6 = 42i64;
                    let Test.5 = CallByName List.5 Test.2 Test.6;
                    ret Test.5;

                procedure Test.0 ():
                    let Test.8 = 1i64;
                    let Test.9 = 2i64;
                    let Test.4 = Array [Test.8, Test.9];
                    let Test.3 = CallByName Test.1 Test.4;
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

                procedure Test.0 ():
                    let Test.5 = 1i64;
                    let Test.2 = Array [Test.5];
                    let Test.3 = 2i64;
                    let Test.1 = CallByName List.5 Test.2 Test.3;
                    ret Test.1;
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
                    let Test.7 = lowlevel ListLen #Attr.2;
                    ret Test.7;

                procedure List.7 (#Attr.2):
                    let Test.8 = lowlevel ListLen #Attr.2;
                    ret Test.8;

                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.6 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.6;

                procedure Test.0 ():
                    let Test.10 = 1i64;
                    let Test.11 = 2i64;
                    let Test.12 = 3i64;
                    let Test.1 = Array [Test.10, Test.11, Test.12];
                    let Test.9 = 1f64;
                    let Test.2 = Array [Test.9];
                    let Test.4 = CallByName List.7 Test.1;
                    dec Test.1;
                    let Test.5 = CallByName List.7 Test.2;
                    dec Test.2;
                    let Test.3 = CallByName Num.14 Test.4 Test.5;
                    ret Test.3;
                "#
            ),
        )
    }

    #[test]
    fn when_joinpoint() {
        compiles_to_ir(
            r#"
            wrapper = \{} ->
                x : [ Red, White, Blue ]
                x = Blue

                y =
                    when x is
                        Red -> 1
                        White -> 2
                        Blue -> 3

                y

            wrapper {}
            "#,
            indoc!(
                r#"
                procedure Test.1 (Test.4):
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

                procedure Test.0 ():
                    let Test.6 = Struct {};
                    let Test.5 = CallByName Test.1 Test.6;
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
                procedure Test.0 ():
                    let Test.2 = true;
                    if Test.2 then
                        let Test.3 = 1i64;
                        ret Test.3;
                    else
                        let Test.1 = 2i64;
                        ret Test.1;
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
                procedure Test.0 ():
                    let Test.4 = true;
                    if Test.4 then
                        let Test.5 = 1i64;
                        ret Test.5;
                    else
                        let Test.2 = false;
                        if Test.2 then
                            let Test.3 = 2i64;
                            ret Test.3;
                        else
                            let Test.1 = 3i64;
                            ret Test.1;
                "#
            ),
        )
    }

    #[test]
    fn when_on_result() {
        compiles_to_ir(
            r#"
            wrapper = \{} ->
                x : Result Int Int
                x = Ok 2

                y =
                    when x is
                        Ok 3 -> 1
                        Ok _ -> 2
                        Err _ -> 3
                y

            wrapper {}
            "#,
            indoc!(
                r#"
                procedure Test.1 (Test.4):
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

                procedure Test.0 ():
                    let Test.6 = Struct {};
                    let Test.5 = CallByName Test.1 Test.6;
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
                procedure Test.0 ():
                    let Test.5 = 2i64;
                    let Test.6 = 3.14f64;
                    let Test.4 = Struct {Test.5, Test.6};
                    let Test.1 = Index 0 Test.4;
                    ret Test.1;
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
                procedure Test.0 ():
                    let Test.7 = 1i64;
                    let Test.8 = 3i64;
                    let Test.9 = 4i64;
                    let Test.5 = Array [Test.7, Test.8, Test.9];
                    let Test.6 = 3.14f64;
                    let Test.4 = Struct {Test.5, Test.6};
                    let Test.1 = Index 0 Test.4;
                    inc Test.1;
                    dec Test.4;
                    ret Test.1;
                "#
            ),
        )
    }

    #[test]
    fn if_guard_bind_variable_false() {
        compiles_to_ir(
            indoc!(
                r#"
                wrapper = \{} ->
                    when 10 is
                        x if x == 5 -> 0
                        _ -> 42

                wrapper {}
                "#
            ),
            indoc!(
                r#"
                procedure Bool.5 (#Attr.2, #Attr.3):
                    let Test.11 = lowlevel Eq #Attr.2 #Attr.3;
                    ret Test.11;

                procedure Test.1 (Test.3):
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

                procedure Test.0 ():
                    let Test.5 = Struct {};
                    let Test.4 = CallByName Test.1 Test.5;
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
                procedure Test.0 ():
                    let Test.1 = 5i64;
                    let Test.3 = 3i64;
                    ret Test.3;
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
                procedure Test.0 ():
                    let Test.1 = 5i64;
                    ret Test.1;
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
                procedure Test.0 ():
                    let Test.2 = 0i64;
                    let Test.6 = true;
                    let Test.7 = 1i64;
                    let Test.8 = lowlevel Eq Test.7 Test.2;
                    let Test.5 = lowlevel And Test.8 Test.6;
                    if Test.5 then
                        let Test.3 = 12i64;
                        ret Test.3;
                    else
                        ret Test.2;
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
                    let Test.11 = lowlevel ListLen #Attr.2;
                    let Test.9 = lowlevel NumLt #Attr.3 Test.11;
                    if Test.9 then
                        let Test.10 = lowlevel ListSet #Attr.2 #Attr.3 #Attr.4;
                        ret Test.10;
                    else
                        ret #Attr.2;

                procedure Test.2 (Test.3):
                    let Test.6 = 0i64;
                    let Test.7 = 0i64;
                    let Test.5 = CallByName List.4 Test.3 Test.6 Test.7;
                    ret Test.5;

                procedure Test.0 ():
                    let Test.12 = 1i64;
                    let Test.13 = 2i64;
                    let Test.14 = 3i64;
                    let Test.1 = Array [Test.12, Test.13, Test.14];
                    let Test.4 = CallByName Test.2 Test.1;
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
                    let Test.8 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.8;

                procedure Test.1 (Test.2):
                    let Test.3 = Index 0 Test.2;
                    let Test.4 = Index 1 Test.2;
                    let Test.7 = CallByName Num.14 Test.3 Test.4;
                    ret Test.7;

                procedure Test.0 ():
                    let Test.9 = 4i64;
                    let Test.10 = 9i64;
                    let Test.6 = Struct {Test.9, Test.10};
                    let Test.5 = CallByName Test.1 Test.6;
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
                    let Test.8 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.8;

                procedure Test.1 (Test.2):
                    let Test.3 = 10i64;
                    let Test.4 = Index 1 Test.2;
                    let Test.7 = CallByName Num.14 Test.3 Test.4;
                    ret Test.7;

                procedure Test.0 ():
                    let Test.9 = 9i64;
                    let Test.6 = Struct {Test.9};
                    let Test.5 = CallByName Test.1 Test.6;
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
                    let Test.8 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.8;

                procedure Test.1 (Test.4):
                    let Test.2 = Index 0 Test.4;
                    let Test.3 = Index 1 Test.4;
                    let Test.7 = CallByName Num.14 Test.2 Test.3;
                    ret Test.7;

                procedure Test.0 ():
                    let Test.9 = 4i64;
                    let Test.10 = 9i64;
                    let Test.6 = Struct {Test.9, Test.10};
                    let Test.5 = CallByName Test.1 Test.6;
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
                    let Test.8 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.8;

                procedure Test.1 (Test.4):
                    let Test.2 = 10i64;
                    let Test.3 = Index 1 Test.4;
                    let Test.7 = CallByName Num.14 Test.2 Test.3;
                    ret Test.7;

                procedure Test.0 ():
                    let Test.9 = 9i64;
                    let Test.6 = Struct {Test.9};
                    let Test.5 = CallByName Test.1 Test.6;
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
                    app Test provides [ main ] imports []

                    swap = \list ->
                        when Pair (List.get list 0) (List.get list 0) is
                            Pair (Ok atI) (Ok atJ) ->
                                list 
                                    |> List.set 0 atJ
                                    |> List.set 0 atI

                            _ ->
                                []

                    main =
                        swap [ 1, 2 ]
                "#
                ),
                indoc!(
                    r#"
                    procedure List.3 (#Attr.2, #Attr.3):
                        let Test.40 = lowlevel ListLen #Attr.2;
                        let Test.36 = lowlevel NumLt #Attr.3 Test.40;
                        if Test.36 then
                            let Test.38 = 1i64;
                            let Test.39 = lowlevel ListGetUnsafe #Attr.2 #Attr.3;
                            let Test.37 = Ok Test.38 Test.39;
                            ret Test.37;
                        else
                            let Test.34 = 0i64;
                            let Test.35 = Struct {};
                            let Test.33 = Err Test.34 Test.35;
                            ret Test.33;

                    procedure List.4 (#Attr.2, #Attr.3, #Attr.4):
                        let Test.14 = lowlevel ListLen #Attr.2;
                        let Test.12 = lowlevel NumLt #Attr.3 Test.14;
                        if Test.12 then
                            let Test.13 = lowlevel ListSet #Attr.2 #Attr.3 #Attr.4;
                            ret Test.13;
                        else
                            ret #Attr.2;

                    procedure Test.1 (Test.2):
                        let Test.41 = 0i64;
                        let Test.30 = CallByName List.3 Test.2 Test.41;
                        let Test.32 = 0i64;
                        let Test.31 = CallByName List.3 Test.2 Test.32;
                        let Test.7 = Struct {Test.30, Test.31};
                        let Test.20 = true;
                        let Test.22 = 1i64;
                        let Test.21 = Index 0 Test.7;
                        let Test.23 = Index 0 Test.21;
                        let Test.29 = lowlevel Eq Test.22 Test.23;
                        let Test.27 = lowlevel And Test.29 Test.20;
                        let Test.25 = 1i64;
                        let Test.24 = Index 1 Test.7;
                        let Test.26 = Index 0 Test.24;
                        let Test.28 = lowlevel Eq Test.25 Test.26;
                        let Test.19 = lowlevel And Test.28 Test.27;
                        if Test.19 then
                            let Test.17 = Index 0 Test.7;
                            let Test.3 = Index 1 Test.17;
                            let Test.16 = Index 1 Test.7;
                            let Test.4 = Index 1 Test.16;
                            let Test.15 = 0i64;
                            let Test.9 = CallByName List.4 Test.2 Test.15 Test.4;
                            let Test.10 = 0i64;
                            let Test.8 = CallByName List.4 Test.9 Test.10 Test.3;
                            ret Test.8;
                        else
                            dec Test.2;
                            let Test.18 = Array [];
                            ret Test.18;

                    procedure Test.0 ():
                        let Test.42 = 1i64;
                        let Test.43 = 2i64;
                        let Test.6 = Array [Test.42, Test.43];
                        let Test.5 = CallByName Test.1 Test.6;
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
                    app Test provides [ main ] imports []

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

                    main =
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
                    app Test provides [ main ] imports []

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



                    quicksort = \originalList ->
                        n = List.len originalList
                        quicksortHelp originalList 0 (n - 1)

                    main =
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

                procedure Test.1 (Test.2, Test.3):
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

                procedure Test.0 ():
                    let Test.5 = 10i64;
                    let Test.6 = 1i64;
                    let Test.4 = CallByName Test.1 Test.5 Test.6;
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
            indoc!(
                r#"
                app Test provides [ main ] imports []

                mkPairOf = \x -> Pair x x

                main =
                    mkPairOf [1,2,3]
                "#
            ),
            indoc!(
                r#"
                procedure Test.1 (Test.2):
                    inc Test.2;
                    let Test.5 = Struct {Test.2, Test.2};
                    ret Test.5;

                procedure Test.0 ():
                    let Test.6 = 1i64;
                    let Test.7 = 2i64;
                    let Test.8 = 3i64;
                    let Test.4 = Array [Test.6, Test.7, Test.8];
                    let Test.3 = CallByName Test.1 Test.4;
                    ret Test.3;
                "#
            ),
        )
    }

    #[test]
    fn fst() {
        compiles_to_ir(
            indoc!(
                r#"
                app Test provides [ main ] imports []

                fst = \x, _ -> x

                main =
                    fst [1,2,3] [3,2,1]
                "#
            ),
            indoc!(
                r#"
                procedure Test.1 (Test.2, Test.3):
                    inc Test.2;
                    ret Test.2;

                procedure Test.0 ():
                    let Test.11 = 1i64;
                    let Test.12 = 2i64;
                    let Test.13 = 3i64;
                    let Test.5 = Array [Test.11, Test.12, Test.13];
                    let Test.8 = 3i64;
                    let Test.9 = 2i64;
                    let Test.10 = 1i64;
                    let Test.6 = Array [Test.8, Test.9, Test.10];
                    let Test.4 = CallByName Test.1 Test.5 Test.6;
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
                app Test provides [ main ] imports []

                x : List Int
                x = [1,2,3]

                add : List Int -> List Int
                add = \y -> List.set y 0 0

                main =
                    List.len (add x) + List.len x
                "#
            ),
            indoc!(
                r#"
                procedure List.4 (#Attr.2, #Attr.3, #Attr.4):
                    let Test.22 = lowlevel ListLen #Attr.2;
                    let Test.20 = lowlevel NumLt #Attr.3 Test.22;
                    if Test.20 then
                        let Test.21 = lowlevel ListSet #Attr.2 #Attr.3 #Attr.4;
                        ret Test.21;
                    else
                        ret #Attr.2;

                procedure List.7 (#Attr.2):
                    let Test.9 = lowlevel ListLen #Attr.2;
                    ret Test.9;

                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.7 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.7;

                procedure Test.1 ():
                    let Test.11 = 1i64;
                    let Test.12 = 2i64;
                    let Test.13 = 3i64;
                    let Test.10 = Array [Test.11, Test.12, Test.13];
                    ret Test.10;

                procedure Test.2 (Test.3):
                    let Test.17 = 0i64;
                    let Test.18 = 0i64;
                    let Test.16 = CallByName List.4 Test.3 Test.17 Test.18;
                    ret Test.16;

                procedure Test.0 ():
                    let Test.15 = FunctionPointer Test.1;
                    let Test.14 = CallByName Test.2 Test.15;
                    let Test.5 = CallByName List.7 Test.14;
                    dec Test.14;
                    let Test.8 = FunctionPointer Test.1;
                    let Test.6 = CallByName List.7 Test.8;
                    dec Test.8;
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
                wrapper = \{} ->
                    List.get [1,2,3] 0

                wrapper {}
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

                procedure Test.1 (Test.2):
                    let Test.16 = 1i64;
                    let Test.17 = 2i64;
                    let Test.18 = 3i64;
                    let Test.6 = Array [Test.16, Test.17, Test.18];
                    let Test.7 = 0i64;
                    let Test.5 = CallByName List.3 Test.6 Test.7;
                    dec Test.6;
                    ret Test.5;

                procedure Test.0 ():
                    let Test.4 = Struct {};
                    let Test.3 = CallByName Test.1 Test.4;
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
                procedure Test.0 ():
                    let Test.4 = 0i64;
                    let Test.6 = 0i64;
                    let Test.8 = 0i64;
                    let Test.10 = 1i64;
                    let Test.9 = Z Test.10;
                    let Test.7 = S Test.8 Test.9;
                    let Test.5 = S Test.6 Test.7;
                    let Test.2 = S Test.4 Test.5;
                    ret Test.2;
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
                procedure Test.0 ():
                    let Test.10 = 0i64;
                    let Test.12 = 0i64;
                    let Test.14 = 0i64;
                    let Test.16 = 1i64;
                    let Test.15 = Z Test.16;
                    let Test.13 = S Test.14 Test.15;
                    let Test.11 = S Test.12 Test.13;
                    let Test.2 = S Test.10 Test.11;
                    let Test.6 = true;
                    let Test.7 = 1i64;
                    let Test.8 = Index 0 Test.2;
                    let Test.9 = lowlevel Eq Test.7 Test.8;
                    let Test.5 = lowlevel And Test.9 Test.6;
                    if Test.5 then
                        let Test.3 = 0i64;
                        ret Test.3;
                    else
                        let Test.4 = 1i64;
                        ret Test.4;
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
                procedure Test.0 ():
                    let Test.18 = 0i64;
                    let Test.20 = 0i64;
                    let Test.22 = 0i64;
                    let Test.24 = 1i64;
                    let Test.23 = Z Test.24;
                    let Test.21 = S Test.22 Test.23;
                    let Test.19 = S Test.20 Test.21;
                    let Test.2 = S Test.18 Test.19;
                    let Test.14 = true;
                    let Test.15 = 0i64;
                    let Test.16 = Index 0 Test.2;
                    let Test.17 = lowlevel Eq Test.15 Test.16;
                    let Test.13 = lowlevel And Test.17 Test.14;
                    if Test.13 then
                        let Test.8 = true;
                        let Test.10 = 0i64;
                        let Test.9 = Index 1 Test.2;
                        let Test.11 = Index 0 Test.9;
                        let Test.12 = lowlevel Eq Test.10 Test.11;
                        let Test.7 = lowlevel And Test.12 Test.8;
                        if Test.7 then
                            let Test.4 = Index 1 Test.2;
                            let Test.3 = 1i64;
                            ret Test.3;
                        else
                            let Test.5 = 0i64;
                            ret Test.5;
                    else
                        let Test.6 = 0i64;
                        ret Test.6;
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
                    let Test.13 = lowlevel NumMul #Attr.2 #Attr.3;
                    ret Test.13;

                procedure Test.1 (Test.6):
                    let Test.19 = true;
                    let Test.21 = false;
                    let Test.20 = Index 0 Test.6;
                    let Test.22 = lowlevel Eq Test.21 Test.20;
                    let Test.18 = lowlevel And Test.22 Test.19;
                    if Test.18 then
                        let Test.8 = Index 1 Test.6;
                        ret Test.8;
                    else
                        let Test.10 = Index 1 Test.6;
                        ret Test.10;

                procedure Test.1 (Test.6):
                    let Test.32 = true;
                    let Test.34 = false;
                    let Test.33 = Index 0 Test.6;
                    let Test.35 = lowlevel Eq Test.34 Test.33;
                    let Test.31 = lowlevel And Test.35 Test.32;
                    if Test.31 then
                        let Test.8 = 3i64;
                        ret Test.8;
                    else
                        let Test.10 = 5i64;
                        ret Test.10;

                procedure Test.0 ():
                    let Test.38 = true;
                    let Test.37 = Struct {Test.38};
                    let Test.5 = CallByName Test.1 Test.37;
                    let Test.36 = false;
                    let Test.28 = Struct {Test.36};
                    let Test.3 = CallByName Test.1 Test.28;
                    let Test.26 = true;
                    let Test.27 = 11i64;
                    let Test.25 = Struct {Test.26, Test.27};
                    let Test.4 = CallByName Test.1 Test.25;
                    let Test.23 = false;
                    let Test.24 = 7i64;
                    let Test.15 = Struct {Test.23, Test.24};
                    let Test.2 = CallByName Test.1 Test.15;
                    let Test.14 = CallByName Num.16 Test.2 Test.3;
                    let Test.12 = CallByName Num.16 Test.14 Test.4;
                    let Test.11 = CallByName Num.16 Test.12 Test.5;
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
                    let Test.6 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.6;

                procedure Test.0 ():
                    let Test.19 = 0i64;
                    let Test.21 = 0i64;
                    let Test.22 = 41i64;
                    let Test.20 = Just Test.21 Test.22;
                    let Test.2 = Just Test.19 Test.20;
                    let Test.10 = true;
                    let Test.12 = 0i64;
                    let Test.11 = Index 1 Test.2;
                    let Test.13 = Index 0 Test.11;
                    let Test.18 = lowlevel Eq Test.12 Test.13;
                    let Test.16 = lowlevel And Test.18 Test.10;
                    let Test.14 = 0i64;
                    let Test.15 = Index 0 Test.2;
                    let Test.17 = lowlevel Eq Test.14 Test.15;
                    let Test.9 = lowlevel And Test.17 Test.16;
                    if Test.9 then
                        let Test.7 = Index 1 Test.2;
                        let Test.3 = Index 1 Test.7;
                        let Test.5 = 1i64;
                        let Test.4 = CallByName Num.14 Test.3 Test.5;
                        ret Test.4;
                    else
                        let Test.8 = 1i64;
                        ret Test.8;
                "#
            ),
        )
    }

    #[test]
    fn linked_list_length_twice() {
        compiles_to_ir(
            indoc!(
                r#"
                LinkedList a : [ Nil, Cons a (LinkedList a) ]

                nil : LinkedList Int
                nil = Nil

                length : LinkedList a -> Int
                length = \list ->
                    when list is
                        Nil -> 0
                        Cons _ rest -> 1 + length rest

                length nil + length nil
                "#
            ),
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.9 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.9;

                procedure Test.3 (Test.4):
                    let Test.15 = true;
                    let Test.16 = 1i64;
                    let Test.17 = Index 0 Test.4;
                    let Test.18 = lowlevel Eq Test.16 Test.17;
                    let Test.14 = lowlevel And Test.18 Test.15;
                    if Test.14 then
                        dec Test.4;
                        let Test.10 = 0i64;
                        ret Test.10;
                    else
                        let Test.5 = Index 2 Test.4;
                        dec Test.4;
                        let Test.12 = 1i64;
                        let Test.13 = CallByName Test.3 Test.5;
                        let Test.11 = CallByName Num.14 Test.12 Test.13;
                        ret Test.11;

                procedure Test.0 ():
                    let Test.20 = 1i64;
                    let Test.2 = Nil Test.20;
                    let Test.7 = CallByName Test.3 Test.2;
                    let Test.8 = CallByName Test.3 Test.2;
                    let Test.6 = CallByName Num.14 Test.7 Test.8;
                    ret Test.6;
                "#
            ),
        )
    }

    #[test]
    fn rigids() {
        compiles_to_ir(
            indoc!(
                r#"
                app Test provides [ main ] imports []

                swap : Int, Int, List a -> List a
                swap = \i, j, list ->
                    when Pair (List.get list i) (List.get list j) is
                        Pair (Ok atI) (Ok atJ) ->
                            foo = atJ

                            list
                                |> List.set i foo
                                |> List.set j atI

                        _ ->
                            []

                main =
                    swap 0 0 [0x1]
                "#
            ),
            indoc!(
                r#"
                procedure List.3 (#Attr.2, #Attr.3):
                    let Test.42 = lowlevel ListLen #Attr.2;
                    let Test.38 = lowlevel NumLt #Attr.3 Test.42;
                    if Test.38 then
                        let Test.40 = 1i64;
                        let Test.41 = lowlevel ListGetUnsafe #Attr.2 #Attr.3;
                        let Test.39 = Ok Test.40 Test.41;
                        ret Test.39;
                    else
                        let Test.36 = 0i64;
                        let Test.37 = Struct {};
                        let Test.35 = Err Test.36 Test.37;
                        ret Test.35;

                procedure List.4 (#Attr.2, #Attr.3, #Attr.4):
                    let Test.18 = lowlevel ListLen #Attr.2;
                    let Test.16 = lowlevel NumLt #Attr.3 Test.18;
                    if Test.16 then
                        let Test.17 = lowlevel ListSet #Attr.2 #Attr.3 #Attr.4;
                        ret Test.17;
                    else
                        ret #Attr.2;

                procedure Test.1 (Test.2, Test.3, Test.4):
                    let Test.33 = CallByName List.3 Test.4 Test.2;
                    let Test.34 = CallByName List.3 Test.4 Test.3;
                    let Test.12 = Struct {Test.33, Test.34};
                    let Test.23 = true;
                    let Test.25 = 1i64;
                    let Test.24 = Index 0 Test.12;
                    let Test.26 = Index 0 Test.24;
                    let Test.32 = lowlevel Eq Test.25 Test.26;
                    let Test.30 = lowlevel And Test.32 Test.23;
                    let Test.28 = 1i64;
                    let Test.27 = Index 1 Test.12;
                    let Test.29 = Index 0 Test.27;
                    let Test.31 = lowlevel Eq Test.28 Test.29;
                    let Test.22 = lowlevel And Test.31 Test.30;
                    if Test.22 then
                        let Test.20 = Index 0 Test.12;
                        let Test.5 = Index 1 Test.20;
                        let Test.19 = Index 1 Test.12;
                        let Test.6 = Index 1 Test.19;
                        let Test.14 = CallByName List.4 Test.4 Test.2 Test.6;
                        let Test.13 = CallByName List.4 Test.14 Test.3 Test.5;
                        ret Test.13;
                    else
                        dec Test.4;
                        let Test.21 = Array [];
                        ret Test.21;

                procedure Test.0 ():
                    let Test.9 = 0i64;
                    let Test.10 = 0i64;
                    let Test.43 = 1i64;
                    let Test.11 = Array [Test.43];
                    let Test.8 = CallByName Test.1 Test.9 Test.10 Test.11;
                    ret Test.8;
                "#
            ),
        )
    }

    #[test]
    fn let_x_in_x() {
        compiles_to_ir(
            indoc!(
                r#"
                x = 5

                answer =
                    1337

                unused =
                    nested = 17
                    nested

                answer
                "#
            ),
            indoc!(
                r#"
                procedure Test.0 ():
                    let Test.1 = 5i64;
                    let Test.4 = 17i64;
                    let Test.2 = 1337i64;
                    ret Test.2;
                "#
            ),
        )
    }

    #[test]
    fn let_x_in_x_indirect() {
        compiles_to_ir(
            indoc!(
                r#"
                x = 5

                answer =
                    1337

                unused =
                    nested = 17

                    i = 1

                    nested

                { answer, unused }.answer
                "#
            ),
            indoc!(
                r#"
                procedure Test.0 ():
                    let Test.1 = 5i64;
                    let Test.4 = 17i64;
                    let Test.5 = 1i64;
                    let Test.2 = 1337i64;
                    let Test.7 = Struct {Test.2, Test.4};
                    let Test.6 = Index 0 Test.7;
                    ret Test.6;
                "#
            ),
        )
    }

    #[test]
    fn nested_closure() {
        compiles_to_ir(
            indoc!(
                r#"
                app Test provides [ main ] imports []

                foo = \{} ->
                    x = 42
                    f = \{} -> x
                    f

                main = 
                    f = foo {}
                    f {}
                "#
            ),
            indoc!(
                r#"
                procedure Test.1 (Test.5):
                    let Test.2 = 42i64;
                    let Test.13 = FunctionPointer Test.3;
                    let Test.3 = Struct {Test.13, Test.2};
                    ret Test.3;

                procedure Test.3 (Test.11, #Attr.12):
                    let Test.2 = Index 0 #Attr.12;
                    ret Test.2;

                procedure Test.0 ():
                    let Test.10 = Struct {};
                    let Test.4 = CallByName Test.1 Test.10;
                    let Test.7 = Struct {};
                    let Test.8 = Index 1 Test.4;
                    let Test.9 = Index 0 Test.4;
                    let Test.6 = CallByPointer Test.9 Test.7 Test.8;
                    ret Test.6;
                "#
            ),
        )
    }

    #[test]
    fn closure_in_list() {
        compiles_to_ir(
            indoc!(
                r#"
                app Test provides [ main ] imports []

                foo = \{} ->
                    x = 41

                    f = \{} -> x

                    [ f ]

                main =
                    items = foo {}

                    List.len items
                "#
            ),
            indoc!(
                r#"
                procedure List.7 (#Attr.2):
                    let Test.7 = lowlevel ListLen #Attr.2;
                    ret Test.7;

                procedure Test.1 (Test.5):
                    let Test.2 = 41i64;
                    let Test.12 = FunctionPointer Test.3;
                    let Test.11 = Struct {Test.12, Test.2};
                    let Test.10 = Array [Test.11];
                    ret Test.10;

                procedure Test.3 (Test.9, #Attr.12):
                    let Test.2 = Index 0 #Attr.12;
                    ret Test.2;

                procedure Test.0 ():
                    let Test.8 = Struct {};
                    let Test.4 = CallByName Test.1 Test.8;
                    let Test.6 = CallByName List.7 Test.4;
                    dec Test.4;
                    ret Test.6;
                "#
            ),
        )
    }
}
