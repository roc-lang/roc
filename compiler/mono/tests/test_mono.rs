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
        let mut buffer =
            String::from("app \"test\" provides [ main ] to \"./platform\"\n\nmain =\n");

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

        let mut loaded = loaded.expect("failed to load module");

        use roc_load::file::MonomorphizedModule;
        let MonomorphizedModule {
            module_id: home,
            procedures,
            exposed_to_host,
            ..
        } = loaded;

        let can_problems = loaded.can_problems.remove(&home).unwrap_or_default();
        let type_problems = loaded.type_problems.remove(&home).unwrap_or_default();
        let mono_problems = loaded.mono_problems.remove(&home).unwrap_or_default();

        if !can_problems.is_empty() {
            println!("Ignoring {} canonicalization problems", can_problems.len());
        }

        assert_eq!(type_problems, Vec::new());
        assert_eq!(mono_problems, Vec::new());

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
            let expected_lines = expected.split("\n").collect::<Vec<&str>>();
            let result_lines = result.split("\n").collect::<Vec<&str>>();

            for line in &result_lines {
                if !line.is_empty() {
                    println!("                {}", line);
                } else {
                    println!("");
                }
            }

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
                procedure .0 ():
                    let .1 = 5i64;
                    ret .1;
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
                procedure .0 ():
                    let .1 = 5i64;
                    ret .1;
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
                procedure .0 ():
                    let .8 = 0i64;
                    let .9 = 3i64;
                    let .2 = Just .8 .9;
                    let .5 = 0i64;
                    let .6 = Index 0 .2;
                    let .7 = lowlevel Eq .5 .6;
                    if .7 then
                        let .1 = Index 1 .2;
                        ret .1;
                    else
                        let .4 = 0i64;
                        ret .4;
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
                procedure .0 ():
                    let .8 = 1i64;
                    let .9 = 1i64;
                    let .10 = 2i64;
                    let .4 = These .8 .9 .10;
                    switch .4:
                        case 2:
                            let .1 = Index 1 .4;
                            ret .1;
                    
                        case 0:
                            let .2 = Index 1 .4;
                            ret .2;
                    
                        default:
                            let .3 = Index 1 .4;
                            ret .3;
                    
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
                procedure .0 ():
                    let .5 = 1i64;
                    let .6 = 3.14f64;
                    let .2 = Struct {.5, .6};
                    let .1 = Index 0 .2;
                    ret .1;
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
                    let .4 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret .4;

                procedure .0 ():
                    let .2 = 1i64;
                    let .3 = 2i64;
                    let .1 = CallByName Num.14 .2 .3;
                    ret .1;
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
                    let .3 = lowlevel NumRound #Attr.2;
                    ret .3;

                procedure .0 ():
                    let .2 = 3.6f64;
                    let .1 = CallByName Num.36 .2;
                    ret .1;
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
                    let .17 = 0i64;
                    let .13 = lowlevel NotEq #Attr.3 .17;
                    if .13 then
                        let .15 = 1i64;
                        let .16 = lowlevel NumDivUnchecked #Attr.2 #Attr.3;
                        let .14 = Ok .15 .16;
                        ret .14;
                    else
                        let .11 = 0i64;
                        let .12 = Struct {};
                        let .10 = Err .11 .12;
                        ret .10;

                procedure .0 ():
                    let .8 = 1000i64;
                    let .9 = 10i64;
                    let .2 = CallByName Num.32 .8 .9;
                    let .5 = 1i64;
                    let .6 = Index 0 .2;
                    let .7 = lowlevel Eq .5 .6;
                    if .7 then
                        let .1 = Index 1 .2;
                        ret .1;
                    else
                        let .4 = -1i64;
                        ret .4;
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
                    let .4 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret .4;

                procedure .0 ():
                    let .1 = 3i64;
                    let .2 = 4i64;
                    let .3 = CallByName Num.14 .1 .2;
                    ret .3;
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
                    let .5 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret .5;

                procedure .0 ():
                    let .10 = 0i64;
                    let .11 = 41i64;
                    let .1 = Just .10 .11;
                    let .7 = 0i64;
                    let .8 = Index 0 .1;
                    let .9 = lowlevel Eq .7 .8;
                    if .9 then
                        let .2 = Index 1 .1;
                        let .4 = 1i64;
                        let .3 = CallByName Num.14 .2 .4;
                        ret .3;
                    else
                        let .6 = 1i64;
                        ret .6;
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
                procedure .0 ():
                    let .3 = 2i64;
                    let .1 = Struct {.3};
                    ret .1;
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
                procedure .1 (.2):
                    let .5 = 2i64;
                    joinpoint .11:
                        let .9 = 0i64;
                        ret .9;
                    in
                    let .10 = 2i64;
                    let .13 = lowlevel Eq .10 .5;
                    if .13 then
                        joinpoint .7 .12:
                            if .12 then
                                let .6 = 42i64;
                                ret .6;
                            else
                                jump .11;
                        in
                        let .8 = false;
                        jump .7 .8;
                    else
                        jump .11;

                procedure .0 ():
                    let .4 = Struct {};
                    let .3 = CallByName .1 .4;
                    ret .3;
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
                    let .5 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret .5;

                procedure .0 ():
                    let .6 = 2i64;
                    let .2 = Struct {.6};
                    let .1 = Index 0 .2;
                    let .4 = 3i64;
                    let .3 = CallByName Num.14 .1 .4;
                    ret .3;
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
                    let .6 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret .6;

                procedure .0 ():
                    let .17 = 0i64;
                    let .19 = 0i64;
                    let .20 = 41i64;
                    let .18 = Just .19 .20;
                    let .2 = Just .17 .18;
                    joinpoint .14:
                        let .8 = 1i64;
                        ret .8;
                    in
                    let .12 = 0i64;
                    let .13 = Index 0 .2;
                    let .16 = lowlevel Eq .12 .13;
                    if .16 then
                        let .9 = Index 1 .2;
                        let .10 = 0i64;
                        let .11 = Index 0 .9;
                        let .15 = lowlevel Eq .10 .11;
                        if .15 then
                            let .7 = Index 1 .2;
                            let .3 = Index 1 .7;
                            let .5 = 1i64;
                            let .4 = CallByName Num.14 .3 .5;
                            ret .4;
                        else
                            jump .14;
                    else
                        jump .14;
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
                    let .6 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret .6;

                procedure .0 ():
                    let .14 = 2i64;
                    let .15 = 3i64;
                    let .3 = Struct {.14, .15};
                    joinpoint .11:
                        let .1 = Index 0 .3;
                        let .2 = Index 1 .3;
                        let .5 = CallByName Num.14 .1 .2;
                        ret .5;
                    in
                    let .9 = Index 1 .3;
                    let .10 = 3i64;
                    let .13 = lowlevel Eq .10 .9;
                    if .13 then
                        let .7 = Index 0 .3;
                        let .8 = 4i64;
                        let .12 = lowlevel Eq .8 .7;
                        if .12 then
                            let .4 = 9i64;
                            ret .4;
                        else
                            jump .11;
                    else
                        jump .11;
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
                procedure .1 (.2):
                    let .6 = 42i64;
                    let .5 = CallByName List.5 .2 .6;
                    ret .5;

                procedure List.5 (#Attr.2, #Attr.3):
                    let .7 = lowlevel ListAppend #Attr.2 #Attr.3;
                    ret .7;

                procedure .0 ():
                    let .8 = 1i64;
                    let .9 = 2i64;
                    let .4 = Array [.8, .9];
                    let .3 = CallByName .1 .4;
                    ret .3;
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
                    let .4 = lowlevel ListAppend #Attr.2 #Attr.3;
                    ret .4;

                procedure .0 ():
                    let .5 = 1i64;
                    let .2 = Array [.5];
                    let .3 = 2i64;
                    let .1 = CallByName List.5 .2 .3;
                    ret .1;
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
                    let .7 = lowlevel ListLen #Attr.2;
                    ret .7;

                procedure List.7 (#Attr.2):
                    let .8 = lowlevel ListLen #Attr.2;
                    ret .8;

                procedure Num.14 (#Attr.2, #Attr.3):
                    let .6 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret .6;

                procedure .0 ():
                    let .10 = 1i64;
                    let .11 = 2i64;
                    let .12 = 3i64;
                    let .1 = Array [.10, .11, .12];
                    let .9 = 1f64;
                    let .2 = Array [.9];
                    let .4 = CallByName List.7 .1;
                    dec .1;
                    let .5 = CallByName List.7 .2;
                    dec .2;
                    let .3 = CallByName Num.14 .4 .5;
                    ret .3;
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
                procedure .1 (.4):
                    let .2 = 0u8;
                    joinpoint .8 .3:
                        ret .3;
                    in
                    switch .2:
                        case 1:
                            let .9 = 1i64;
                            jump .8 .9;
                    
                        case 2:
                            let .10 = 2i64;
                            jump .8 .10;
                    
                        default:
                            let .11 = 3i64;
                            jump .8 .11;
                    

                procedure .0 ():
                    let .6 = Struct {};
                    let .5 = CallByName .1 .6;
                    ret .5;
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
                procedure .0 ():
                    let .2 = true;
                    if .2 then
                        let .3 = 1i64;
                        ret .3;
                    else
                        let .1 = 2i64;
                        ret .1;
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
                procedure .0 ():
                    let .4 = true;
                    if .4 then
                        let .5 = 1i64;
                        ret .5;
                    else
                        let .2 = false;
                        if .2 then
                            let .3 = 2i64;
                            ret .3;
                        else
                            let .1 = 3i64;
                            ret .1;
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
                procedure .1 (.4):
                    let .18 = 1i64;
                    let .19 = 2i64;
                    let .2 = Ok .18 .19;
                    joinpoint .8 .3:
                        ret .3;
                    in
                    let .15 = 1i64;
                    let .16 = Index 0 .2;
                    let .17 = lowlevel Eq .15 .16;
                    if .17 then
                        let .12 = Index 1 .2;
                        let .13 = 3i64;
                        let .14 = lowlevel Eq .13 .12;
                        if .14 then
                            let .9 = 1i64;
                            jump .8 .9;
                        else
                            let .10 = 2i64;
                            jump .8 .10;
                    else
                        let .11 = 3i64;
                        jump .8 .11;

                procedure .0 ():
                    let .6 = Struct {};
                    let .5 = CallByName .1 .6;
                    ret .5;
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
                procedure .0 ():
                    let .5 = 2i64;
                    let .6 = 3.14f64;
                    let .4 = Struct {.5, .6};
                    let .1 = Index 0 .4;
                    ret .1;
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
                procedure .0 ():
                    let .7 = 1i64;
                    let .8 = 3i64;
                    let .9 = 4i64;
                    let .5 = Array [.7, .8, .9];
                    let .6 = 3.14f64;
                    let .4 = Struct {.5, .6};
                    let .1 = Index 0 .4;
                    inc .1;
                    dec .4;
                    ret .1;
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
                procedure .1 (.3):
                    let .6 = 10i64;
                    joinpoint .8 .13:
                        if .13 then
                            let .7 = 0i64;
                            ret .7;
                        else
                            let .12 = 42i64;
                            ret .12;
                    in
                    let .10 = 5i64;
                    let .9 = CallByName Bool.5 .6 .10;
                    jump .8 .9;

                procedure Bool.5 (#Attr.2, #Attr.3):
                    let .11 = lowlevel Eq #Attr.2 #Attr.3;
                    ret .11;

                procedure .0 ():
                    let .5 = Struct {};
                    let .4 = CallByName .1 .5;
                    ret .4;
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
                procedure .0 ():
                    let .1 = 5i64;
                    let .3 = 3i64;
                    ret .3;
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
                procedure .0 ():
                    let .1 = 5i64;
                    ret .1;
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
                procedure .0 ():
                    let .2 = 0i64;
                    let .5 = 1i64;
                    let .6 = lowlevel Eq .5 .2;
                    if .6 then
                        let .3 = 12i64;
                        ret .3;
                    else
                        ret .2;
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
                procedure .2 (.3):
                    let .6 = 0i64;
                    let .7 = 0i64;
                    let .5 = CallByName List.4 .3 .6 .7;
                    ret .5;

                procedure List.4 (#Attr.2, #Attr.3, #Attr.4):
                    let .11 = lowlevel ListLen #Attr.2;
                    let .9 = lowlevel NumLt #Attr.3 .11;
                    if .9 then
                        let .10 = lowlevel ListSet #Attr.2 #Attr.3 #Attr.4;
                        ret .10;
                    else
                        ret #Attr.2;

                procedure .0 ():
                    let .12 = 1i64;
                    let .13 = 2i64;
                    let .14 = 3i64;
                    let .1 = Array [.12, .13, .14];
                    let .4 = CallByName .2 .1;
                    ret .4;
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
                procedure .1 (.2):
                    let .3 = Index 0 .2;
                    let .4 = Index 1 .2;
                    let .7 = CallByName Num.14 .3 .4;
                    ret .7;

                procedure Num.14 (#Attr.2, #Attr.3):
                    let .8 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret .8;

                procedure .0 ():
                    let .9 = 4i64;
                    let .10 = 9i64;
                    let .6 = Struct {.9, .10};
                    let .5 = CallByName .1 .6;
                    ret .5;
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
                procedure .1 (.2):
                    let .3 = 10i64;
                    let .4 = Index 1 .2;
                    let .7 = CallByName Num.14 .3 .4;
                    ret .7;

                procedure Num.14 (#Attr.2, #Attr.3):
                    let .8 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret .8;

                procedure .0 ():
                    let .9 = 9i64;
                    let .6 = Struct {.9};
                    let .5 = CallByName .1 .6;
                    ret .5;
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
                procedure .1 (.4):
                    let .2 = Index 0 .4;
                    let .3 = Index 1 .4;
                    let .7 = CallByName Num.14 .2 .3;
                    ret .7;

                procedure Num.14 (#Attr.2, #Attr.3):
                    let .8 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret .8;

                procedure .0 ():
                    let .9 = 4i64;
                    let .10 = 9i64;
                    let .6 = Struct {.9, .10};
                    let .5 = CallByName .1 .6;
                    ret .5;
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
                procedure .1 (.4):
                    let .2 = 10i64;
                    let .3 = Index 1 .4;
                    let .7 = CallByName Num.14 .2 .3;
                    ret .7;

                procedure Num.14 (#Attr.2, #Attr.3):
                    let .8 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret .8;

                procedure .0 ():
                    let .9 = 9i64;
                    let .6 = Struct {.9};
                    let .5 = CallByName .1 .6;
                    ret .5;
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
                    procedure List.3 (#Attr.2, #Attr.3):
                        let .38 = lowlevel ListLen #Attr.2;
                        let .34 = lowlevel NumLt #Attr.3 .38;
                        if .34 then
                            let .36 = 1i64;
                            let .37 = lowlevel ListGetUnsafe #Attr.2 #Attr.3;
                            let .35 = Ok .36 .37;
                            ret .35;
                        else
                            let .32 = 0i64;
                            let .33 = Struct {};
                            let .31 = Err .32 .33;
                            ret .31;

                    procedure List.4 (#Attr.2, #Attr.3, #Attr.4):
                        let .14 = lowlevel ListLen #Attr.2;
                        let .12 = lowlevel NumLt #Attr.3 .14;
                        if .12 then
                            let .13 = lowlevel ListSet #Attr.2 #Attr.3 #Attr.4;
                            ret .13;
                        else
                            ret #Attr.2;

                    procedure .1 (.2):
                        let .39 = 0i64;
                        let .28 = CallByName List.3 .2 .39;
                        let .30 = 0i64;
                        let .29 = CallByName List.3 .2 .30;
                        let .7 = Struct {.28, .29};
                        joinpoint .25:
                            let .18 = Array [];
                            ret .18;
                        in
                        let .19 = Index 0 .7;
                        let .20 = 1i64;
                        let .21 = Index 0 .19;
                        let .27 = lowlevel Eq .20 .21;
                        if .27 then
                            let .22 = Index 1 .7;
                            let .23 = 1i64;
                            let .24 = Index 0 .22;
                            let .26 = lowlevel Eq .23 .24;
                            if .26 then
                                let .17 = Index 0 .7;
                                let .3 = Index 1 .17;
                                let .16 = Index 1 .7;
                                let .4 = Index 1 .16;
                                let .15 = 0i64;
                                let .9 = CallByName List.4 .2 .15 .4;
                                let .10 = 0i64;
                                let .8 = CallByName List.4 .9 .10 .3;
                                ret .8;
                            else
                                dec .2;
                                jump .25;
                        else
                            dec .2;
                            jump .25;

                    procedure .0 ():
                        let .40 = 1i64;
                        let .41 = 2i64;
                        let .6 = Array [.40, .41];
                        let .5 = CallByName .1 .6;
                        ret .5;
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
                    app "test" provides [ main ] to "./platform"

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
                procedure .1 (.2):
                    let .39 = 0i64;
                    let .28 = CallByName List.3 .2 .39;
                    let .30 = 0i64;
                    let .29 = CallByName List.3 .2 .30;
                    let .7 = Struct {.28, .29};
                    joinpoint .25:
                        let .18 = Array [];
                        ret .18;
                    in
                    let .22 = Index 1 .7;
                    let .23 = 1i64;
                    let .24 = Index 0 .22;
                    let .27 = lowlevel Eq .23 .24;
                    if .27 then
                        let .19 = Index 0 .7;
                        let .20 = 1i64;
                        let .21 = Index 0 .19;
                        let .26 = lowlevel Eq .20 .21;
                        if .26 then
                            let .17 = Index 0 .7;
                            let .3 = Index 1 .17;
                            let .16 = Index 1 .7;
                            let .4 = Index 1 .16;
                            let .15 = 0i64;
                            let .9 = CallByName List.4 .2 .15 .4;
                            let .10 = 0i64;
                            let .8 = CallByName List.4 .9 .10 .3;
                            ret .8;
                        else
                            dec .2;
                            jump .25;
                    else
                        dec .2;
                        jump .25;

                procedure List.3 (#Attr.2, #Attr.3):
                    let .38 = lowlevel ListLen #Attr.2;
                    let .34 = lowlevel NumLt #Attr.3 .38;
                    if .34 then
                        let .36 = 1i64;
                        let .37 = lowlevel ListGetUnsafe #Attr.2 #Attr.3;
                        let .35 = Ok .36 .37;
                        ret .35;
                    else
                        let .32 = 0i64;
                        let .33 = Struct {};
                        let .31 = Err .32 .33;
                        ret .31;

                procedure List.4 (#Attr.2, #Attr.3, #Attr.4):
                    let .14 = lowlevel ListLen #Attr.2;
                    let .12 = lowlevel NumLt #Attr.3 .14;
                    if .12 then
                        let .13 = lowlevel ListSet #Attr.2 #Attr.3 #Attr.4;
                        ret .13;
                    else
                        ret #Attr.2;

                procedure .0 ():
                    let .40 = 1i64;
                    let .41 = 2i64;
                    let .6 = Array [.40, .41];
                    let .5 = CallByName .1 .6;
                    ret .5;
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
                    app "test" provides [ main ] to "./platform"

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
                    app "test" provides [ main ] to "./platform"

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
                procedure .1 (.2, .3):
                    joinpoint .7 .2 .3:
                        let .15 = 0i64;
                        let .16 = lowlevel Eq .15 .2;
                        if .16 then
                            ret .3;
                        else
                            let .13 = 1i64;
                            let .10 = CallByName Num.15 .2 .13;
                            let .11 = CallByName Num.16 .2 .3;
                            jump .7 .10 .11;
                    in
                    jump .7 .2 .3;

                procedure Num.15 (#Attr.2, #Attr.3):
                    let .14 = lowlevel NumSub #Attr.2 #Attr.3;
                    ret .14;

                procedure Num.16 (#Attr.2, #Attr.3):
                    let .12 = lowlevel NumMul #Attr.2 #Attr.3;
                    ret .12;

                procedure .0 ():
                    let .5 = 10i64;
                    let .6 = 1i64;
                    let .4 = CallByName .1 .5 .6;
                    ret .4;
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
                procedure .1 (.3):
                    let .13 = true;
                    let .15 = Index 0 .3;
                    let .14 = 1i64;
                    let .16 = lowlevel Eq .14 .15;
                    let .12 = lowlevel And .16 .13;
                    if .12 then
                        let .10 = true;
                        ret .10;
                    else
                        let .11 = false;
                        ret .11;

                let .6 = 0i64;
                let .7 = 2i64;
                let .9 = 1i64;
                let .8 = Nil .9;
                let .5 = Cons .6 .7 .8;
                let .4 = CallByName .1 .5;
                ret .4;
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
                procedure .1 (.3):
                    let .13 = true;
                    let .15 = Index 0 .3;
                    let .14 = 1i64;
                    let .16 = lowlevel Eq .14 .15;
                    let .12 = lowlevel And .16 .13;
                    if .12 then
                        let .10 = true;
                        ret .10;
                    else
                        let .11 = false;
                        ret .11;

                let .6 = 0i64;
                let .7 = 2i64;
                let .9 = 1i64;
                let .8 = Nil .9;
                let .5 = Cons .6 .7 .8;
                let .4 = CallByName .1 .5;
                ret .4;
                "#
            ),
        )
    }

    #[test]
    fn mk_pair_of() {
        compiles_to_ir(
            indoc!(
                r#"
                app "test" provides [ main ] to "./platform"

                mkPairOf = \x -> Pair x x

                main =
                    mkPairOf [1,2,3]
                "#
            ),
            indoc!(
                r#"
                procedure .1 (.2):
                    inc .2;
                    let .5 = Struct {.2, .2};
                    ret .5;

                procedure .0 ():
                    let .6 = 1i64;
                    let .7 = 2i64;
                    let .8 = 3i64;
                    let .4 = Array [.6, .7, .8];
                    let .3 = CallByName .1 .4;
                    ret .3;
                "#
            ),
        )
    }

    #[test]
    fn fst() {
        compiles_to_ir(
            indoc!(
                r#"
                app "test" provides [ main ] to "./platform"

                fst = \x, _ -> x

                main =
                    fst [1,2,3] [3,2,1]
                "#
            ),
            indoc!(
                r#"
                procedure .1 (.2, .3):
                    inc .2;
                    ret .2;

                procedure .0 ():
                    let .11 = 1i64;
                    let .12 = 2i64;
                    let .13 = 3i64;
                    let .5 = Array [.11, .12, .13];
                    let .8 = 3i64;
                    let .9 = 2i64;
                    let .10 = 1i64;
                    let .6 = Array [.8, .9, .10];
                    let .4 = CallByName .1 .5 .6;
                    dec .6;
                    dec .5;
                    ret .4;
                "#
            ),
        )
    }

    #[test]
    fn list_cannot_update_inplace() {
        compiles_to_ir(
            indoc!(
                r#"
                app "test" provides [ main ] to "./platform"

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
                procedure .1 ():
                    let .11 = 1i64;
                    let .12 = 2i64;
                    let .13 = 3i64;
                    let .10 = Array [.11, .12, .13];
                    ret .10;

                procedure .2 (.3):
                    let .17 = 0i64;
                    let .18 = 0i64;
                    let .16 = CallByName List.4 .3 .17 .18;
                    ret .16;

                procedure List.4 (#Attr.2, #Attr.3, #Attr.4):
                    let .22 = lowlevel ListLen #Attr.2;
                    let .20 = lowlevel NumLt #Attr.3 .22;
                    if .20 then
                        let .21 = lowlevel ListSet #Attr.2 #Attr.3 #Attr.4;
                        ret .21;
                    else
                        ret #Attr.2;

                procedure List.7 (#Attr.2):
                    let .9 = lowlevel ListLen #Attr.2;
                    ret .9;

                procedure Num.14 (#Attr.2, #Attr.3):
                    let .7 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret .7;

                procedure .0 ():
                    let .15 = FunctionPointer .1;
                    let .14 = CallByName .2 .15;
                    let .5 = CallByName List.7 .14;
                    dec .14;
                    let .8 = FunctionPointer .1;
                    let .6 = CallByName List.7 .8;
                    dec .8;
                    let .4 = CallByName Num.14 .5 .6;
                    ret .4;
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
                procedure .1 (.2):
                    let .16 = 1i64;
                    let .17 = 2i64;
                    let .18 = 3i64;
                    let .6 = Array [.16, .17, .18];
                    let .7 = 0i64;
                    let .5 = CallByName List.3 .6 .7;
                    dec .6;
                    ret .5;

                procedure List.3 (#Attr.2, #Attr.3):
                    let .15 = lowlevel ListLen #Attr.2;
                    let .11 = lowlevel NumLt #Attr.3 .15;
                    if .11 then
                        let .13 = 1i64;
                        let .14 = lowlevel ListGetUnsafe #Attr.2 #Attr.3;
                        let .12 = Ok .13 .14;
                        ret .12;
                    else
                        let .9 = 0i64;
                        let .10 = Struct {};
                        let .8 = Err .9 .10;
                        ret .8;

                procedure .0 ():
                    let .4 = Struct {};
                    let .3 = CallByName .1 .4;
                    ret .3;
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
                procedure .0 ():
                    let .4 = 0i64;
                    let .6 = 0i64;
                    let .8 = 0i64;
                    let .10 = 1i64;
                    let .9 = Z .10;
                    let .7 = S .8 .9;
                    let .5 = S .6 .7;
                    let .2 = S .4 .5;
                    ret .2;
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
                procedure .0 ():
                    let .8 = 0i64;
                    let .10 = 0i64;
                    let .12 = 0i64;
                    let .14 = 1i64;
                    let .13 = Z .14;
                    let .11 = S .12 .13;
                    let .9 = S .10 .11;
                    let .2 = S .8 .9;
                    let .5 = 1i64;
                    let .6 = Index 0 .2;
                    dec .2;
                    let .7 = lowlevel Eq .5 .6;
                    if .7 then
                        let .3 = 0i64;
                        ret .3;
                    else
                        let .4 = 1i64;
                        ret .4;
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
                procedure .0 ():
                    let .14 = 0i64;
                    let .16 = 0i64;
                    let .18 = 0i64;
                    let .20 = 1i64;
                    let .19 = Z .20;
                    let .17 = S .18 .19;
                    let .15 = S .16 .17;
                    let .2 = S .14 .15;
                    let .11 = 0i64;
                    let .12 = Index 0 .2;
                    let .13 = lowlevel Eq .11 .12;
                    if .13 then
                        let .7 = Index 1 .2;
                        inc .7;
                        let .8 = 0i64;
                        let .9 = Index 0 .7;
                        dec .7;
                        let .10 = lowlevel Eq .8 .9;
                        if .10 then
                            let .4 = Index 1 .2;
                            dec .2;
                            let .3 = 1i64;
                            ret .3;
                        else
                            dec .2;
                            let .5 = 0i64;
                            ret .5;
                    else
                        dec .2;
                        let .6 = 0i64;
                        ret .6;
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
                procedure .1 (.6):
                    let .18 = Index 0 .6;
                    let .19 = false;
                    let .20 = lowlevel Eq .19 .18;
                    if .20 then
                        let .8 = Index 1 .6;
                        ret .8;
                    else
                        let .10 = Index 1 .6;
                        ret .10;

                procedure .1 (.6):
                    let .29 = Index 0 .6;
                    let .30 = false;
                    let .31 = lowlevel Eq .30 .29;
                    if .31 then
                        let .8 = 3i64;
                        ret .8;
                    else
                        let .10 = 5i64;
                        ret .10;

                procedure Num.16 (#Attr.2, #Attr.3):
                    let .13 = lowlevel NumMul #Attr.2 #Attr.3;
                    ret .13;

                procedure .0 ():
                    let .34 = true;
                    let .33 = Struct {.34};
                    let .5 = CallByName .1 .33;
                    let .32 = false;
                    let .26 = Struct {.32};
                    let .3 = CallByName .1 .26;
                    let .24 = true;
                    let .25 = 11i64;
                    let .23 = Struct {.24, .25};
                    let .4 = CallByName .1 .23;
                    let .21 = false;
                    let .22 = 7i64;
                    let .15 = Struct {.21, .22};
                    let .2 = CallByName .1 .15;
                    let .14 = CallByName Num.16 .2 .3;
                    let .12 = CallByName Num.16 .14 .4;
                    let .11 = CallByName Num.16 .12 .5;
                    ret .11;
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
                    let .6 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret .6;

                procedure .0 ():
                    let .17 = 0i64;
                    let .19 = 0i64;
                    let .20 = 41i64;
                    let .18 = Just .19 .20;
                    let .2 = Just .17 .18;
                    joinpoint .14:
                        let .8 = 1i64;
                        ret .8;
                    in
                    let .12 = 0i64;
                    let .13 = Index 0 .2;
                    let .16 = lowlevel Eq .12 .13;
                    if .16 then
                        let .9 = Index 1 .2;
                        let .10 = 0i64;
                        let .11 = Index 0 .9;
                        let .15 = lowlevel Eq .10 .11;
                        if .15 then
                            let .7 = Index 1 .2;
                            let .3 = Index 1 .7;
                            let .5 = 1i64;
                            let .4 = CallByName Num.14 .3 .5;
                            ret .4;
                        else
                            jump .14;
                    else
                        jump .14;
                "#
            ),
        )
    }

    #[test]
    #[ignore]
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
                    let .9 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret .9;

                procedure .3 (.4):
                    let .15 = true;
                    let .16 = 1i64;
                    let .17 = Index 0 .4;
                    let .18 = lowlevel Eq .16 .17;
                    let .14 = lowlevel And .18 .15;
                    if .14 then
                        dec .4;
                        let .10 = 0i64;
                        ret .10;
                    else
                        let .5 = Index 2 .4;
                        dec .4;
                        let .12 = 1i64;
                        let .13 = CallByName .3 .5;
                        let .11 = CallByName Num.14 .12 .13;
                        ret .11;

                procedure .0 ():
                    let .20 = 1i64;
                    let .2 = Nil .20;
                    let .7 = CallByName .3 .2;
                    let .8 = CallByName .3 .2;
                    let .6 = CallByName Num.14 .7 .8;
                    ret .6;
                "#
            ),
        )
    }

    #[test]
    fn rigids() {
        compiles_to_ir(
            indoc!(
                r#"
                app "test" provides [ main ] to "./platform"

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
                procedure .1 (.2, .3, .4):
                    let .31 = CallByName List.3 .4 .2;
                    let .32 = CallByName List.3 .4 .3;
                    let .12 = Struct {.31, .32};
                    joinpoint .28:
                        let .21 = Array [];
                        ret .21;
                    in
                    let .25 = Index 1 .12;
                    let .26 = 1i64;
                    let .27 = Index 0 .25;
                    let .30 = lowlevel Eq .26 .27;
                    if .30 then
                        let .22 = Index 0 .12;
                        let .23 = 1i64;
                        let .24 = Index 0 .22;
                        let .29 = lowlevel Eq .23 .24;
                        if .29 then
                            let .20 = Index 0 .12;
                            let .5 = Index 1 .20;
                            let .19 = Index 1 .12;
                            let .6 = Index 1 .19;
                            let .14 = CallByName List.4 .4 .2 .6;
                            let .13 = CallByName List.4 .14 .3 .5;
                            ret .13;
                        else
                            dec .4;
                            jump .28;
                    else
                        dec .4;
                        jump .28;

                procedure List.3 (#Attr.2, #Attr.3):
                    let .40 = lowlevel ListLen #Attr.2;
                    let .36 = lowlevel NumLt #Attr.3 .40;
                    if .36 then
                        let .38 = 1i64;
                        let .39 = lowlevel ListGetUnsafe #Attr.2 #Attr.3;
                        let .37 = Ok .38 .39;
                        ret .37;
                    else
                        let .34 = 0i64;
                        let .35 = Struct {};
                        let .33 = Err .34 .35;
                        ret .33;

                procedure List.4 (#Attr.2, #Attr.3, #Attr.4):
                    let .18 = lowlevel ListLen #Attr.2;
                    let .16 = lowlevel NumLt #Attr.3 .18;
                    if .16 then
                        let .17 = lowlevel ListSet #Attr.2 #Attr.3 #Attr.4;
                        ret .17;
                    else
                        ret #Attr.2;

                procedure .0 ():
                    let .9 = 0i64;
                    let .10 = 0i64;
                    let .41 = 1i64;
                    let .11 = Array [.41];
                    let .8 = CallByName .1 .9 .10 .11;
                    ret .8;
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
                procedure .0 ():
                    let .1 = 5i64;
                    let .4 = 17i64;
                    let .2 = 1337i64;
                    ret .2;
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
                procedure .0 ():
                    let .1 = 5i64;
                    let .4 = 17i64;
                    let .5 = 1i64;
                    let .2 = 1337i64;
                    let .7 = Struct {.2, .4};
                    let .6 = Index 0 .7;
                    ret .6;
                "#
            ),
        )
    }

    #[test]
    fn nested_closure() {
        compiles_to_ir(
            indoc!(
                r#"
                app "test" provides [ main ] to "./platform"

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
                procedure .1 (.5):
                    let .2 = 42i64;
                    let .13 = FunctionPointer .3;
                    let .3 = Struct {.13, .2};
                    ret .3;

                procedure .3 (.11, #Attr.12):
                    let .2 = Index 0 #Attr.12;
                    ret .2;

                procedure .0 ():
                    let .10 = Struct {};
                    let .4 = CallByName .1 .10;
                    let .7 = Struct {};
                    let .8 = Index 1 .4;
                    let .9 = Index 0 .4;
                    let .6 = CallByPointer .9 .7 .8;
                    ret .6;
                "#
            ),
        )
    }

    #[test]
    fn closure_in_list() {
        compiles_to_ir(
            indoc!(
                r#"
                app "test" provides [ main ] to "./platform"

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
                procedure .1 (.5):
                    let .2 = 41i64;
                    let .12 = FunctionPointer .3;
                    let .11 = Struct {.12, .2};
                    let .10 = Array [.11];
                    ret .10;

                procedure .3 (.9, #Attr.12):
                    let .2 = Index 0 #Attr.12;
                    ret .2;

                procedure List.7 (#Attr.2):
                    let .7 = lowlevel ListLen #Attr.2;
                    ret .7;

                procedure .0 ():
                    let .8 = Struct {};
                    let .4 = CallByName .1 .8;
                    let .6 = CallByName List.7 .4;
                    dec .4;
                    ret .6;
                "#
            ),
        )
    }
}
