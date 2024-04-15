#[cfg(test)]
mod suffixed_tests {

    use bumpalo::Bump;
    use roc_can::desugar::desugar_defs_node_values;
    use roc_parse::test_helpers::parse_defs_with;
    use roc_test_utils::assert_multiline_str_eq;

    fn run_test(src: &str, expected: &str) {
        let arena = &Bump::new();
        let mut defs = parse_defs_with(arena, src).unwrap();
        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);
        print!("{:#?}", &defs);
        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    /**
     * This example tests a suffixed statement, followed
     * by a Body with an empty record pattern.
     *
     * The def final expression is explicitly provided.
     *
    ```roc
    main =
        line! "Ahoy"
        {} = "There" |> Stdout.line!

        Task.ok {}

    main =
        Task.await [line "Ahoy"] \{} ->
            Task.await [Stdout.line "there"] \{} ->
                Task.ok {}

    main =
        Task.await [line "Ahoy"] \{} -> Stdout.line "there"
    ```
    */
    #[test]
    fn multi_defs_stmts() {
        run_test(
            r#"
            main = 
                line! "Ahoy"
                {} = "There" |> Stdout.line!
                
                Task.ok {}
            "#,
            r#"Defs { tags: [Index(2147483648)], regions: [@0-125], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @29-36 Apply(@29-36 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@29-36 Apply(@29-36 Var { module_name: "", ident: "line", suffixed: 0 }, [@30-36 Str(PlainLine("Ahoy"))], Space), @29-36 Closure([@29-36 RecordDestructure([])], @58-81 Apply(@58-81 Var { module_name: "Stdout", ident: "line", suffixed: 0 }, [@58-65 Str(PlainLine("There"))], BinOp(Pizza)))], BangSuffix))] }"#,
        );
    }

    /**
     * The most simple suffixed example. A single statement
     * without arguments and a final expression.
    ```roc
    main =
        foo!

        ok {}

    main =
        Task.await [foo] \{} ->
            ok {}
    ```
    */
    #[test]

    fn basic() {
        run_test(
            r#"
        main = 
            foo!
    
            ok {}
        "#,
            r#"Defs { tags: [Index(2147483648)], regions: [@0-47], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @24-24 Apply(@24-24 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@24-24 Var { module_name: "", ident: "foo", suffixed: 0 }, @24-24 Closure([@24-24 RecordDestructure([])], @42-47 Apply(@42-44 Var { module_name: "", ident: "ok", suffixed: 0 }, [@45-47 Record([])], Space))], BangSuffix))] }"#,
        );
    }

    /**
     * A single suffixed statement with arguments applied.
     * Note there is no final expression.
    ```roc
    main = foo! "bar" {} "baz"

    main =
        Task.await [foo "bar" {} "baz"] \#!a0 -> Task.ok #!a0
    ```
    */
    #[test]
    fn last_suffixed_single() {
        run_test(
            r#"
            main = foo! "bar" {} "baz"
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-26], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @0-26 Apply(@0-26 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@0-26 Apply(@0-26 Var { module_name: "", ident: "foo", suffixed: 0 }, [@12-17 Str(PlainLine("bar")), @18-20 Record([]), @21-26 Str(PlainLine("baz"))], Space), @0-26 Closure([@0-26 Identifier { ident: "#!a0", suffixed: 0 }], @0-26 Apply(@0-26 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@0-26 Var { module_name: "", ident: "#!a0", suffixed: 0 }], BangSuffix))], BangSuffix))] }"##,
        );
    }

    /**
     * Multiple suffixed statements with no
     * arguments, and no final expression.
    ```roc
    main =
        foo!
        bar!
        baz!

    main =
        Task.await foo \{} ->
            Task.await bar \{} ->
                Task.await baz \{} ->
                    Task.ok {}

    main =
        Task.await foo \{} ->
            Task.await bar \{} ->
                baz
    ```
    */
    #[test]
    fn last_suffixed_multiple() {
        run_test(
            r#"
            main = 
                foo!
                bar!
                baz!
            "#,
            r#"Defs { tags: [Index(2147483648)], regions: [@0-70], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @28-28 Apply(@28-28 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@28-28 Var { module_name: "", ident: "foo", suffixed: 0 }, @28-28 Closure([@28-28 RecordDestructure([])], @45-49 Apply(@45-49 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@45-49 Var { module_name: "", ident: "bar", suffixed: 0 }, @45-49 Closure([@45-49 RecordDestructure([])], @66-70 Var { module_name: "", ident: "baz", suffixed: 0 })], BangSuffix))], BangSuffix))] }"#,
        );
    }

    /**
     * A definition with a closure that contains a Defs node, which also
     * contains a suffixed binops statement.
    ```roc
    main =
        x = \msg ->
            msg |> line!
            ok {}

        x "hi"

    main =

        x = \msg ->
            Task.await [line msg] \{} -> ok {}

        x "hi"
    ```
    */
    #[test]
    fn closure_simple() {
        run_test(
            r#"
            main =
                x = \msg ->
                    msg |> line! 
                    ok {}

                x "hi"
            "#,
            r#"Defs { tags: [Index(2147483648)], regions: [@0-118], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @23-118 Defs(Defs { tags: [Index(2147483649)], regions: [@27-94], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@23-24 Identifier { ident: "x", suffixed: 0 }, @27-94 Closure([@28-31 Identifier { ident: "msg", suffixed: 0 }], @55-94 Defs(Defs { tags: [Index(2147483648)], regions: [@55-67], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@55-67 RecordDestructure([]), @55-67 Apply(@62-67 Var { module_name: "", ident: "line", suffixed: 1 }, [@55-58 Var { module_name: "", ident: "msg", suffixed: 0 }], BinOp(Pizza)))] }, @89-94 Apply(@89-91 Var { module_name: "", ident: "ok", suffixed: 0 }, [@92-94 Record([])], Space)))), Body(@23-24 Identifier { ident: "x", suffixed: 0 }, @27-94 Closure([@28-31 Identifier { ident: "msg", suffixed: 0 }], @55-67 Apply(@55-67 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@55-67 Apply(@55-67 Var { module_name: "", ident: "line", suffixed: 0 }, [@55-58 Var { module_name: "", ident: "msg", suffixed: 0 }], BinOp(Pizza)), @55-67 Closure([@55-67 RecordDestructure([])], @89-94 Apply(@89-91 Var { module_name: "", ident: "ok", suffixed: 0 }, [@92-94 Record([])], Space))], BangSuffix)))] }, @112-118 Apply(@112-113 Var { module_name: "", ident: "x", suffixed: 0 }, [@114-118 Str(PlainLine("hi"))], Space)))] }"#,
        );
    }

    /**
     * Example of unwrapping a pipline statement
     *
     * Note pipelines are desugared into Apply functions,
     * however this also tests the parser.
     *
    ```roc
    main =
        "hello"
        |> Str.concat "world"
        |> line!

        Task.ok {}

    main =
        Task.await [line [Str.concat "hello" "world"]] \{} ->
            Task.ok {}

    main =
        line (Str.concat "hello" "world")
    ```
    */
    #[test]
    fn simple_pizza() {
        run_test(
            r#"
            main = 
                "hello"
                |> Str.concat "world"
                |> line!
        
                Task.ok {}
            "#,
            r#"Defs { tags: [Index(2147483648)], regions: [@0-130], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @24-94 Apply(@24-94 Var { module_name: "", ident: "line", suffixed: 0 }, [@24-69 Apply(@51-61 Var { module_name: "Str", ident: "concat", suffixed: 0 }, [@24-31 Str(PlainLine("hello")), @62-69 Str(PlainLine("world"))], BinOp(Pizza))], BinOp(Pizza)))] }"#,
        );
    }

    /**
     * Example with a parens suffixed sub-expression
     * in the function part of an Apply.
     *
     * Note how the parens unwraps into an intermediate answer #!a0 instead of
     * unwrapping the def `do`.
     *
    ```roc
    main =
        do = (sayMultiple!) "hi"
        do

    main =
        Task.await [sayMultiple] \#!a0 ->
            do = (#!a0) "hi"
            do
    ```
    */
    #[test]
    fn body_parens_apply() {
        run_test(
            r#"
            main =
                do = (sayMultiple!) "hi"
                do
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-66], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @28-47 Apply(@28-47 Var { module_name: "Task", ident: "await", suffixed: 0 }, [Var { module_name: "", ident: "sayMultiple", suffixed: 0 }, @28-47 Closure([Identifier { ident: "#!a0", suffixed: 0 }], @28-47 Defs(Defs { tags: [Index(2147483650)], regions: [@28-47], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@23-25 Identifier { ident: "do", suffixed: 0 }, @28-47 Apply(@29-41 ParensAround(Var { module_name: "", ident: "sayMultiple", suffixed: 1 }), [@43-47 Str(PlainLine("hi"))], Space)), Body(@23-25 Identifier { ident: "do", suffixed: 0 }, @28-47 Apply(@29-41 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 }), [@43-47 Str(PlainLine("hi"))], Space)), Body(@23-25 Identifier { ident: "do", suffixed: 0 }, @28-47 Apply(@29-41 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 }), [@43-47 Str(PlainLine("hi"))], Space))] }, @64-66 Var { module_name: "", ident: "do", suffixed: 0 }))], BangSuffix))] }"##,
        );
    }

    /**
     * Example of unwrapping mixed Body defs with
     * Var's of both single and multiple suffixes
    ```roc
    main =
        a = foo!
        b = bar!!
        baz a b

    main =
        Task.await [foo] \a ->
            b = bar!!
            baz a b

    main =
        Task.await [foo] \a ->
            Tas.await [bar] \#!a0 ->
                b = #!a0!
                baz a b

    main =
        Task.await [foo] \a ->
            Task.await [bar] \#!a0 ->
                Task.await #!a0 \b -> baz a b
    ```
    */
    #[test]
    fn var_suffixes() {
        run_test(
            r#"
            main =
                a = foo!
                b = bar!!
                baz a b
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-81], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @27-31 Apply(@27-31 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@27-31 Var { module_name: "", ident: "foo", suffixed: 0 }, @27-31 Closure([@23-24 Identifier { ident: "a", suffixed: 0 }], @48-57 Apply(@48-57 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@48-57 Var { module_name: "", ident: "bar", suffixed: 0 }, @48-57 Closure([@48-57 Identifier { ident: "#!a0", suffixed: 0 }], @48-57 Apply(@48-57 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@48-57 Var { module_name: "", ident: "#!a0", suffixed: 0 }, @48-57 Closure([@48-49 Identifier { ident: "b", suffixed: 0 }], @74-81 Apply(@74-77 Var { module_name: "", ident: "baz", suffixed: 0 }, [@78-79 Var { module_name: "", ident: "a", suffixed: 0 }, @80-81 Var { module_name: "", ident: "b", suffixed: 0 }], Space))], BangSuffix))], BangSuffix))], BangSuffix))] }"##,
        );
    }

    /**
     * Example with a multiple suffixed Var
     *
     * Note it unwraps into an intermediate answer `#!a0`
     *
    ```roc
    main =
        foo!!
        bar

    main =
        Task.await [foo] \#!a0 ->
            #!a0!
            bar

    main =
        Task.await [foo] \#!a0 ->
            Task.await [#!a0] \{} -> bar
    ```
    */
    #[test]
    fn multiple_suffix() {
        run_test(
            r#"
            main = 
                foo!!
                bar
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-49], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @29-29 Apply(@29-29 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@29-29 Var { module_name: "", ident: "foo", suffixed: 0 }, @29-29 Closure([@29-29 Identifier { ident: "#!a0", suffixed: 0 }], @29-29 Apply(@29-29 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@29-29 Var { module_name: "", ident: "#!a0", suffixed: 0 }, @29-29 Closure([@29-29 RecordDestructure([])], @46-49 Var { module_name: "", ident: "bar", suffixed: 0 })], BangSuffix))], BangSuffix))] }"##,
        );
    }

    /**
     * A suffixed expression in the function part of the Apply
    ```roc
    main =
        x = (foo! "bar") "hello"
        baz x

    main =
        Task.await [foo "bar"] \#!a0 ->
            x = (#!a0) "hello"
            baz x
    ```
    */
    #[test]
    fn apply_function_suffixed() {
        run_test(
            r#"
            main = 
                x = (foo! "bar") "hello"
                baz x
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-70], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @28-48 Apply(@28-48 Var { module_name: "Task", ident: "await", suffixed: 0 }, [Apply(Var { module_name: "", ident: "foo", suffixed: 0 }, [@34-39 Str(PlainLine("bar"))], Space), @28-48 Closure([Identifier { ident: "#!a0", suffixed: 0 }], @28-48 Defs(Defs { tags: [Index(2147483650)], regions: [@28-48], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-48 Apply(@29-39 ParensAround(Apply(@29-33 Var { module_name: "", ident: "foo", suffixed: 1 }, [@34-39 Str(PlainLine("bar"))], Space)), [@41-48 Str(PlainLine("hello"))], Space)), Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-48 Apply(@29-39 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 }), [@41-48 Str(PlainLine("hello"))], Space)), Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-48 Apply(@29-39 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 }), [@41-48 Str(PlainLine("hello"))], Space))] }, @65-70 Apply(@65-68 Var { module_name: "", ident: "baz", suffixed: 0 }, [@69-70 Var { module_name: "", ident: "x", suffixed: 0 }], Space)))], BangSuffix))] }"##,
        );
    }

    /**
     * A suffixed expression in an Apply argument position.
    ```roc
    main =
       x = bar (foo! "hello")
       baz x

    main =
       Task.await [foo "hello"] \#!a0 ->
            x = bar (#!a0)
            baz x
    ```
    */
    #[test]
    fn apply_argument_suffixed() {
        run_test(
            r#"
            main = 
                x = bar (foo! "hello")
                baz x
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-68], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @28-46 Apply(@28-46 Var { module_name: "Task", ident: "await", suffixed: 0 }, [Apply(Var { module_name: "", ident: "foo", suffixed: 0 }, [@38-45 Str(PlainLine("hello"))], Space), @28-46 Closure([Identifier { ident: "#!a0", suffixed: 0 }], @28-46 Defs(Defs { tags: [Index(2147483650)], regions: [@28-46], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-46 Apply(@28-31 Var { module_name: "", ident: "bar", suffixed: 0 }, [@33-45 ParensAround(Apply(@33-37 Var { module_name: "", ident: "foo", suffixed: 1 }, [@38-45 Str(PlainLine("hello"))], Space))], Space)), Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-46 Apply(@28-31 Var { module_name: "", ident: "bar", suffixed: 0 }, [@33-45 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 })], Space)), Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-46 Apply(@28-31 Var { module_name: "", ident: "bar", suffixed: 0 }, [@33-45 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 })], Space))] }, @63-68 Apply(@63-66 Var { module_name: "", ident: "baz", suffixed: 0 }, [@67-68 Var { module_name: "", ident: "x", suffixed: 0 }], Space)))], BangSuffix))] }"##,
        );
    }

    /**
     * Example where the suffixed def is not the first def
    ```roc
    main =
        msg = "hello"
        x = foo! msg
        bar x

    main =
        msg = "hello"
        Task.await [foo msg] \x -> bar x
    ```
    */
    #[test]
    fn multiple_def_first_suffixed() {
        run_test(
            r#"
            main = 
                msg = "hello"
                x = foo! msg
                bar x
            "#,
            r#"Defs { tags: [Index(2147483648)], regions: [@0-88], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @0-88 Defs(Defs { tags: [Index(2147483649)], regions: [@30-37], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@24-27 Identifier { ident: "msg", suffixed: 0 }, @30-37 Str(PlainLine("hello"))), Body(@24-27 Identifier { ident: "msg", suffixed: 0 }, @30-37 Str(PlainLine("hello")))] }, @0-88 Apply(@0-88 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@54-66 Apply(@54-66 Var { module_name: "", ident: "foo", suffixed: 0 }, [@63-66 Var { module_name: "", ident: "msg", suffixed: 0 }], Space), @0-88 Closure([@54-55 Identifier { ident: "x", suffixed: 0 }], @83-88 Apply(@83-86 Var { module_name: "", ident: "bar", suffixed: 0 }, [@87-88 Var { module_name: "", ident: "x", suffixed: 0 }], Space))], BangSuffix)))] }"#,
        );
    }

    /**
     * Annotated defs and a suffixed expression
     * with annotations inside a closure
    ```roc
    main =
        x : Str -> Task _ _
        x = \msg ->

            y : Task {} _
            y = line! msg
            y

        x "foo"

    main =
        x : Str -> Task _ _
        x = \msg ->
            Task.await [line msg] \y -> y

        x "foo"
    ```
    */

    #[test]
    fn closure_with_annotations() {
        run_test(
            r#"
            main = 
                x : Str -> Task _ _
                x = \msg ->

                    y : Task {} _
                    y = line! msg
                    y

                x "foo"
            "#,
            r#"Defs { tags: [Index(2147483648)], regions: [@0-187], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @0-187 Defs(Defs { tags: [Index(2147483650)], regions: [@60-162], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Annotation(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-43 Function([@28-31 Apply("", "Str", [])], @35-43 Apply("", "Task", [@40-41 Inferred, @42-43 Inferred]))), AnnotatedBody { ann_pattern: @24-25 Identifier { ident: "x", suffixed: 0 }, ann_type: @28-43 Function([@28-31 Apply("", "Str", [])], @35-43 Apply("", "Task", [@40-41 Inferred, @42-43 Inferred])), comment: None, body_pattern: @60-61 Identifier { ident: "x", suffixed: 0 }, body_expr: @60-162 Closure([@65-68 Identifier { ident: "msg", suffixed: 0 }], @93-162 Defs(Defs { tags: [Index(2147483649)], regions: [@93-140], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Annotation(@93-94 Identifier { ident: "y", suffixed: 0 }, @97-106 Apply("", "Task", [@102-104 Record { fields: [], ext: None }, @105-106 Inferred])), AnnotatedBody { ann_pattern: @93-94 Identifier { ident: "y", suffixed: 0 }, ann_type: @97-106 Apply("", "Task", [@102-104 Record { fields: [], ext: None }, @105-106 Inferred]), comment: None, body_pattern: @127-128 Identifier { ident: "y", suffixed: 0 }, body_expr: @127-140 Apply(@131-136 Var { module_name: "", ident: "line", suffixed: 1 }, [@137-140 Var { module_name: "", ident: "msg", suffixed: 0 }], Space) }] }, @161-162 Var { module_name: "", ident: "y", suffixed: 0 })) }, AnnotatedBody { ann_pattern: @24-25 Identifier { ident: "x", suffixed: 0 }, ann_type: @28-43 Function([@28-31 Apply("", "Str", [])], @35-43 Apply("", "Task", [@40-41 Inferred, @42-43 Inferred])), comment: None, body_pattern: @60-61 Identifier { ident: "x", suffixed: 0 }, body_expr: @60-162 Closure([@65-68 Identifier { ident: "msg", suffixed: 0 }], @127-140 Apply(@127-140 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@127-140 Apply(@127-140 Var { module_name: "", ident: "line", suffixed: 0 }, [@137-140 Var { module_name: "", ident: "msg", suffixed: 0 }], Space), @127-140 Closure([@127-128 Identifier { ident: "y", suffixed: 0 }], @161-162 Var { module_name: "", ident: "y", suffixed: 0 })], BangSuffix)) }] }, @180-187 Apply(@180-181 Var { module_name: "", ident: "x", suffixed: 0 }, [@182-187 Str(PlainLine("foo"))], Space)))] }"#,
        );
    }

    /**
     * Nested suffixed expressions
    ```roc
    main =
        z = foo! (bar! baz) (blah stuff)
        doSomething z

    main =
        Task.await [bar baz] \#!a0 ->
            z = foo! (#!a0) (blah stuff)
            doSomething z

    main =
        Task.await [bar baz] \#!a0 ->
            Task.await [foo (#!a0) (blah stuff)] \z -> doSomething z
    ```
    */

    #[test]
    fn nested_suffixed() {
        run_test(
            r#"
            main = 
                z = foo! (bar! baz) (blah stuff)
                doSomething z
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-86], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @28-56 Apply(@28-56 Var { module_name: "Task", ident: "await", suffixed: 0 }, [Apply(Var { module_name: "", ident: "bar", suffixed: 0 }, [@39-42 Var { module_name: "", ident: "baz", suffixed: 0 }], Space), @28-56 Closure([Identifier { ident: "#!a0", suffixed: 0 }], @28-56 Apply(@28-56 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@28-56 Apply(@28-56 Var { module_name: "", ident: "foo", suffixed: 0 }, [@34-42 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 }), @45-55 ParensAround(Apply(@45-49 Var { module_name: "", ident: "blah", suffixed: 0 }, [@50-55 Var { module_name: "", ident: "stuff", suffixed: 0 }], Space))], Space), @28-56 Closure([@24-25 Identifier { ident: "z", suffixed: 0 }], @73-86 Apply(@73-84 Var { module_name: "", ident: "doSomething", suffixed: 0 }, [@85-86 Var { module_name: "", ident: "z", suffixed: 0 }], Space))], BangSuffix))], BangSuffix))] }"##,
        );
    }

    /**
     * A closure that contains a Defs node
    ```roc
    main = foo "bar" {} "baz"

    foo : Str, {}, Str -> Task {} I32
    foo = \a, _, b ->
        line! a
        line! b

        Task.ok {}

    foo : Str, {}, Str -> Task {} I32
    foo = \a, _, b ->
        Task.await line a \{} ->
            line! b

            Task.ok {}

    foo : Str, {}, Str -> Task {} I32
    foo = \a, _, b ->
        Task.await [line a] \{} ->
            Task.await [line b] \{} ->
                Task.ok {}

    foo : Str, {}, Str -> Task {} I32
    foo = \a, _, b ->
        Task.await [line a] \{} -> line b
    ```
    */
    #[test]
    fn closure_with_defs() {
        run_test(
            r#"
            main = 

                foo : Str, {}, Str -> Task {} I32
                foo = \a, _, b -> 
                    line! a
                    line! b
            
                    Task.ok {}
                    
                foo "bar" {} "baz"
            "#,
            r#"Defs { tags: [Index(2147483648)], regions: [@0-249], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @25-249 Defs(Defs { tags: [Index(2147483650)], regions: [@81-193], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Annotation(@25-28 Identifier { ident: "foo", suffixed: 0 }, @31-58 Function([@31-34 Apply("", "Str", []), @36-38 Record { fields: [], ext: None }, @40-43 Apply("", "Str", [])], @47-58 Apply("", "Task", [@52-54 Record { fields: [], ext: None }, @55-58 Apply("", "I32", [])]))), AnnotatedBody { ann_pattern: @25-28 Identifier { ident: "foo", suffixed: 0 }, ann_type: @31-58 Function([@31-34 Apply("", "Str", []), @36-38 Record { fields: [], ext: None }, @40-43 Apply("", "Str", [])], @47-58 Apply("", "Task", [@52-54 Record { fields: [], ext: None }, @55-58 Apply("", "I32", [])])), comment: None, body_pattern: @75-78 Identifier { ident: "foo", suffixed: 0 }, body_expr: @81-193 Closure([@82-83 Identifier { ident: "a", suffixed: 0 }, @85-86 Underscore(""), @88-89 Identifier { ident: "b", suffixed: 0 }], @114-193 Defs(Defs { tags: [Index(2147483648), Index(2147483649)], regions: [@119-121, @142-149], space_before: [Slice(start = 0, length = 0), Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0), Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@119-121 RecordDestructure([]), @119-121 Apply(@114-119 Var { module_name: "", ident: "line", suffixed: 1 }, [@120-121 Var { module_name: "", ident: "a", suffixed: 0 }], Space)), Body(@142-149 RecordDestructure([]), @142-149 Apply(@142-147 Var { module_name: "", ident: "line", suffixed: 1 }, [@148-149 Var { module_name: "", ident: "b", suffixed: 0 }], Space))] }, @183-193 Apply(@183-190 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@191-193 Record([])], Space))) }, AnnotatedBody { ann_pattern: @25-28 Identifier { ident: "foo", suffixed: 0 }, ann_type: @31-58 Function([@31-34 Apply("", "Str", []), @36-38 Record { fields: [], ext: None }, @40-43 Apply("", "Str", [])], @47-58 Apply("", "Task", [@52-54 Record { fields: [], ext: None }, @55-58 Apply("", "I32", [])])), comment: None, body_pattern: @75-78 Identifier { ident: "foo", suffixed: 0 }, body_expr: @81-193 Closure([@82-83 Identifier { ident: "a", suffixed: 0 }, @85-86 Underscore(""), @88-89 Identifier { ident: "b", suffixed: 0 }], @119-121 Apply(@119-121 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@119-121 Apply(@119-121 Var { module_name: "", ident: "line", suffixed: 0 }, [@120-121 Var { module_name: "", ident: "a", suffixed: 0 }], Space), @119-121 Closure([@119-121 RecordDestructure([])], @142-149 Apply(@142-149 Var { module_name: "", ident: "line", suffixed: 0 }, [@148-149 Var { module_name: "", ident: "b", suffixed: 0 }], Space))], BangSuffix)) }] }, @231-249 Apply(@231-234 Var { module_name: "", ident: "foo", suffixed: 0 }, [@235-240 Str(PlainLine("bar")), @241-243 Record([]), @244-249 Str(PlainLine("baz"))], Space)))] }"#,
        );
    }

    /**
     * Test when the suffixed def being unwrapped is not the first or last
    ```roc
    main =
        a = "Foo"
        Stdout.line! a

        printBar!

    printBar =
        b = "Bar"
        Stdout.line b

    main =
        a = "Foo"
        Task.await [Stdout.line a] \{} ->
            printBar!

    main =
        a = "Foo"
        Task.await [Stdout.line a] \{} ->
            Task.await [printBar] \{} ->
                Task.ok {}

    main =
        a = "Foo"
        Task.await [Stdout.line a] \{} ->
            printBar
    ```
    */
    #[test]
    fn defs_suffixed_middle() {
        run_test(
            r#"
            main =
                a = "Foo"
                Stdout.line! a

                printBar!
                
            printBar =
                b = "Bar"
                Stdout.line b
            "#,
            r#"Defs { tags: [Index(2147483648), Index(2147483649)], regions: [@0-90, @120-186], space_before: [Slice(start = 0, length = 0), Slice(start = 0, length = 2)], space_after: [Slice(start = 0, length = 0), Slice(start = 2, length = 0)], spaces: [Newline, Newline], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @23-90 Defs(Defs { tags: [Index(2147483649)], regions: [@27-32], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@23-24 Identifier { ident: "a", suffixed: 0 }, @27-32 Str(PlainLine("Foo"))), Body(@23-24 Identifier { ident: "a", suffixed: 0 }, @27-32 Str(PlainLine("Foo")))] }, @23-90 Apply(@23-90 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@49-63 Apply(@49-63 Var { module_name: "Stdout", ident: "line", suffixed: 0 }, [@62-63 Var { module_name: "", ident: "a", suffixed: 0 }], Space), @23-90 Closure([@49-63 RecordDestructure([])], @81-90 Var { module_name: "", ident: "printBar", suffixed: 0 })], BangSuffix))), Body(@120-128 Identifier { ident: "printBar", suffixed: 0 }, @147-186 Defs(Defs { tags: [Index(2147483649)], regions: [@151-156], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@147-148 Identifier { ident: "b", suffixed: 0 }, @151-156 Str(PlainLine("Bar"))), Body(@147-148 Identifier { ident: "b", suffixed: 0 }, @151-156 Str(PlainLine("Bar")))] }, @173-186 Apply(@173-184 Var { module_name: "Stdout", ident: "line", suffixed: 0 }, [@185-186 Var { module_name: "", ident: "b", suffixed: 0 }], Space)))] }"#,
        );
    }

    /**
     * A simple if-then-else statement which is split 
    ```roc

    main =
        isTrue = Task.ok Bool.true
        isFalse = Task.ok Bool.false

        if isFalse! then
            line "fail"
        else if isTrue! then
            line "success"
        else
            line "fail"

    main =
        isTrue = Task.ok Bool.true

        Task.await isFalse \#!a0 ->
            if #!a0 then
                line "fail"
            else
                Task.await isTrue \#!a1 ->
                    if #!a0 then
                        line "success"
                    else
                        line "fail"
    ```
    */
    #[test]
    fn if_simple() {
        run_test(
            r#"
            main =
                isTrue = Task.ok Bool.true
                isFalse = Task.ok Bool.false

                if isFalse! then
                    line "fail"
                else if isTrue! then 
                    line "success"
                else
                    line "fail"
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-286], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @23-286 Defs(Defs { tags: [Index(2147483650), Index(2147483651)], regions: [@32-49, @76-94], space_before: [Slice(start = 0, length = 0), Slice(start = 0, length = 1)], space_after: [Slice(start = 0, length = 0), Slice(start = 1, length = 0)], spaces: [Newline], type_defs: [], value_defs: [Body(@23-29 Identifier { ident: "isTrue", suffixed: 0 }, @32-49 Apply(@32-39 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@40-49 Var { module_name: "Bool", ident: "true", suffixed: 0 }], Space)), Body(@66-73 Identifier { ident: "isFalse", suffixed: 0 }, @76-94 Apply(@76-83 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@84-94 Var { module_name: "Bool", ident: "false", suffixed: 0 }], Space)), Body(@23-29 Identifier { ident: "isTrue", suffixed: 0 }, @32-49 Apply(@32-39 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@40-49 Var { module_name: "Bool", ident: "true", suffixed: 0 }], Space)), Body(@66-73 Identifier { ident: "isFalse", suffixed: 0 }, @76-94 Apply(@76-83 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@84-94 Var { module_name: "Bool", ident: "false", suffixed: 0 }], Space))] }, @115-123 Apply(@115-123 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@115-123 Var { module_name: "", ident: "isFalse", suffixed: 0 }, @115-123 Closure([@115-123 Identifier { ident: "#!a0", suffixed: 0 }], @112-286 If([(@115-123 Var { module_name: "", ident: "#!a0", suffixed: 0 }, @149-160 Apply(@149-153 Var { module_name: "", ident: "line", suffixed: 0 }, [@154-160 Str(PlainLine("fail"))], Space))], @185-192 Apply(@185-192 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@185-192 Var { module_name: "", ident: "isTrue", suffixed: 0 }, @185-192 Closure([@185-192 Identifier { ident: "#!a1", suffixed: 0 }], @112-286 If([(@185-192 Var { module_name: "", ident: "#!a1", suffixed: 0 }, @219-233 Apply(@219-223 Var { module_name: "", ident: "line", suffixed: 0 }, [@224-233 Str(PlainLine("success"))], Space))], @275-286 Apply(@275-279 Var { module_name: "", ident: "line", suffixed: 0 }, [@280-286 Str(PlainLine("fail"))], Space)))], BangSuffix)))], BangSuffix)))] }"##,
        );
    }

    /**
     * A more complex example including the use of nested Defs nodes
    ```roc
    # OTHER DEFS AND INTERMEDIATE STEPS NOT SHOWN
    msg =
        Task.await isTrue \#!a0 ->
            if !(#!a0) then
                Task.await line "fail" \{} -> err 1
            else
                Task.await isFalsey Bool.false \#!a1 ->
                    if (#!a0) then
                        Task.await line "nope" \{} -> ok {}
                    else
                        # note the unwrapping here doesn't use {}
                        # as the parsed Defs is unwrapped earlier to
                        # an Apply
                        Task.await line "success" \#!a2 -> Task.ok #!a2
    ```
    */
    #[test]
    fn if_complex() {
        run_test(
            r#"
            main =
                isTrue = Task.ok Bool.true
                isFalsey = \x -> Task.ok x
                msg : Task {} I32
                msg = 
                    if !(isTrue!) then
                        line! "fail" 
                        err 1
                    else if (isFalsey! Bool.false) then 
                        line! "nope"
                        ok {}
                    else
                        line! "success"

                msg
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-466], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @0-466 Defs(Defs { tags: [Index(2147483652), Index(2147483653), Index(2147483654)], regions: [@32-49, @77-92, @143-445], space_before: [Slice(start = 0, length = 0), Slice(start = 0, length = 1), Slice(start = 1, length = 1)], space_after: [Slice(start = 0, length = 0), Slice(start = 1, length = 0), Slice(start = 2, length = 0)], spaces: [Newline, Newline], type_defs: [], value_defs: [Body(@23-29 Identifier { ident: "isTrue", suffixed: 0 }, @32-49 Apply(@32-39 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@40-49 Var { module_name: "Bool", ident: "true", suffixed: 0 }], Space)), Body(@66-74 Identifier { ident: "isFalsey", suffixed: 0 }, @77-92 Closure([@78-79 Identifier { ident: "x", suffixed: 0 }], @83-92 Apply(@83-90 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@91-92 Var { module_name: "", ident: "x", suffixed: 0 }], Space))), Annotation(@109-112 Identifier { ident: "msg", suffixed: 0 }, @115-126 Apply("", "Task", [@120-122 Record { fields: [], ext: None }, @123-126 Apply("", "I32", [])])), AnnotatedBody { ann_pattern: @109-112 Identifier { ident: "msg", suffixed: 0 }, ann_type: @115-126 Apply("", "Task", [@120-122 Record { fields: [], ext: None }, @123-126 Apply("", "I32", [])]), comment: None, body_pattern: @143-146 Identifier { ident: "msg", suffixed: 0 }, body_expr: @143-445 If([(@173-183 Apply(@173-174 Var { module_name: "Bool", ident: "not", suffixed: 0 }, [@175-182 ParensAround(Var { module_name: "", ident: "isTrue", suffixed: 1 })], UnaryOp(Not)), @213-256 Defs(Defs { tags: [Index(2147483648)], regions: [@218-225], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@218-225 RecordDestructure([]), @218-225 Apply(@213-218 Var { module_name: "", ident: "line", suffixed: 1 }, [@219-225 Str(PlainLine("fail"))], Space))] }, @251-256 Apply(@251-254 Var { module_name: "", ident: "err", suffixed: 0 }, [@255-256 Num("1")], Space))), (@285-307 ParensAround(Apply(@286-295 Var { module_name: "", ident: "isFalsey", suffixed: 1 }, [@296-306 Var { module_name: "Bool", ident: "false", suffixed: 0 }], Space)), @338-380 Defs(Defs { tags: [Index(2147483648)], regions: [@343-350], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@343-350 RecordDestructure([]), @343-350 Apply(@338-343 Var { module_name: "", ident: "line", suffixed: 1 }, [@344-350 Str(PlainLine("nope"))], Space))] }, @375-380 Apply(@375-377 Var { module_name: "", ident: "ok", suffixed: 0 }, [@378-380 Record([])], Space)))], @430-445 Apply(@430-435 Var { module_name: "", ident: "line", suffixed: 1 }, [@436-445 Str(PlainLine("success"))], Space)) }, Body(@23-29 Identifier { ident: "isTrue", suffixed: 0 }, @32-49 Apply(@32-39 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@40-49 Var { module_name: "Bool", ident: "true", suffixed: 0 }], Space)), Body(@66-74 Identifier { ident: "isFalsey", suffixed: 0 }, @77-92 Closure([@78-79 Identifier { ident: "x", suffixed: 0 }], @83-92 Apply(@83-90 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@91-92 Var { module_name: "", ident: "x", suffixed: 0 }], Space))), AnnotatedBody { ann_pattern: @109-112 Identifier { ident: "msg", suffixed: 0 }, ann_type: @115-126 Apply("", "Task", [@120-122 Record { fields: [], ext: None }, @123-126 Apply("", "I32", [])]), comment: None, body_pattern: @143-146 Identifier { ident: "msg", suffixed: 0 }, body_expr: Apply(Var { module_name: "Task", ident: "await", suffixed: 0 }, [Var { module_name: "", ident: "isTrue", suffixed: 0 }, Closure([Identifier { ident: "#!a0", suffixed: 0 }], @143-445 If([(@173-183 Apply(@173-174 Var { module_name: "Bool", ident: "not", suffixed: 0 }, [@175-182 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 })], UnaryOp(Not)), @218-225 Apply(@218-225 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@218-225 Apply(@218-225 Var { module_name: "", ident: "line", suffixed: 0 }, [@219-225 Str(PlainLine("fail"))], Space), @218-225 Closure([@218-225 RecordDestructure([])], @251-256 Apply(@251-254 Var { module_name: "", ident: "err", suffixed: 0 }, [@255-256 Num("1")], Space))], BangSuffix))], Apply(Var { module_name: "Task", ident: "await", suffixed: 0 }, [Apply(Var { module_name: "", ident: "isFalsey", suffixed: 0 }, [@296-306 Var { module_name: "Bool", ident: "false", suffixed: 0 }], Space), Closure([Identifier { ident: "#!a1", suffixed: 0 }], @143-445 If([(@285-307 ParensAround(Var { module_name: "", ident: "#!a1", suffixed: 0 }), @343-350 Apply(@343-350 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@343-350 Apply(@343-350 Var { module_name: "", ident: "line", suffixed: 0 }, [@344-350 Str(PlainLine("nope"))], Space), @343-350 Closure([@343-350 RecordDestructure([])], @375-380 Apply(@375-377 Var { module_name: "", ident: "ok", suffixed: 0 }, [@378-380 Record([])], Space))], BangSuffix))], @430-445 Apply(@430-445 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@430-445 Apply(@430-445 Var { module_name: "", ident: "line", suffixed: 0 }, [@436-445 Str(PlainLine("success"))], Space), @430-445 Closure([@430-445 Identifier { ident: "#!a2", suffixed: 0 }], @143-445 Apply(@143-445 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@430-445 Var { module_name: "", ident: "#!a2", suffixed: 0 }], BangSuffix))], BangSuffix)))], BangSuffix)))], BangSuffix) }] }, @463-466 Var { module_name: "", ident: "msg", suffixed: 0 }))] }"##,
        );
    }
}
