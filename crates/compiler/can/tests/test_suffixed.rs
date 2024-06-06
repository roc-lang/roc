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
            r#"Defs { tags: [Index(2147483648)], regions: [@0-125], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @29-36 Apply(@29-36 Var { module_name: "Task", ident: "await" }, [@29-36 Apply(@29-36 Var { module_name: "", ident: "line" }, [@30-36 Str(PlainLine("Ahoy"))], Space), @29-36 Closure([@29-36 RecordDestructure([])], @58-80 Apply(@58-80 Var { module_name: "Stdout", ident: "line" }, [@58-65 Str(PlainLine("There"))], BinOp(Pizza)))], BangSuffix))] }"#,
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
            r#"Defs { tags: [Index(2147483648)], regions: [@0-47], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @24-24 Apply(@24-24 Var { module_name: "Task", ident: "await" }, [@24-24 Var { module_name: "", ident: "foo" }, @24-24 Closure([@24-24 RecordDestructure([])], @42-47 Apply(@42-44 Var { module_name: "", ident: "ok" }, [@45-47 Record([])], Space))], BangSuffix))] }"#,
        );
    }

    /**
     * A single suffixed statement with arguments applied.
     * Note there is no final expression.
    ```roc
    main = foo! "bar" {} "baz"

    main = foo "bar" {} "baz"
    ```
    */
    #[test]
    fn last_suffixed_single() {
        run_test(
            r#"
            main = foo! "bar" {} "baz"
            "#,
            r#"Defs { tags: [Index(2147483648)], regions: [@0-26], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @0-26 Apply(@0-26 Var { module_name: "", ident: "foo" }, [@12-17 Str(PlainLine("bar")), @18-20 Record([]), @21-26 Str(PlainLine("baz"))], Space))] }"#,
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
            r#"Defs { tags: [Index(2147483648)], regions: [@0-70], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @28-28 Apply(@28-28 Var { module_name: "Task", ident: "await" }, [@28-28 Var { module_name: "", ident: "foo" }, @28-28 Closure([@28-28 RecordDestructure([])], @45-49 Apply(@45-49 Var { module_name: "Task", ident: "await" }, [@45-49 Var { module_name: "", ident: "bar" }, @45-49 Closure([@45-49 RecordDestructure([])], @66-70 Var { module_name: "", ident: "baz" })], BangSuffix))], BangSuffix))] }"#,
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
            r#"Defs { tags: [Index(2147483648)], regions: [@0-118], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @23-118 Defs(Defs { tags: [Index(2147483649)], regions: [@27-94], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@23-24 Identifier { ident: "x" }, @27-94 Closure([@28-31 Identifier { ident: "msg" }], @55-94 Defs(Defs { tags: [Index(2147483648)], regions: [@55-66], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@55-66 RecordDestructure([]), @55-66 Apply(@62-66 TaskAwaitBang(Var { module_name: "", ident: "line" }), [@55-58 Var { module_name: "", ident: "msg" }], BinOp(Pizza)))] }, @89-94 Apply(@89-91 Var { module_name: "", ident: "ok" }, [@92-94 Record([])], Space)))), Body(@23-24 Identifier { ident: "x" }, @27-94 Closure([@28-31 Identifier { ident: "msg" }], @55-66 Apply(@55-66 Var { module_name: "Task", ident: "await" }, [@55-66 Apply(@55-66 Var { module_name: "", ident: "line" }, [@55-58 Var { module_name: "", ident: "msg" }], BinOp(Pizza)), @55-66 Closure([@55-66 RecordDestructure([])], @89-94 Apply(@89-91 Var { module_name: "", ident: "ok" }, [@92-94 Record([])], Space))], BangSuffix)))] }, @112-118 Apply(@112-113 Var { module_name: "", ident: "x" }, [@114-118 Str(PlainLine("hi"))], Space)))] }"#,
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
            r#"Defs { tags: [Index(2147483648)], regions: [@0-130], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @24-93 Apply(@24-93 Var { module_name: "", ident: "line" }, [@24-69 Apply(@51-61 Var { module_name: "Str", ident: "concat" }, [@24-31 Str(PlainLine("hello")), @62-69 Str(PlainLine("world"))], BinOp(Pizza))], BinOp(Pizza)))] }"#,
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
            r##"Defs { tags: [Index(2147483648)], regions: [@0-66], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @28-47 Apply(@28-47 Var { module_name: "Task", ident: "await" }, [Var { module_name: "", ident: "sayMultiple" }, @28-47 Closure([Identifier { ident: "#!a0" }], @28-47 Defs(Defs { tags: [Index(2147483650)], regions: [@28-47], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@23-25 Identifier { ident: "do" }, @28-47 Apply(@29-41 ParensAround(TaskAwaitBang(Var { module_name: "", ident: "sayMultiple" })), [@43-47 Str(PlainLine("hi"))], Space)), Body(@23-25 Identifier { ident: "do" }, @28-47 Apply(@29-41 ParensAround(Var { module_name: "", ident: "#!a0" }), [@43-47 Str(PlainLine("hi"))], Space)), Body(@23-25 Identifier { ident: "do" }, @28-47 Apply(@29-41 ParensAround(Var { module_name: "", ident: "#!a0" }), [@43-47 Str(PlainLine("hi"))], Space))] }, @64-66 Var { module_name: "", ident: "do" }))], BangSuffix))] }"##,
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
            r##"Defs { tags: [Index(2147483648)], regions: [@0-81], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @27-31 Apply(@27-31 Var { module_name: "Task", ident: "await" }, [@27-31 Var { module_name: "", ident: "foo" }, @27-31 Closure([@23-24 Identifier { ident: "a" }], @27-31 Apply(@27-31 Var { module_name: "Task", ident: "await" }, [@48-57 Var { module_name: "", ident: "bar" }, @27-31 Closure([@48-57 Identifier { ident: "#!a0" }], @48-57 Apply(@48-57 Var { module_name: "Task", ident: "await" }, [@48-57 Var { module_name: "", ident: "#!a0" }, @48-57 Closure([@48-49 Identifier { ident: "b" }], @74-81 Apply(@74-77 Var { module_name: "", ident: "baz" }, [@78-79 Var { module_name: "", ident: "a" }, @80-81 Var { module_name: "", ident: "b" }], Space))], BangSuffix))], BangSuffix))], BangSuffix))] }"##,
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
            r##"Defs { tags: [Index(2147483648)], regions: [@0-49], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @24-49 Apply(@24-49 Var { module_name: "Task", ident: "await" }, [@29-29 Var { module_name: "", ident: "foo" }, @24-49 Closure([@29-29 Identifier { ident: "#!a0" }], @29-29 Apply(@29-29 Var { module_name: "Task", ident: "await" }, [@29-29 Var { module_name: "", ident: "#!a0" }, @29-29 Closure([@29-29 RecordDestructure([])], @46-49 Var { module_name: "", ident: "bar" })], BangSuffix))], BangSuffix))] }"##,
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
            r##"Defs { tags: [Index(2147483648)], regions: [@0-70], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @28-48 Apply(@28-48 Var { module_name: "Task", ident: "await" }, [Apply(Var { module_name: "", ident: "foo" }, [@34-39 Str(PlainLine("bar"))], Space), @28-48 Closure([Identifier { ident: "#!a0" }], @28-48 Defs(Defs { tags: [Index(2147483650)], regions: [@28-48], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@24-25 Identifier { ident: "x" }, @28-48 Apply(@29-39 ParensAround(Apply(@29-32 TaskAwaitBang(Var { module_name: "", ident: "foo" }), [@34-39 Str(PlainLine("bar"))], Space)), [@41-48 Str(PlainLine("hello"))], Space)), Body(@24-25 Identifier { ident: "x" }, @28-48 Apply(@29-39 ParensAround(Var { module_name: "", ident: "#!a0" }), [@41-48 Str(PlainLine("hello"))], Space)), Body(@24-25 Identifier { ident: "x" }, @28-48 Apply(@29-39 ParensAround(Var { module_name: "", ident: "#!a0" }), [@41-48 Str(PlainLine("hello"))], Space))] }, @65-70 Apply(@65-68 Var { module_name: "", ident: "baz" }, [@69-70 Var { module_name: "", ident: "x" }], Space)))], BangSuffix))] }"##,
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
            r##"Defs { tags: [Index(2147483648)], regions: [@0-68], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @28-46 Apply(@28-46 Var { module_name: "Task", ident: "await" }, [Apply(Var { module_name: "", ident: "foo" }, [@38-45 Str(PlainLine("hello"))], Space), @28-46 Closure([Identifier { ident: "#!a0" }], @28-46 Defs(Defs { tags: [Index(2147483650)], regions: [@28-46], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@24-25 Identifier { ident: "x" }, @28-46 Apply(@28-31 Var { module_name: "", ident: "bar" }, [@33-45 ParensAround(Apply(@33-36 TaskAwaitBang(Var { module_name: "", ident: "foo" }), [@38-45 Str(PlainLine("hello"))], Space))], Space)), Body(@24-25 Identifier { ident: "x" }, @28-46 Apply(@28-31 Var { module_name: "", ident: "bar" }, [@33-45 ParensAround(Var { module_name: "", ident: "#!a0" })], Space)), Body(@24-25 Identifier { ident: "x" }, @28-46 Apply(@28-31 Var { module_name: "", ident: "bar" }, [@33-45 ParensAround(Var { module_name: "", ident: "#!a0" })], Space))] }, @63-68 Apply(@63-66 Var { module_name: "", ident: "baz" }, [@67-68 Var { module_name: "", ident: "x" }], Space)))], BangSuffix))] }"##,
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
            r#"Defs { tags: [Index(2147483648)], regions: [@0-88], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @0-88 Defs(Defs { tags: [Index(2147483649)], regions: [@30-37], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@24-27 Identifier { ident: "msg" }, @30-37 Str(PlainLine("hello"))), Body(@24-27 Identifier { ident: "msg" }, @30-37 Str(PlainLine("hello")))] }, @0-88 Apply(@0-88 Var { module_name: "Task", ident: "await" }, [@54-66 Apply(@54-66 Var { module_name: "", ident: "foo" }, [@63-66 Var { module_name: "", ident: "msg" }], Space), @0-88 Closure([@54-55 Identifier { ident: "x" }], @83-88 Apply(@83-86 Var { module_name: "", ident: "bar" }, [@87-88 Var { module_name: "", ident: "x" }], Space))], BangSuffix)))] }"#,
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
            r#"Defs { tags: [Index(2147483648)], regions: [@0-187], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @0-187 Defs(Defs { tags: [Index(2147483650)], regions: [@60-162], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Annotation(@24-25 Identifier { ident: "x" }, @28-43 Function([@28-31 Apply("", "Str", [])], @35-43 Apply("", "Task", [@40-41 Inferred, @42-43 Inferred]))), AnnotatedBody { ann_pattern: @24-25 Identifier { ident: "x" }, ann_type: @28-43 Function([@28-31 Apply("", "Str", [])], @35-43 Apply("", "Task", [@40-41 Inferred, @42-43 Inferred])), comment: None, body_pattern: @60-61 Identifier { ident: "x" }, body_expr: @60-162 Closure([@65-68 Identifier { ident: "msg" }], @93-162 Defs(Defs { tags: [Index(2147483649)], regions: [@93-140], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Annotation(@93-94 Identifier { ident: "y" }, @97-106 Apply("", "Task", [@102-104 Record { fields: [], ext: None }, @105-106 Inferred])), AnnotatedBody { ann_pattern: @93-94 Identifier { ident: "y" }, ann_type: @97-106 Apply("", "Task", [@102-104 Record { fields: [], ext: None }, @105-106 Inferred]), comment: None, body_pattern: @127-128 Identifier { ident: "y" }, body_expr: @127-140 Apply(@131-135 TaskAwaitBang(Var { module_name: "", ident: "line" }), [@137-140 Var { module_name: "", ident: "msg" }], Space) }] }, @161-162 Var { module_name: "", ident: "y" })) }, AnnotatedBody { ann_pattern: @24-25 Identifier { ident: "x" }, ann_type: @28-43 Function([@28-31 Apply("", "Str", [])], @35-43 Apply("", "Task", [@40-41 Inferred, @42-43 Inferred])), comment: None, body_pattern: @60-61 Identifier { ident: "x" }, body_expr: @60-162 Closure([@65-68 Identifier { ident: "msg" }], @127-140 Apply(@127-140 Var { module_name: "Task", ident: "await" }, [@127-140 Apply(@127-140 Var { module_name: "", ident: "line" }, [@137-140 Var { module_name: "", ident: "msg" }], Space), @127-140 Closure([@127-128 Identifier { ident: "y" }], @161-162 Var { module_name: "", ident: "y" })], BangSuffix)) }] }, @180-187 Apply(@180-181 Var { module_name: "", ident: "x" }, [@182-187 Str(PlainLine("foo"))], Space)))] }"#,
        );
    }

    /**
     * Nested suffixed expressions
    ```roc
    run = line! (nextMsg!)

    run = Task.await nextMsg \#!a0 -> line! (#!a0)

    run = Task.await nextMsg \#!a0 -> line (#!a0)
    ```
    */
    #[test]
    fn nested_simple() {
        run_test(
            r#"
            run = line! (nextMsg!)
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-22], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-3 Identifier { ident: "run" }, @0-22 Apply(@0-22 Var { module_name: "Task", ident: "await" }, [Var { module_name: "", ident: "nextMsg" }, @0-22 Closure([Identifier { ident: "#!a0" }], @0-22 Apply(@0-22 Var { module_name: "", ident: "line" }, [@13-21 ParensAround(Var { module_name: "", ident: "#!a0" })], Space))], BangSuffix))] }"##,
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
    fn nested_complex() {
        run_test(
            r#"
            main = 
                z = foo! (bar! baz) (blah stuff)
                doSomething z
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-86], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @28-56 Apply(@28-56 Var { module_name: "Task", ident: "await" }, [Apply(Var { module_name: "", ident: "bar" }, [@39-42 Var { module_name: "", ident: "baz" }], Space), @28-56 Closure([Identifier { ident: "#!a0" }], @28-56 Apply(@28-56 Var { module_name: "Task", ident: "await" }, [@28-56 Apply(@28-56 Var { module_name: "", ident: "foo" }, [@34-42 ParensAround(Var { module_name: "", ident: "#!a0" }), @45-55 ParensAround(Apply(@45-49 Var { module_name: "", ident: "blah" }, [@50-55 Var { module_name: "", ident: "stuff" }], Space))], Space), @28-56 Closure([@24-25 Identifier { ident: "z" }], @73-86 Apply(@73-84 Var { module_name: "", ident: "doSomething" }, [@85-86 Var { module_name: "", ident: "z" }], Space))], BangSuffix))], BangSuffix))] }"##,
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
            r#"Defs { tags: [Index(2147483648)], regions: [@0-249], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @25-249 Defs(Defs { tags: [Index(2147483650)], regions: [@81-193], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Annotation(@25-28 Identifier { ident: "foo" }, @31-58 Function([@31-34 Apply("", "Str", []), @36-38 Record { fields: [], ext: None }, @40-43 Apply("", "Str", [])], @47-58 Apply("", "Task", [@52-54 Record { fields: [], ext: None }, @55-58 Apply("", "I32", [])]))), AnnotatedBody { ann_pattern: @25-28 Identifier { ident: "foo" }, ann_type: @31-58 Function([@31-34 Apply("", "Str", []), @36-38 Record { fields: [], ext: None }, @40-43 Apply("", "Str", [])], @47-58 Apply("", "Task", [@52-54 Record { fields: [], ext: None }, @55-58 Apply("", "I32", [])])), comment: None, body_pattern: @75-78 Identifier { ident: "foo" }, body_expr: @81-193 Closure([@82-83 Identifier { ident: "a" }, @85-86 Underscore(""), @88-89 Identifier { ident: "b" }], @114-193 Defs(Defs { tags: [Index(2147483648), Index(2147483649)], regions: [@119-121, @142-149], space_before: [Slice(start = 0, length = 0), Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0), Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@119-121 RecordDestructure([]), @119-121 Apply(@114-118 TaskAwaitBang(Var { module_name: "", ident: "line" }), [@120-121 Var { module_name: "", ident: "a" }], Space)), Body(@142-149 RecordDestructure([]), @142-149 Apply(@142-146 TaskAwaitBang(Var { module_name: "", ident: "line" }), [@148-149 Var { module_name: "", ident: "b" }], Space))] }, @183-193 Apply(@183-190 Var { module_name: "Task", ident: "ok" }, [@191-193 Record([])], Space))) }, AnnotatedBody { ann_pattern: @25-28 Identifier { ident: "foo" }, ann_type: @31-58 Function([@31-34 Apply("", "Str", []), @36-38 Record { fields: [], ext: None }, @40-43 Apply("", "Str", [])], @47-58 Apply("", "Task", [@52-54 Record { fields: [], ext: None }, @55-58 Apply("", "I32", [])])), comment: None, body_pattern: @75-78 Identifier { ident: "foo" }, body_expr: @81-193 Closure([@82-83 Identifier { ident: "a" }, @85-86 Underscore(""), @88-89 Identifier { ident: "b" }], @119-121 Apply(@119-121 Var { module_name: "Task", ident: "await" }, [@119-121 Apply(@119-121 Var { module_name: "", ident: "line" }, [@120-121 Var { module_name: "", ident: "a" }], Space), @119-121 Closure([@119-121 RecordDestructure([])], @142-149 Apply(@142-149 Var { module_name: "", ident: "line" }, [@148-149 Var { module_name: "", ident: "b" }], Space))], BangSuffix)) }] }, @231-249 Apply(@231-234 Var { module_name: "", ident: "foo" }, [@235-240 Str(PlainLine("bar")), @241-243 Record([]), @244-249 Str(PlainLine("baz"))], Space)))] }"#,
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
            r#"Defs { tags: [Index(2147483648), Index(2147483649)], regions: [@0-90, @120-186], space_before: [Slice(start = 0, length = 0), Slice(start = 0, length = 2)], space_after: [Slice(start = 0, length = 0), Slice(start = 2, length = 0)], spaces: [Newline, Newline], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @23-90 Defs(Defs { tags: [Index(2147483649)], regions: [@27-32], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@23-24 Identifier { ident: "a" }, @27-32 Str(PlainLine("Foo"))), Body(@23-24 Identifier { ident: "a" }, @27-32 Str(PlainLine("Foo")))] }, @23-90 Apply(@23-90 Var { module_name: "Task", ident: "await" }, [@49-63 Apply(@49-63 Var { module_name: "Stdout", ident: "line" }, [@62-63 Var { module_name: "", ident: "a" }], Space), @23-90 Closure([@49-63 RecordDestructure([])], @81-90 Var { module_name: "", ident: "printBar" })], BangSuffix))), Body(@120-128 Identifier { ident: "printBar" }, @147-186 Defs(Defs { tags: [Index(2147483649)], regions: [@151-156], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@147-148 Identifier { ident: "b" }, @151-156 Str(PlainLine("Bar"))), Body(@147-148 Identifier { ident: "b" }, @151-156 Str(PlainLine("Bar")))] }, @173-186 Apply(@173-184 Var { module_name: "Stdout", ident: "line" }, [@185-186 Var { module_name: "", ident: "b" }], Space)))] }"#,
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
            r##"Defs { tags: [Index(2147483648)], regions: [@0-286], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @23-286 Defs(Defs { tags: [Index(2147483650), Index(2147483651)], regions: [@32-49, @76-94], space_before: [Slice(start = 0, length = 0), Slice(start = 0, length = 1)], space_after: [Slice(start = 0, length = 0), Slice(start = 1, length = 0)], spaces: [Newline], type_defs: [], value_defs: [Body(@23-29 Identifier { ident: "isTrue" }, @32-49 Apply(@32-39 Var { module_name: "Task", ident: "ok" }, [@40-49 Var { module_name: "Bool", ident: "true" }], Space)), Body(@66-73 Identifier { ident: "isFalse" }, @76-94 Apply(@76-83 Var { module_name: "Task", ident: "ok" }, [@84-94 Var { module_name: "Bool", ident: "false" }], Space)), Body(@23-29 Identifier { ident: "isTrue" }, @32-49 Apply(@32-39 Var { module_name: "Task", ident: "ok" }, [@40-49 Var { module_name: "Bool", ident: "true" }], Space)), Body(@66-73 Identifier { ident: "isFalse" }, @76-94 Apply(@76-83 Var { module_name: "Task", ident: "ok" }, [@84-94 Var { module_name: "Bool", ident: "false" }], Space))] }, @115-123 Apply(@115-123 Var { module_name: "Task", ident: "await" }, [@115-123 Var { module_name: "", ident: "isFalse" }, @115-123 Closure([@115-123 Identifier { ident: "#!a0" }], @112-286 If([(@115-123 Var { module_name: "", ident: "#!a0" }, @149-160 Apply(@149-153 Var { module_name: "", ident: "line" }, [@154-160 Str(PlainLine("fail"))], Space))], @185-192 Apply(@185-192 Var { module_name: "Task", ident: "await" }, [@185-192 Var { module_name: "", ident: "isTrue" }, @185-192 Closure([@185-192 Identifier { ident: "#!a1" }], @112-286 If([(@185-192 Var { module_name: "", ident: "#!a1" }, @219-233 Apply(@219-223 Var { module_name: "", ident: "line" }, [@224-233 Str(PlainLine("success"))], Space))], @275-286 Apply(@275-279 Var { module_name: "", ident: "line" }, [@280-286 Str(PlainLine("fail"))], Space)))], BangSuffix)))], BangSuffix)))] }"##,
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
                    if (#!a1) then
                        Task.await line "nope" \{} -> ok {}
                    else
                        line "success"
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
            r##"Defs { tags: [Index(2147483648)], regions: [@0-466], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @0-466 Defs(Defs { tags: [Index(2147483652), Index(2147483653), Index(2147483654)], regions: [@32-49, @77-92, @143-445], space_before: [Slice(start = 0, length = 0), Slice(start = 0, length = 1), Slice(start = 1, length = 1)], space_after: [Slice(start = 0, length = 0), Slice(start = 1, length = 0), Slice(start = 2, length = 0)], spaces: [Newline, Newline], type_defs: [], value_defs: [Body(@23-29 Identifier { ident: "isTrue" }, @32-49 Apply(@32-39 Var { module_name: "Task", ident: "ok" }, [@40-49 Var { module_name: "Bool", ident: "true" }], Space)), Body(@66-74 Identifier { ident: "isFalsey" }, @77-92 Closure([@78-79 Identifier { ident: "x" }], @83-92 Apply(@83-90 Var { module_name: "Task", ident: "ok" }, [@91-92 Var { module_name: "", ident: "x" }], Space))), Annotation(@109-112 Identifier { ident: "msg" }, @115-126 Apply("", "Task", [@120-122 Record { fields: [], ext: None }, @123-126 Apply("", "I32", [])])), AnnotatedBody { ann_pattern: @109-112 Identifier { ident: "msg" }, ann_type: @115-126 Apply("", "Task", [@120-122 Record { fields: [], ext: None }, @123-126 Apply("", "I32", [])]), comment: None, body_pattern: @143-146 Identifier { ident: "msg" }, body_expr: @143-445 If([(@173-183 Apply(@173-174 Var { module_name: "Bool", ident: "not" }, [@175-182 ParensAround(TaskAwaitBang(Var { module_name: "", ident: "isTrue" }))], UnaryOp(Not)), @213-256 Defs(Defs { tags: [Index(2147483648)], regions: [@218-225], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@218-225 RecordDestructure([]), @218-225 Apply(@213-217 TaskAwaitBang(Var { module_name: "", ident: "line" }), [@219-225 Str(PlainLine("fail"))], Space))] }, @251-256 Apply(@251-254 Var { module_name: "", ident: "err" }, [@255-256 Num("1")], Space))), (@285-307 ParensAround(Apply(@286-294 TaskAwaitBang(Var { module_name: "", ident: "isFalsey" }), [@296-306 Var { module_name: "Bool", ident: "false" }], Space)), @338-380 Defs(Defs { tags: [Index(2147483648)], regions: [@343-350], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@343-350 RecordDestructure([]), @343-350 Apply(@338-342 TaskAwaitBang(Var { module_name: "", ident: "line" }), [@344-350 Str(PlainLine("nope"))], Space))] }, @375-380 Apply(@375-377 Var { module_name: "", ident: "ok" }, [@378-380 Record([])], Space)))], @430-445 Apply(@430-434 TaskAwaitBang(Var { module_name: "", ident: "line" }), [@436-445 Str(PlainLine("success"))], Space)) }, Body(@23-29 Identifier { ident: "isTrue" }, @32-49 Apply(@32-39 Var { module_name: "Task", ident: "ok" }, [@40-49 Var { module_name: "Bool", ident: "true" }], Space)), Body(@66-74 Identifier { ident: "isFalsey" }, @77-92 Closure([@78-79 Identifier { ident: "x" }], @83-92 Apply(@83-90 Var { module_name: "Task", ident: "ok" }, [@91-92 Var { module_name: "", ident: "x" }], Space))), AnnotatedBody { ann_pattern: @109-112 Identifier { ident: "msg" }, ann_type: @115-126 Apply("", "Task", [@120-122 Record { fields: [], ext: None }, @123-126 Apply("", "I32", [])]), comment: None, body_pattern: @143-146 Identifier { ident: "msg" }, body_expr: Apply(Var { module_name: "Task", ident: "await" }, [Var { module_name: "", ident: "isTrue" }, Closure([Identifier { ident: "#!a0" }], @143-445 If([(@173-183 Apply(@173-174 Var { module_name: "Bool", ident: "not" }, [@175-182 ParensAround(Var { module_name: "", ident: "#!a0" })], UnaryOp(Not)), @218-225 Apply(@218-225 Var { module_name: "Task", ident: "await" }, [@218-225 Apply(@218-225 Var { module_name: "", ident: "line" }, [@219-225 Str(PlainLine("fail"))], Space), @218-225 Closure([@218-225 RecordDestructure([])], @251-256 Apply(@251-254 Var { module_name: "", ident: "err" }, [@255-256 Num("1")], Space))], BangSuffix))], Apply(Var { module_name: "Task", ident: "await" }, [Apply(Var { module_name: "", ident: "isFalsey" }, [@296-306 Var { module_name: "Bool", ident: "false" }], Space), Closure([Identifier { ident: "#!a1" }], @143-445 If([(@285-307 ParensAround(Var { module_name: "", ident: "#!a1" }), @343-350 Apply(@343-350 Var { module_name: "Task", ident: "await" }, [@343-350 Apply(@343-350 Var { module_name: "", ident: "line" }, [@344-350 Str(PlainLine("nope"))], Space), @343-350 Closure([@343-350 RecordDestructure([])], @375-380 Apply(@375-377 Var { module_name: "", ident: "ok" }, [@378-380 Record([])], Space))], BangSuffix))], @430-445 Apply(@430-445 Var { module_name: "", ident: "line" }, [@436-445 Str(PlainLine("success"))], Space)))], BangSuffix)))], BangSuffix) }] }, @463-466 Var { module_name: "", ident: "msg" }))] }"##,
        );
    }

    /**
     * Unwrap a trailing binops
     ```roc
     copy = \a,b ->
        Task.await line "FOO" \{} ->
            CMD.new "cp"
            |> mapErr! ERR

    copy = \a,b ->
        Task.await line "FOO" \{} ->
            Task.await (CMD.new "cp" |> mapErr ERR) \#!a0 -> #!a0

    copy = \a,b ->
        Task.await line "FOO" \{} ->
            CMD.new "cp" |> mapErr ERR
     ```
     */
    #[test]
    fn trailing_binops() {
        run_test(
            r#"
            copy = \a,b ->
                line! "FOO"

                CMD.new "cp"
                |> mapErr! ERR
            "#,
            r#"Defs { tags: [Index(2147483648)], regions: [@0-103], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "copy" }, @7-103 Closure([@8-9 Identifier { ident: "a" }, @10-11 Identifier { ident: "b" }], @36-42 Apply(@36-42 Var { module_name: "Task", ident: "await" }, [@36-42 Apply(@36-42 Var { module_name: "", ident: "line" }, [@37-42 Str(PlainLine("FOO"))], Space), @36-42 Closure([@36-42 RecordDestructure([])], @60-103 Apply(@60-103 Var { module_name: "", ident: "mapErr" }, [@60-72 Apply(@60-67 Var { module_name: "CMD", ident: "new" }, [@68-72 Str(PlainLine("cp"))], Space), @100-103 Tag("ERR")], BinOp(Pizza)))], BangSuffix)))] }"#,
        );
    }

    /**
     * Unwrap a when expression
     ```roc
     list =
        when getList! is
            [] -> "empty"
            _ -> "non-empty"

    list =
        Task.await getList \#!a0 ->
            when #!a0 is
                [] -> "empty"
                _ -> "non-empty"
     ```
     */
    #[test]
    fn when_simple() {
        run_test(
            r#"
            list = 
                when getList! is
                    [] -> "empty"
                    _ -> "non-empty"
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-111], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "list" }, @0-111 Apply(@0-111 Var { module_name: "Task", ident: "await" }, [@29-37 Var { module_name: "", ident: "getList" }, @0-111 Closure([@29-37 Identifier { ident: "#!a0" }], @0-111 When(@29-37 Var { module_name: "", ident: "#!a0" }, [WhenBranch { patterns: [@61-63 List([])], value: @67-74 Str(PlainLine("empty")), guard: None }, WhenBranch { patterns: [@95-96 Underscore("")], value: @100-111 Str(PlainLine("non-empty")), guard: None }]))], BangSuffix))] }"##,
        );
    }

    /**
     * Unwrap a when expression
     ```roc
    list =
        when getList! is
            [] ->
                line! "foo"
                line! "bar"
            _ ->
                ok {}

    list =
        Task.await getList \#!a0 ->
            when getList is
                [] ->
                    line! "foo"
                    line! "bar"
                _ ->
                    ok {}

    list =
        Task.await getList \#!a0 ->
            when getList is
                [] ->
                    Task.await line "foo" \{} -> line! "bar"
                _ ->
                    ok {}
     ```
     */
    #[test]
    fn when_branches() {
        run_test(
            r#"
            list = 
                when getList! is
                    [] -> 
                        line! "foo"
                        line! "bar"
                    _ -> 
                        ok {}
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-195], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "list" }, @0-195 Apply(@0-195 Var { module_name: "Task", ident: "await" }, [@29-37 Var { module_name: "", ident: "getList" }, @0-195 Closure([@29-37 Identifier { ident: "#!a0" }], @0-195 When(@29-37 Var { module_name: "", ident: "#!a0" }, [WhenBranch { patterns: [@61-63 List([])], value: @97-103 Apply(@97-103 Var { module_name: "Task", ident: "await" }, [@97-103 Apply(@97-103 Var { module_name: "", ident: "line" }, [@98-103 Str(PlainLine("foo"))], Space), @97-103 Closure([@97-103 RecordDestructure([])], @128-139 Apply(@128-139 Var { module_name: "", ident: "line" }, [@134-139 Str(PlainLine("bar"))], Space))], BangSuffix), guard: None }, WhenBranch { patterns: [@160-161 Underscore("")], value: @190-195 Apply(@190-192 Var { module_name: "", ident: "ok" }, [@193-195 Record([])], Space), guard: None }]))], BangSuffix))] }"##,
        );
    }

    #[test]
    fn trailing_suffix_inside_when() {
        run_test(
            r#"
            main =
                result = Stdin.line!

                when result is
                    End ->
                        Task.ok {}

                    Input name ->
                        Stdout.line! "Hello, $(name)"
            "#,
            r#"Defs { tags: [Index(2147483648)], regions: [@0-226], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @32-43 Apply(@32-43 Var { module_name: "Task", ident: "await" }, [@32-43 Var { module_name: "Stdin", ident: "line" }, @32-43 Closure([@23-29 Identifier { ident: "result" }], @61-226 When(@66-72 Var { module_name: "", ident: "result" }, [WhenBranch { patterns: [@96-99 Tag("End")], value: @127-137 Apply(@127-134 Var { module_name: "Task", ident: "ok" }, [@135-137 Record([])], Space), guard: None }, WhenBranch { patterns: [@159-169 Apply(@159-164 Tag("Input"), [@165-169 Identifier { ident: "name" }])], value: @197-226 Apply(@197-226 Var { module_name: "Stdout", ident: "line" }, [@210-226 Str(Line([Plaintext("Hello, "), Interpolated(@220-224 Var { module_name: "", ident: "name" })]))], Space), guard: None }]))], BangSuffix))] }"#,
        );
    }

    /*
    main =
        foo = getFoo!
        dbg foo
        bar foo

    main =
        Task.await getFoo \foo ->
            dbg foo
            bar! foo

    main =
        Task.await getFoo \foo ->
            dbg foo
            bar foo
     */
    #[test]
    fn dbg_simple() {
        run_test(
            r#"
            main =
                foo = getFoo!
                dbg foo
                bar! foo
            "#,
            r#"Defs { tags: [Index(2147483648)], regions: [@0-85], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @29-36 Apply(@29-36 Var { module_name: "Task", ident: "await" }, [@29-36 Var { module_name: "", ident: "getFoo" }, @29-36 Closure([@23-26 Identifier { ident: "foo" }], @53-85 LowLevelDbg(("test.roc:4", "   "), @57-60 Apply(@57-60 Var { module_name: "Inspect", ident: "toStr" }, [@57-60 Var { module_name: "", ident: "foo" }], Space), @77-85 Apply(@77-85 Var { module_name: "", ident: "bar" }, [@82-85 Var { module_name: "", ident: "foo" }], Space)))], BangSuffix))] }"#,
        );
    }

    // main =
    //    Task.await a \#!a0 ->
    //        c = b #!a0
    //        Task.ok c
    #[test]
    fn apply_argument_single() {
        run_test(
            r#"
            main =
                c = b a!
                c
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-49], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @27-31 Apply(@27-31 Var { module_name: "Task", ident: "await" }, [@29-30 Var { module_name: "", ident: "a" }, @27-31 Closure([@29-30 Identifier { ident: "#!a0" }], @27-31 Defs(Defs { tags: [Index(2147483650)], regions: [@27-31], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@23-24 Identifier { ident: "c" }, @27-31 Apply(@27-28 Var { module_name: "", ident: "b" }, [@29-30 TaskAwaitBang(Var { module_name: "", ident: "a" })], Space)), Body(@23-24 Identifier { ident: "c" }, @27-31 Apply(@27-28 Var { module_name: "", ident: "b" }, [@29-30 Var { module_name: "", ident: "#!a0" }], Space)), Body(@23-24 Identifier { ident: "c" }, @27-31 Apply(@27-28 Var { module_name: "", ident: "b" }, [@29-30 Var { module_name: "", ident: "#!a0" }], Space))] }, @48-49 Var { module_name: "", ident: "c" }))], BangSuffix))] }"##,
        );
    }

    // main =
    //    Task.await a \#!a0 ->
    //        Task.await x \#!a1 ->
    //            c = b #!a0 #!a1
    //            Task.ok c
    #[test]
    fn apply_argument_multiple() {
        run_test(
            r#"
            main =
                c = b a! x!
                c
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-52], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @27-34 Apply(@27-34 Var { module_name: "Task", ident: "await" }, [@29-30 Var { module_name: "", ident: "a" }, @27-34 Closure([@29-30 Identifier { ident: "#!a0" }], @27-34 Apply(@27-34 Var { module_name: "Task", ident: "await" }, [@32-33 Var { module_name: "", ident: "x" }, @27-34 Closure([@32-33 Identifier { ident: "#!a1" }], @27-34 Defs(Defs { tags: [Index(2147483651)], regions: [@27-34], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@23-24 Identifier { ident: "c" }, @27-34 Apply(@27-28 Var { module_name: "", ident: "b" }, [@29-30 TaskAwaitBang(Var { module_name: "", ident: "a" }), @32-33 TaskAwaitBang(Var { module_name: "", ident: "x" })], Space)), Body(@23-24 Identifier { ident: "c" }, @27-34 Apply(@27-28 Var { module_name: "", ident: "b" }, [@29-30 Var { module_name: "", ident: "#!a0" }, @32-33 TaskAwaitBang(Var { module_name: "", ident: "x" })], Space)), Body(@23-24 Identifier { ident: "c" }, @27-34 Apply(@27-28 Var { module_name: "", ident: "b" }, [@29-30 Var { module_name: "", ident: "#!a0" }, @32-33 Var { module_name: "", ident: "#!a1" }], Space)), Body(@23-24 Identifier { ident: "c" }, @27-34 Apply(@27-28 Var { module_name: "", ident: "b" }, [@29-30 Var { module_name: "", ident: "#!a0" }, @32-33 Var { module_name: "", ident: "#!a1" }], Space))] }, @51-52 Var { module_name: "", ident: "c" }))], BangSuffix))], BangSuffix))] }"##,
        );
    }

    // main =
    //    Task.await a \#!a0 ->
    //        c = b #!a0
    //        Task.ok c
    #[test]
    fn bang_in_pipe_root() {
        run_test(
            r#"
            main =
                c = a! |> b
                c
            "#,
            r##"Defs { tags: [Index(2147483648)], regions: [@0-52], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @27-34 Apply(@27-34 Var { module_name: "Task", ident: "await" }, [@27-28 Var { module_name: "", ident: "a" }, @27-34 Closure([@27-28 Identifier { ident: "#!a0" }], @27-34 Defs(Defs { tags: [Index(2147483650)], regions: [@27-34], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@23-24 Identifier { ident: "c" }, @27-34 Apply(@33-34 Var { module_name: "", ident: "b" }, [@27-28 TaskAwaitBang(Var { module_name: "", ident: "a" })], BinOp(Pizza))), Body(@23-24 Identifier { ident: "c" }, @27-34 Apply(@33-34 Var { module_name: "", ident: "b" }, [@27-28 Var { module_name: "", ident: "#!a0" }], BinOp(Pizza))), Body(@23-24 Identifier { ident: "c" }, @27-34 Apply(@33-34 Var { module_name: "", ident: "b" }, [@27-28 Var { module_name: "", ident: "#!a0" }], BinOp(Pizza)))] }, @51-52 Var { module_name: "", ident: "c" }))], BangSuffix))] }"##,
        );
    }

    #[test]
    fn expect_then_bang() {
        run_test(
            r#"
            main =
                expect 1 == 2
                x!
            "#,
            r#"Defs { tags: [Index(2147483648)], regions: [@0-55], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @0-55 Expect(@30-36 Apply(@32-34 Var { module_name: "Bool", ident: "isEq" }, [@30-31 Num("1"), @35-36 Num("2")], BinOp(Equals)), @53-55 Var { module_name: "", ident: "x" }))] }"#,
        );
    }

    #[test]
    fn deep_when() {
        run_test(
            r#"
            main =
                when a is
                    0 ->
                        when b is
                            1 ->
                                c!
            "#,
            r#"Defs { tags: [Index(2147483648)], regions: [@0-159], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main" }, @0-159 When(@28-29 Var { module_name: "", ident: "a" }, [WhenBranch { patterns: [@53-54 NumLiteral("0")], value: @82-159 When(@87-88 Var { module_name: "", ident: "b" }, [WhenBranch { patterns: [@120-121 NumLiteral("1")], value: @157-159 Var { module_name: "", ident: "c" }, guard: None }]), guard: None }]))] }"#,
        );
    }
}

#[cfg(test)]
mod test_suffixed_helpers {

    use roc_can::suffixed::is_matching_intermediate_answer;
    use roc_module::called_via::CalledVia;
    use roc_module::ident::ModuleName;
    use roc_parse::ast::Expr;
    use roc_parse::ast::Pattern;
    use roc_region::all::Loc;

    #[test]
    fn test_matching_answer() {
        let loc_pat = Loc::at_zero(Pattern::Identifier { ident: "#!a0" });
        let loc_new = Loc::at_zero(Expr::Var {
            module_name: "",
            ident: "#!a0",
        });

        std::assert!(is_matching_intermediate_answer(&loc_pat, &loc_new));
    }

    #[test]
    fn test_matching_answer_task_ok() {
        let loc_pat = Loc::at_zero(Pattern::Identifier { ident: "#!a0" });
        let intermetiate = &[&Loc::at_zero(Expr::Var {
            module_name: "",
            ident: "#!a0",
        })];
        let task_ok = Loc::at_zero(Expr::Var {
            module_name: ModuleName::TASK,
            ident: "ok",
        });

        let loc_new = Loc::at_zero(Expr::Apply(&task_ok, intermetiate, CalledVia::BangSuffix));

        std::assert!(is_matching_intermediate_answer(&loc_pat, &loc_new));
    }
}
