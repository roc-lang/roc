#[cfg(test)]
mod suffixed_tests {

    use bumpalo::Bump;
    use roc_can::desugar::desugar_defs_node_values;
    use roc_parse::test_helpers::parse_defs_with;
    use roc_test_utils::assert_multiline_str_eq;

    /**
    ```roc
    main =
        line! "Ahoy"
        {} = "There" |> Stdout.line!

        Task.ok {}

    main =
        Task.await [line "Ahoy"] \{} ->
            Task.await [Stdout.line "there"] \{} ->
                Task.ok {}
    ```
    */
    #[test]
    fn multi_defs_stmts() {
        let arena = &Bump::new();

        let src = r#"
            main = 
                line! "Ahoy"
                {} = "There" |> Stdout.line!
                
                Task.ok {}
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r#"Defs { tags: [Index(2147483648)], regions: [@0-36], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @24-36 Apply(@24-36 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@24-36 Apply(@24-36 Var { module_name: "", ident: "line", suffixed: 0 }, [@30-36 Str(PlainLine("Ahoy"))], Space), @24-36 Closure([@24-36 RecordDestructure([])], @58-81 Apply(@58-81 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@58-81 Apply(@58-81 Var { module_name: "Stdout", ident: "line", suffixed: 0 }, [@58-65 Str(PlainLine("There"))], BinOp(Pizza)), @58-81 Closure([@53-55 RecordDestructure([])], @115-125 Apply(@115-122 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@123-125 Record([])], Space))], BangSuffix))], BangSuffix))] }"#;

        print!("{:#?}", &defs);

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    /**
    The hello world of examples

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
        let arena = &Bump::new();

        let src = r#"
            main = 
                foo!
        
                ok {}
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r#"Defs { tags: [Index(2147483648)], regions: [@0-28], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @24-28 Apply(@24-28 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@24-28 Var { module_name: "", ident: "foo", suffixed: 0 }, @24-28 Closure([@24-28 RecordDestructure([])], @54-59 Apply(@54-56 Var { module_name: "", ident: "ok", suffixed: 0 }, [@57-59 Record([])], Space))], BangSuffix))] }"#;

        print!("{:#?}", &defs);

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    /**
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
        let arena = &Bump::new();

        let src = r#"
            main =
                x = \msg ->
                    msg |> line! 
                    ok {}

                x "hi"
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r#"Defs { tags: [Index(2147483648)], regions: [@0-118], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @23-118 Defs(Defs { tags: [Index(2147483649)], regions: [@27-94], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@23-24 Identifier { ident: "x", suffixed: 0 }, @27-94 Closure([@28-31 Identifier { ident: "msg", suffixed: 0 }], @55-94 Defs(Defs { tags: [Index(2147483648)], regions: [@55-67], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@55-67 RecordDestructure([]), @55-67 Apply(@62-67 Var { module_name: "", ident: "line", suffixed: 1 }, [@55-58 Var { module_name: "", ident: "msg", suffixed: 0 }], BinOp(Pizza)))] }, @89-94 Apply(@89-91 Var { module_name: "", ident: "ok", suffixed: 0 }, [@92-94 Record([])], Space)))), Body(@23-24 Identifier { ident: "x", suffixed: 0 }, @27-94 Closure([@28-31 Identifier { ident: "msg", suffixed: 0 }], @55-67 Apply(@55-67 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@55-67 Apply(@55-67 Var { module_name: "", ident: "line", suffixed: 0 }, [@55-58 Var { module_name: "", ident: "msg", suffixed: 0 }], BinOp(Pizza)), @55-67 Closure([@55-67 RecordDestructure([])], @89-94 Apply(@89-91 Var { module_name: "", ident: "ok", suffixed: 0 }, [@92-94 Record([])], Space))], BangSuffix)))] }, @112-118 Apply(@112-113 Var { module_name: "", ident: "x", suffixed: 0 }, [@114-118 Str(PlainLine("hi"))], Space)))] }"#;

        print!("{:#?}", &defs);

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    /**
    Example of unwrapping a pipline statement

    ```roc
    main =
        "hello"
        |> Str.concat "world"
        |> line!

        Task.ok {}

    main =
        Task.await [line [Str.concat "hello" "world"]] \{} ->
            Task.ok {}
    ```
    */
    #[test]
    fn simple_pizza() {
        let arena = &Bump::new();

        let src = r#"
            main = 
                "hello"
                |> Str.concat "world"
                |> line!
        
                Task.ok {}
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r#"Defs { tags: [Index(2147483648)], regions: [@0-130], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @24-94 Apply(@24-94 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@24-94 Apply(@24-94 Var { module_name: "", ident: "line", suffixed: 0 }, [@24-69 Apply(@51-61 Var { module_name: "Str", ident: "concat", suffixed: 0 }, [@24-31 Str(PlainLine("hello")), @62-69 Str(PlainLine("world"))], BinOp(Pizza))], BinOp(Pizza)), @24-94 Closure([@51-94 RecordDestructure([])], @120-130 Apply(@120-127 Var { module_name: "Task", ident: "ok", suffixed: 0 }, [@128-130 Record([])], Space))], BangSuffix))] }"#;

        print!("{:#?}", &defs);

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    /**
    Example with a Parens sub-expression in an Apply function

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
        let arena = &Bump::new();

        let src = r#"
            main =
                do = (sayMultiple!) "hi"
                do
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r##"Defs { tags: [Index(2147483648)], regions: [@0-66], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @28-47 Apply(@28-47 Var { module_name: "Task", ident: "await", suffixed: 0 }, [Var { module_name: "", ident: "sayMultiple", suffixed: 0 }, @28-47 Closure([Identifier { ident: "#!a0", suffixed: 0 }], @28-47 Defs(Defs { tags: [Index(2147483650)], regions: [@28-47], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@23-25 Identifier { ident: "do", suffixed: 0 }, @28-47 Apply(@29-41 ParensAround(Var { module_name: "", ident: "sayMultiple", suffixed: 1 }), [@43-47 Str(PlainLine("hi"))], Space)), Body(@23-25 Identifier { ident: "do", suffixed: 0 }, @28-47 Apply(@29-41 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 }), [@43-47 Str(PlainLine("hi"))], Space)), Body(@23-25 Identifier { ident: "do", suffixed: 0 }, @28-47 Apply(@29-41 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 }), [@43-47 Str(PlainLine("hi"))], Space))] }, @64-66 Var { module_name: "", ident: "do", suffixed: 0 }))], BangSuffix))] }"##;

        print!("{:#?}", &defs);

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    /**
    Example of handling Var's with single and multiple suffixes

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
        let arena = &Bump::new();

        let src = r#"
            main =
                a = foo!
                b = bar!!
                baz a b
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r##"Defs { tags: [Index(2147483648)], regions: [@0-81], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @27-31 Apply(@27-31 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@27-31 Var { module_name: "", ident: "foo", suffixed: 0 }, @27-31 Closure([@23-24 Identifier { ident: "a", suffixed: 0 }], @27-31 Apply(@27-31 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@52-57 Var { module_name: "", ident: "bar", suffixed: 0 }, @27-31 Closure([@52-57 Identifier { ident: "#!a0", suffixed: 0 }], @52-57 Apply(@52-57 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@52-57 Var { module_name: "", ident: "#!a0", suffixed: 0 }, @52-57 Closure([@48-49 Identifier { ident: "b", suffixed: 0 }], @74-81 Apply(@74-77 Var { module_name: "", ident: "baz", suffixed: 0 }, [@78-79 Var { module_name: "", ident: "a", suffixed: 0 }, @80-81 Var { module_name: "", ident: "b", suffixed: 0 }], Space))], BangSuffix))], BangSuffix))], BangSuffix))] }"##;

        print!("{:#?}", &defs);

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    /**
    Example with multiple suffixes on a var

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
        let arena = &Bump::new();

        let src = r#"
            main = 
                foo!!
                bar
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r##"Defs { tags: [Index(2147483648)], regions: [@0-29], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @0-29 Apply(@0-29 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@24-29 Var { module_name: "", ident: "foo", suffixed: 0 }, @0-29 Closure([@24-29 Identifier { ident: "#!a0", suffixed: 0 }], @24-29 Apply(@24-29 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@24-29 Var { module_name: "", ident: "#!a0", suffixed: 0 }, @24-29 Closure([@24-29 RecordDestructure([])], @46-49 Var { module_name: "", ident: "bar", suffixed: 0 })], BangSuffix))], BangSuffix))] }"##;

        print!("{:#?}", &defs);

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    /**
    Example of nesting suffixed Apply functions

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
        let arena = &Bump::new();

        let src = r#"
            main = 
                x = (foo! "bar") "hello"
                baz x
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r##"Defs { tags: [Index(2147483648)], regions: [@0-70], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @28-48 Apply(@28-48 Var { module_name: "Task", ident: "await", suffixed: 0 }, [Apply(Var { module_name: "", ident: "foo", suffixed: 0 }, [@34-39 Str(PlainLine("bar"))], Space), @28-48 Closure([Identifier { ident: "#!a0", suffixed: 0 }], @28-48 Defs(Defs { tags: [Index(2147483650)], regions: [@28-48], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-48 Apply(@29-39 ParensAround(Apply(@29-33 Var { module_name: "", ident: "foo", suffixed: 1 }, [@34-39 Str(PlainLine("bar"))], Space)), [@41-48 Str(PlainLine("hello"))], Space)), Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-48 Apply(@29-39 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 }), [@41-48 Str(PlainLine("hello"))], Space)), Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-48 Apply(@29-39 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 }), [@41-48 Str(PlainLine("hello"))], Space))] }, @65-70 Apply(@65-68 Var { module_name: "", ident: "baz", suffixed: 0 }, [@69-70 Var { module_name: "", ident: "x", suffixed: 0 }], Space)))], BangSuffix))] }"##;

        print!("{:#?}", &defs);

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    /**
    Example of an Applly with an argument that needs to be unwrapped
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
        let arena = &Bump::new();

        let src = r#"
            main = 
                x = bar (foo! "hello")
                baz x
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r##"Defs { tags: [Index(2147483648)], regions: [@0-68], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @28-46 Apply(@28-46 Var { module_name: "Task", ident: "await", suffixed: 0 }, [Apply(Var { module_name: "", ident: "foo", suffixed: 0 }, [@38-45 Str(PlainLine("hello"))], Space), @28-46 Closure([Identifier { ident: "#!a0", suffixed: 0 }], @28-46 Defs(Defs { tags: [Index(2147483650)], regions: [@28-46], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-46 Apply(@28-31 Var { module_name: "", ident: "bar", suffixed: 0 }, [@33-45 ParensAround(Apply(@33-37 Var { module_name: "", ident: "foo", suffixed: 1 }, [@38-45 Str(PlainLine("hello"))], Space))], Space)), Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-46 Apply(@28-31 Var { module_name: "", ident: "bar", suffixed: 0 }, [@33-45 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 })], Space)), Body(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-46 Apply(@28-31 Var { module_name: "", ident: "bar", suffixed: 0 }, [@33-45 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 })], Space))] }, @63-68 Apply(@63-66 Var { module_name: "", ident: "baz", suffixed: 0 }, [@67-68 Var { module_name: "", ident: "x", suffixed: 0 }], Space)))], BangSuffix))] }"##;

        print!("{:#?}", &defs);

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    /**
    Example with multiple defs, the suffixed is not the first def

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
        let arena = &Bump::new();

        let src = r#"
            main = 
                msg = "hello"
                x = foo! msg
                bar x
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r#"Defs { tags: [Index(2147483648)], regions: [@0-88], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @24-88 Defs(Defs { tags: [Index(2147483649)], regions: [@30-37], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@24-27 Identifier { ident: "msg", suffixed: 0 }, @30-37 Str(PlainLine("hello"))), Body(@24-27 Identifier { ident: "msg", suffixed: 0 }, @30-37 Str(PlainLine("hello")))] }, @24-88 Apply(@24-88 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@58-66 Apply(@58-66 Var { module_name: "", ident: "foo", suffixed: 0 }, [@63-66 Var { module_name: "", ident: "msg", suffixed: 0 }], Space), @24-88 Closure([@54-55 Identifier { ident: "x", suffixed: 0 }], @83-88 Apply(@83-86 Var { module_name: "", ident: "bar", suffixed: 0 }, [@87-88 Var { module_name: "", ident: "x", suffixed: 0 }], Space))], BangSuffix)))] }"#;

        print!("{:#?}", &defs);

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    /**
    Example of a suffixed inside a closure, and annotated blocks

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
        let arena = &Bump::new();

        let src = r#"
            main = 
                x : Str -> Task _ _
                x = \msg ->

                    y : Task {} _
                    y = line! msg
                    y

                x "foo"
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r#"Defs { tags: [Index(2147483648)], regions: [@0-187], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @24-187 Defs(Defs { tags: [Index(2147483650)], regions: [@64-162], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Annotation(@24-25 Identifier { ident: "x", suffixed: 0 }, @28-43 Function([@28-31 Apply("", "Str", [])], @35-43 Apply("", "Task", [@40-41 Inferred, @42-43 Inferred]))), AnnotatedBody { ann_pattern: @24-25 Identifier { ident: "x", suffixed: 0 }, ann_type: @28-43 Function([@28-31 Apply("", "Str", [])], @35-43 Apply("", "Task", [@40-41 Inferred, @42-43 Inferred])), comment: None, body_pattern: @60-61 Identifier { ident: "x", suffixed: 0 }, body_expr: @64-162 Closure([@65-68 Identifier { ident: "msg", suffixed: 0 }], @93-162 Defs(Defs { tags: [Index(2147483649)], regions: [@93-140], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Annotation(@93-94 Identifier { ident: "y", suffixed: 0 }, @97-106 Apply("", "Task", [@102-104 Record { fields: [], ext: None }, @105-106 Inferred])), AnnotatedBody { ann_pattern: @93-94 Identifier { ident: "y", suffixed: 0 }, ann_type: @97-106 Apply("", "Task", [@102-104 Record { fields: [], ext: None }, @105-106 Inferred]), comment: None, body_pattern: @127-128 Identifier { ident: "y", suffixed: 0 }, body_expr: @131-140 Apply(@131-136 Var { module_name: "", ident: "line", suffixed: 1 }, [@137-140 Var { module_name: "", ident: "msg", suffixed: 0 }], Space) }] }, @161-162 Var { module_name: "", ident: "y", suffixed: 0 })) }, AnnotatedBody { ann_pattern: @24-25 Identifier { ident: "x", suffixed: 0 }, ann_type: @28-43 Function([@28-31 Apply("", "Str", [])], @35-43 Apply("", "Task", [@40-41 Inferred, @42-43 Inferred])), comment: None, body_pattern: @60-61 Identifier { ident: "x", suffixed: 0 }, body_expr: @64-162 Closure([@65-68 Identifier { ident: "msg", suffixed: 0 }], @131-140 Apply(@131-140 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@131-140 Apply(@131-140 Var { module_name: "", ident: "line", suffixed: 0 }, [@137-140 Var { module_name: "", ident: "msg", suffixed: 0 }], Space), @131-140 Closure([@127-128 Identifier { ident: "y", suffixed: 0 }], @161-162 Var { module_name: "", ident: "y", suffixed: 0 })], BangSuffix)) }] }, @180-187 Apply(@180-181 Var { module_name: "", ident: "x", suffixed: 0 }, [@182-187 Str(PlainLine("foo"))], Space)))] }"#;

        print!("{:#?}", &defs);

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }

    /**
    Example of a suffixed inside a closure, and annotated blocks

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
        let arena = &Bump::new();

        let src = r#"
            main = 
                z = foo! (bar! baz) (blah stuff)
                doSomething z
            "#;

        let mut defs = parse_defs_with(arena, src).unwrap();

        desugar_defs_node_values(arena, &mut defs, src, &mut None, "test.roc", true);

        let expected = r##"Defs { tags: [Index(2147483648)], regions: [@0-86], space_before: [Slice(start = 0, length = 0)], space_after: [Slice(start = 0, length = 0)], spaces: [], type_defs: [], value_defs: [Body(@0-4 Identifier { ident: "main", suffixed: 0 }, @28-56 Apply(@28-56 Var { module_name: "Task", ident: "await", suffixed: 0 }, [Apply(Var { module_name: "", ident: "bar", suffixed: 0 }, [@39-42 Var { module_name: "", ident: "baz", suffixed: 0 }], Space), @28-56 Closure([Identifier { ident: "#!a0", suffixed: 0 }], @28-56 Apply(@28-56 Var { module_name: "Task", ident: "await", suffixed: 0 }, [@28-56 Apply(@28-56 Var { module_name: "", ident: "foo", suffixed: 0 }, [@34-42 ParensAround(Var { module_name: "", ident: "#!a0", suffixed: 0 }), @45-55 ParensAround(Apply(@45-49 Var { module_name: "", ident: "blah", suffixed: 0 }, [@50-55 Var { module_name: "", ident: "stuff", suffixed: 0 }], Space))], Space), @28-56 Closure([@24-25 Identifier { ident: "z", suffixed: 0 }], @73-86 Apply(@73-84 Var { module_name: "", ident: "doSomething", suffixed: 0 }, [@85-86 Var { module_name: "", ident: "z", suffixed: 0 }], Space))], BangSuffix))], BangSuffix))] }"##;

        print!("{:#?}", &defs);

        assert_multiline_str_eq!(format!("{:?}", &defs).as_str(), expected);
    }
}
