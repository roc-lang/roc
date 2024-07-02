#![cfg(test)]

use indoc::indoc;

#[cfg(target_os = "linux")]
static BUILD_ONCE: std::sync::Once = std::sync::Once::new();

#[cfg(target_os = "linux")]
fn build_host() {
    use roc_build::program::build_and_preprocess_host;
    use roc_linker::preprocessed_host_filename;

    let platform_main_roc =
        roc_command_utils::root_dir().join("crates/valgrind/zig-platform/main.roc");

    // tests always run on the host
    let target = target_lexicon::Triple::host().into();

    // the preprocessed host is stored beside the platform's main.roc
    let preprocessed_host_path =
        platform_main_roc.with_file_name(preprocessed_host_filename(target));

    // valgrind does not support avx512 yet: https://bugs.kde.org/show_bug.cgi?id=383010
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    if is_x86_feature_detected!("avx512f") {
        std::env::set_var("NO_AVX512", "1");
    }

    let stub_dll_symbols = exposed_symbols.stub_dll_symbols();

    build_and_preprocess_host_lowlevel(
        roc_mono::ir::OptLevel::Normal,
        target,
        &platform_main_roc,
        &preprocessed_host_path,
        roc_linker::ExposedSymbols {
            top_level_values: vec![String::from("mainForHost")],
            exported_closure_types: vec![],
        },
    );
}

fn valgrind_test(source: &str) {
    #[cfg(target_os = "linux")]
    {
        valgrind_test_linux(source)
    }

    #[cfg(not(target_os = "linux"))]
    {
        let _ = source;
    }
}

#[cfg(target_os = "linux")]
fn valgrind_test_linux(source: &str) {
    use roc_build::program::BuiltFile;

    // the host is identical for all tests so we only want to build it once
    BUILD_ONCE.call_once(build_host);

    let pf = roc_command_utils::root_dir().join("crates/valgrind/zig-platform/main.roc");

    assert!(pf.exists(), "cannot find platform {:?}", &pf);

    let concat_header = !source.trim().starts_with("app ");

    let mut app_module_source = if concat_header {
        format!(
            indoc::indoc!(
                r#"
                app "test"
                    packages {{ pf: "{}" }}
                    imports []
                    provides [main] to pf

                main =
            "#
            ),
            pf.to_str().unwrap()
        )
    } else {
        String::new()
    };

    for line in source.lines() {
        if concat_header {
            app_module_source.push_str("    ");
        }
        app_module_source.push_str(line);
        app_module_source.push('\n');
    }

    if !concat_header {
        app_module_source =
            app_module_source.replace("replace_me_platform_path", &pf.display().to_string());
    }

    let temp_dir = tempfile::tempdir().unwrap();
    let app_module_path = temp_dir.path().join("app.roc");

    let arena = bumpalo::Bump::new();
    let res_binary_path =
        roc_build::program::build_str_test(&arena, &app_module_path, &app_module_source, true);

    match res_binary_path {
        Ok(BuiltFile {
            binary_path,
            problems,
            total_time: _,
            expect_metadata: _,
        }) => {
            if problems.exit_code() != 0 {
                panic!("there are problems")
            }

            run_with_valgrind(&binary_path);
        }
        Err(roc_build::program::BuildFileError::LoadingProblem(
            roc_load::LoadingProblem::FormattedReport(report),
        )) => {
            eprintln!("{report}");
            panic!("");
        }
        Err(e) => panic!("{e:?}"),
    }

    drop(temp_dir)
}

#[allow(unused)]
fn run_with_valgrind(binary_path: &std::path::Path) {
    use cli_utils::helpers::{extract_valgrind_errors, ValgrindError, ValgrindErrorXWhat};

    // If possible, report the generated executable name relative to the current dir.
    let generated_filename = binary_path
        .strip_prefix(std::env::current_dir().unwrap())
        .unwrap_or(binary_path)
        .to_str()
        .unwrap();

    let (valgrind_out, raw_xml) =
        cli_utils::helpers::run_with_valgrind([], &[generated_filename.to_string()]);

    if valgrind_out.status.success() {
        let memory_errors = extract_valgrind_errors(&raw_xml).unwrap_or_else(|err| {
            panic!(
                indoc!(
                    r"
                    failed to parse the `valgrind` xml output:

                        Error was:

                            {:?}

                        valgrind xml was:

                            {}

                        valgrind stdout was:

                            {}

                        valgrind stderr was:

                            {}
                    "
                ),
                err, raw_xml, valgrind_out.stdout, valgrind_out.stderr
            );
        });

        if !memory_errors.is_empty() {
            for error in memory_errors {
                let ValgrindError {
                    kind,
                    what: _,
                    xwhat,
                } = error;
                println!("Valgrind Error: {kind}\n");

                if let Some(ValgrindErrorXWhat {
                    text,
                    leakedbytes: _,
                    leakedblocks: _,
                }) = xwhat
                {
                    println!("    {text}");
                }
            }
            panic!("Valgrind found memory errors");
        }
    } else {
        let exit_code = match valgrind_out.status.code() {
            Some(code) => format!("exit code {code}"),
            None => "no exit code".to_string(),
        };

        panic!(
            "`valgrind` exited with {}. valgrind stdout was: \"{}\"\n\nvalgrind stderr was: \"{}\"",
            exit_code, valgrind_out.stdout, valgrind_out.stderr
        );
    }
}

#[test]
fn list_concat_consumes_first_argument() {
    valgrind_test("List.concat (List.withCapacity 1024) [1,2,3] |> List.len |> Num.toStr");
}

#[test]
fn list_concat_consumes_second_argument() {
    valgrind_test(indoc!(
        r"
        (
            a : List U8
            a = []
            b = List.reserve [] 11
            List.concat a b
            |> List.len
            |> Num.toStr
        )
        "
    ));
}

#[test]
fn str_capacity_concat() {
    valgrind_test(r#"Str.withCapacity 42 |> Str.concat "foobar""#);
}

#[test]
fn split_not_present() {
    valgrind_test(indoc!(
        r#"
        Str.split (Str.concat "a string that is stored on the heap" "!") "\n"
            |> Str.joinWith ""
        "#
    ));
}

#[test]
fn str_concat_first_argument_not_unique() {
    valgrind_test(indoc!(
        r#"
        (
            str1 = Str.reserve "" 48
            str2 = "a"

            out = Str.concat str1 str2
            if Bool.false then
                out
            else
                str1
            )
        "#
    ));
}

#[test]
fn list_concat_empty_list_zero_sized_type() {
    valgrind_test(indoc!(
        r"
        (
            a = List.reserve [] 11
            b = []
            List.concat a b
            |> List.len
            |> Num.toStr
        )
        "
    ));
}

#[test]
fn str_trim_end_capacity() {
    valgrind_test(indoc!(
        r#"
        (
            str = "a" |> Str.reserve 30
            out = str |> Str.trimEnd

            if out == "" then "A" else "B"
        )
        "#
    ));
}

#[test]
fn str_trim_start_capacity() {
    valgrind_test(indoc!(
        r#"
        (
            str = "    a" |> Str.reserve 30
            out = str |> Str.trimStart

            if out == "" then "A" else "B"
        )
        "#
    ));
}

#[test]
fn str_concat_later_referencing_empty_list_with_capacity() {
    valgrind_test(indoc!(
        r"
        (
            a : List U8
            a = List.withCapacity 1

            List.concat a [58]
            |> List.len
            |> Num.addWrap (List.len a)
            |> Num.toStr
        )
        "
    ));
}

#[test]
fn joinpoint_with_closure() {
    valgrind_test(indoc!(
        r#"
        (
            Animal : [Cat, Dog, Goose]

            makeSound : Animal -> Str
            makeSound = \animal ->
                dogSound = "Woof"
                when animal is
                    Cat | Dog if isCat animal -> "Miauw"
                    Goose -> "Honk"
                    _ -> dogSound

            isCat : Animal -> Bool
            isCat = \animal ->
                when animal is
                    Cat -> Bool.true
                    _ -> Bool.false

            test =
                catSound = makeSound Cat
                dogSound = makeSound Dog
                gooseSound = makeSound Goose
                "Cat: $(catSound), Dog: $(dogSound), Goose: $(gooseSound)"

            test
        )
        "#
    ));
}

#[test]
fn joinpoint_with_reuse() {
    valgrind_test(indoc!(
        r#"
        (
            LinkedList a : [Cons a (LinkedList a), Nil]

            # mapLinkedList : LinkedList a, (a -> b) -> LinkedList b
            mapLinkedList = \linkedList, f -> when linkedList is
                Nil -> Nil
                Cons x xs ->
                    x2 = if Bool.true then x else x
                    Cons (f x2) (mapLinkedList xs f)

            # printLinkedList : LinkedList a, (a -> Str) -> Str
            printLinkedList = \linkedList, f ->
                when linkedList is
                Nil -> "Nil"
                Cons x xs ->
                    strX = f x
                    strXs = printLinkedList xs f
                    "Cons $(strX) ($(strXs))"

            test =
                newList = mapLinkedList (Cons 1 (Cons 2 (Cons 3 Nil))) (\x -> x + 1)
                printLinkedList newList Num.toStr

            test
        )
        "#
    ));
}

#[test]
fn tree_rebalance() {
    valgrind_test(indoc!(
        r#"
        app "test"
            packages { pf: "replace_me_platform_path" }
            imports []
            provides [main] to pf

        main = show (insert 0 {} Empty)

        insert : Key k, v, RedBlackTree (Key k) v -> RedBlackTree (Key k) v
        insert = \key, value, dict ->
            when insertHelp key value dict is
                Node Red k v l r -> Node Black k v l r
                x -> x

        insertHelp : Key k, v, RedBlackTree (Key k) v -> RedBlackTree (Key k) v
        insertHelp = \key, value, dict ->
            when dict is
                Empty ->
                    # New nodes are always red. If it violates the rules, it will be fixed
                    # when balancing.
                    Node Red key value Empty Empty

                Node nColor nKey nValue nLeft nRight ->
                    when Num.compare key nKey is
                        LT -> balance nColor nKey nValue (insertHelp key value nLeft) nRight
                        EQ -> Node nColor nKey value nLeft nRight
                        GT -> balance nColor nKey nValue nLeft (insertHelp key value nRight)

        balance : NodeColor, k, v, RedBlackTree k v, RedBlackTree k v -> RedBlackTree k v
        balance = \color, key, value, left, right ->
            when right is
                Node Red rK rV rLeft rRight ->
                    when left is
                        Node Red lK lV lLeft lRight ->
                            Node
                                Red
                                key
                                value
                                (Node Black lK lV lLeft lRight)
                                (Node Black rK rV rLeft rRight)

                        _ ->
                            Node color rK rV (Node Red key value left rLeft) rRight

                _ ->
                    when left is
                        Node Red lK lV (Node Red llK llV llLeft llRight) lRight ->
                            Node
                                Red
                                lK
                                lV
                                (Node Black llK llV llLeft llRight)
                                (Node Black key value lRight right)

                        _ ->
                            Node color key value left right


        show : RedBlackTree I64 {} -> Str
        show = \tree -> showRBTree tree Num.toStr (\{} -> "{}")

        showRBTree : RedBlackTree k v, (k -> Str), (v -> Str) -> Str
        showRBTree = \tree, showKey, showValue ->
            when tree is
                Empty -> "Empty"
                Node color key value left right ->
                    sColor = showColor color
                    sKey = showKey key
                    sValue = showValue value
                    sL = nodeInParens left showKey showValue
                    sR = nodeInParens right showKey showValue

                    "Node $(sColor) $(sKey) $(sValue) $(sL) $(sR)"

        nodeInParens : RedBlackTree k v, (k -> Str), (v -> Str) -> Str
        nodeInParens = \tree, showKey, showValue ->
            when tree is
                Empty ->
                    showRBTree tree showKey showValue

                Node _ _ _ _ _ ->
                    inner = showRBTree tree showKey showValue

                    "($(inner))"

        showColor : NodeColor -> Str
        showColor = \color ->
            when color is
                Red -> "Red"
                Black -> "Black"

        NodeColor : [Red, Black]

        RedBlackTree k v : [Node NodeColor k v (RedBlackTree k v) (RedBlackTree k v), Empty]

        Key k : Num k


        "#
    ));
}

#[test]
fn lowlevel_list_calls() {
    valgrind_test(indoc!(
        r"
        (
            a = List.map [1,1,1,1,1] (\x -> x + 0)
            b = List.map2 a [1,1,1,1,1] (\x, y -> x + y)
            c = List.map3 a b [1,1,1,1,1] (\x, y, z -> x + y + z)
            d = List.map4 a b c [1,1,1,1,1] (\x, y, z, w -> x + y + z + w)
            e = List.sortWith d (\x, y -> Num.compare x y)

            Num.toStr (List.len e)
        )
        "
    ));
}

#[test]
fn joinpoint_nullpointer() {
    valgrind_test(indoc!(
        r#"
        (
            LinkedList a : [Cons a (LinkedList a), Nil]

            printLinkedList : LinkedList Str -> Str
            printLinkedList = \linkedList->
                when linkedList is
                    Nil -> "Nil"
                    Cons x xs ->
                        strXs = printLinkedList xs
                        "Cons $(x) ($(strXs))"

            linkedListHead : LinkedList Str -> LinkedList Str
            linkedListHead = \linkedList ->
                string = when linkedList is
                    Cons s _ -> s
                    Nil -> ""
                Cons string Nil

            test =
                cons = printLinkedList (linkedListHead (Cons "foo" Nil))
                nil = printLinkedList (linkedListHead (Nil))
                "$(cons) - $(nil)"

            test
        )
        "#
    ));
}

#[test]
fn freeing_boxes() {
    valgrind_test(indoc!(
        r#"
        (
            # Without refcounted field
            a : I32
            a = 7
                |> Box.box
                |> Box.unbox

            # With refcounted field
            b : Str
            b =
                "Testing123. This will definitely be a large string that is on the heap."
                |> Box.box
                |> Box.unbox

            a
            |> Num.toStr
            |> Str.concat b
        )
        "#
    ));
}
