#![cfg(test)]

use std::process::Command;

use indoc::indoc;

#[cfg(target_os = "linux")]
static BUILD_ONCE: std::sync::Once = std::sync::Once::new();

#[allow(dead_code)]
const ZIG_PLATFORM_DIR: &str = "crates/valgrind_tests/zig-platform";

#[cfg(target_os = "linux")]
fn build_host() {
    let platform_main_roc = get_platform_main_roc_path();

    // tests always run on the host
    let target = target_lexicon::Triple::host().into();

    // valgrind does not support avx512 yet: https://bugs.kde.org/show_bug.cgi?id=383010
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    if is_x86_feature_detected!("avx512f") {
        std::env::set_var("NO_AVX512", "1");
    }

    let stub_dll_symbols = roc_linker::ExposedSymbols {
        top_level_values: vec![String::from("main_for_host")],
        exported_closure_types: vec![],
    }
    .stub_dll_symbols();

    let opt_level = roc_mono::ir::OptLevel::Normal;

    let stub_lib = roc_linker::generate_stub_lib_from_loaded(
        target,
        platform_main_roc.as_path(),
        stub_dll_symbols.as_slice(),
    );

    debug_assert!(stub_lib.exists());

    let host_dest = roc_build::link::rebuild_host(
        opt_level,
        target,
        platform_main_roc.as_path(),
        Some(&stub_lib),
    );

    let preprocessed_path = platform_main_roc.with_file_name(target.prebuilt_surgical_host());
    let metadata_path = platform_main_roc.with_file_name(target.metadata_file_name());

    roc_linker::preprocess_host(
        target,
        host_dest.as_path(),
        metadata_path.as_path(),
        preprocessed_path.as_path(),
        &stub_lib,
        false,
        false,
    )
}

fn valgrind_test(source: &str) {
    copy_zig_glue::initialize_zig_test_platforms();

    #[cfg(target_os = "linux")]
    {
        valgrind_test_linux(source)
    }

    #[cfg(not(target_os = "linux"))]
    {
        let _ = source;
    }
}

// used in linux valgrind tests
#[allow(dead_code)]
fn get_platform_main_roc_path() -> std::path::PathBuf {
    let zig_platform_dir = roc_command_utils::root_dir().join(ZIG_PLATFORM_DIR);

    assert!(
        zig_platform_dir.exists(),
        "zig-platform directory does not exist: {:?}\n\tDid you change its name?",
        zig_platform_dir
    );

    let pf_main_roc_path = zig_platform_dir.join("main.roc");

    assert!(
        pf_main_roc_path.exists(),
        "Cannot find platform main.roc at {:?}",
        &pf_main_roc_path
    );

    pf_main_roc_path
}

#[cfg(target_os = "linux")]
fn valgrind_test_linux(source: &str) {
    use roc_build::program::BuiltFile;

    // the host is identical for all tests so we only want to build it once
    BUILD_ONCE.call_once(build_host);

    let pf_main_roc_path = get_platform_main_roc_path();

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
            pf_main_roc_path.to_str().unwrap()
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
        app_module_source = app_module_source.replace(
            "replace_me_platform_path",
            &pf_main_roc_path.display().to_string(),
        );
    }

    let temp_dir = tempfile::tempdir().unwrap();
    let app_module_path = temp_dir.path().join("app.roc");

    let arena = bumpalo::Bump::new();
    let build_host_requested = false;
    let res_binary_path = roc_build::program::build_str_test(
        &arena,
        &app_module_path,
        &app_module_source,
        build_host_requested,
    );

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
            roc_load::LoadingProblem::FormattedReport(report, _),
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
    use cli_test_utils::command::run_command;

    // If possible, report the generated executable name relative to the current dir.
    let binary_filename_only = binary_path
        .strip_prefix(std::env::current_dir().unwrap())
        .unwrap_or(binary_path)
        .to_str()
        .unwrap();

    let mut valgrind_command = Command::new("valgrind");

    valgrind_command.args([
        "--leak-check=full",
        "--error-exitcode=1",
        "--errors-for-leak-kinds=definite,possible",
        binary_filename_only,
    ]);

    let valgrind_out = run_command(valgrind_command, None);

    valgrind_out.assert_zero_exit();
}

#[test]
fn list_concat_consumes_first_argument() {
    valgrind_test("List.concat (List.with_capacity 1024) [1,2,3] |> List.len |> Num.to_str");
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
            |> Num.to_str
        )
        "
    ));
}

#[test]
fn str_capacity_concat() {
    valgrind_test(r#"Str.with_capacity 42 |> Str.concat "foobar""#);
}

#[test]
fn split_not_present() {
    valgrind_test(indoc!(
        r#"
        Str.split_on (Str.concat "a string that is stored on the heap" "!") "\n"
            |> Str.join_with ""
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
            |> Num.to_str
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
            out = str |> Str.trim_end

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
            out = str |> Str.trim_start

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
            a = List.with_capacity 1

            List.concat a [58]
            |> List.len
            |> Num.add_wrap (List.len a)
            |> Num.to_str
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

            make_sound : Animal -> Str
            make_sound = \animal ->
                dog_sound = "Woof"
                when animal is
                    Cat | Dog if is_cat animal -> "Miauw"
                    Goose -> "Honk"
                    _ -> dog_sound

            is_cat : Animal -> Bool
            is_cat = \animal ->
                when animal is
                    Cat -> Bool.true
                    _ -> Bool.false

            test =
                cat_sound = make_sound Cat
                dog_sound = make_sound Dog
                goose_sound = make_sound Goose
                "Cat: ${cat_sound}, Dog: ${dog_sound}, Goose: ${goose_sound}"

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

            # map_linked_list : LinkedList a, (a -> b) -> LinkedList b
            map_linked_list = \linked_list, f -> when linked_list is
                Nil -> Nil
                Cons x xs ->
                    x2 = if Bool.true then x else x
                    Cons (f x2) (map_linked_list xs f)

            # print_linked_list : LinkedList a, (a -> Str) -> Str
            print_linked_list = \linked_list, f ->
                when linked_list is
                Nil -> "Nil"
                Cons x xs ->
                    str_x = f x
                    str_xs = print_linked_list xs f
                    "Cons ${str_x} (${str_xs})"

            test =
                new_list = map_linked_list (Cons 1 (Cons 2 (Cons 3 Nil))) (\x -> x + 1)
                print_linked_list new_list Num.to_str

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
            when insert_help key value dict is
                Node Red k v l r -> Node Black k v l r
                x -> x

        insert_help : Key k, v, RedBlackTree (Key k) v -> RedBlackTree (Key k) v
        insert_help = \key, value, dict ->
            when dict is
                Empty ->
                    # New nodes are always red. If it violates the rules, it will be fixed
                    # when balancing.
                    Node Red key value Empty Empty

                Node n_color n_key n_value n_left n_right ->
                    when Num.compare key n_key is
                        LT -> balance n_color n_key n_value (insert_help key value n_left) n_right
                        EQ -> Node n_color n_key value n_left n_right
                        GT -> balance n_color n_key n_value n_left (insert_help key value n_right)

        balance : NodeColor, k, v, RedBlackTree k v, RedBlackTree k v -> RedBlackTree k v
        balance = \color, key, value, left, right ->
            when right is
                Node Red r_k r_v r_left r_right ->
                    when left is
                        Node Red l_k l_v l_left l_right ->
                            Node
                                Red
                                key
                                value
                                (Node Black l_k l_v l_left l_right)
                                (Node Black r_k r_v r_left r_right)

                        _ ->
                            Node color r_k r_v (Node Red key value left r_left) r_right

                _ ->
                    when left is
                        Node Red l_k l_v (Node Red ll_k ll_v ll_left ll_right) l_right ->
                            Node
                                Red
                                l_k
                                l_v
                                (Node Black ll_k ll_v ll_left ll_right)
                                (Node Black key value l_right right)

                        _ ->
                            Node color key value left right


        show : RedBlackTree I64 {} -> Str
        show = \tree -> show_rb_tree tree Num.to_str (\{} -> "{}")

        show_rb_tree : RedBlackTree k v, (k -> Str), (v -> Str) -> Str
        show_rb_tree = \tree, show_key, show_value ->
            when tree is
                Empty -> "Empty"
                Node color key value left right ->
                    s_color = show_color color
                    s_key = show_key key
                    s_value = show_value value
                    s_l = node_in_parens left show_key show_value
                    s_r = node_in_parens right show_key show_value

                    "Node ${s_color} ${s_key} ${s_value} ${s_l} ${s_r}"

        node_in_parens : RedBlackTree k v, (k -> Str), (v -> Str) -> Str
        node_in_parens = \tree, show_key, show_value ->
            when tree is
                Empty ->
                    show_rb_tree tree show_key show_value

                Node _ _ _ _ _ ->
                    inner = show_rb_tree tree show_key show_value

                    "(${inner})"

        show_color : NodeColor -> Str
        show_color = \color ->
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
            e = List.sort_with d (\x, y -> Num.compare x y)

            Num.to_str (List.len e)
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

            print_linked_list : LinkedList Str -> Str
            print_linked_list = \linked_list->
                when linked_list is
                    Nil -> "Nil"
                    Cons x xs ->
                        str_xs = print_linked_list xs
                        "Cons ${x} (${str_xs})"

            linked_list_head : LinkedList Str -> LinkedList Str
            linked_list_head = \linked_list ->
                string = when linked_list is
                    Cons s _ -> s
                    Nil -> ""
                Cons string Nil

            test =
                cons = print_linked_list (linked_list_head (Cons "foo" Nil))
                nil = print_linked_list (linked_list_head (Nil))
                "${cons} - ${nil}"

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
            |> Num.to_str
            |> Str.concat b
        )
        "#
    ));
}

#[test]
fn joinpoint_that_owns() {
    valgrind_test(indoc!(
        r#"
        (
        write_indents = \buf, indents ->
            if indents <= 0 then
                buf
            else
                buf
                |> Str.concat "    "
                |> write_indents (indents - 1)

        List.walk [{}, {}] "" \accum, {} -> accum |> write_indents 4
        )
        "#
    ));
}
