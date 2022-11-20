//! Supports evaluating `expect` and printing contextual information when they fail.
#[cfg(not(windows))]
use {
    roc_intern::GlobalInterner,
    roc_module::symbol::Interns,
    roc_mono::{
        ir::ProcLayout,
        layout::{CapturesNiche, Layout, LayoutCache},
    },
    roc_parse::ast::Expr,
    roc_repl_eval::{eval::jit_to_ast, ReplAppMemory},
    roc_target::TargetInfo,
    roc_types::subs::{Subs, Variable},
    std::sync::Arc,
};

#[cfg(not(windows))]
mod app;
#[cfg(not(windows))]
pub mod run;

#[cfg(not(windows))]
use app::{ExpectMemory, ExpectReplApp};

#[cfg(not(windows))]
#[allow(clippy::too_many_arguments)]
pub fn get_values<'a>(
    target_info: TargetInfo,
    arena: &'a bumpalo::Bump,
    subs: &Subs,
    interns: &'a Interns,
    layout_interner: &Arc<GlobalInterner<'a, Layout<'a>>>,
    start: *const u8,
    start_offset: usize,
    variables: &[Variable],
) -> (usize, Vec<Expr<'a>>) {
    let mut result = Vec::with_capacity(variables.len());

    let memory = ExpectMemory { start };

    let app = ExpectReplApp {
        memory: arena.alloc(memory),
        offset: start_offset,
    };

    let app = arena.alloc(app);

    for (i, variable) in variables.iter().enumerate() {
        let start = app.memory.deref_usize(start_offset + i * 8);
        app.offset = start;

        let expr = {
            let variable = *variable;

            // TODO: pass layout_cache to jit_to_ast directly
            let mut layout_cache = LayoutCache::new(layout_interner.fork(), target_info);
            let layout = layout_cache.from_var(arena, variable, subs).unwrap();

            let proc_layout = ProcLayout {
                arguments: &[],
                result: layout,
                captures_niche: CapturesNiche::no_niche(),
            };

            jit_to_ast(
                arena,
                app,
                "expect_repl_main_fn",
                proc_layout,
                variable,
                subs,
                interns,
                layout_interner.fork(),
                target_info,
            )
        };

        result.push(expr);
    }

    (app.offset, result)
}

#[cfg(not(windows))]
#[cfg(test)]
mod test {
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use roc_gen_llvm::{llvm::build::LlvmBackendMode, run_roc::RocCallResult, run_roc_dylib};
    use roc_load::{ExecutionMode, LoadConfig, Threading};
    use roc_packaging::cache::RocCacheDir;
    use roc_reporting::report::{RenderTarget, DEFAULT_PALETTE};
    use target_lexicon::Triple;

    use crate::run::expect_mono_module_to_dylib;

    use super::*;

    fn run_expect_test(source: &str, expected: &str) {
        let arena = bumpalo::Bump::new();
        let arena = &arena;

        let triple = Triple::host();
        let target = &triple;

        let opt_level = roc_mono::ir::OptLevel::Normal;
        let target_info = TargetInfo::from(target);

        // Step 1: compile the app and generate the .o file
        let src_dir = tempfile::tempdir().unwrap();
        let filename = src_dir.path().join("Test.roc");

        std::fs::write(&filename, source).unwrap();

        let load_config = LoadConfig {
            target_info,
            render: RenderTarget::ColorTerminal,
            palette: DEFAULT_PALETTE,
            threading: Threading::Single,
            exec_mode: ExecutionMode::Test,
        };
        let loaded = roc_load::load_and_monomorphize_from_str(
            arena,
            filename,
            source,
            src_dir.path().to_path_buf(),
            Default::default(),
            RocCacheDir::Disallowed,
            load_config,
        )
        .unwrap();

        let mut loaded = loaded;
        let mut expectations = std::mem::take(&mut loaded.expectations);
        let loaded = loaded;

        let interns = loaded.interns.clone();

        let (lib, expects, layout_interner) = expect_mono_module_to_dylib(
            arena,
            target.clone(),
            loaded,
            opt_level,
            LlvmBackendMode::CliTest,
        )
        .unwrap();

        let arena = &bumpalo::Bump::new();
        let interns = arena.alloc(interns);

        const BUFFER_SIZE: usize = 1024;

        let mut shared_buffer = [0u8; BUFFER_SIZE];
        let mut memory = crate::run::ExpectMemory::from_slice(&mut shared_buffer);

        // communicate the mmapped name to zig/roc
        let set_shared_buffer = run_roc_dylib!(lib, "set_shared_buffer", (*mut u8, usize), ());
        let mut result = RocCallResult::default();
        unsafe { set_shared_buffer((shared_buffer.as_mut_ptr(), BUFFER_SIZE), &mut result) };

        let mut writer = Vec::with_capacity(1024);
        let (_failed, _passed) = crate::run::run_expects_with_memory(
            &mut writer,
            RenderTarget::ColorTerminal,
            arena,
            interns,
            &layout_interner.into_global(),
            &lib,
            &mut expectations,
            expects,
            &mut memory,
        )
        .unwrap();

        // Remove ANSI escape codes from the answer - for example:
        //
        //     Before: "42 \u{1b}[35m:\u{1b}[0m Num *"
        //     After:  "42 : Num *"
        let bytes = strip_ansi_escapes::strip(writer).unwrap();
        let actual = String::from_utf8(bytes).unwrap();

        if !actual.is_empty() {
            // trim off the first line; it contains a path in a tempdir that
            // changes between test runs
            let p = actual.bytes().position(|c| c == b'\n').unwrap();
            let (_, x) = actual.split_at(p);
            let x = x.trim();
            let expected = expected.trim_end();

            if x != expected {
                println!("{}", x);
            }

            assert_eq!(x, expected);
        } else {
            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn equals_pass() {
        run_expect_test(
            r#"
            app "test" provides [main] to "./platform"

            main = 0

            expect 1 == 1
            "#,
            "",
        );
    }

    #[test]
    fn equals_fail() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                expect 1 == 2
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                5│  expect 1 == 2
                    ^^^^^^^^^^^^^
                "#
            ),
        );
    }

    #[test]
    fn lookup_integer() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                expect
                    a = 1
                    b = 2

                    a == b
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                5│>  expect
                6│>      a = 1
                7│>      b = 2
                8│>
                9│>      a == b

                When it failed, these variables had these values:

                a : Num *
                a = 1

                b : Num *
                b = 2
                "#
            ),
        );
    }

    #[test]
    fn lookup_list_of_strings() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                expect
                    a = ["foo"]
                    b = ["a string so long that it cannot be short"]

                    a == b
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                5│>  expect
                6│>      a = ["foo"]
                7│>      b = ["a string so long that it cannot be short"]
                8│>
                9│>      a == b

                When it failed, these variables had these values:

                a : List Str
                a = ["foo"]

                b : List Str
                b = ["a string so long that it cannot be short"]
                "#
            ),
        );
    }

    #[test]
    fn lookup_list_of_list_of_strings() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                expect
                    a = [["foo"], []]
                    b = [["a string so long that it cannot be short", "bar"]]

                    a == b
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                5│>  expect
                6│>      a = [["foo"], []]
                7│>      b = [["a string so long that it cannot be short", "bar"]]
                8│>
                9│>      a == b

                When it failed, these variables had these values:

                a : List (List Str)
                a = [["foo"], []]

                b : List (List Str)
                b = [["a string so long that it cannot be short", "bar"]]
                "#
            ),
        );
    }

    #[test]
    fn lookup_copy_result() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                expect
                    items = [0, 1]
                    expected : Result I64 [OutOfBounds]
                    expected = Ok 42

                    List.get items 0 == expected
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                 5│>  expect
                 6│>      items = [0, 1]
                 7│>      expected : Result I64 [OutOfBounds]
                 8│>      expected = Ok 42
                 9│>
                10│>      List.get items 0 == expected

                When it failed, these variables had these values:

                items : List (Num *)
                items = [0, 1]

                expected : Result I64 [OutOfBounds]
                expected = Ok 42
                "#
            ),
        );
    }

    #[test]
    fn lookup_clone_result() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                expect
                    a : Result Str Str
                    a = Ok "foo"

                    b : Result Str Str
                    b = Err "bar"

                    a == b
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                 5│>  expect
                 6│>      a : Result Str Str
                 7│>      a = Ok "foo"
                 8│>
                 9│>      b : Result Str Str
                10│>      b = Err "bar"
                11│>
                12│>      a == b

                When it failed, these variables had these values:

                a : Result Str Str
                a = Ok "foo"

                b : Result Str Str
                b = Err "bar"
                "#
            ),
        );
    }

    #[test]
    fn lookup_copy_record() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                expect
                    vec1 = { x: 1.0, y: 2.0 }
                    vec2 = { x: 4.0, y: 8.0 }

                    vec1 == vec2
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                5│>  expect
                6│>      vec1 = { x: 1.0, y: 2.0 }
                7│>      vec2 = { x: 4.0, y: 8.0 }
                8│>
                9│>      vec1 == vec2

                When it failed, these variables had these values:

                vec1 : { x : Frac *, y : Frac * }
                vec1 = { x: 1, y: 2 }

                vec2 : { x : Frac *, y : Frac * }
                vec2 = { x: 4, y: 8 }
                "#
            ),
        );
    }

    #[test]
    fn two_strings() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                expect
                    strings = ["Astra mortemque praestare gradatim", "Profundum et fundamentum"]

                    strings == []
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                5│>  expect
                6│>      strings = ["Astra mortemque praestare gradatim", "Profundum et fundamentum"]
                7│>
                8│>      strings == []

                When it failed, these variables had these values:

                strings : List Str
                strings = ["Astra mortemque praestare gradatim", "Profundum et fundamentum"]
                "#
            ),
        );
    }

    #[test]
    fn compare_long_strings() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                expect
                    a = "Astra mortemque praestare gradatim"
                    b = "Profundum et fundamentum"

                    a == b
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                5│>  expect
                6│>      a = "Astra mortemque praestare gradatim"
                7│>      b = "Profundum et fundamentum"
                8│>
                9│>      a == b

                When it failed, these variables had these values:

                a : Str
                a = "Astra mortemque praestare gradatim"

                b : Str
                b = "Profundum et fundamentum"
                "#
            ),
        );
    }

    #[test]
    fn struct_with_strings() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                expect
                    a = {
                        utopia: "Astra mortemque praestare gradatim",
                        brillist: "Profundum et fundamentum",
                    }

                    a != a
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                 5│>  expect
                 6│>      a = {
                 7│>          utopia: "Astra mortemque praestare gradatim",
                 8│>          brillist: "Profundum et fundamentum",
                 9│>      }
                10│>
                11│>      a != a

                When it failed, these variables had these values:

                a : { brillist : Str, utopia : Str }
                a = { brillist: "Profundum et fundamentum", utopia: "Astra mortemque praestare gradatim" }
                "#
            ),
        );
    }

    #[test]
    fn box_with_strings() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                expect
                    a = Box.box "Astra mortemque praestare gradatim"
                    b = Box.box "Profundum et fundamentum"

                    a == b
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                5│>  expect
                6│>      a = Box.box "Astra mortemque praestare gradatim"
                7│>      b = Box.box "Profundum et fundamentum"
                8│>
                9│>      a == b

                When it failed, these variables had these values:

                a : Box Str
                a = Box.box "Astra mortemque praestare gradatim"

                b : Box Str
                b = Box.box "Profundum et fundamentum"
                "#
            ),
        );
    }

    #[test]
    fn result_with_strings() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                expect
                    a = Ok "Astra mortemque praestare gradatim"
                    b = Err "Profundum et fundamentum"

                    a == b
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                5│>  expect
                6│>      a = Ok "Astra mortemque praestare gradatim"
                7│>      b = Err "Profundum et fundamentum"
                8│>
                9│>      a == b

                When it failed, these variables had these values:

                a : [Ok Str]
                a = Ok "Astra mortemque praestare gradatim"

                b : [Err Str]
                b = Err "Profundum et fundamentum"
                "#
            ),
        );
    }

    #[test]
    fn linked_list() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                ConsList a : [ Nil, Cons a (ConsList a) ]

                cons = \list, x -> Cons x list

                expect
                    a : ConsList Str
                    a = Nil

                    b : ConsList Str
                    b = Nil
                        |> cons "Astra mortemque praestare gradatim"
                        |> cons "Profundum et fundamentum"

                    a == b
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                 9│>  expect
                10│>      a : ConsList Str
                11│>      a = Nil
                12│>
                13│>      b : ConsList Str
                14│>      b = Nil
                15│>          |> cons "Astra mortemque praestare gradatim"
                16│>          |> cons "Profundum et fundamentum"
                17│>
                18│>      a == b

                When it failed, these variables had these values:

                a : ConsList Str
                a = Nil

                b : ConsList Str
                b = Cons "Profundum et fundamentum" (Cons "Astra mortemque praestare gradatim" Nil)
                "#
            ),
        );
    }

    #[test]
    fn nullable_tree() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                Tree a : [ Empty, Leaf a, Node (Tree a) (Tree a) ]

                cons = \list, x -> Cons x list

                expect
                    a : Tree Str
                    a = Leaf "Astra mortemque praestare gradatim"

                    b : Tree Str
                    b = Node Empty Empty

                    a == b
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                 9│>  expect
                10│>      a : Tree Str
                11│>      a = Leaf "Astra mortemque praestare gradatim"
                12│>
                13│>      b : Tree Str
                14│>      b = Node Empty Empty
                15│>
                16│>      a == b

                When it failed, these variables had these values:

                a : Tree Str
                a = Leaf "Astra mortemque praestare gradatim"

                b : Tree Str
                b = Node Empty Empty
                "#
            ),
        );
    }

    #[test]
    fn recursive_tree() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                Tree a : [ Leaf a, Node (Tree a) (Tree a) ]

                expect
                    a : Tree Str
                    a = Leaf "Astra mortemque praestare gradatim"

                    b : Tree Str
                    b = Node (Leaf "a") (Leaf "b")

                    a == b
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                 7│>  expect
                 8│>      a : Tree Str
                 9│>      a = Leaf "Astra mortemque praestare gradatim"
                10│>
                11│>      b : Tree Str
                12│>      b = Node (Leaf "a") (Leaf "b")
                13│>
                14│>      a == b

                When it failed, these variables had these values:

                a : Tree Str
                a = Leaf "Astra mortemque praestare gradatim"

                b : Tree Str
                b = Node (Leaf "a") (Leaf "b")
                "#
            ),
        );
    }

    #[test]
    fn rose_tree() {
        run_expect_test(
            indoc!(
                r#"
                app "test" provides [main] to "./platform"

                main = 0

                RoseTree a : [Tree a (List (RoseTree a))]

                expect
                    a : RoseTree Str
                    a = Tree "Astra mortemque praestare gradatim" []

                    b : RoseTree Str
                    b = Tree "foo" [ Tree "bar" [] ]

                    a == b
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                 7│>  expect
                 8│>      a : RoseTree Str
                 9│>      a = Tree "Astra mortemque praestare gradatim" []
                10│>
                11│>      b : RoseTree Str
                12│>      b = Tree "foo" [ Tree "bar" [] ]
                13│>
                14│>      a == b

                When it failed, these variables had these values:

                a : RoseTree Str
                a = Tree "Astra mortemque praestare gradatim" []

                b : RoseTree Str
                b = Tree "foo" [Tree "bar" []]
                "#
            ),
        );
    }

    #[test]
    fn big_recursive_tag_copied_back() {
        run_expect_test(
            indoc!(
                r#"
                interface Test exposes [] imports []

                NonEmpty := [
                    First Str U8,
                    Next (List { item: Str, rest: NonEmpty }),
                ]

                expect
                    nonEmpty =
                        a = "abcdefgh"
                        b = @NonEmpty (First "ijkl" 67u8)
                        c = Next [{ item: a, rest: b }]
                        @NonEmpty c

                    when nonEmpty is
                        _ -> Bool.false
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                 8│>  expect
                 9│>      nonEmpty =
                10│>          a = "abcdefgh"
                11│>          b = @NonEmpty (First "ijkl" 67u8)
                12│>          c = Next [{ item: a, rest: b }]
                13│>          @NonEmpty c
                14│>
                15│>      when nonEmpty is
                16│>          _ -> Bool.false

                When it failed, these variables had these values:

                nonEmpty : NonEmpty
                nonEmpty = @NonEmpty (Next [{ item: "abcdefgh", rest: @NonEmpty (First "ijkl" 67) }])
                "#
            ),
        );
    }

    #[test]
    fn arg_parser() {
        run_expect_test(
            indoc!(
                r#"
                interface Test exposes [] imports []

                makeForcer : {} -> (Str -> U8)
                makeForcer = \{} -> \_ -> 2u8

                expect
                    forcer = makeForcer {}

                    case = ""

                    forcer case == 5u8
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                 6│>  expect
                 7│>      forcer = makeForcer {}
                 8│>
                 9│>      case = ""
                10│>
                11│>      forcer case == 5u8

                When it failed, these variables had these values:

                case : Str
                case = ""
                "#
            ),
        );
    }

    #[test]
    fn issue_i4389() {
        run_expect_test(
            indoc!(
                r#"
                interface Test exposes [] imports []

                expect
                    totalCount = \{} -> 1u8
                    totalCount {} == 96u8
                "#
            ),
            indoc!(
                r#"
                This expectation failed:

                3│>  expect
                4│>      totalCount = \{} -> 1u8
                5│>      totalCount {} == 96u8
                "#
            ),
        );
    }
}
