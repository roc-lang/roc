#![cfg(test)]
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
// we actually want to compare against the literal float bits
#![allow(clippy::float_cmp)]

/// Used in the with_larger_debug_stack() function, for tests that otherwise
/// run out of stack space in debug builds (but don't in --release builds)
#[allow(dead_code)]
const EXPANDED_STACK_SIZE: usize = 8 * 1024 * 1024;

use bumpalo::Bump;
use indoc::{formatdoc, indoc};
use roc_collections::all::MutMap;
use roc_load::ExecutionMode;
use roc_load::FunctionKind;
use roc_load::LoadConfig;
use roc_load::LoadMonomorphizedError;
use roc_load::Threading;
use roc_module::symbol::Interns;
use roc_module::symbol::Symbol;
use roc_mono::ir::Proc;
use roc_mono::ir::ProcLayout;
use roc_mono::layout::STLayoutInterner;
use roc_test_utils::TAG_LEN_ENCODER_FMT;
use test_mono_macros::*;

const TARGET: roc_target::Target = roc_target::Target::LinuxX64;

/// err decoder is a trivial implementation of a decoder which only returns an error
/// useful when you need a decoder implementation, but want minimal code generation
pub const ERR_DECODER_FMT: &str = r#"
ErrDecoder := {} implements [
        DecoderFormatting {
            u8: decode_u8,
            u16: decode_u16,
            u32: decode_u32,
            u64: decode_u64,
            u128: decode_u128,
            i8: decode_i8,
            i16: decode_i16,
            i32: decode_i32,
            i64: decode_i64,
            i128: decode_i128,
            f32: decode_f32,
            f64: decode_f64,
            dec: decode_dec,
            bool: decode_bool,
            string: decode_string,
            list: decode_list,
            record: decode_record,
            tuple: decode_tuple,
        },
    ]
decode_u8 = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_u16 = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_u32 = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_u64 = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_u128 = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_i8 = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_i16 = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_i32 = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_i64 = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_i128 = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_f32 = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_f64 = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_dec = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_bool = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_string = Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_list : Decoder elem ErrDecoder -> Decoder (List elem) ErrDecoder
decode_list = \_ -> Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_record : state, (state, Str -> [Keep (Decoder state ErrDecoder), Skip]), (state, ErrDecoder -> Result val DecodeError) -> Decoder val ErrDecoder
decode_record = \_, _, _ -> Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
decode_tuple : state, (state, U64 -> [Next (Decoder state ErrDecoder), TooLong]), (state -> Result val DecodeError) -> Decoder val ErrDecoder
decode_tuple = \_, _, _ -> Decode.custom \rest, @ErrDecoder {} -> { result: Err TooShort, rest }
"#;

/// Without this, some tests pass in `cargo test --release` but fail without
/// the --release flag because they run out of stack space. This increases
/// stack size for debug builds only, while leaving the stack space at the default
/// amount for release builds.
#[allow(dead_code)]
#[cfg(debug_assertions)]
pub fn with_larger_debug_stack<F>(run_test: F)
where
    F: FnOnce(),
    F: Send,
    F: 'static,
{
    std::thread::Builder::new()
        .stack_size(EXPANDED_STACK_SIZE)
        .spawn(run_test)
        .expect("Error while spawning expanded dev stack size thread")
        .join()
        .expect("Error while joining expanded dev stack size thread")
}

/// In --release builds, don't increase the stack size. Run the test normally.
/// This way, we find out if any of our tests are blowing the stack even after
/// optimizations in release builds.
#[allow(dead_code)]
#[cfg(not(debug_assertions))]
#[inline(always)]
pub fn with_larger_debug_stack<F>(run_test: F)
where
    F: FnOnce(),
    F: Send,
    F: 'static,
{
    run_test()
}

fn promote_expr_to_module(src: &str) -> String {
    let mut buffer = String::from("app \"test\" provides [main] to \"./platform\"\n\nmain =\n");

    for line in src.lines() {
        // indent the body!
        buffer.push_str("    ");
        buffer.push_str(line);
        buffer.push('\n');
    }

    buffer
}

fn compiles_to_ir(test_name: &str, src: &str, mode: &str, allow_type_errors: bool, no_check: bool) {
    use roc_packaging::cache::RocCacheDir;
    use std::path::PathBuf;

    let exec_mode = match mode {
        "exec" => ExecutionMode::Executable,
        "test" => ExecutionMode::Test,
        _ => panic!("Invalid test_mono exec mode {mode}"),
    };

    let arena = &Bump::new();

    let filename = PathBuf::from("Test.roc");
    let src_dir = PathBuf::from("fake/test/path");

    let module_src;
    let temp;
    if src.starts_with("app") || src.starts_with("interface") {
        // this is already a module
        module_src = src;
    } else {
        // this is an expression, promote it to a module
        temp = promote_expr_to_module(src);
        module_src = &temp;
    }

    let load_config = LoadConfig {
        target: TARGET,
        // TODO parameterize
        function_kind: FunctionKind::LambdaSet,
        threading: Threading::Single,
        render: roc_reporting::report::RenderTarget::Generic,
        palette: roc_reporting::report::DEFAULT_PALETTE,
        exec_mode,
    };
    let loaded = roc_load::load_and_monomorphize_from_str(
        arena,
        filename,
        module_src,
        src_dir,
        None,
        RocCacheDir::Disallowed,
        load_config,
    );

    let mut loaded = match loaded {
        Ok(x) => x,
        Err(LoadMonomorphizedError::LoadingProblem(roc_load::LoadingProblem::FormattedReport(
            report,
            _,
        ))) => {
            println!("{report}");
            panic!();
        }
        Err(e) => panic!("{e:?}"),
    };

    use roc_load::MonomorphizedModule;
    let MonomorphizedModule {
        module_id: home,
        procedures,
        exposed_to_host,
        mut layout_interner,
        interns,
        ..
    } = loaded;

    let can_problems = loaded.can_problems.remove(&home).unwrap_or_default();
    let type_problems = loaded.type_problems.remove(&home).unwrap_or_default();

    if !can_problems.is_empty() {
        println!("Ignoring {} canonicalization problems", can_problems.len());
    }

    if !(allow_type_errors || type_problems.is_empty()) {
        panic!("mono test has type problems:\n\n{:#?}", type_problems);
    }

    let main_fn_symbol = exposed_to_host.top_level_values.keys().copied().next();

    if !no_check {
        check_procedures(arena, &interns, &mut layout_interner, &procedures);
    }

    verify_procedures(test_name, layout_interner, procedures, main_fn_symbol);
}

fn check_procedures<'a>(
    arena: &'a Bump,
    interns: &Interns,
    interner: &mut STLayoutInterner<'a>,
    procedures: &MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) {
    use roc_mono::debug::{check_procs, format_problems};
    let problems = check_procs(arena, interner, procedures);
    if problems.is_empty() {
        return;
    }
    let formatted = format_problems(interns, interner, problems);
    panic!("IR problems found:\n{formatted}");
}

fn verify_procedures<'a>(
    test_name: &str,
    interner: STLayoutInterner<'a>,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
    opt_main_fn_symbol: Option<Symbol>,
) {
    let mut procs_string = procedures
        .values()
        .map(|proc| proc.to_pretty(&interner, 200, false))
        .collect::<Vec<_>>();

    let opt_main_fn = opt_main_fn_symbol.map(|main_fn_symbol| {
        let index = procedures
            .keys()
            .position(|(s, _)| *s == main_fn_symbol)
            .unwrap();
        procs_string.swap_remove(index)
    });

    procs_string.sort();

    if let Some(main_fn) = opt_main_fn {
        procs_string.push(main_fn);
    }

    let result = procs_string.join("\n");

    let path = format!("generated/{test_name}.txt");
    std::fs::create_dir_all("generated").unwrap();
    std::fs::write(&path, result).unwrap();

    use std::process::Command;

    let is_tracked = Command::new("git")
        .args(["ls-files", "--error-unmatch", &path])
        .output()
        .unwrap();

    if !is_tracked.status.success() {
        panic!(
            "The file {:?} is not tracked by git. Try using `git add` on it",
            &path
        );
    }

    let has_changes = Command::new("git")
        .args(["diff", "--color=always", &path])
        .output()
        .unwrap();

    if !has_changes.status.success() {
        eprintln!("`git diff {:?}` failed", &path);
        unreachable!();
    }

    if !has_changes.stdout.is_empty() {
        println!("{}", std::str::from_utf8(&has_changes.stdout).unwrap());
        panic!(indoc!(
            r#"

            Mono output has changed! This is normal when making changes to the builtins.
            To fix it; run these commands locally:

                cargo test -p test_mono -p uitest --no-fail-fast
                git add -u
                git commit -S -m "update mono tests"
                git push origin YOUR_BRANCH_NAME

            "#
        ));
    }
}

#[mono_test]
fn ir_int_literal() {
    r"
    5
    "
}

#[mono_test]
fn ir_int_add() {
    r"
    x = [1,2]
    5 + 4 + 3 + List.len x
    "
}

#[mono_test]
fn ir_assignment() {
    r"
    x = 5

    x
    "
}

#[mono_test]
fn ir_when_maybe() {
    r"
    when Just 3 is
        Just n -> n
        Nothing -> 0
    "
}

#[mono_test]
fn ir_when_these() {
    r"
    when These 1 2 is
        This x -> x
        That y -> y
        These x _ -> x
    "
}

#[mono_test]
fn ir_when_record() {
    r"
    when { x: 1, y: 3.14 } is
        { x } -> x
    "
}

#[mono_test]
fn ir_plus() {
    r"
    1 + 2
    "
}

#[mono_test]
fn ir_round() {
    r"
    Num.round 3.6
    "
}

#[mono_test]
fn ir_when_idiv() {
    r"
    when Num.div_trunc_checked 1000 10 is
        Ok val -> val
        Err _ -> -1
    "
}

#[mono_test]
fn ir_two_defs() {
    r"
    x = 3
    y = 4

    x + y
    "
}

#[mono_test]
fn ir_when_just() {
    r"
    x : [Nothing, Just I64]
    x = Just 41

    when x is
        Just v -> v + 0x1
        Nothing -> 0x1
    "
}

#[mono_test]
fn one_element_tag() {
    r"
    x : [Pair I64]
    x = Pair 2

    x
    "
}

#[mono_test]
fn guard_pattern_true() {
    r"
    wrapper = \{} ->
        when 2 is
            2 if Bool.false -> 42
            _ -> 0

    wrapper {}
    "
}

#[mono_test]
fn when_on_record() {
    r"
    when { x: 0x2 } is
        { x } -> x + 3
    "
}

#[mono_test]
fn when_nested_maybe() {
    r"
    Maybe a : [Nothing, Just a]

    x : Maybe (Maybe I64)
    x = Just (Just 41)

    when x is
        Just (Just v) -> v + 0x1
        _ -> 0x1
    "
}

#[mono_test]
fn when_on_two_values() {
    r"
    when Pair 2 3 is
        Pair 4 3 -> 9
        Pair a b -> a + b
    "
}

#[mono_test]
fn dict() {
    r"
    Dict.len (Dict.empty {})
    "
}

#[mono_test]
fn list_append_closure() {
    r"
    my_function = \l -> List.append l 42

    my_function [1, 2]
    "
}

#[mono_test]
fn list_append() {
    // TODO this leaks at the moment
    // ListAppend needs to decrement its arguments
    r"
    List.append [1] 2
    "
}

#[mono_test]
fn list_len() {
    r"
    x = [1,2,3]
    y = [1.0]

    List.len x + List.len y
    "
}

#[mono_test]
fn when_joinpoint() {
    r"
    wrapper = \{} ->
        x : [Red, White, Blue]
        x = Blue

        y =
            when x is
                Red -> 1
                White -> 2
                Blue -> 3

        y

    wrapper {}
    "
}

#[mono_test]
fn simple_if() {
    r"
    if Bool.true then
        1
    else
        2
    "
}

#[mono_test]
fn if_multi_branch() {
    r"
    if Bool.true then
        1
    else if Bool.false then
        2
    else
        3
    "
}

#[mono_test]
fn when_on_result() {
    r"
    wrapper = \{} ->
        x : Result I64 I64
        x = Ok 2

        y =
            when x is
                Ok 3 -> 1
                Ok _ -> 2
                Err _ -> 3
        y

    wrapper {}
    "
}

#[mono_test]
fn let_with_record_pattern() {
    r"
    { x } = { x: 0x2, y: 3.14 }

    x
    "
}

#[mono_test]
fn let_with_record_pattern_list() {
    r"
    { x } = { x: [1, 3, 4], y: 3.14 }

    x
    "
}

#[mono_test]
fn if_guard_bind_variable_false() {
    r"
    wrapper = \{} ->
        when 10 is
            x if x == 5 -> 0
            _ -> 42

    wrapper {}
    "
}

#[mono_test]
fn alias_variable() {
    r"
    x = 5
    y = x

    3
    "
}

#[mono_test]
fn alias_variable_and_return_it() {
    r"
    x = 5
    y = x

    y
    "
}

#[mono_test]
fn branch_store_variable() {
    r"
    when 0 is
        1 -> 12
        a -> a
    "
}

#[mono_test]
fn list_pass_to_function() {
    r"
    x : List I64
    x = [1,2,3]

    id : List I64 -> List I64
    id = \y -> List.set y 0 0

    id x
    "
}

#[mono_test]
fn record_optional_field_let_no_use_default() {
    r"
    f = \r ->
        { x ? 10, y } = r
        x + y


    f { x: 4, y: 9 }
    "
}

#[mono_test]
fn record_optional_field_let_use_default() {
    r"
    f = \r ->
        { x ? 10, y } = r
        x + y


    f { y: 9 }
    "
}

#[mono_test]
fn record_optional_field_function_no_use_default() {
    r"
    f = \{ x ? 10, y } -> x + y


    f { x: 4, y: 9 }
    "
}

#[mono_test]
fn record_optional_field_function_use_default() {
    r"
    f = \{ x ? 10, y } -> x + y


    f { y: 9 }
    "
}

#[mono_test]
fn record_as_pattern_in_closure_arg() {
    r"
    f = \{x, y, w, h} -> (x + w, y + h)

    g = \({ x, y } as box) ->
        (right, bottom) = f box
        (x, y, right, bottom)

    g { x: 1, y: 2, w: 3, h: 4 }
    "
}

#[mono_test]
fn opaque_as_pattern_in_closure_arg() {
    r"
    Opaque := U64

    f = \(@Opaque x) -> x * 2
    g = \(@Opaque x as s) -> (x, f s)

    g (@Opaque 42)
    "
}

#[mono_test]
fn quicksort_help() {
    // do we still need with_larger_debug_stack?
    r"
    quicksort_help : List (Num a), I64, I64 -> List (Num a)
    quicksort_help = \list, low, high ->
        if low < high then
            (Pair partition_index partitioned) = Pair 0 []

            partitioned
            |> quicksort_help low (partition_index - 1)
            |> quicksort_help (partition_index + 1) high
        else
            list

    quicksort_help [] 0 0
    "
}

#[mono_test]
fn quicksort_swap() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        swap = \list ->
            when Pair (List.get list 0) (List.get list 0) is
                Pair (Ok at_i) (Ok at_j) ->
                    list
                    |> List.set 0 at_j
                    |> List.set 0 at_i

                _ ->
                    []

        main =
            swap [1, 2]
        "#
    )
}

// #[ignore]
// #[mono_test]
// fn quicksort_partition_help() {
//     indoc!(
//         r#"
//         app "test" provides [main] to "./platform"

//         partition_help : I64, I64, List (Num a), I64, (Num a) -> [Pair I64 (List (Num a))]
//         partition_help = \i, j, list, high, pivot ->
//             if j < high then
//                 when List.get list j is
//                     Ok value ->
//                         if value <= pivot then
//                             partition_help (i + 1) (j + 1) (swap (i + 1) j list) high pivot
//                         else
//                             partition_help i (j + 1) list high pivot

//                     Err _ ->
//                         Pair i list
//             else
//                 Pair i list

//         main =
//             partition_help 0 0 [] 0 0
//         "#
//     )
// }

// #[ignore]
// #[mono_test]
// fn quicksort_full() {
//     indoc!(
//         r#"
//         app "test" provides [main] to "./platform"

//         quicksort_help : List (Num a), I64, I64 -> List (Num a)
//         quicksort_help = \list, low, high ->
//             if low < high then
//                 (Pair partition_index partitioned) = partition low high list

//                 partitioned
//                     |> quicksort_help low (partition_index - 1)
//                     |> quicksort_help (partition_index + 1) high
//             else
//                 list

//         swap : I64, I64, List a -> List a
//         swap = \i, j, list ->
//             when Pair (List.get list i) (List.get list j) is
//                 Pair (Ok at_i) (Ok at_j) ->
//                     list
//                         |> List.set i at_j
//                         |> List.set j at_i

//                 _ ->
//                     []

//         partition : I64, I64, List (Num a) -> [Pair I64 (List (Num a))]
//         partition = \low, high, initial_list ->
//             when List.get initial_list high is
//                 Ok pivot ->
//                     when partition_help (low - 1) low initial_list high pivot is
//                         Pair new_i newList ->
//                             Pair (new_i + 1) (swap (new_i + 1) high newList)

//                 Err _ ->
//                     Pair (low - 1) initial_list

//         partition_help : I64, I64, List (Num a), I64, (Num a) -> [Pair I64 (List (Num a))]
//         partition_help = \i, j, list, high, pivot ->
//             if j < high then
//                 when List.get list j is
//                     Ok value ->
//                         if value <= pivot then
//                             partition_help (i + 1) (j + 1) (swap (i + 1) j list) high pivot
//                         else
//                             partition_help i (j + 1) list high pivot

//                     Err _ ->
//                         Pair i list
//             else
//                 Pair i list

//         quicksort = \original_list ->
//             n = List.len original_list
//             quicksort_help original_list 0 (n - 1)

//         main =
//             quicksort [1,2,3]
//         "#
//     )
// }

#[mono_test]
fn factorial() {
    r"
    factorial = \n, accum ->
        when n is
            0 ->
                accum

            _ ->
                factorial (n - 1) (n * accum)

    factorial 10 1
    "
}

#[mono_test]
fn is_nil() {
    r"
    ConsList a : [Cons a (ConsList a), Nil]

    is_nil : ConsList a -> Bool
    is_nil = \list ->
        when list is
            Nil -> Bool.true
            Cons _ _ -> Bool.false

    is_nil (Cons 0x2 Nil)
    "
}

#[mono_test]
#[ignore]
fn has_none() {
    r"
    Maybe a : [Just a, Nothing]
    ConsList a : [Cons a (ConsList a), Nil]

    has_none : ConsList (Maybe a) -> Bool
    has_none = \list ->
        when list is
            Nil -> Bool.false
            Cons Nothing _ -> Bool.true
            Cons (Just _) xs -> has_none xs

    has_none (Cons (Just 3) Nil)
    "
}

#[mono_test]
fn mk_pair_of() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        mk_pair_of = \x -> Pair x x

        main =
            mk_pair_of [1,2,3]
        "#
    )
}

#[mono_test]
fn fst() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        fst = \x, _ -> x

        main =
            fst [1,2,3] [3,2,1]
        "#
    )
}

#[mono_test]
fn list_cannot_update_inplace() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        x : List I64
        x = [1,2,3]

        add : List I64 -> List I64
        add = \y -> List.set y 0 0

        main =
            List.len (add x) + List.len x
        "#
    )
}

#[mono_test]
fn list_get() {
    r"
    wrapper = \{} ->
        List.get [1,2,3] 0

    wrapper {}
    "
}

#[mono_test]
fn peano() {
    r"
    Peano : [S Peano, Z]

    three : Peano
    three = S (S (S Z))

    three
    "
}

#[mono_test]
fn peano1() {
    r"
    Peano : [S Peano, Z]

    three : Peano
    three = S (S (S Z))

    when three is
        Z -> 0
        S _ -> 1
    "
}

#[mono_test]
fn peano2() {
    r"
    Peano : [S Peano, Z]

    three : Peano
    three = S (S (S Z))

    when three is
        S (S _) -> 1
        S (_) -> 0
        Z -> 0
    "
}

#[mono_test]
fn optional_when() {
    r"
    f = \r ->
        when r is
            { x: Blue, y ? 3 } -> y
            { x: Red, y ? 5 } -> y

    a = f { x: Blue, y: 7 }
    b = f { x: Blue }
    c = f { x: Red, y: 11 }
    d = f { x: Red }

    a * b * c * d
    "
}

#[mono_test]
fn optional_field_with_binary_op() {
    r"
        { bar ? 1 + 1 } = {}
        bar
    "
}

#[mono_test]
fn nested_optional_field_with_binary_op() {
    r#"
        when { x: ([{}], "foo") } is
            { x: ([{ bar ? 1 + 1 }], _) } -> bar
            _ -> 0
    "#
}

#[mono_test]
fn multiline_record_pattern() {
    r"
        x = { a: 1, b: 2, c: 3 }
        {
            a,
            b,
            c,
        } = x

        a + b + c
    "
}

#[mono_test]
fn nested_pattern_match() {
    r"
    Maybe a : [Nothing, Just a]

    x : Maybe (Maybe I64)
    x = Just (Just 41)

    when x is
        Just (Just v) -> v + 0x1
        _ -> 0x1
    "
}

#[mono_test]
#[ignore]
fn linked_list_length_twice() {
    r"
    LinkedList a : [Nil, Cons a (LinkedList a)]

    nil : LinkedList I64
    nil = Nil

    length : LinkedList a -> I64
    length = \list ->
        when list is
            Nil -> 0
            Cons _ rest -> 1 + length rest

    length nil + length nil
    "
}

#[mono_test]
fn rigids() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        swap : U64, U64, List a -> List a
        swap = \i, j, list ->
            when Pair (List.get list i) (List.get list j) is
                Pair (Ok at_i) (Ok at_j) ->
                    foo = at_j

                    list
                        |> List.set i foo
                        |> List.set j at_i

                _ ->
                    []

        main =
            swap 0 0 [0x1]
        "#
    )
}

#[mono_test]
fn let_x_in_x() {
    r"
    x = 5

    answer =
        1337

    unused =
        nested = 17
        nested

    answer
    "
}

#[mono_test]
fn let_x_in_x_indirect() {
    r"
    x = 5

    answer =
        1337

    unused =
        nested = 17

        i = 1

        nested

    { answer, unused }.answer
    "
}

#[mono_test]
fn nested_closure() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        foo = \{} ->
            x = 42
            f = \{} -> x
            f

        main =
            f = foo {}
            f {}
        "#
    )
}

#[mono_test]
fn closure_in_list() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        foo = \{} ->
            x = 41

            f = \{} -> x

            [f]

        main =
            items = foo {}

            List.len items
        "#
    )
}

#[ignore]
#[mono_test]
fn somehow_drops_definitions() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        one : I64
        one = 1

        two : I64
        two = 2

        increment : I64 -> I64
        increment = \x -> x + one

        double : I64 -> I64
        double = \x -> x * two

        apply : (a -> a), a -> a
        apply = \f, x -> f x

        main =
            apply (if Bool.true then increment else double) 42
        "#
    )
}

#[mono_test]
fn specialize_closures() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"


        apply : (a -> a), a -> a
        apply = \f, x -> f x

        main =
            one : I64
            one = 1

            two : I64
            two = 2

            b : Bool
            b = Bool.true

            increment : I64 -> I64
            increment = \x -> x + one

            double : I64 -> I64
            double = \x -> if b then x * two else x

            apply (if Bool.true then increment else double) 42
        "#
    )
}

#[mono_test]
fn specialize_lowlevel() {
    indoc!(
        r#"
         app "test" provides [main] to "./platform"

         apply : (a -> a), a -> a
         apply = \f, x -> f x

         main =
             one : I64
             one = 1

             two : I64
             two = 2

             increment : I64 -> I64
             increment = \x -> x + one

             double : I64 -> I64
             double = \x -> x * two

             (if Bool.true then increment else double) 42
         "#
    )
}

#[mono_test]
fn empty_list_of_function_type() {
    // see https://github.com/roc-lang/roc/issues/1732
    indoc!(
        r#"
         app "test" provides [main] to "./platform"

         main =
            my_list : List (Str -> Str)
            my_list = []

            my_closure : Str -> Str
            my_closure = \_ -> "bar"

            choose =
                if Bool.false then
                    my_list
                else
                    [my_closure]

            when List.get choose 0 is
                Ok f -> f "foo"
                Err _ -> "bad!"
            "#
    )
}

#[mono_test]
fn monomorphized_ints() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            x = 100

            f : U8, U32 -> U64
            f = \_, _ -> 18

            f x x
        "#
    )
}

#[mono_test]
fn monomorphized_floats() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            x = 100.0

            f : F32, F64 -> U64
            f = \_, _ -> 18

            f x x
        "#
    )
}

#[mono_test]
#[ignore = "TODO"]
fn monomorphized_ints_aliased() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            y = 100
            w1 = y
            w2 = y

            f = \_, _ -> 1

            f1 : U8, U32 -> U64
            f1 = f

            f2 : U32, U8 -> U64
            f2 = f

            f1 w1 w2 + f2 w1 w2
        "#
    )
}

#[mono_test]
fn monomorphized_tag() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            b = \{} -> Bar
            f : [Foo, Bar], [Bar, Baz] -> U8
            f = \_, _ -> 18
            f (b {}) (b {})
        "#
    )
}

#[mono_test]
fn monomorphized_tag_with_aliased_args() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            b = Bool.false
            c = Bool.false
            a = A b c
            f : [A Bool Bool] -> U64
            f = \_ -> 1
            f a
        "#
    )
}

#[mono_test]
fn monomorphized_list() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            l = \{} -> [1, 2, 3]

            f : List U8, List U16 -> U64
            f = \_, _ -> 18

            f (l {}) (l {})
        "#
    )
}

#[mono_test]
fn monomorphized_applied_tag() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            a = A "A"
            f = \x ->
                when x is
                    A y -> y
                    B y -> y
            f a
        "#
    )
}

#[mono_test]
#[ignore = "Cannot compile polymorphic closures yet"]
fn aliased_polymorphic_closure() {
    indoc!(
        r"
        n : U8
        n = 1
        f = \{} -> (\a -> n)
        g = f {}
        g {}
        "
    )
}

#[mono_test]
fn issue_2535_let_weakened_fields_referenced_in_list() {
    indoc!(
        r#"
        app "test" provides [nums] to "./platform"

        alpha = { a: 1, b: 2 }

        nums : List U8
        nums =
            [
                alpha.a,
                alpha.b,
           ]
        "#
    )
}

#[mono_test]
fn issue_2725_alias_polymorphic_lambda() {
    indoc!(
        r"
        wrap = \value -> Tag value
        wrap_it = wrap
        wrap_it 42
        "
    )
}

#[mono_test]
fn issue_2583_specialize_errors_behind_unified_branches() {
    indoc!(
        r#"
        if Bool.true then List.first [] else Str.to_i64 ""
        "#
    )
}

#[mono_test]
fn issue_2810() {
    indoc!(
        r"
        Command : [Command Tool]

        Job : [Job Command]

        Tool : [SystemTool, FromJob Job]

        a : Job
        a = Job (Command (FromJob (Job (Command SystemTool))))
        a
        "
    )
}

#[mono_test]
fn issue_2811() {
    indoc!(
        r#"
        x = Command { tool: "bash" }
        Command c = x
        c.tool
        "#
    )
}

#[mono_test]
fn specialize_ability_call() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        MHash implements
            hash : a -> U64 where a implements MHash

        Id := U64 implements [MHash {hash}]

        hash : Id -> U64
        hash = \@Id n -> n

        main = hash (@Id 1234)
        "#
    )
}

#[mono_test]
fn opaque_assign_to_symbol() {
    indoc!(
        r#"
        app "test" provides [out] to "./platform"

        Variable := U8

        from_utf8 : U8 -> Result Variable [InvalidVariableUtf8]
        from_utf8 = \char ->
            Ok (@Variable char)

        out = from_utf8 98
        "#
    )
}

#[mono_test]
fn encode() {
    indoc!(
        r#"
        app "test" provides [my_u8_bytes] to "./platform"

        MEncoder fmt := List U8, fmt -> List U8 where fmt implements Format

        MEncoding implements
          to_encoder : val -> MEncoder fmt where val implements MEncoding, fmt implements Format

        Format implements
          u8 : U8 -> MEncoder fmt where fmt implements Format


        Linear := {} implements [Format {u8}]

        u8 = \n -> @MEncoder (\lst, @Linear {} -> List.append lst n)

        MyU8 := U8 implements [MEncoding {to_encoder}]

        to_encoder = \@MyU8 n -> u8 n

        my_u8_bytes =
            when to_encoder (@MyU8 15) is
                @MEncoder do_encode -> do_encode [] (@Linear {})
        "#
    )
}

// #[ignore]
// #[mono_test]
// fn static_str_closure() {
//     indoc!(
//         r#"
//         app "test" provides [main] to "./platform"

//         main : Str
//         main =
//             x = "long string that is malloced"

//             f : {} -> Str
//             f = (\_ -> x)

//             f {}
//         "#
//     )
// }

#[mono_test]
fn list_map_closure_borrows() {
    indoc!(
        r#"
        app "test" provides [out] to "./platform"

        list = [Str.concat "lllllllllllllllllllllooooooooooong" "g"]

        example1 = List.map list \string -> Str.repeat string 2

        out =
            when List.get example1 0 is
                Ok s -> s
                Err _ -> "Hello, World!\n"
        "#
    )
}

#[mono_test]
fn list_map_closure_owns() {
    indoc!(
        r#"
        app "test" provides [out] to "./platform"

        list = [Str.concat "lllllllllllllllllllllooooooooooong" "g"]

        example2 = List.map list \string -> Str.concat string "!"

        out =
            when List.get example2 0 is
                Ok s -> s
                Err _ -> "Hello, World!\n"
        "#
    )
}

#[mono_test]
fn list_sort_asc() {
    indoc!(
        r#"
        app "test" provides [out] to "./platform"

        out = List.sort_asc [4, 3, 2, 1]
        "#
    )
}

#[mono_test]
#[ignore]
fn encode_custom_type() {
    &formatdoc!(
        r#"
        app "test"
            imports [Encode.{{ to_encoder }}]
            provides [main] to "./platform"

        {TAG_LEN_ENCODER_FMT}

        HelloWorld := {{}}
        to_encoder = \@HelloWorld {{}} ->
            Encode.custom \bytes, fmt ->
                bytes
                    |> Encode.append_with (Encode.string "Hello, World!\n") fmt

        main =
            result = Str.from_utf8 (Encode.to_bytes (@HelloWorld {{}}) tag_len_fmt)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test]
fn encode_derived_string() {
    &formatdoc!(
        r#"
        app "test"
            imports [Encode.{{ to_encoder }}]
            provides [main] to "./platform"

        {TAG_LEN_ENCODER_FMT}

        main =
            result = Str.from_utf8 (Encode.to_bytes "abc" tag_len_fmt)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test]
#[ignore = "TODO"]
fn encode_derived_record() {
    &formatdoc!(
        r#"
        app "test"
            imports [Encode.{{ to_encoder }}]
            provides [main] to "./platform"

        {TAG_LEN_ENCODER_FMT}

        main =
            result = Str.from_utf8 (Encode.to_bytes {{a: "a"}} tag_len_fmt)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test]
fn choose_correct_recursion_var_under_record() {
    indoc!(
        r#"
        Parser : [
            Specialize Parser,
            Record (List {parser: Parser}),
        ]

        print_combinator_parser : Parser -> Str
        print_combinator_parser = \parser ->
            when parser is
                Specialize p ->
                    printed = print_combinator_parser p
                    if Bool.false then printed else "foo"
                Record fields ->
                    fields
                        |> List.map \f ->
                            printed = print_combinator_parser f.parser
                            if Bool.false then printed else "foo"
                        |> List.first
                        |> Result.withDefault ("foo")

        print_combinator_parser (Record [])
        "#
    )
}

#[mono_test]
fn tail_call_elimination() {
    indoc!(
        r"
        sum = \n, accum ->
            when n is
                0 -> accum
                _ -> sum (n - 1) (n + accum)

        sum 1_000_000 0
        "
    )
}

#[mono_test]
fn tail_call_with_same_layout_different_lambda_sets() {
    indoc!(
        r#"
        chain = \in, build_lazy ->
            \{} ->
                thunk = build_lazy in
                thunk {}

        chain 1u8 \_ -> chain 1u8 \_ -> (\{} -> "")
        "#
    )
}

#[mono_test]
fn tail_call_with_different_layout() {
    indoc!(
        r#"
        chain = \in, build_lazy ->
            \{} ->
                thunk = build_lazy in
                thunk {}

        chain 1u8 \_ -> chain 1u16 \_ -> (\{} -> "")
        "#
    )
}

#[mono_test]
fn lambda_capture_niche_u8_vs_u64() {
    indoc!(
        r"
        capture : _ -> ({} -> Str)
        capture = \val ->
            \{} ->
                Num.to_str val

        x : [True, False]
        x = True

        fun =
            when x is
                True -> capture 123u64
                False -> capture 18u8

        fun {}
        "
    )
}

#[mono_test]
fn lambda_capture_niches_with_other_lambda_capture() {
    indoc!(
        r#"
        capture : a -> ({} -> Str)
        capture = \val ->
            \{} ->
                when val is
                    _ -> ""

        capture2 = \val -> \{} -> "${val}"

        x : [A, B, C]
        x = A

        fun =
            when x is
                A -> capture {}
                B -> capture2 "foo"
                C -> capture 1u64

        fun {}
        "#
    )
}

#[mono_test]
fn lambda_capture_niches_with_non_capturing_function() {
    indoc!(
        r#"
        capture : a -> ({} -> Str)
        capture = \val ->
            \{} ->
                when val is
                    _ -> ""

        triv = \{} -> ""

        x : [A, B, C]
        x = A

        fun =
            when x is
                A -> capture {}
                B -> triv
                C -> capture 1u64

        fun {}
        "#
    )
}

#[mono_test]
fn lambda_capture_niches_have_captured_function_in_closure() {
    indoc!(
        r#"
        Lazy a : {} -> a

        after : Lazy a, (a -> Lazy b) -> Lazy b
        after = \effect, map ->
            thunk = \{} ->
                when map (effect {}) is
                    b -> b {}
            thunk

        f = \_ -> \_ -> ""
        g = \{ s1 } -> \_ -> s1

        x : [True, False]
        x = True

        fun =
            when x is
                True -> after (\{} -> "") f
                False -> after (\{} -> {s1: "s1"}) g

        fun {}
        "#
    )
}

#[mono_test]
fn lambda_set_niche_same_layout_different_constructor() {
    indoc!(
        r#"
        capture : a -> ({} -> Str)
        capture = \val ->
            thunk =
                \{} ->
                    when val is
                        _ -> ""
            thunk

        x : [True, False]
        x = True

        fun =
            when x is
                True -> capture {a: ""}
                False -> capture (A "")
        fun
        "#
    )
}

#[mono_test]
fn choose_u64_layout() {
    indoc!(
        r"
        9999999999999999999 + 1
        "
    )
}

#[mono_test]
fn choose_i128_layout() {
    indoc!(
        r"
        {
            a: 18446744073709551616 + 1,
            b: -9223372036854775809 + 1,
        }
        "
    )
}

#[mono_test]
fn choose_u128_layout() {
    indoc!(
        r"
        170141183460469231731687303715884105728 + 1
        "
    )
}

#[mono_test]
fn recursive_call_capturing_function() {
    indoc!(
        r"
        a = \b ->
            c : U32 -> U32
            c = \d ->
                if Bool.true then d else c (d+b)
            c 0

        a 6
        "
    )
}

#[mono_test]
fn call_function_in_empty_list() {
    indoc!(
        r"
        lst : List ({} -> {})
        lst = []
        List.map lst \f -> f {}
        "
    )
}

#[mono_test]
fn call_function_in_empty_list_unbound() {
    indoc!(
        r"
        lst = []
        List.map lst \f -> f {}
        "
    )
}

#[mono_test]
fn instantiate_annotated_as_recursive_alias_toplevel() {
    indoc!(
        r#"
        app "test" provides [it] to "./platform"

        Value : [Nil, Array (List Value)]

        foo : [Nil]_
        foo = Nil

        it : Value
        it = foo
        "#
    )
}

#[mono_test]
fn instantiate_annotated_as_recursive_alias_polymorphic_expr() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            Value : [Nil, Array (List Value)]

            foo : [Nil]_
            foo = Nil

            it : Value
            it = foo

            it
        "#
    )
}

#[mono_test]
fn instantiate_annotated_as_recursive_alias_multiple_polymorphic_expr() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            Value : [Nil, Array (List Value)]

            foo : {} -> [Nil]_
            foo = \{} -> Nil

            v1 : Value
            v1 = foo {}

            Value2 : [Nil, B U16, Array (List Value)]

            v2 : Value2
            v2 = foo {}

            {v1, v2}
        "#
    )
}

#[mono_test(large_stack = "true")]
fn encode_derived_record_one_field_string() {
    &formatdoc!(
        r#"
        app "test"
            imports [Encode.{{ to_encoder }}]
            provides [main] to "./platform"

        {TAG_LEN_ENCODER_FMT}

        main =
            result = Str.from_utf8 (Encode.to_bytes {{a: "foo"}} tag_len_fmt)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test(large_stack = "true")]
fn encode_derived_record_two_field_strings() {
    &formatdoc!(
        r#"
        app "test"
            imports [Encode.{{ to_encoder }}]
            provides [main] to "./platform"

        {TAG_LEN_ENCODER_FMT}

        main =
            result = Str.from_utf8 (Encode.to_bytes {{a: "foo", b: "bar"}} tag_len_fmt)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test(large_stack = "true")]
fn encode_derived_nested_record_string() {
    &formatdoc!(
        r#"
        app "test"
            imports [Encode.{{ to_encoder }}]
            provides [main] to "./platform"

        {TAG_LEN_ENCODER_FMT}

        main =
            result = Str.from_utf8 (Encode.to_bytes {{a: {{b: "bar"}}}} tag_len_fmt)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test]
fn encode_derived_tag_one_field_string() {
    &formatdoc!(
        r#"
        app "test"
            imports [Encode.{{ to_encoder }}]
            provides [main] to "./platform"

        {TAG_LEN_ENCODER_FMT}

        main =
            x : [A Str]
            x = A "foo"
            result = Str.from_utf8 (Encode.to_bytes x tag_len_fmt)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test]
fn polymorphic_expression_unification() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        RenderTree : [
            Text Str,
            Indent (List RenderTree),
        ]
        parse_function : Str -> RenderTree
        parse_function = \name ->
            last = Indent [Text ".trace(\"${name}\")" ]
            Indent [last]

        values = parse_function "interface_header"

        main = values == Text ""
        "#
    )
}

#[mono_test]
fn encode_derived_tag_two_payloads_string() {
    &formatdoc!(
        r#"
        app "test"
            imports [Encode.{{ to_encoder }}]
            provides [main] to "./platform"

        {TAG_LEN_ENCODER_FMT}

        main =
            x : [A Str Str]
            x = A "foo" "foo"
            result = Str.from_utf8 (Encode.to_bytes x tag_len_fmt)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test]
fn issue_3560_nested_tag_constructor_is_newtype() {
    indoc!(
        r#"
        when Wrapper (Payload "err") is
            Wrapper (Payload str) -> str
            Wrapper (AlternatePayload str) -> str
        "#
    )
}

#[mono_test]
fn issue_3669() {
    indoc!(
        r#"
        Peano a := [
            Zero,
            Successor (Peano a)
        ]

        unwrap : Peano a -> {}
        unwrap = \@Peano p ->
            when p is
                Zero -> {}
                Successor inner -> unwrap inner

        when unwrap (@Peano Zero) == {} is
            _ -> ""
        "#
    )
}

#[mono_test]
fn num_width_gt_u8_layout_as_float() {
    indoc!(
        r"
        1 / 200
        "
    )
}

#[mono_test]
fn match_on_result_with_uninhabited_error_branch() {
    indoc!(
        r#"
        x : Result Str []
        x = Ok "abc"

        when x is
            Ok s -> s
        "#
    )
}

#[mono_test]
fn unreachable_void_constructor() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        x : []

        main = if Bool.true then Ok x else Err "abc"
        "#
    )
}

#[mono_test]
fn unreachable_branch_is_eliminated_but_produces_lambda_specializations() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        provide_thunk = \x ->
            when x is
                Ok _ ->
                    t1 = \{} -> "t1"
                    t1
                # During specialization of `main` we specialize this function,
                # which leads to elimination of this branch, because it is unreachable
                # (it can only match the uninhabited type `Err []`).
                #
                # However, naive elimination of this branch would mean we don't traverse
                # the branch body. If we don't do so, we will fail to see and specialize `t2`,
                # which is problematic - while `t2` won't ever be reached in this specialization,
                # it is still part of the lambda set, and `thunk {}` (in main) will match over
                # it before calling.
                #
                # So, this test verifies that we eliminate this branch, but still specialize
                # everything we need.
                Err _ ->
                    t2 = \{} -> "t2"
                    t2

        main =
            x : Result Str []
            x = Ok "abc"

            thunk = provide_thunk x

            thunk {}
        "#
    )
}

#[mono_test]
fn match_list() {
    indoc!(
        r#"
        l = [A, B]

        when l is
            [] -> "A"
            [A] -> "B"
            [A, A, ..] -> "C"
            [A, B, ..] -> "D"
            [B, ..] -> "E"
        "#
    )
}

#[mono_test]
fn recursive_function_and_union_with_inference_hole() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        Html state : [
            Element (List (Html state)),
        ]

        translate_static : Html _ -> Html _
        translate_static = \node ->
            when node is
                Element children ->
                    new_children = List.map children translate_static

                    Element new_children

        main = when translate_static (Element []) is
            _ -> ""
        "#
    )
}

#[mono_test]
fn crash() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        get_infallible = \result -> when result is
            Ok x -> x
            _ -> crash "turns out this was fallible"

        main =
            x : [Ok U64, Err Str]
            x = Ok 78
            get_infallible x
        "#
    )
}

#[mono_test]
fn function_pointer_lambda_set() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        number = \{} -> 1u64

        parse = \parser -> parser {}

        main =
            parser = number
            parse parser
        "#
    )
}

#[mono_test]
fn anonymous_closure_lifted_to_named_issue_2403() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            f =
                n = 1
                \{} -> n
            g = f {}
            g
        "#
    )
}

#[mono_test]
fn toplevel_accessor_fn_thunk() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        ra = .field

        main =
            ra { field : 15u8 }
        "#
    )
}

#[mono_test]
fn list_one_vs_one_spread_issue_4685() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main = when [""] is
            [] -> "A"
            [_] -> "B"
            [_, ..] -> "C"
        "#
    )
}

#[mono_test]
fn tuple_pattern_match() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main = when (1, 2) is
            (1, _) -> "A"
            (_, 2) -> "B"
            (_, _) -> "C"
        "#
    )
}

#[mono_test(mode = "test")]
fn issue_4705() {
    indoc!(
        r"
        interface Test exposes [] imports []

        go : {} -> Bool
        go = \{} -> Bool.true

        expect
            input = {}
            x = go input
            x
        "
    )
}

#[mono_test(mode = "test", large_stack = "true")]
fn issue_4749() {
    &formatdoc!(
        r#"
        interface Test exposes [] imports []

        expect
            got : [Y [Y1, Y2], Z [Z1, Z2]]_
            got = Z Z1

            t : [A [A1, A2]]_
            t = A A1

            got != t
        "#
    )
}

#[mono_test(mode = "test")]
fn lambda_set_with_imported_toplevels_issue_4733() {
    indoc!(
        r"
        interface Test exposes [] imports []

        fn = \{} ->
            instr : [ Op (U64, U64 -> U64) ]
            instr = if Bool.true then (Op Num.mul) else (Op Num.add)

            Op op = instr

            \a -> op a a

        expect ((fn {}) 3) == 9
        "
    )
}

#[mono_test]
fn order_list_size_tests_issue_4732() {
    indoc!(
        r#"
        when [] is
            [1, ..]          -> "B1"
            [2, 1, ..]       -> "B2"
            [3, 2, 1, ..]    -> "B3"
            [4, 3, 2, 1, ..] -> "B4"
            _                -> "Catchall"
        "#
    )
}

#[mono_test]
fn anonymous_closure_in_polymorphic_expression_issue_4717() {
    indoc!(
        r#"
        app "test" provides [main] to "platform"

        chomp_while : (List U8) -> (List U8)
        chomp_while = \input ->
                index = List.walk_until input 0 \i, _ -> Break i

                if index == 0 then
                    input
                else
                    List.drop_first input index

        main = chomp_while [1u8, 2u8, 3u8]
        "#
    )
}

#[mono_test]
fn list_map_take_capturing_or_noncapturing() {
    indoc!(
        r#"
        app "test" provides [main] to "platform"

        main =
            x = 1u8
            y = 2u8
            f = when "" is
                "A" ->
                    g = \n -> n + x
                    g
                "B" ->
                    h = \n -> n + y
                    h
                _   ->
                    k = \n -> n + n
                    k
            List.map [1u8, 2u8, 3u8] f
        "#
    )
}

#[mono_test]
fn issue_4557() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        is_eq_q = \q1, q2 -> when T q1 q2 is
            T (U f1) (U f2) -> Bool.or (is_eq_q (U f2) (U f1)) (f1 {} == f2 {})

        main = is_eq_q (U \{} -> "a") (U \{} -> "a")
        "#
    )
}

#[mono_test]
fn nullable_wrapped_with_nullable_not_last_index() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        Parser : [
            OneOrMore Parser,
            Keyword Str,
            CharLiteral,
        ]

        to_id_parser : Parser -> Str
        to_id_parser = \parser ->
            when parser is
                OneOrMore _ -> "a"
                Keyword _ -> "b"
                CharLiteral -> "c"

        main = to_id_parser CharLiteral == "c"
        "#
    )
}

#[mono_test]
fn pattern_as_toplevel() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        record = { a: 42i64, b: "foo" }

        main =
            when record is
                { a: 42i64 } as r -> record == r
                _ -> Bool.false
        "#
    )
}

#[mono_test]
fn pattern_as_nested() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        record = { a: 42i64, b: "foo" }

        main =
            when Pair {} record is
                Pair {} ({ a: 42i64 } as r) -> record == r
                _ -> Bool.false
        "#
    )
}

#[mono_test]
fn pattern_as_of_symbol() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            when "foo" is
                a as b -> a == b
        "#
    )
}

#[mono_test]
fn function_specialization_information_in_lambda_set_thunk() {
    // https://github.com/roc-lang/roc/issues/4734
    // https://github.com/roc-lang/rfcs/blob/main/0010-let-generalization-lets-not.md
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        and_then = \{} ->
            x = 10
            \new_fn -> Num.add (new_fn {}) x

        between = and_then {}

        main = between \{} -> between \{} -> 10
        "#
    )
}

#[mono_test]
fn function_specialization_information_in_lambda_set_thunk_independent_defs() {
    // https://github.com/roc-lang/roc/issues/4734
    // https://github.com/roc-lang/rfcs/blob/main/0010-let-generalization-lets-not.md
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        and_then = \{} ->
            x = 10u8
            \new_fn -> Num.add (new_fn {}) x

        between1 = and_then {}

        between2 = and_then {}

        main = between1 \{} -> between2 \{} -> 10u8
        "#
    )
}

#[mono_test(mode = "test", large_stack = "true")]
fn issue_4772_weakened_monomorphic_destructure() {
    &formatdoc!(
        r#"
        interface Test exposes [] imports []

        {ERR_DECODER_FMT}

        get_number =
            {{ result, rest }} = Decode.from_bytes_partial (Str.to_utf8 "-1234") (@ErrDecoder {{}})

            when result is
                Ok val ->
                    when Str.to_i64 val is
                        Ok number ->
                            Ok {{val : number, input : rest}}
                        Err InvalidNumStr ->
                            Err (ParsingFailure "not a number")

                Err _ ->
                    Err (ParsingFailure "not a number")

        expect
            result = get_number
            result == Ok {{val : -1234i64, input : []}}
        "#
    )
}

#[mono_test]
fn weakening_avoids_overspecialization() {
    // Without weakening of let-bindings, this program would force two specializations of
    // `index` - to `U64` and the default integer type, `I64`. The test is to ensure only one
    // specialization, that of `U64`, exists.
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main : (List U8) -> (List U8)
        main = \input ->
            index = List.walk_until input 0 \i, _ -> Break i

            if index == 0 then
                input
            else
                List.drop_first input index
        "#
    )
}

#[mono_test]
fn recursively_build_effect() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        greeting =
            hi = "Hello"
            name = "World"

            "${hi}, ${name}!"

        main =
            when nest_help 4 is
                _ -> greeting

        nest_help : I64 -> XEffect {}
        nest_help = \m ->
            when m is
                0 ->
                    always {}

                _ ->
                    always {} |> after \_ -> nest_help (m - 1)


        XEffect a := {} -> a

        always : a -> XEffect a
        always = \x -> @XEffect (\{} -> x)

        after : XEffect a, (a -> XEffect b) -> XEffect b
        after = \(@XEffect e), toB ->
            @XEffect \{} ->
                when toB (e {}) is
                    @XEffect e2 ->
                        e2 {}
        "#
    )
}

#[mono_test]
fn recursive_lambda_set_has_nested_non_recursive_lambda_sets_issue_5026() {
    indoc!(
        r#"
        app "test" provides [looper] to "./platform"

        Effect : {} -> Str

        after = \build_next ->
            after_inner = \{} -> (build_next "foobar") {}
            after_inner

        await : (Str -> Effect) -> Effect
        await = \cont -> after (\result -> cont result)

        looper = await \_ -> if Bool.true then looper else \{} -> "done"
        "#
    )
}

#[mono_test]
fn unspecialized_lambda_set_unification_keeps_all_concrete_types_without_unification() {
    // This is a regression test for the ambient lambda set specialization algorithm.
    //
    // In the program below, monomorphization of `to_encoder_q` with the `Q` in `main` induces the
    // resolution of `t.a` and `t.b`, and the unification of their pending unspecialization lambda
    // sets, when `t.a` and `t.b` have been resolved to concrete types, but before the
    // specialization procedure steps in to resolve the lambda sets concretely. That's because
    // monomorphization unifies the general type of `to_encoder_q` with the concrete type, forcing
    // concretization of `t`, but the specialization procedure runs only after the unification is
    // complete.
    //
    // In this case, it's imperative that the unspecialized lambda sets of `to_encoder t.a` and
    // `to_encoder t.b` wind up in the same lambda set, that is in
    //
    // tag : @MEncoder (Bytes, Linear -[[] + @MU8:to_encoder:1 + @MStr:to_encoder+1] -> Bytes)
    //       -[lTag]->
    //       @MEncoder (Bytes, Linear -[[Linear:lTag:3 { @MEncoder (Bytes, Linear -[[] + @MU8:to_encoder:1 + @MStr:to_encoder:1] -> Bytes) }]] -> Bytes)
    //
    // rather than forcing the lambda set inside to `tag` to become disjoint, as e.g.
    //
    // tag : @MEncoder (Bytes, Linear -[[] + @MU8:to_encoder:1 + @MStr:to_encoder+1] -> Bytes)
    //       -[lTag]->
    //       @MEncoder (Bytes, Linear -[[
    //                      Linear:lTag:3 { @MEncoder (Bytes, Linear -[[] + @MU8:to_encoder:1] -> Bytes) },
    //                      Linear:lTag:3 { @MEncoder (Bytes, Linear -[[] + @MStr:to_encoder:1] -> Bytes) },
    //                  ]] -> Bytes)
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        MEncoder fmt := List U8, fmt -> List U8 where fmt implements Format

        MEncoding implements
          to_encoder : val -> MEncoder fmt where val implements MEncoding, fmt implements Format

        Format implements
          u8 : {} -> MEncoder fmt where fmt implements Format
          str : {} -> MEncoder fmt where fmt implements Format
          tag : MEncoder fmt -> MEncoder fmt where fmt implements Format

        Linear := {} implements [Format {u8: lU8, str: lStr, tag: lTag}]

        MU8 := U8 implements [MEncoding {to_encoder: to_encoder_u8}]
        MStr := Str implements [MEncoding {to_encoder: to_encoder_str}]

        Q a b := { a: a, b: b }

        lU8 = \{} -> @MEncoder (\lst, @Linear {} -> lst)
        lStr = \{} -> @MEncoder (\lst, @Linear {} -> lst)

        lTag = \@MEncoder doFormat -> @MEncoder (\lst, @Linear {} ->
            doFormat lst (@Linear {})
        )

        to_encoder_u8 = \@MU8 _ -> u8 {}

        to_encoder_str = \@MStr _ -> str {}

        to_encoder_q =
            \@Q t -> \fmt ->
                @MEncoder doit = if Bool.true
                    then tag (to_encoder t.a)
                    else tag (to_encoder t.b)

                doit [] fmt

        main =
            fmt = to_encoder_q (@Q {a : @MStr "", b: @MU8 7})
            fmt (@Linear {})
        "#
    )
}

#[mono_test]
fn unspecialized_lambda_set_unification_keeps_all_concrete_types_without_unification_of_unifiable()
{
    // This is a regression test for the ambient lambda set specialization algorithm.
    //
    // The principle of the test is equivalent to that of `unspecialized_lambda_set_unification_keeps_all_concrete_types_without_unification`.
    //
    // However, this test requires a larger reproduction because it is negative behavior is only
    // visible in the presence of builtin ability usage (in this case, `Encoding` and
    // `EncoderFormatting`).
    //
    // In this test, the payload types `[A]*` and `[B]*` of the encoded type `Q` are unifiable in
    // their unspecialized lambda set representations under `to_encoder_q`; however, they must not
    // be, because they in fact represent to different specializations of needed encoders. In
    // particular, the lambda set `[[] + [A]:to_encoder:1 + [B]:to_encoder:1]` must be preserved,
    // rather than collapsing to `[[] + [A, B]:to_encoder:1]`.
    &formatdoc!(
        r#"
        app "test" imports [] provides [main] to "./platform"

        {TAG_LEN_ENCODER_FMT}

        Q a b := {{ a: a, b: b }} implements [Encoding {{to_encoder: to_encoder_q}}]

        to_encoder_q =
            \@Q t -> Encode.custom \bytes, fmt ->
                f = if Bool.true
                    then Encode.tag "A" [Encode.to_encoder t.a]
                    else Encode.tag "B" [Encode.to_encoder t.b]

                Encode.append_with bytes f fmt

        accessor = @Q {{a : A, b: B}}

        main =
            Encode.to_bytes accessor tag_len_fmt
        "#
    )
}

#[mono_test]
fn unspecialized_lambda_set_unification_does_not_duplicate_identical_concrete_types() {
    // This is a regression test for the ambient lambda set specialization algorithm.
    //
    // The principle of the test is equivalent to that of `unspecialized_lambda_set_unification_keeps_all_concrete_types_without_unification`.
    //
    // However, this test requires a larger reproduction because it is negative behavior is only
    // visible in the presence of builtin ability usage (in this case, `Encoding` and
    // `EncoderFormatting`).
    //
    // In this test, the payload types `Str` and `Str` of the encoded type `Q` are unifiable in
    // their unspecialized lambda set representations under `to_encoder_q`, and moreoever they are
    // equivalent specializations, since they both come from the same root variable `x`. In as
    // such, the lambda set `[[] + Str:to_encoder:1]` should be produced during compaction, rather
    // than staying as the expanded `[[] + Str:to_encoder:1 + Str:to_encoder:1]` after the types of
    // `t.a` and `t.b` are filled in.
    &formatdoc!(
        r#"
        app "test" imports [] provides [main] to "./platform"

        {TAG_LEN_ENCODER_FMT}

        Q a b := {{ a: a, b: b }} implements [Encoding {{to_encoder: to_encoder_q}}]

        to_encoder_q =
            \@Q t -> Encode.custom \bytes, fmt ->
                f = if Bool.true
                    then Encode.tag "A" [Encode.to_encoder t.a]
                    else Encode.tag "B" [Encode.to_encoder t.b]

                Encode.append_with bytes f fmt

        accessor =
            x = ""
            @Q {{a : x, b: x}}

        main =
            Encode.to_bytes accessor tag_len_fmt
        "#
    )
}

#[mono_test]
fn inline_return_joinpoints_in_bool_lambda_set() {
    indoc!(
        r#"
        app "test" provides [f] to "./platform"

        f = \x ->
            caller = if Bool.false then f else \n -> n
            caller (x + 1)
        "#
    )
}

#[mono_test]
fn inline_return_joinpoints_in_enum_lambda_set() {
    indoc!(
        r#"
        app "test" provides [f] to "./platform"

        f = \x ->
            caller = \t -> when t is
                A -> f
                B -> \n -> n
                C -> \n -> n + 1
                D -> \n -> n + 2
            (caller A) (x + 1)
        "#
    )
}

#[mono_test]
fn inline_return_joinpoints_in_union_lambda_set() {
    indoc!(
        r#"
        app "test" provides [f] to "./platform"

        f = \x ->
            caller = \t -> when t is
                A -> f
                B -> \n -> n + x
            (caller A) (x + 1)
        "#
    )
}

#[mono_test]
fn recursive_closure_with_transiently_used_capture() {
    indoc!(
        r#"
        app "test" provides [f] to "./platform"

        then_do = \x, callback ->
            callback x

        f = \{} ->
            code = 10u16

            bf = \{} ->
                then_do code \_ -> bf {}

            bf {}
        "#
    )
}

#[mono_test]
fn when_guard_appears_multiple_times_in_compiled_decision_tree_issue_5176() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        go : U8 -> U8
        go = \byte ->
            when byte is
                15 if Bool.true -> 1
                b if Bool.true -> b + 2
                _ -> 3

        main = go '.'
        "#
    )
}

#[mono_test]
fn recursive_lambda_set_resolved_only_upon_specialization() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        fact_cps = \n, cont ->
            if n == 0u8 then
                cont 1u8
            else
                fact_cps (n - 1) \value -> cont (n * value)

        main =
            fact_cps 5 \x -> x
        "#
    )
}

#[mono_test]
fn compose_recursive_lambda_set_productive_nullable_wrapped() {
    indoc!(
        r#"
         app "test" provides [main] to "./platform"

         compose = \forward -> \f, g ->
            if forward
            then \x -> g (f x)
            else \x -> f (g x)

         identity = \x -> x
         exclaim = \s -> "${s}!"
         whisper = \s -> "(${s})"

         main =
             res: Str -> Str
             res = List.walk [ exclaim, whisper ] identity (compose Bool.true)
             res "hello"
         "#
    )
}

#[mono_test]
fn issue_4759() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            update { a : { x : "x", y: "y" } }

        update = \state -> { state & a : { x : "ux", y: "uy" } }
        "#
    )
}

#[mono_test]
fn layout_cache_structure_with_multiple_recursive_structures() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        Chain : [
            End,
            Link Chain,
        ]

        LinkedList : [Nil, Cons { first : Chain, rest : LinkedList }]

        main =
            base : LinkedList
            base = Nil

            walker : LinkedList, Chain -> LinkedList
            walker = \rest, first -> Cons { first, rest }

            list : List Chain
            list = []

            r = List.walk list base walker

            r
        "#
    )
}

#[mono_test]
fn issue_4770() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            is_correct_order { left: IsList [IsInteger 10], right: IsList [IsInteger 20] }

        is_correct_order = \pair ->
            when pair is
                { left: IsInteger left, right: IsInteger right } -> left < right
                { left: IsList l, right: IsList r } ->
                    if List.map2 l r (\left, right -> { left, right }) |> List.all is_correct_order then
                        List.len l < List.len r
                    else
                        Bool.false

                { left: IsList _, right: IsInteger _ } -> is_correct_order { left: pair.left, right: IsList [pair.right] }
                { left: IsInteger _, right: IsList _ } -> is_correct_order { left: IsList [pair.left], right: pair.right }
        "#
    )
}

#[mono_test(allow_type_errors = "true")]
fn error_on_erroneous_condition() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main = if True then 1 else 2
        "#
    )
}

#[mono_test]
fn binary_tree_fbip() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            tree = Node (Node (Node (Node Tip Tip) Tip) (Node Tip Tip)) (Node Tip Tip)
            check_fbip tree

        Tree : [Node Tree Tree, Tip]

        check : Tree -> Num a
        check = \t -> when t is
            Node l r -> check l + check r + 1
            Tip -> 0

        Visit : [NodeR Tree Visit, Done]

        check_fbip : Tree -> Num a
        check_fbip = \t -> check_fbip_helper t Done 0

        check_fbip_helper : Tree, Visit, Num a-> Num a
        check_fbip_helper = \t, v, a -> when t is
            Node l r -> check_fbip_helper l (NodeR r v) (a + 1)
            Tip -> when v is
                NodeR r v2 -> check_fbip_helper r v2 a
                Done -> a
        "#
    )
}

#[mono_test(large_stack = "true")]
fn rb_tree_fbip() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main = Leaf
            |> ins 0 0
            |> ins 5 1
            |> ins 6 2
            |> ins 4 3
            |> ins 9 4
            |> ins 3 5
            |> ins 2 6
            |> ins 1 7
            |> ins 8 8
            |> ins 7 9

        Color : [Red, Black]

        Tree a : [Node Color (Tree a) I32 a (Tree a), Leaf]

        ins : Tree a, I32, a -> Tree a
        ins = \t, k, v -> when t is
            Leaf -> Node Red Leaf k v Leaf
            Node Black l kx vx r ->
                if k < kx
                    then when l is
                        Node Red _ _ _ _ -> when (ins l k v) is
                            Node _ (Node Red ly ky vy ry) kz vz rz -> Node Red (Node Black ly ky vy ry) kz vz (Node Black rz kx vx r)
                            Node _ lz kz vz (Node Red ly ky vy ry) -> Node Red (Node Black lz kz vz ly) ky vy (Node Black ry kx vx r)
                            Node _ ly ky vy ry -> Node Black (Node Red ly ky vy ry) kx vx r
                            Leaf -> Leaf
                        _ -> Node Black (ins l k v) kx vx r
                else
                    if k > kx
                        then when r is
                            Node Red _ _ _ _ -> when ins r k v is
                                Node _ (Node Red ly ky vy ry) kz vz rz -> Node Red (Node Black ly ky vy ry) kz vz (Node Black rz kx vx r)
                                Node _ lz kz vz (Node Red ly ky vy ry) -> Node Red (Node Black lz kz vz ly) ky vy (Node Black ry kx vx r)
                                Node _ ly ky vy ry -> Node Black (Node Red ly ky vy ry) kx vx r
                                Leaf -> Leaf
                            _ -> Node Black l kx vx (ins r k v)
                    else Node Black l k v r
            Node Red l kx vx r ->
                if k < kx
                    then Node Red (ins l k v) kx vx r
                else
                    if k > kx
                        then Node Red l kx vx (ins r k v)
                        else Node Red l k v r
        "#
    )
}

#[mono_test]
fn specialize_after_match() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            list_a : LinkedList Str
            list_a = Nil

            list_b : LinkedList Str
            list_b = Nil

            longest_linked_list list_a list_b

        LinkedList a : [Cons a (LinkedList a), Nil]

        longest_linked_list : LinkedList a, LinkedList a -> U64
        longest_linked_list = \list_a, list_b -> when list_a is
            Nil -> linked_list_length list_b
            Cons a aa -> when list_b is
                Nil -> linked_list_length list_a
                Cons b bb ->
                    length_a = (linked_list_length aa) + 1
                    length_b = linked_list_length list_b
                    if length_a > length_b
                        then length_a
                        else length_b

        linked_list_length : LinkedList a -> U64
        linked_list_length = \list -> when list is
            Nil -> 0
            Cons _ rest -> 1 + linked_list_length rest
        "#
    )
}

#[mono_test]
fn drop_specialize_after_struct() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        Tuple a b : { left : a, right : b }

        main =
            v = "value"
            t = { left: v, right: v }
            "result"
        "#
    )
}

#[mono_test]
fn record_update() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"
        main = f {a: [], b: [], c:[]}
        f : {a: List U64, b: List U64, c: List U64} -> {a: List U64, b: List U64, c: List U64}
        f = \record -> {record & a: List.set record.a 7 7, b: List.set record.b 8 8}
        "#
    )
}

#[mono_test]
fn drop_specialize_after_jump() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        Tuple a b : { left : a, right : b }

        main =
            v = "value"
            t = { left: { left: v, right: v }, right: v }
            tuple_item t

        tuple_item = \t ->
            true = Bool.true
            l = t.left
            x = if true then 1 else 0
            ll = l.left
            { left: t, right: ll}
        "#
    )
}

#[mono_test(mode = "test")]
fn dbg_in_expect() {
    indoc!(
        r#"
        interface Test exposes [] imports []

        expect
            dbg ""
            Bool.true
        "#
    )
}

#[mono_test]
fn drop_specialize_before_jump() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        Tuple a b : { left : a, right : b }

        main =
            v = "value"
            t = { left: v, right: v }
            tuple_item t

        tuple_item = \t ->
            true = Bool.true
            l = t.left
            x = if true then 1 else 0
            {left: l, right: {left: l, right: t}}
        "#
    )
}

#[mono_test]
fn dbg_str_followed_by_number() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            dbg ""
            42
        "#
    )
}

#[mono_test]
fn dbg_expr() {
    indoc!(
        r#"
        1 + (dbg 2)
        "#
    )
}

#[mono_test]
fn dbg_nested_expr() {
    indoc!(
        r#"
        dbg (dbg (dbg 1))
        "#
    )
}

#[mono_test]
fn dbg_inside_string() {
    indoc!(
        r#"
        "Hello ${dbg "world"}!"
        "#
    )
}

#[mono_test]
fn pizza_dbg() {
    indoc!(
        r#"
        1
        |> dbg
        |> Num.add 2
        |> dbg
        "#
    )
}

#[mono_test]
fn linked_list_reverse() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        LinkedList a : [Nil, Cons a (LinkedList a)]

        reverse : LinkedList a -> LinkedList a
        reverse = \list -> reverse_help Nil list

        reverse_help : LinkedList a, LinkedList a -> LinkedList a
        reverse_help = \accum, list ->
            when list is
                Nil -> accum
                Cons first rest -> reverse_help (Cons first accum) rest

        main : LinkedList I64
        main = reverse (Cons 42 Nil)
        "#
    )
}

#[mono_test]
fn linked_list_map() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        LinkedList a : [Nil, Cons a (LinkedList a)]

        map : (a -> b), LinkedList a -> LinkedList b
        map = \f, list ->
            when list is
                Nil -> Nil
                Cons x xs -> Cons (f x) (map f xs)

        main : LinkedList I64
        main = map (\x -> x + 1i64) (Cons 42 Nil)
        "#
    )
}

#[mono_test]
fn linked_list_filter() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        LinkedList a : [Nil, Cons a (LinkedList a)]

        filter : LinkedList a, (a -> Bool) -> LinkedList a
        filter = \list, predicate ->
            when list is
                Nil -> Nil
                Cons x xs ->
                    if predicate x then
                        Cons x (filter xs predicate)
                    else
                        filter xs predicate


        main : LinkedList I64
        main = filter (Cons 1 (Cons 2 Nil)) Num.isEven
        "#
    )
}

#[mono_test]
fn capture_void_layout_task() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        Fx a : {} -> a

        OtherTask ok err : Fx (Result ok err)

        succeed : ok -> OtherTask ok *
        succeed = \ok -> \{} -> Ok ok

        after : Fx a, (a -> Fx b) -> Fx b
        after = \fx, to_next ->
            after_inner = \{} ->
                fx_out = fx {}
                next = to_next fx_out
                next {}

            after_inner

        await : OtherTask a err, (a -> OtherTask b err) -> OtherTask b err
        await = \fx, to_next ->
            inner = after fx \result ->
                when result is
                    Ok a ->
                        b_fx = to_next a
                        b_fx
                    Err e -> (\{} -> Err e)
            inner

        for_each : List a, (a -> OtherTask {} err) -> OtherTask {} err
        for_each = \list, from_elem ->
            List.walk list (succeed {}) \task, elem ->
                await task \{} -> from_elem elem

        main : OtherTask {} []
        main =
            for_each [] \_ -> succeed {}
        "#
    )
}

#[mono_test]
fn non_nullable_unwrapped_instead_of_nullable_wrapped() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        Ast : [ A, B, C Str Ast ]

        main : Str
        main =
            x : Ast
            x = A

            when x is
                A -> "A"
                B -> "B"
                C _ _ -> "C"
        "#
    )
}

#[mono_test]
#[ignore = "Hits an unimplemented for abilities, not sure why..."]
fn inspect_custom_type() {
    indoc!(
        r#"
        app "test"
            imports []
            provides [main] to "./platform"

        HelloWorld := {} implements [Inspect { to_inspector: my_to_inspector }]

        my_to_inspector : HelloWorld -> Inspector f where f implements InspectFormatter
        my_to_inspector = \@HellowWorld {} ->
            Inspect.custom \fmt ->
                Inspect.apply (Inspect.str "Hello, World!\n") fmt

        main =
            Inspect.inspect (@HelloWorld {})
        "#
    )
}

#[mono_test]
fn inspect_derived_string() {
    indoc!(
        r#"
        app "test"
            imports []
            provides [main] to "./platform"

        main = Inspect.to_str "abc"
        "#
    )
}

#[mono_test]
fn inspect_derived_record() {
    indoc!(
        r#"
        app "test"
            imports []
            provides [main] to "./platform"

        main = Inspect.to_str {a: 7, b: 3dec}
        "#
    )
}
#[mono_test]
fn inspect_derived_record_one_field_string() {
    indoc!(
        r#"
        app "test"
            imports []
            provides [main] to "./platform"

        main = Inspect.to_str {a: "foo"}
        "#
    )
}

#[mono_test]
fn inspect_derived_record_two_field_strings() {
    indoc!(
        r#"
        app "test"
            imports []
            provides [main] to "./platform"

        main = Inspect.to_str {a: "foo", b: "bar"}
        "#
    )
}

#[mono_test]
fn inspect_derived_nested_record_string() {
    indoc!(
        r#"
        app "test"
            imports []
            provides [main] to "./platform"

        main = Inspect.to_str {a: {b: "bar"}}
        "#
    )
}

#[mono_test]
fn inspect_derived_tag_one_field_string() {
    indoc!(
        r#"
        app "test"
            imports []
            provides [main] to "./platform"

        main =
            x : [A Str]
            x = A "foo"
            Inspect.to_str x
        "#
    )
}

#[mono_test]
fn inspect_derived_tag_two_payloads_string() {
    indoc!(
        r#"
        app "test"
            imports []
            provides [main] to "./platform"

        main =
            x : [A Str Str]
            x = A "foo" "foo"
            Inspect.to_str x
        "#
    )
}

#[mono_test]
fn inspect_derived_list() {
    indoc!(
        r#"
        app "test"
            imports []
            provides [main] to "./platform"

        main = Inspect.to_str [1, 2, 3]
        "#
    )
}

#[mono_test(large_stack = "true")]
fn inspect_derived_dict() {
    indoc!(
        r#"
        app "test"
            imports []
            provides [main] to "./platform"

        main =
            Dict.from_list [("a", 1), ("b", 2)]
            |> Inspect.to_str
        "#
    )
}

#[mono_test]
fn issue_6196() {
    indoc!(
        r#"
        nth : List a, U64 -> Result a [OutOfBounds]
        nth = \l, i ->
            when (l, i) is
                ([], _) -> Err OutOfBounds
                ([e, ..], 0) -> Ok e
                ([_, .. as rest], _) -> nth rest (i - 1)

        nth ["a"] 0
        "#
    )
}

#[mono_test]
fn issue_5513() {
    indoc!(
        r"
        f = \state ->
            { state & a: state.b }
        f { a: 0, b: 0 }
        "
    )
}

#[mono_test]
fn issue_6174() {
    indoc!(
        r"
        g = Bool.false

        a = \_ ->
            if g then
                Ok 0
            else
                Err NoNumber

        b = \_ ->
            if g then
                Ok 0
            else
                Err NoNumber

        c = \_ ->
            [a {}, b {}]

        c {}
        "
    )
}

#[mono_test]
fn issue_6606_1() {
    indoc!(
        r"
        foo = \_ -> 0

        f =
            when [] is
                [.. as rest] if Bool.false -> foo rest
                [] -> 1
                _ -> 2

        f
        "
    )
}

#[mono_test]
fn issue_6606_2() {
    indoc!(
        r"
        foo = \_ -> 0

        f =
            when [] is
                [[.. as rest]] if Bool.false -> foo rest
                [[_]] -> 1
                _ -> 2

        f
        "
    )
}

#[mono_test]
fn dec_refcount_for_usage_after_early_return_in_if() {
    indoc!(
        r#"
        display_n = \n ->
            first = Num.to_str n
            second =
                if n == 1 then
                    return "early 1"
                else
                    third = Num.to_str (n + 1)
                    if n == 2 then
                        return "early 2"
                    else
                        third

            "${first}, ${second}"

        display_n 3
        "#
    )
}

#[mono_test]
fn return_annotated() {
    indoc!(
        r#"
        validate_input : Str -> Result U64 _
        validate_input = \str ->
            num = try Str.to_u64 str

            Ok num

        validate_input "123"
        "#
    )
}
