#![cfg(test)]
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
// we actually want to compare against the literal float bits
#![allow(clippy::float_cmp)]

#[macro_use]
extern crate indoc;

/// Used in the with_larger_debug_stack() function, for tests that otherwise
/// run out of stack space in debug builds (but don't in --release builds)
#[allow(dead_code)]
const EXPANDED_STACK_SIZE: usize = 8 * 1024 * 1024;

use roc_collections::all::MutMap;
use roc_load::ExecutionMode;
use roc_load::LoadConfig;
use roc_load::Threading;
use roc_module::symbol::Symbol;
use roc_mono::ir::Proc;
use roc_mono::ir::ProcLayout;
use roc_mono::layout::STLayoutInterner;
use test_mono_macros::*;

const TARGET_INFO: roc_target::TargetInfo = roc_target::TargetInfo::default_x86_64();

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

fn compiles_to_ir(test_name: &str, src: &str) {
    use bumpalo::Bump;
    use roc_packaging::cache::RocCacheDir;
    use std::path::PathBuf;

    let arena = &Bump::new();

    let filename = PathBuf::from("Test.roc");
    let src_dir = PathBuf::from("fake/test/path");

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

    let load_config = LoadConfig {
        target_info: TARGET_INFO,
        threading: Threading::Single,
        render: roc_reporting::report::RenderTarget::Generic,
        palette: roc_reporting::report::DEFAULT_PALETTE,
        exec_mode: ExecutionMode::Executable,
    };
    let loaded = roc_load::load_and_monomorphize_from_str(
        arena,
        filename,
        module_src,
        src_dir,
        Default::default(),
        RocCacheDir::Disallowed,
        load_config,
    );

    let mut loaded = match loaded {
        Ok(x) => x,
        Err(roc_load::LoadingProblem::FormattedReport(report)) => {
            println!("{}", report);
            panic!();
        }
        Err(e) => panic!("{:?}", e),
    };

    use roc_load::MonomorphizedModule;
    let MonomorphizedModule {
        module_id: home,
        procedures,
        exposed_to_host,
        layout_interner,
        ..
    } = loaded;

    let can_problems = loaded.can_problems.remove(&home).unwrap_or_default();
    let type_problems = loaded.type_problems.remove(&home).unwrap_or_default();

    if !can_problems.is_empty() {
        println!("Ignoring {} canonicalization problems", can_problems.len());
    }

    assert!(type_problems.is_empty());

    debug_assert_eq!(exposed_to_host.values.len(), 1);

    let main_fn_symbol = exposed_to_host.values.keys().copied().next().unwrap();

    verify_procedures(test_name, layout_interner, procedures, main_fn_symbol);
}

fn verify_procedures<'a>(
    test_name: &str,
    interner: STLayoutInterner<'a>,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
    main_fn_symbol: Symbol,
) {
    let index = procedures
        .keys()
        .position(|(s, _)| *s == main_fn_symbol)
        .unwrap();

    let mut procs_string = procedures
        .values()
        .map(|proc| proc.to_pretty(&interner, 200))
        .collect::<Vec<_>>();

    let main_fn = procs_string.swap_remove(index);

    procs_string.sort();
    procs_string.push(main_fn);

    let result = procs_string.join("\n");

    let path = format!("generated/{}.txt", test_name);
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
        panic!("Output changed: resolve conflicts and `git add` the file.");
    }
}

#[mono_test]
fn ir_int_literal() {
    r#"
    5
    "#
}

#[mono_test]
fn ir_int_add() {
    r#"
    x = [1,2]
    5 + 4 + 3 + List.len x
    "#
}

#[mono_test]
fn ir_assignment() {
    r#"
    x = 5

    x
    "#
}

#[mono_test]
fn ir_when_maybe() {
    r#"
    when Just 3 is
        Just n -> n
        Nothing -> 0
    "#
}

#[mono_test]
fn ir_when_these() {
    r#"
    when These 1 2 is
        This x -> x
        That y -> y
        These x _ -> x
    "#
}

#[mono_test]
fn ir_when_record() {
    r#"
    when { x: 1, y: 3.14 } is
        { x } -> x
    "#
}

#[mono_test]
fn ir_plus() {
    r#"
    1 + 2
    "#
}

#[mono_test]
fn ir_round() {
    r#"
    Num.round 3.6
    "#
}

#[mono_test]
fn ir_when_idiv() {
    r#"
    when Num.divTruncChecked 1000 10 is
        Ok val -> val
        Err _ -> -1
    "#
}

#[mono_test]
fn ir_two_defs() {
    r#"
    x = 3
    y = 4

    x + y
    "#
}

#[mono_test]
fn ir_when_just() {
    r#"
    x : [Nothing, Just I64]
    x = Just 41

    when x is
        Just v -> v + 0x1
        Nothing -> 0x1
    "#
}

#[mono_test]
fn one_element_tag() {
    r#"
    x : [Pair I64]
    x = Pair 2

    x
    "#
}

#[mono_test]
fn guard_pattern_true() {
    r#"
    wrapper = \{} ->
        when 2 is
            2 if Bool.false -> 42
            _ -> 0

    wrapper {}
    "#
}

#[mono_test]
fn when_on_record() {
    r#"
    when { x: 0x2 } is
        { x } -> x + 3
    "#
}

#[mono_test]
fn when_nested_maybe() {
    r#"
    Maybe a : [Nothing, Just a]

    x : Maybe (Maybe I64)
    x = Just (Just 41)

    when x is
        Just (Just v) -> v + 0x1
        _ -> 0x1
    "#
}

#[mono_test]
fn when_on_two_values() {
    r#"
    when Pair 2 3 is
        Pair 4 3 -> 9
        Pair a b -> a + b
    "#
}

#[mono_test]
fn dict() {
    r#"
    Dict.len Dict.empty
    "#
}

#[mono_test]
fn list_append_closure() {
    r#"
    myFunction = \l -> List.append l 42

    myFunction [1, 2]
    "#
}

#[mono_test]
fn list_append() {
    // TODO this leaks at the moment
    // ListAppend needs to decrement its arguments
    r#"
    List.append [1] 2
    "#
}

#[mono_test]
fn list_len() {
    r#"
    x = [1,2,3]
    y = [1.0]

    List.len x + List.len y
    "#
}

#[mono_test]
fn when_joinpoint() {
    r#"
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
    "#
}

#[mono_test]
fn simple_if() {
    r#"
    if Bool.true then
        1
    else
        2
    "#
}

#[mono_test]
fn if_multi_branch() {
    r#"
    if Bool.true then
        1
    else if Bool.false then
        2
    else
        3
    "#
}

#[mono_test]
fn when_on_result() {
    r#"
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
    "#
}

#[mono_test]
fn let_with_record_pattern() {
    r#"
    { x } = { x: 0x2, y: 3.14 }

    x
    "#
}

#[mono_test]
fn let_with_record_pattern_list() {
    r#"
    { x } = { x: [1, 3, 4], y: 3.14 }

    x
    "#
}

#[mono_test]
fn if_guard_bind_variable_false() {
    r#"
    wrapper = \{} ->
        when 10 is
            x if x == 5 -> 0
            _ -> 42

    wrapper {}
    "#
}

#[mono_test]
fn alias_variable() {
    r#"
    x = 5
    y = x

    3
    "#
}

#[mono_test]
fn alias_variable_and_return_it() {
    r#"
    x = 5
    y = x

    y
    "#
}

#[mono_test]
fn branch_store_variable() {
    r#"
    when 0 is
        1 -> 12
        a -> a
    "#
}

#[mono_test]
fn list_pass_to_function() {
    r#"
    x : List I64
    x = [1,2,3]

    id : List I64 -> List I64
    id = \y -> List.set y 0 0

    id x
    "#
}

#[mono_test]
fn record_optional_field_let_no_use_default() {
    r#"
    f = \r ->
        { x ? 10, y } = r
        x + y


    f { x: 4, y: 9 }
    "#
}

#[mono_test]
fn record_optional_field_let_use_default() {
    r#"
    f = \r ->
        { x ? 10, y } = r
        x + y


    f { y: 9 }
    "#
}

#[mono_test]
fn record_optional_field_function_no_use_default() {
    r#"
    f = \{ x ? 10, y } -> x + y


    f { x: 4, y: 9 }
    "#
}

#[mono_test]
fn record_optional_field_function_use_default() {
    r#"
    f = \{ x ? 10, y } -> x + y


    f { y: 9 }
    "#
}

#[mono_test]
fn quicksort_help() {
    // do we still need with_larger_debug_stack?
    r#"
    quicksortHelp : List (Num a), I64, I64 -> List (Num a)
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
}

#[mono_test]
fn quicksort_swap() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        swap = \list ->
            when Pair (List.get list 0) (List.get list 0) is
                Pair (Ok atI) (Ok atJ) ->
                    list
                    |> List.set 0 atJ
                    |> List.set 0 atI

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

//         partitionHelp : I64, I64, List (Num a), I64, (Num a) -> [Pair I64 (List (Num a))]
//         partitionHelp = \i, j, list, high, pivot ->
//             if j < high then
//                 when List.get list j is
//                     Ok value ->
//                         if value <= pivot then
//                             partitionHelp (i + 1) (j + 1) (swap (i + 1) j list) high pivot
//                         else
//                             partitionHelp i (j + 1) list high pivot

//                     Err _ ->
//                         Pair i list
//             else
//                 Pair i list

//         main =
//             partitionHelp 0 0 [] 0 0
//         "#
//     )
// }

// #[ignore]
// #[mono_test]
// fn quicksort_full() {
//     indoc!(
//         r#"
//         app "test" provides [main] to "./platform"

//         quicksortHelp : List (Num a), I64, I64 -> List (Num a)
//         quicksortHelp = \list, low, high ->
//             if low < high then
//                 (Pair partitionIndex partitioned) = partition low high list

//                 partitioned
//                     |> quicksortHelp low (partitionIndex - 1)
//                     |> quicksortHelp (partitionIndex + 1) high
//             else
//                 list

//         swap : I64, I64, List a -> List a
//         swap = \i, j, list ->
//             when Pair (List.get list i) (List.get list j) is
//                 Pair (Ok atI) (Ok atJ) ->
//                     list
//                         |> List.set i atJ
//                         |> List.set j atI

//                 _ ->
//                     []

//         partition : I64, I64, List (Num a) -> [Pair I64 (List (Num a))]
//         partition = \low, high, initialList ->
//             when List.get initialList high is
//                 Ok pivot ->
//                     when partitionHelp (low - 1) low initialList high pivot is
//                         Pair newI newList ->
//                             Pair (newI + 1) (swap (newI + 1) high newList)

//                 Err _ ->
//                     Pair (low - 1) initialList

//         partitionHelp : I64, I64, List (Num a), I64, (Num a) -> [Pair I64 (List (Num a))]
//         partitionHelp = \i, j, list, high, pivot ->
//             if j < high then
//                 when List.get list j is
//                     Ok value ->
//                         if value <= pivot then
//                             partitionHelp (i + 1) (j + 1) (swap (i + 1) j list) high pivot
//                         else
//                             partitionHelp i (j + 1) list high pivot

//                     Err _ ->
//                         Pair i list
//             else
//                 Pair i list

//         quicksort = \originalList ->
//             n = List.len originalList
//             quicksortHelp originalList 0 (n - 1)

//         main =
//             quicksort [1,2,3]
//         "#
//     )
// }

#[mono_test]
fn factorial() {
    r#"
    factorial = \n, accum ->
        when n is
            0 ->
                accum

            _ ->
                factorial (n - 1) (n * accum)

    factorial 10 1
    "#
}

#[mono_test]
fn is_nil() {
    r#"
    ConsList a : [Cons a (ConsList a), Nil]

    isNil : ConsList a -> Bool
    isNil = \list ->
        when list is
            Nil -> Bool.true
            Cons _ _ -> Bool.false

    isNil (Cons 0x2 Nil)
    "#
}

#[mono_test]
#[ignore]
fn has_none() {
    r#"
    Maybe a : [Just a, Nothing]
    ConsList a : [Cons a (ConsList a), Nil]

    hasNone : ConsList (Maybe a) -> Bool
    hasNone = \list ->
        when list is
            Nil -> Bool.false
            Cons Nothing _ -> Bool.true
            Cons (Just _) xs -> hasNone xs

    hasNone (Cons (Just 3) Nil)
    "#
}

#[mono_test]
fn mk_pair_of() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        mkPairOf = \x -> Pair x x

        main =
            mkPairOf [1,2,3]
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
    r#"
    wrapper = \{} ->
        List.get [1,2,3] 0

    wrapper {}
    "#
}

#[mono_test]
fn peano() {
    r#"
    Peano : [S Peano, Z]

    three : Peano
    three = S (S (S Z))

    three
    "#
}

#[mono_test]
fn peano1() {
    r#"
    Peano : [S Peano, Z]

    three : Peano
    three = S (S (S Z))

    when three is
        Z -> 0
        S _ -> 1
    "#
}

#[mono_test]
fn peano2() {
    r#"
    Peano : [S Peano, Z]

    three : Peano
    three = S (S (S Z))

    when three is
        S (S _) -> 1
        S (_) -> 0
        Z -> 0
    "#
}

#[mono_test]
fn optional_when() {
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
}

#[mono_test]
fn nested_pattern_match() {
    r#"
    Maybe a : [Nothing, Just a]

    x : Maybe (Maybe I64)
    x = Just (Just 41)

    when x is
        Just (Just v) -> v + 0x1
        _ -> 0x1
    "#
}

#[mono_test]
#[ignore]
fn linked_list_length_twice() {
    r#"
    LinkedList a : [Nil, Cons a (LinkedList a)]

    nil : LinkedList I64
    nil = Nil

    length : LinkedList a -> I64
    length = \list ->
        when list is
            Nil -> 0
            Cons _ rest -> 1 + length rest

    length nil + length nil
    "#
}

#[mono_test]
fn rigids() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        swap : Nat, Nat, List a -> List a
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
    )
}

#[mono_test]
fn let_x_in_x() {
    r#"
    x = 5

    answer =
        1337

    unused =
        nested = 17
        nested

    answer
    "#
}

#[mono_test]
fn let_x_in_x_indirect() {
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
            myList : List (Str -> Str)
            myList = []

            myClosure : Str -> Str
            myClosure = \_ -> "bar"

            choose =
                if Bool.false then
                    myList
                else
                    [myClosure]

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

            f : U8, U32 -> Nat
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

            f : F32, F64 -> Nat
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

            f1 : U8, U32 -> Nat
            f1 = f

            f2 : U32, U8 -> Nat
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
            b = Bar
            f : [Foo, Bar], [Bar, Baz] -> U8
            f = \_, _ -> 18
            f b b
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
            f : [A Bool Bool] -> Nat
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
            l = [1, 2, 3]

            f : List U8, List U16 -> Nat
            f = \_, _ -> 18

            f l l
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
        r#"
        n : U8
        n = 1
        f = \{} -> (\a -> n)
        g = f {}
        g {}
        "#
    )
}

#[mono_test]
fn issue_2535_polymorphic_fields_referenced_in_list() {
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
        r#"
        wrap = \value -> Tag value
        wrapIt = wrap
        wrapIt 42
        "#
    )
}

#[mono_test]
fn issue_2583_specialize_errors_behind_unified_branches() {
    indoc!(
        r#"
        if Bool.true then List.first [] else Str.toI64 ""
        "#
    )
}

#[mono_test]
fn issue_2810() {
    indoc!(
        r#"
        Command : [Command Tool]

        Job : [Job Command]

        Tool : [SystemTool, FromJob Job]

        a : Job
        a = Job (Command (FromJob (Job (Command SystemTool))))
        a
        "#
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

        MHash has
            hash : a -> U64 | a has MHash

        Id := U64 has [MHash {hash}]

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

        fromUtf8 : U8 -> Result Variable [InvalidVariableUtf8]
        fromUtf8 = \char ->
            Ok (@Variable char)

        out = fromUtf8 98
        "#
    )
}

#[mono_test]
fn encode() {
    indoc!(
        r#"
        app "test" provides [myU8Bytes] to "./platform"

        MEncoder fmt := List U8, fmt -> List U8 | fmt has Format

        MEncoding has
          toEncoder : val -> MEncoder fmt | val has MEncoding, fmt has Format

        Format has
          u8 : U8 -> MEncoder fmt | fmt has Format


        Linear := {} has [Format {u8}]

        u8 = \n -> @MEncoder (\lst, @Linear {} -> List.append lst n)

        MyU8 := U8 has [MEncoding {toEncoder}]

        toEncoder = \@MyU8 n -> u8 n

        myU8Bytes =
            when toEncoder (@MyU8 15) is
                @MEncoder doEncode -> doEncode [] (@Linear {})
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

        out = List.sortAsc [4, 3, 2, 1]
        "#
    )
}

#[mono_test]
#[ignore]
fn encode_custom_type() {
    indoc!(
        r#"
        app "test"
            imports [Encode.{ toEncoder }, Json]
            provides [main] to "./platform"

        HelloWorld := {}
        toEncoder = \@HelloWorld {} ->
            Encode.custom \bytes, fmt ->
                bytes
                    |> Encode.appendWith (Encode.string "Hello, World!\n") fmt

        main =
            result = Str.fromUtf8 (Encode.toBytes (@HelloWorld {}) Json.toUtf8)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test]
fn encode_derived_string() {
    indoc!(
        r#"
        app "test"
            imports [Encode.{ toEncoder }, Json]
            provides [main] to "./platform"

        main =
            result = Str.fromUtf8 (Encode.toBytes "abc" Json.toUtf8)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test]
#[ignore = "TODO"]
fn encode_derived_record() {
    indoc!(
        r#"
        app "test"
            imports [Encode.{ toEncoder }, Json]
            provides [main] to "./platform"

        main =
            result = Str.fromUtf8 (Encode.toBytes {a: "a"} Json.toUtf8)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test]
fn tail_call_elimination() {
    indoc!(
        r#"
        sum = \n, accum ->
            when n is
                0 -> accum
                _ -> sum (n - 1) (n + accum)

        sum 1_000_000 0
        "#
    )
}

#[mono_test]
fn tail_call_with_same_layout_different_lambda_sets() {
    indoc!(
        r#"
        chain = \in, buildLazy ->
            \{} ->
                thunk = buildLazy in
                thunk {}

        chain 1u8 \_ -> chain 1u8 \_ -> (\{} -> "")
        "#
    )
}

#[mono_test]
fn tail_call_with_different_layout() {
    indoc!(
        r#"
        chain = \in, buildLazy ->
            \{} ->
                thunk = buildLazy in
                thunk {}

        chain 1u8 \_ -> chain 1u16 \_ -> (\{} -> "")
        "#
    )
}

#[mono_test]
fn lambda_capture_niche_u8_vs_u64() {
    indoc!(
        r#"
        capture : _ -> ({} -> Str)
        capture = \val ->
            \{} ->
                Num.toStr val

        x : [True, False]
        x = True

        fun =
            when x is
                True -> capture 123u64
                False -> capture 18u8

        fun {}
        "#
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

        capture2 = \val -> \{} -> "\(val)"

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
        r#"
        9999999999999999999 + 1
        "#
    )
}

#[mono_test]
fn choose_i128_layout() {
    indoc!(
        r#"
        {
            a: 18446744073709551616 + 1,
            b: -9223372036854775809 + 1,
        }
        "#
    )
}

#[mono_test]
fn choose_u128_layout() {
    indoc!(
        r#"
        170141183460469231731687303715884105728 + 1
        "#
    )
}

#[mono_test]
fn recursive_call_capturing_function() {
    indoc!(
        r#"
        a = \b ->
            c : U32 -> U32
            c = \d ->
                if Bool.true then d else c (d+b)
            c 0

        a 6
        "#
    )
}

#[mono_test]
fn call_function_in_empty_list() {
    indoc!(
        r#"
        lst : List ({} -> {})
        lst = []
        List.map lst \f -> f {}
        "#
    )
}

#[mono_test]
fn call_function_in_empty_list_unbound() {
    indoc!(
        r#"
        lst = []
        List.map lst \f -> f {}
        "#
    )
}

#[mono_test]
fn instantiate_annotated_as_recursive_alias_toplevel() {
    indoc!(
        r#"
        app "test" provides [it] to "./platform"

        Value : [Nil, Array (List Value)]

        foo : [Nil]*
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

            foo : [Nil]*
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

            foo : [Nil]*
            foo = Nil

            v1 : Value
            v1 = foo

            Value2 : [Nil, B U16, Array (List Value)]

            v2 : Value2
            v2 = foo

            {v1, v2}
        "#
    )
}

#[mono_test]
fn encode_derived_record_one_field_string() {
    indoc!(
        r#"
        app "test"
            imports [Encode.{ toEncoder }, Json]
            provides [main] to "./platform"

        main =
            result = Str.fromUtf8 (Encode.toBytes {a: "foo"} Json.toUtf8)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test]
fn encode_derived_record_two_field_strings() {
    indoc!(
        r#"
        app "test"
            imports [Encode.{ toEncoder }, Json]
            provides [main] to "./platform"

        main =
            result = Str.fromUtf8 (Encode.toBytes {a: "foo", b: "bar"} Json.toUtf8)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test]
fn encode_derived_nested_record_string() {
    indoc!(
        r#"
        app "test"
            imports [Encode.{ toEncoder }, Json]
            provides [main] to "./platform"

        main =
            result = Str.fromUtf8 (Encode.toBytes {a: {b: "bar"}} Json.toUtf8)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test]
fn encode_derived_tag_one_field_string() {
    indoc!(
        r#"
        app "test"
            imports [Encode.{ toEncoder }, Json]
            provides [main] to "./platform"

        main =
            x : [A Str]
            x = A "foo"
            result = Str.fromUtf8 (Encode.toBytes x Json.toUtf8)
            when result is
                Ok s -> s
                _ -> "<bad>"
        "#
    )
}

#[mono_test]
fn encode_derived_tag_two_payloads_string() {
    indoc!(
        r#"
        app "test"
            imports [Encode.{ toEncoder }, Json]
            provides [main] to "./platform"

        main =
            x : [A Str Str]
            x = A "foo" "foo"
            result = Str.fromUtf8 (Encode.toBytes x Json.toUtf8)
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
        r#"
        1 / 200
        "#
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

        provideThunk = \x ->
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

            thunk = provideThunk x

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
#[ignore = "https://github.com/roc-lang/roc/issues/4561"]
fn recursive_function_and_union_with_inference_hole() {
    let _tracing_guards = roc_tracing::setup_tracing!();

    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        Html state : [
            Element (List (Html state)),
        ]

        translateStatic : Html _ -> Html _
        translateStatic = \node ->
            when node is
                Element children ->
                    newChildren = List.map children translateStatic

                    Element newChildren

        main = when translateStatic (Element []) is
            _ -> ""
        "#
    )
}

#[mono_test]
fn crash() {
    indoc!(
        r#"
        app "test" provides [main] to "./platform"

        getInfallible = \result -> when result is
            Ok x -> x
            _ -> crash "turns out this was fallible"

        main =
            x : [Ok U64, Err Str]
            x = Ok 78
            getInfallible x
        "#
    )
}
