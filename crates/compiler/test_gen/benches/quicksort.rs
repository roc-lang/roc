#[path = "../src/helpers/mod.rs"]
mod helpers;

// defines roc_alloc and friends
pub use helpers::platform_functions::*;

use bumpalo::Bump;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use roc_gen_llvm::{llvm::build::LlvmBackendMode, run_roc::RocCallResult, run_roc_dylib};
use roc_mono::ir::OptLevel;
use roc_std::RocList;

// results April 9, 2023
//
// > pure roc quicksort      time:   [106.97 us 107.27 us 107.63 us]
// > roc zig quicksort       time:   [34.765 us 35.301 us 35.865 us]
// > rust std sort           time:   [20.413 us 20.623 us 20.838 us]

type Input = RocList<i64>;
type Output = RocList<i64>;

type Main<I, O> = unsafe extern "C" fn(I, *mut RocCallResult<O>);

const ZIG_ROC_QUICKSORT: &str = indoc::indoc!(
    r#"
    app "bench" provides [main] to "./platform"

    main : List I64 -> List I64
    main = \originalList -> List.sortAsc originalList
    "#
);

const PURE_ROC_QUICKSORT: &str = indoc::indoc!(
    r#"
    app "bench" provides [main] to "./platform"

    main : List I64 -> List I64
    main = \originalList ->
        n = List.len originalList

        quicksortHelp originalList 0 (n - 1)

    quicksortHelp : List (Num a), U64, U64 -> List (Num a)
    quicksortHelp = \list, low, high ->
        if low < high then
            when partition low high list is
                Pair partitionIndex partitioned ->
                    partitioned
                    |> quicksortHelp low (partitionIndex - 1)
                    |> quicksortHelp (partitionIndex + 1) high
        else
            list

    partition : U64, U64, List (Num a) -> [Pair U64 (List (Num a))]
    partition = \low, high, initialList ->
        when List.get initialList high is
            Ok pivot ->
                when partitionHelp low low initialList high pivot is
                    Pair newI newList ->
                        Pair newI (List.swap newList newI high)

            Err _ ->
                Pair low initialList

    partitionHelp : U64, U64, List (Num c), U64, Num c -> [Pair U64 (List (Num c))]
    partitionHelp = \i, j, list, high, pivot ->
        if j < high then
            when List.get list j is
                Ok value ->
                    if value <= pivot then
                        partitionHelp (i + 1) (j + 1) (List.swap list i j) high pivot
                    else
                        partitionHelp i (j + 1) list high pivot

                Err _ ->
                    Pair i list
        else
            Pair i list
    "#
);

fn roc_function<'a>(
    arena: &'a Bump,
    source: &str,
) -> libloading::Symbol<'a, Main<*mut Input, Output>> {
    let config = helpers::llvm::HelperConfig {
        mode: LlvmBackendMode::GenTest,
        ignore_problems: false,
        emit_debug_info: true,
        opt_level: OptLevel::Optimize,
    };

    let context = inkwell::context::Context::create();
    let (main_fn_name, errors, lib) = helpers::llvm::helper(
        arena,
        config,
        source,
        arena.alloc(context),
        roc_load::FunctionKind::LambdaSet,
    );

    assert!(errors.is_empty(), "Encountered errors:\n{errors}");

    run_roc_dylib!(arena.alloc(lib), main_fn_name, *mut Input, Output)
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let arena = Bump::new();

    let pure_roc_quicksort_main = roc_function(&arena, PURE_ROC_QUICKSORT);
    let roc_zig_quicksort_main = roc_function(&arena, ZIG_ROC_QUICKSORT);

    let input_numbers: Vec<_> = std::iter::repeat([1, 2, 3, 4, 5, 6, 7, 8])
        .flatten()
        .take(1000)
        .collect();

    let input = arena.alloc(RocList::from_slice(&input_numbers));

    c.bench_function("pure roc quicksort", |b| {
        b.iter(|| unsafe {
            let mut main_result = RocCallResult::default();

            assert!(input.is_unique());

            // reset_input
            input.copy_from_slice(&input_numbers);

            pure_roc_quicksort_main(black_box(input), &mut main_result);
        })
    });

    c.bench_function("roc zig quicksort", |b| {
        b.iter(|| unsafe {
            let mut main_result = RocCallResult::default();

            assert!(input.is_unique());

            // reset_input
            input.copy_from_slice(&input_numbers);

            roc_zig_quicksort_main(black_box(input), &mut main_result);
        })
    });

    c.bench_function("rust std sort", |b| {
        b.iter(|| unsafe {
            assert!(input.is_unique());

            // reset_input
            input.copy_from_slice(&input_numbers);

            // an attempt to block optimizing based on the input list
            let ptr = black_box(input.as_mut_ptr());
            let input = std::slice::from_raw_parts_mut(ptr, 1000);

            input.sort()
        })
    });
}

criterion_group!(quicksort_benches, criterion_benchmark);
criterion_main!(quicksort_benches);
