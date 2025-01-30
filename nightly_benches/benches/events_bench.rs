// Keep this benchmark. It's commented because it requires nightly rust.
use cli_test_utils::bench_utils::{
    bench_cfold, bench_deriv, bench_nqueens, bench_quicksort, bench_rbtree_ck, bench_rbtree_delete,
};
use criterion_perf_events::Perf;
use perfcnt::linux::HardwareEventType as Hardware;
use perfcnt::linux::PerfCounterBuilderLinux as Builder;

use criterion::{criterion_group, criterion_main, BenchmarkGroup, Criterion, SamplingMode};

fn bench_group(c: &mut Criterion<Perf>, hw_event_str: &str) {
    let mut group = c.benchmark_group(format!("bench-group_no-opt_{}", hw_event_str));
    // calculate statistics based on a fixed(flat) 100 runs
    group.sampling_mode(SamplingMode::Flat);

    let bench_funcs: Vec<fn(Option<&mut BenchmarkGroup<Perf>>)> = vec![
        bench_nqueens,
        bench_cfold,
        bench_deriv,
        bench_rbtree_ck,
        // bench_rbtree_delete,
        bench_quicksort,
    ];

    for bench_func in bench_funcs.iter() {
        bench_func(Some(&mut group))
    }

    group.finish();
}

use perfcnt::linux::HardwareEventType;

fn init_criterion(event: HardwareEventType) -> Criterion<Perf> {
    Criterion::default().with_measurement(Perf::new(Builder::from_hardware_event(event)))
}

fn bench_instructions(c: &mut Criterion<Perf>) {
    bench_group(c, "instructions")
}

criterion_group!(
    name = benches_instructions;
    config = init_criterion(Hardware::Instructions);
    targets = bench_instructions
);

fn bench_cache_refs(c: &mut Criterion<Perf>) {
    bench_group(c, "cache_refs")
}

criterion_group!(
    name = benches_cache_refs;
    config = init_criterion(Hardware::CacheReferences);
    targets = bench_cache_refs
);

fn bench_cache_misses(c: &mut Criterion<Perf>) {
    bench_group(c, "cache_misses")
}

criterion_group!(
    name = benches_cache_misses;
    config = init_criterion(Hardware::CacheMisses);
    targets = bench_cache_misses
);

fn bench_branch_instructions(c: &mut Criterion<Perf>) {
    bench_group(c, "branch_instructions")
}

criterion_group!(
    name = benches_branch_instructions;
    config = init_criterion(Hardware::BranchInstructions);
    targets = bench_branch_instructions
);

fn bench_branch_misses(c: &mut Criterion<Perf>) {
    bench_group(c, "branch_misses")
}

criterion_group!(
    name = benches_branch_misses;
    config = init_criterion(Hardware::BranchMisses);
    targets = bench_branch_misses
);

criterion_main!(
    benches_instructions,
    benches_cache_refs,
    benches_cache_misses,
    benches_branch_instructions,
    benches_branch_misses
);
