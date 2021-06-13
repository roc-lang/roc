use cli_utils::bench_utils::{
    bench_cfold, bench_deriv, bench_nqueens, bench_quicksort, bench_rbtree_ck, bench_rbtree_delete,
};
use criterion::{
    criterion_group, criterion_main, measurement::WallTime, BenchmarkGroup, Criterion, SamplingMode,
};

fn bench_group_wall_time(c: &mut Criterion) {
    let mut group = c.benchmark_group("bench-group_wall-time");
    // calculate statistics based on a fixed(flat) 300 runs
    group.sampling_mode(SamplingMode::Flat);
    group.sample_size(300);

    let bench_funcs: Vec<fn(Option<&mut BenchmarkGroup<WallTime>>) -> ()> = vec![
        bench_nqueens,       // queens 11
        bench_cfold,         // e = mkExpr 17 1
        bench_deriv,         // nest deriv 8 f
        bench_rbtree_ck,     // ms = makeMap 5 80000
        bench_rbtree_delete, // m = makeMap 100000
        bench_quicksort,     // list size 10000
    ];

    for bench_func in bench_funcs.iter() {
        bench_func(Some(&mut group))
    }

    group.finish();
}

criterion_group!(benches, bench_group_wall_time);
criterion_main!(benches);
