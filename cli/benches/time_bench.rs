use cli_utils::bench_utils::{
    bench_cfold, bench_deriv, bench_nqueens,
    bench_rbtree_ck, bench_rbtree_delete,
};
use criterion::{
    criterion_group, criterion_main, measurement::WallTime, BenchmarkGroup, Criterion, SamplingMode,
};

fn bench_group_wall_time(c: &mut Criterion) {
    let mut group = c.benchmark_group("bench-group_wall-time");
    // calculate statistics based on a fixed(flat) 100 runs
    group.sampling_mode(SamplingMode::Flat);
    group.sample_size(200);

    let bench_funcs: Vec<fn(Option<&mut BenchmarkGroup<WallTime>>) -> ()> = vec![
        bench_nqueens, // 11
        bench_cfold, // e = mkExpr 12 1
        bench_deriv, // nest deriv 7 f
        bench_rbtree_ck, // ms = makeMap 5 5600
        bench_rbtree_delete, // m = makeMap 6000
        // TODO quicksort
    ];

    for bench_func in bench_funcs.iter() {
        bench_func(Some(&mut group))
    }

    group.finish();
}

criterion_group!(benches, bench_group_wall_time);
criterion_main!(benches);
