use cli_utils::bench_utils::{
    bench_astar, bench_base64, bench_cfold, bench_closure, bench_deriv, bench_nqueens,
    bench_rbtree, bench_rbtree_delete,
};
use criterion::{
    criterion_group, criterion_main, measurement::WallTime, BenchmarkGroup, Criterion, SamplingMode,
};

fn bench_group_sample_100(c: &mut Criterion) {
    let mut group = c.benchmark_group("bench-group-sample-100-unoptimized");
    // calculate statistics based on a fixed(flat) 100 runs
    group.sampling_mode(SamplingMode::Flat);

    let bench_funcs: Vec<fn(Option<&mut BenchmarkGroup<WallTime>>) -> ()> = vec![
        bench_nqueens,
        bench_cfold,
        bench_deriv,
        bench_rbtree,
        bench_rbtree_delete,
        bench_astar,
        bench_base64,
        bench_closure,
    ];

    for bench_func in bench_funcs.iter() {
        bench_func(Some(&mut group))
    }

    group.finish();
}

criterion_group!(benches, bench_group_sample_100);
criterion_main!(benches);
