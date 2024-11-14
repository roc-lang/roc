use std::time::Duration;

use cli_test_utils::bench_utils::{
    bench_cfold, bench_deriv, bench_nqueens, bench_quicksort, bench_rbtree_ck,
};
use criterion::{measurement::WallTime, BenchmarkGroup, Criterion, SamplingMode};

fn bench_group_wall_time(c: &mut Criterion) {
    let mut group = c.benchmark_group("bench-group_wall-time");
    // calculate statistics based on a fixed(flat) x runs
    group.sampling_mode(SamplingMode::Flat);

    let default_nr_of_runs = 200;
    let nr_of_runs = match std::env::var("BENCH_DRY_RUN") {
        Ok(val) => {
            if val == "1" {
                10 // minimum value allowed by criterion
            } else {
                default_nr_of_runs
            }
        }
        Err(_) => default_nr_of_runs,
    };

    group.sample_size(nr_of_runs);

    let bench_funcs: Vec<fn(Option<&mut BenchmarkGroup<WallTime>>)> = vec![
        bench_nqueens,   // queens 11
        bench_cfold,     // e = mkExpr 17 1
        bench_deriv,     // nest deriv 8 f
        bench_rbtree_ck, // ms = makeMap 5 80000
        // bench_rbtree_delete, // m = makeMap 100000
        bench_quicksort, // list size 10000
    ];

    for bench_func in bench_funcs.iter() {
        bench_func(Some(&mut group))
    }

    group.finish();
}

// use short warm up and measurement time on dry run
fn make_config() -> Criterion {
    let default_config = Criterion::default();

    match std::env::var("BENCH_DRY_RUN") {
        Ok(val) => {
            if val == "1" {
                default_config
                    .warm_up_time(Duration::new(1, 0))
                    .measurement_time(Duration::new(1, 0))
            } else {
                default_config
            }
        }
        Err(_) => default_config,
    }
}

fn all_benches() {
    let mut criterion: Criterion<_> = make_config().configure_from_args();

    bench_group_wall_time(&mut criterion);
}

fn main() {
    all_benches();

    Criterion::default().configure_from_args().final_summary();
}
