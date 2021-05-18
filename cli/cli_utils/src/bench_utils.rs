use criterion::{
    black_box, measurement::WallTime, BenchmarkGroup
};
use crate::helpers::{example_file, run_cmd, run_roc};
use std::path::Path;

// run without optimization, without input
fn exec_bench_simple(
    file: &Path,
    executable_filename: &str,
    expected_ending: &str,
    bench_group_opt: Option<&mut BenchmarkGroup<WallTime>>,
) {
    exec_benchmark(
        file,
        "",
        executable_filename,
        expected_ending,
        false,
        bench_group_opt,
    )
}

fn exec_benchmark(
    file: &Path,
    stdin_str: &str,
    executable_filename: &str,
    expected_ending: &str,
    run_optimized: bool,
    bench_group_opt: Option<&mut BenchmarkGroup<WallTime>>,
) {
    let flags: &[&str] = if run_optimized { &["--optimize"] } else { &[] };

    let compile_out = run_roc(&[&["build", file.to_str().unwrap()], flags].concat());

    if !compile_out.stderr.is_empty() {
        panic!("{}", compile_out.stderr);
    }

    assert!(
        compile_out.status.success(),
        "build ended with bad status {:?}",
        compile_out
    );

    check_cmd_output(file, stdin_str, executable_filename, expected_ending);

    bench_cmd(file, stdin_str, executable_filename, bench_group_opt);
}

fn check_cmd_output(
    file: &Path,
    stdin_str: &str,
    executable_filename: &str,
    expected_ending: &str,
) {
    let out = run_cmd(
        file.with_file_name(executable_filename).to_str().unwrap(),
        stdin_str,
        &[],
    );

    if !&out.stdout.ends_with(expected_ending) {
        panic!(
            "expected output to end with {:?} but instead got {:#?}",
            expected_ending, out
        );
    }
    assert!(out.status.success());
}

fn bench_cmd(
    file: &Path,
    stdin_str: &str,
    executable_filename: &str,
    bench_group_opt: Option<&mut BenchmarkGroup<WallTime>>,
) {
    if let Some(bench_group) = bench_group_opt {
        bench_group.bench_function(&format!("Benchmarking {:?}", executable_filename), |b| {
            b.iter(|| {
                run_cmd(
                    black_box(file.with_file_name(executable_filename).to_str().unwrap()),
                    black_box(stdin_str),
                    &[],
                )
            })
        });
    } else {
        run_cmd(
            black_box(file.with_file_name(executable_filename).to_str().unwrap()),
            black_box(stdin_str),
            &[],
        );
    }

}

pub fn bench_nqueens(bench_group_opt: Option<&mut BenchmarkGroup<WallTime>>) {
    exec_bench_simple(
        &example_file("benchmarks", "NQueens.roc"),
        "nqueens",
        "4\n",
        bench_group_opt,
    );
}

pub fn bench_cfold(bench_group_opt: Option<&mut BenchmarkGroup<WallTime>>) {
    exec_bench_simple(
        &example_file("benchmarks", "CFold.roc"),
        "cfold",
        "11 & 11\n",
        bench_group_opt,
    );
}

pub fn bench_deriv(bench_group_opt: Option<&mut BenchmarkGroup<WallTime>>) {
    exec_bench_simple(
        &example_file("benchmarks", "Deriv.roc"),
        "deriv",
        "1 count: 6\n2 count: 22\n",
        bench_group_opt,
    );
}

pub fn bench_rbtree(bench_group_opt: Option<&mut BenchmarkGroup<WallTime>>) {
    exec_bench_simple(
        &example_file("benchmarks", "RBTreeInsert.roc"),
        "rbtree-insert",
        "Node Black 0 {} Empty Empty\n",
        bench_group_opt,
    );
}

pub fn bench_rbtree_delete(bench_group_opt: Option<&mut BenchmarkGroup<WallTime>>) {
    exec_bench_simple(
        &example_file("benchmarks", "RBTreeDel.roc"),
        "rbtree-del",
        "30\n",
        bench_group_opt,
    );
}

pub fn bench_astar(bench_group_opt: Option<&mut BenchmarkGroup<WallTime>>) {
    exec_bench_simple(
        &example_file("benchmarks", "TestAStar.roc"),
        "test-astar",
        "True\n",
        bench_group_opt,
    );
}

pub fn bench_base64(bench_group_opt: Option<&mut BenchmarkGroup<WallTime>>) {
    exec_bench_simple(
        &example_file("benchmarks", "TestBase64.roc"),
        "test-base64",
        "encoded: SGVsbG8gV29ybGQ=\ndecoded: Hello World\n",
        bench_group_opt,
    );
}

pub fn bench_closure(bench_group_opt: Option<&mut BenchmarkGroup<WallTime>>) {
    exec_bench_simple(
        &example_file("benchmarks", "Closure.roc"),
        "closure",
        "",
        bench_group_opt,
    );
}