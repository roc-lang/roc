fn main() {
    println!("""
        To run benchmarks:
            - Install cargo criterion: cargo install cargo-criterion
            - Necessary to get cache misses...: sudo sh -c 'echo 1 >/proc/sys/kernel/perf_event_paranoid'
            - run: cargo criterion
    """);
}
