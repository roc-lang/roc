# Optional-index representation benchmark

Both binaries use the production column implementation. `biased` issues IDs
from zero and stores `id + 1`; `reserved-zero` leaves payload slot zero unused
and stores IDs unchanged. Both leave one quarter of the column absent.
Checksums are compared against independently accumulated expected results,
then normalized outside the measured loop to make the variants comparable.

From the repository root:

```sh
zig build --build-file ci/benchmarks_zig/optional_index/build.zig
taskset -c 0-15 perf stat -e cpu_core/instructions/ \
  ci/benchmarks_zig/optional_index/zig-out/bin/biased 1048576 100 payload
taskset -c 0-15 perf stat -e cpu_core/instructions/ \
  ci/benchmarks_zig/optional_index/zig-out/bin/reserved-zero 1048576 100 payload
```

Repeat alternating binaries; also run `index` instead of `payload`. The PMU
name and affinity above are specific to the measurement host: use the correct
instruction event and CPU set for another host. Timing is deliberately not
the comparison metric.

The counted process includes construction and destruction, so choose enough
rounds to make lookups dominate. The extra payload slot costs eight logical
bytes in this benchmark, not necessarily eight allocator-resident bytes.
Real consumers may have several parallel arrays and must account for a spare
entry in each. Microbenchmark savings do not establish a whole-compiler win.
