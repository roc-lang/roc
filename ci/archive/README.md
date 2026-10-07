# Archived CI scripts

Nothing in the repository references these scripts: no workflow, `build.zig`
step, other script, or document. They are kept here, unchanged, pending
deletion, in case one turns out to be used by hand. Move a script back to its
original location under `ci/` if you still need it. Scripts that locate the
repository root relative to their own path (`$(dirname "$0")/..`) will need
that adjusted if run from here.

| Script | Original location |
|---|---|
| `build_basic_cli.sh` | `ci/` |
| `build_basic_webserver.sh` | `ci/` |
| `update_basic_cli_url.sh` | `ci/` |
| `guarded_zig.sh` | `ci/` |
| `check_mir_cutover_contracts.pl` | `ci/` |
| `lambda_mono_mutation_check.sh` + `lambda_mono_mutations/` | `ci/` |
| `spec_constr_mutation_check.sh` + `spec_constr_mutations/` | `ci/` |
| `benchmarks_zig/run_snapshot_benchmark.sh` | `ci/benchmarks_zig/` |
