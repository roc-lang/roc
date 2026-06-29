#!/usr/bin/env bash
set -euo pipefail

# FX File Execution Benchmarks
# Compares roc binary execution time between main and PR branches

MAIN_ROC=$1
PR_ROC=$2
BASE_BRANCH="${3:-}"  # Optional: base branch name
PR_BRANCH="${4:-}"    # Optional: PR/current branch name

# Check if file content differs between branches
# Returns 0 if same, 1 if different, 2 if comparison not possible
check_file_changed() {
    local file="$1"
    local base_branch="$2"
    local pr_branch="$3"

    if [ -z "$base_branch" ] || [ -z "$pr_branch" ]; then
        return 2  # Branches not provided
    fi

    local base_sha pr_sha
    base_sha=$(git show "$base_branch:$file" 2>/dev/null | shasum -a 256 | cut -d' ' -f1) || return 2
    pr_sha=$(git show "$pr_branch:$file" 2>/dev/null | shasum -a 256 | cut -d' ' -f1) || return 2

    if [ "$base_sha" != "$pr_sha" ]; then
        return 1  # Different
    fi
    return 0  # Same
}

# Print diff between branches for a file (indented for readability)
print_file_diff() {
    local file="$1"
    local base_branch="$2"
    local pr_branch="$3"

    echo "    Diff:"
    git diff "$base_branch".."$pr_branch" -- "$file" 2>/dev/null | sed 's/^/    /'
}

print_probe_log() {
    local label="$1"
    local probe_log="$2"

    echo "  $label:"
    if [ -s "$probe_log" ]; then
        tail -n 40 "$probe_log" | sed 's/^/    /'
    else
        echo "    <no output>"
    fi
}

print_lldb_backtrace() {
    if ! command -v lldb >/dev/null 2>&1; then
        echo "  lldb is not available on this runner"
        return 0
    fi

    local -a cmd=("$@")

    echo "  lldb backtrace for: ${cmd[*]}"
    lldb --batch \
        -o "run" \
        -o "thread backtrace all" \
        -- "${cmd[@]}" 2>&1 | tail -n 240 | sed 's/^/    /' || true
}

diagnose_pr_benchmark_failure() {
    local pr_roc="$1"
    local fx_file="$2"
    local roc_extra_args="$3"
    local roc_subcommand="${4:-}"

    local -a cmd=("$pr_roc")
    if [ -n "$roc_subcommand" ]; then
        cmd+=("$roc_subcommand")
    fi
    cmd+=("$fx_file" "--no-cache")

    if [ -n "$roc_extra_args" ]; then
        local -a extra_arg_array=()
        read -r -a extra_arg_array <<< "$roc_extra_args"
        cmd+=("${extra_arg_array[@]}")
    fi

    echo "Debugging failed PR benchmark command: ${cmd[*]}"

    local attempt
    for attempt in 1 2 3 4 5; do
        local debug_log="/tmp/bench_pr_failure_${attempt}.log"

        set +e
        "${cmd[@]}" >"$debug_log" 2>&1
        local exit_code=$?
        set -e

        echo "  PR debug attempt $attempt exit code: $exit_code"
        print_probe_log "PR debug attempt $attempt output" "$debug_log"

        if [ "$exit_code" -eq 139 ] || grep -qi "Segmentation fault" "$debug_log"; then
            print_lldb_backtrace "${cmd[@]}"
            return 0
        fi
    done

    echo "  Crash did not reproduce in direct retry loop; running once under lldb anyway."
    print_lldb_backtrace "${cmd[@]}"
}

exit_code_is_benchmarkable() {
    local exit_code="$1"
    local extra_args="$2"

    if [ "$exit_code" -eq 0 ]; then
        return 0
    fi

    if [[ "$extra_args" =~ --ignore-failure=([^[:space:]]+) ]]; then
        local ignore_codes="${BASH_REMATCH[1]}"
        local old_ifs="$IFS"
        IFS=','
        for code in $ignore_codes; do
            if [ "$exit_code" -eq "$code" ]; then
                IFS="$old_ifs"
                return 0
            fi
        done
        IFS="$old_ifs"
    fi

    if [[ "$extra_args" =~ --discard-failure=([^[:space:]]+) ]]; then
        local discard_codes="${BASH_REMATCH[1]}"
        local old_ifs="$IFS"
        IFS=','
        for code in $discard_codes; do
            if [ "$exit_code" -eq "$code" ]; then
                IFS="$old_ifs"
                return 0
            fi
        done
        IFS="$old_ifs"
    fi

    return 1
}

is_intentional_error_fixture() {
    local filename="$1"

    # These files intentionally exercise diagnostics, --allow-errors, or
    # runtime crash reporting. They are test fixtures, not successful execution
    # benchmarks. Do not add compiler crash/regression repros here; those should
    # fail if the PR binary cannot run them.
    case "$filename" in
        allow_errors_type_mismatch.roc|\
        division_by_zero.roc|\
        inspect_wrong_sig_test.roc|\
        issue8433.roc|\
        issue8517.roc|\
        issue8826_full.roc|\
        issue8826_minimal.roc|\
        issue8943.roc|\
        num_method_call.roc|\
        parse_error.roc|\
        run_allow_errors.roc|\
        stack_overflow_runtime.roc|\
        test_type_mismatch.roc|\
        unused_state_var.roc)
            return 0
            ;;
    esac

    return 1
}

probe_command() {
    local roc_bin="$1"
    local fx_file="$2"
    local roc_extra_args="$3"
    local label="$4"
    local roc_subcommand="${5:-}"
    local filename
    filename=$(basename "$fx_file")
    local subcommand_suffix=""
    if [ -n "$roc_subcommand" ]; then
        subcommand_suffix="_${roc_subcommand}"
    fi
    local probe_log="/tmp/bench_probe_${filename%.roc}_${label}${subcommand_suffix}.log"

    local -a cmd=("$roc_bin")
    if [ -n "$roc_subcommand" ]; then
        cmd+=("$roc_subcommand")
    fi
    cmd+=("$fx_file" "--no-cache")
    if [ -n "$roc_extra_args" ]; then
        local -a extra_arg_array=()
        read -r -a extra_arg_array <<< "$roc_extra_args"
        cmd+=("${extra_arg_array[@]}")
    fi

    set +e
    "${cmd[@]}" >"$probe_log" 2>&1
    PROBE_EXIT_CODE=$?
    set -e
    PROBE_LOG="$probe_log"
}

preflight_benchmark() {
    local extra_args="$1"
    local main_roc="$2"
    local pr_roc="$3"
    local fx_file="$4"
    local roc_extra_args="$5"
    local roc_subcommand="${6:-}"

    local filename
    filename=$(basename "$fx_file")

    probe_command "$main_roc" "$fx_file" "$roc_extra_args" "main" "$roc_subcommand"
    local main_exit_code="$PROBE_EXIT_CODE"
    local main_log="$PROBE_LOG"

    probe_command "$pr_roc" "$fx_file" "$roc_extra_args" "pr" "$roc_subcommand"
    local pr_exit_code="$PROBE_EXIT_CODE"
    local pr_log="$PROBE_LOG"

    # Note: when roc_subcommand is "check" or "build", the printed output may
    # include execution-time metadata, so we only compare exit codes. For the
    # interpret case (empty subcommand) we still compare stdout/stderr because
    # diverging output usually means the two binaries are doing different work.
    if exit_code_is_benchmarkable "$main_exit_code" "$extra_args"; then
        if exit_code_is_benchmarkable "$pr_exit_code" "$extra_args"; then
            if [ "$main_exit_code" -ne "$pr_exit_code" ]; then
                echo "Skipping $filename (main exit code $main_exit_code differs from pr exit code $pr_exit_code)"
                print_probe_log "main output" "$main_log"
                print_probe_log "pr output" "$pr_log"
                PRECHECK_SKIP_REASON="exit_code_diff"
                return 2
            fi

            if [ -z "$roc_subcommand" ] && ! cmp -s "$main_log" "$pr_log"; then
                echo "Skipping $filename (main and PR outputs differ, so this is not a comparable benchmark)"
                echo "  main exit code: $main_exit_code"
                echo "  pr exit code: $pr_exit_code"
                print_probe_log "main output" "$main_log"
                print_probe_log "pr output" "$pr_log"
                PRECHECK_SKIP_REASON="output_diff"
                return 2
            fi
            return 0
        fi

        echo "ERROR: PR command is not benchmarkable for $filename"
        echo "  main exit code: $main_exit_code"
        echo "  pr exit code: $pr_exit_code"
        print_probe_log "pr output" "$pr_log"
        return 1
    fi

    echo "Skipping $filename (main baseline is not benchmarkable here)"
    echo "  main exit code: $main_exit_code"
    echo "  pr exit code: $pr_exit_code"
    print_probe_log "main output" "$main_log"
    if ! exit_code_is_benchmarkable "$pr_exit_code" "$extra_args"; then
        print_probe_log "pr output" "$pr_log"
    fi
    PRECHECK_SKIP_REASON="main_unbenchmarkable"
    return 2
}

# Determine whether `roc build <file>` produces a byte-identical executable for
# the two binaries. A confirmed build slowdown whose output executable is
# byte-identical is definitionally a false positive: the compiler produced the
# same program, so the timing difference is measurement or binary-layout noise
# (e.g. a larger compiler binary with slightly different code locality) rather
# than real work.
#
# roc embeds nothing version-specific into the linked executable (the compiler
# version only reaches the DWARF producer string in the intermediate bitcode,
# which is stripped before linking), and its linker is deterministic, so two
# compiler builds that do the same work emit byte-identical executables.
#
# Each binary builds in an isolated working directory and cache so their outputs
# cannot clobber each other; --no-cache bypasses cache reads but still emits the
# executable. Returns 0 only when both executables are present and byte-identical;
# any uncertainty (a build failure or a missing executable) returns 1 so we fail
# safe toward the normal slowdown failure.
build_executable_output_identical() {
    local main_roc="$1"
    local pr_roc="$2"
    local fx_file="$3"
    local roc_extra_args="$4"

    # roc resolves a platform's relative path against the source file's location
    # and writes the executable into the current directory, so we pass an
    # absolute source path and build from a scratch directory per binary.
    local abs_fx
    abs_fx="$(cd "$(dirname "$fx_file")" && pwd)/$(basename "$fx_file")" || return 1

    local work
    work=$(mktemp -d) || return 1
    local main_dir="$work/main" pr_dir="$work/pr"
    mkdir -p "$main_dir" "$pr_dir"

    local -a extra_arg_array=()
    if [ -n "$roc_extra_args" ]; then
        read -r -a extra_arg_array <<< "$roc_extra_args"
    fi
    # Guard the array expansion so an empty extra-args list does not trip
    # `set -u` on older bash (e.g. macOS's bash 3.2).
    if ! ( cd "$main_dir" && XDG_CACHE_HOME="$main_dir/.cache" "$main_roc" build "$abs_fx" --no-cache "${extra_arg_array[@]+"${extra_arg_array[@]}"}" >/dev/null 2>&1 ); then
        rm -rf "$work"
        return 1
    fi
    if ! ( cd "$pr_dir" && XDG_CACHE_HOME="$pr_dir/.cache" "$pr_roc" build "$abs_fx" --no-cache "${extra_arg_array[@]+"${extra_arg_array[@]}"}" >/dev/null 2>&1 ); then
        rm -rf "$work"
        return 1
    fi

    local main_exe pr_exe
    main_exe=$(find "$main_dir" -maxdepth 1 -type f -perm -u+x | head -1)
    pr_exe=$(find "$pr_dir" -maxdepth 1 -type f -perm -u+x | head -1)
    if [ -z "$main_exe" ] || [ -z "$pr_exe" ]; then
        rm -rf "$work"
        return 1
    fi

    local identical=1
    cmp -s "$main_exe" "$pr_exe" || identical=0
    rm -rf "$work"
    [ "$identical" -eq 1 ]
}

# Run hyperfine benchmark and return percentage change via global variable
# Returns 0 on success, 1 on failure
# Sets BENCH_PCT_CHANGE on success
run_benchmark() {
    local json_file="$1"
    local extra_args="$2"
    local main_roc="$3"
    local pr_roc="$4"
    local fx_file="$5"
    local roc_extra_args="$6"
    local roc_subcommand="${7:-}"

    local subcommand_arg=""
    if [ -n "$roc_subcommand" ]; then
        subcommand_arg=" $roc_subcommand"
    fi

    local cmd=(
        hyperfine
        --warmup 1
        --min-runs 3
        --shell=none
        --show-output
        --export-json "$json_file"
        $extra_args
        -n "main" "$main_roc$subcommand_arg $fx_file --no-cache $roc_extra_args"
        -n "pr" "$pr_roc$subcommand_arg $fx_file --no-cache $roc_extra_args"
    )

    echo "Running: ${cmd[*]}"

    if ! "${cmd[@]}" 2>&1; then
        diagnose_pr_benchmark_failure "$pr_roc" "$fx_file" "$roc_extra_args" "$roc_subcommand"
        return 1
    fi

    if [ -f "$json_file" ]; then
        local main_median pr_median
        main_median=$(jq -r '.results[] | select(.command | contains("main")) | .median' "$json_file")
        pr_median=$(jq -r '.results[] | select(.command | contains("pr")) | .median' "$json_file")

        if [ -n "$main_median" ] && [ -n "$pr_median" ] && [ "$main_median" != "null" ] && [ "$pr_median" != "null" ]; then
            BENCH_PCT_CHANGE=$(awk "BEGIN {printf \"%.2f\", (($pr_median - $main_median) / $main_median) * 100}")
            BENCH_ABS_DELTA_MS=$(awk "BEGIN {printf \"%.2f\", ($pr_median - $main_median) * 1000}")
            return 0
        fi
    fi
    return 1
}

echo "=== Building FX platform ==="
zig build build-test-hosts -Dplatform=fx -Doptimize=ReleaseFast

echo ""
echo "=== FX File Execution Benchmarks ==="
echo "Main binary: $MAIN_ROC"
echo "PR binary: $PR_ROC"
echo ""

# Collect all fx files, excluding those that use Stdin or don't have main! entry point
FX_FILES=""
for fx_file in test/fx/*.roc; do
    filename=$(basename "$fx_file")
    if is_intentional_error_fixture "$filename"; then
        echo "Skipping $fx_file (intentional error fixture)"
        continue
    fi
    # Skip files that don't have a main! entry point (app [ main! ])
    if ! grep -qE '^app[[:space:]]*\[[[:space:]]*main![[:space:]]*\]' "$fx_file" 2>/dev/null; then
        echo "Skipping $fx_file (no main! entry point)"
        continue
    fi
    # Skip files that import Stdin (they require interactive input)
    if grep -q "import pf.Stdin" "$fx_file" 2>/dev/null; then
        echo "Skipping $fx_file (uses Stdin)"
        continue
    fi
    FX_FILES="$FX_FILES $fx_file"
done

echo ""
echo "=== Running benchmarks ==="

SLOWER_DETECTED=0
SLOWER_FILES=()
CHANGED_FILES=()  # Files that differ between branches
SKIPPED_BASELINE_FILES=()
SKIPPED_EXIT_CODE_DIFF_FILES=()
SKIPPED_OUTPUT_DIFF_FILES=()

# Files for which we additionally benchmark `roc check`.
CHECK_FILES=(
    test/fx/issue8826_full.roc
    test/fx/aoc_day2.roc
    test/fx/host_boxed_fn_boundary.roc
    test/fx/record_builder_cli_parser.roc
    test/fx/zst_nested_singleton_shapes.roc
    test/fx/index_oob_instantiate.roc
    test/fx/wildcard_match_open_union_bug.roc
    test/fx/repeating_pattern_segfault.roc
    test/fx/primitive_encode.roc
    test/fx/dbg_corrupts_recursive_tag_union.roc
)

# Files for which we additionally benchmark `roc build`.
BUILD_FILES=(
    test/fx/issue8826_full.roc
    test/fx/aoc_day2.roc
    test/fx/host_boxed_fn_boundary.roc
    test/fx/record_builder_cli_parser.roc
    test/fx/zst_nested_singleton_shapes.roc
    test/fx/index_oob_instantiate.roc
    test/fx/wildcard_match_open_union_bug.roc
    test/fx/repeating_pattern_segfault.roc
    test/fx/primitive_encode.roc
)

# Pick hyperfine failure-handling args for a check/build benchmark on a given
# file. issue8826_full.roc intentionally exits with a non-137 non-zero code, so
# use --ignore-failure to keep its timings. Everything else only tolerates 137
# (flaky OOM-killer style exits).
check_build_extra_args_for() {
    local filename="$1"
    case "$filename" in
        issue8826_full.roc)
            echo "--ignore-failure"
            ;;
        *)
            echo "--discard-failure=137"
            ;;
    esac
}

# Run a full benchmark (preflight + hyperfine + slowdown confirmation) for one
# file, optionally with a roc subcommand ("" for interpret, "check", "build").
# Updates SLOWER_DETECTED / SLOWER_FILES / CHANGED_FILES / SKIPPED_* globals.
benchmark_file() {
    local fx_file="$1"
    local roc_subcommand="$2"
    local extra_args="$3"
    local roc_extra_args="$4"

    local filename
    filename=$(basename "$fx_file")

    local display_name="$filename"
    local json_prefix="bench"
    if [ -n "$roc_subcommand" ]; then
        display_name="${roc_subcommand} ${filename}"
        json_prefix="bench_${roc_subcommand}"
    fi

    echo "--- Benchmarking: $display_name ---"

    local preflight_status=0
    if preflight_benchmark "$extra_args" "$MAIN_ROC" "$PR_ROC" "$fx_file" "$roc_extra_args" "$roc_subcommand"; then
        :
    else
        preflight_status=$?
        if [ "$preflight_status" -eq 2 ]; then
            case "${PRECHECK_SKIP_REASON:-}" in
                exit_code_diff)
                    SKIPPED_EXIT_CODE_DIFF_FILES+=("$display_name")
                    ;;
                output_diff)
                    SKIPPED_OUTPUT_DIFF_FILES+=("$display_name")
                    ;;
                *)
                    SKIPPED_BASELINE_FILES+=("$display_name")
                    ;;
            esac
            echo ""
            return 0
        fi
        echo "ERROR: Preflight failed for $display_name"
        exit 1
    fi

    # Run hyperfine comparison
    # Using median for robustness against outliers.
    # Note: Our "Change" percentage may differ slightly from hyperfine's "X times faster"
    # summary because hyperfine uses mean while we use median.
    if ! run_benchmark "/tmp/${json_prefix}_${filename}.json" "$extra_args" "$MAIN_ROC" "$PR_ROC" "$fx_file" "$roc_extra_args" "$roc_subcommand"; then
        echo "ERROR: Benchmark failed for $display_name"
        exit 1
    fi

    local pct_change="$BENCH_PCT_CHANGE"
    local abs_delta_ms="$BENCH_ABS_DELTA_MS"
    echo "  Change: ${pct_change}% (${abs_delta_ms} ms)"

    # Check for meaningful slower execution - requires both relative and absolute slowdown.
    local is_slower
    is_slower=$(awk "BEGIN {print ($pct_change > 4 && $abs_delta_ms > 5) ? 1 : 0}")
    if [ "$is_slower" = "1" ]; then
        echo "  Potential slowdown detected (${pct_change}%, ${abs_delta_ms} ms), running confirmation..."

        if ! run_benchmark "/tmp/${json_prefix}_${filename}_confirm.json" "$extra_args" "$MAIN_ROC" "$PR_ROC" "$fx_file" "$roc_extra_args" "$roc_subcommand"; then
            echo "ERROR: Confirmation benchmark failed for $display_name"
            exit 1
        fi

        local confirm_pct_change="$BENCH_PCT_CHANGE"
        local confirm_abs_delta_ms="$BENCH_ABS_DELTA_MS"
        echo "  Confirmation change: ${confirm_pct_change}% (${confirm_abs_delta_ms} ms)"

        # Only report slowdown if both runs exceed both thresholds.
        local confirm_is_slower
        confirm_is_slower=$(awk "BEGIN {print ($confirm_pct_change > 4 && $confirm_abs_delta_ms > 5) ? 1 : 0}")
        if [ "$confirm_is_slower" = "1" ]; then
            # A build slowdown whose output executable is byte-identical is a
            # definitional false positive (the same program cannot have cost
            # more to produce), so pass it without a human override.
            if [ "$roc_subcommand" = "build" ] && \
               build_executable_output_identical "$MAIN_ROC" "$PR_ROC" "$fx_file" "$roc_extra_args"; then
                echo "  IDENTICAL OUTPUT: $display_name produces a byte-identical executable on both binaries; treating the timing difference as a false positive (measurement or binary-layout noise), not a regression."
                echo ""
                return 0
            fi

            echo "  SLOWER EXECUTION CONFIRMED in $display_name (${pct_change}% / ${abs_delta_ms} ms then ${confirm_pct_change}% / ${confirm_abs_delta_ms} ms)"
            SLOWER_DETECTED=1
            SLOWER_FILES+=("$display_name")

            # Check if file content changed between branches
            local check_result=0
            check_file_changed "$fx_file" "$BASE_BRANCH" "$PR_BRANCH" || check_result=$?
            if [ "$check_result" = "1" ]; then
                echo "    NOTE: File content differs between branches - comparison may not be meaningful"
                print_file_diff "$fx_file" "$BASE_BRANCH" "$PR_BRANCH"
                CHANGED_FILES+=("$display_name")
            fi
        else
            echo "  Slowdown NOT confirmed (first: ${pct_change}% / ${abs_delta_ms} ms, second: ${confirm_pct_change}% / ${confirm_abs_delta_ms} ms)"
        fi
    fi
    echo ""
}

for fx_file in $FX_FILES; do
    benchmark_file "$fx_file" "" "--discard-failure=137" ""
done

echo ""
echo "=== Running roc check benchmarks ==="
for fx_file in "${CHECK_FILES[@]}"; do
    extra_args=$(check_build_extra_args_for "$(basename "$fx_file")")
    benchmark_file "$fx_file" "check" "$extra_args" ""
done

echo ""
echo "=== Running roc build benchmarks ==="
for fx_file in "${BUILD_FILES[@]}"; do
    extra_args=$(check_build_extra_args_for "$(basename "$fx_file")")
    benchmark_file "$fx_file" "build" "$extra_args" ""
done

# Summary
echo "=== FX Benchmark Summary ==="
if [ "${#SKIPPED_BASELINE_FILES[@]}" -gt 0 ]; then
    echo "Skipped benchmarks because the main baseline command was not benchmarkable:"
    for f in "${SKIPPED_BASELINE_FILES[@]}"; do
        echo "  - $f"
    done
    echo ""
fi
if [ "${#SKIPPED_EXIT_CODE_DIFF_FILES[@]}" -gt 0 ]; then
    echo "Skipped benchmarks because the main and PR exit codes differed:"
    for f in "${SKIPPED_EXIT_CODE_DIFF_FILES[@]}"; do
        echo "  - $f"
    done
    echo ""
fi
if [ "${#SKIPPED_OUTPUT_DIFF_FILES[@]}" -gt 0 ]; then
    echo "Skipped benchmarks because the main and PR outputs differed:"
    for f in "${SKIPPED_OUTPUT_DIFF_FILES[@]}"; do
        echo "  - $f"
    done
    echo ""
fi
if [ "$SLOWER_DETECTED" = "1" ]; then
    echo "SLOWER EXECUTION detected in the following files:"
    for f in "${SLOWER_FILES[@]}"; do
        # Check if this file is in the CHANGED_FILES list
        found_changed=0
        if [ "${#CHANGED_FILES[@]}" -gt 0 ]; then
            for c in "${CHANGED_FILES[@]}"; do
                if [ "$c" = "$f" ]; then
                    found_changed=1
                    break
                fi
            done
        fi
        if [ "$found_changed" = "1" ]; then
            echo "  - $f (file changed)"
        else
            echo "  - $f"
        fi
    done
    echo ""
    echo "Ask Richard, Anton, or Luke to override this failure if you believe it is justified."
    exit 1
else
    echo "No significant slowdowns detected (threshold: >4% and >5 ms)"
fi
