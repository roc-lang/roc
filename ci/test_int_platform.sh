#!/usr/bin/env bash

set -euo pipefail

# Colors for output (minimal usage)
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Test configuration
ROC_CLI="./zig-out/bin/roc"
INT_APP="test/int/app.roc"
TEST_OUTPUT_DIR="tmp_test_outputs"

# Supported targets for cross-compilation
CROSS_TARGETS=(
    "x64musl"
    "arm64musl"
    "x64glibc"
    "arm64glibc"
)

# Test results tracking
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0
FAILED_TESTS=()

print_header() {
    echo "================================"
    echo "  Roc Int Platform Test Suite  "
    echo "================================"
    echo
}

print_section() {
    echo ">>> $1"
}

print_success() {
    echo -e "${GREEN}PASS${NC} $1"
}

print_error() {
    echo -e "${RED}FAIL${NC} $1"
}

print_info() {
    echo "INFO $1"
}

# Portable timeout wrapper:
# - Uses GNU coreutils 'timeout' if available
# - Falls back to 'gtimeout' (Homebrew coreutils on macOS)
# - Otherwise uses a shell-based timer that sends SIGTERM after N seconds
# Usage: run_with_timeout <seconds> <command> [args...]
run_with_timeout() {
    local seconds="$1"; shift
    if command -v timeout >/dev/null 2>&1; then
        timeout "${seconds}s" "$@"
        return $?
    elif command -v gtimeout >/dev/null 2>&1; then
        gtimeout "${seconds}s" "$@"
        return $?
    else
        ( "$@" ) &
        local cmd_pid=$!
        ( sleep "$seconds"; kill -0 "$cmd_pid" 2>/dev/null && kill -TERM "$cmd_pid" 2>/dev/null ) &
        local timer_pid=$!
        wait "$cmd_pid"
        local exit_code=$?
        kill -TERM "$timer_pid" 2>/dev/null || true
        return "$exit_code"
    fi
}

cleanup() {
    if [ -d "$TEST_OUTPUT_DIR" ]; then
        rm -rf "$TEST_OUTPUT_DIR"
    fi
}

setup() {
    # Create output directory
    mkdir -p "$TEST_OUTPUT_DIR"

    # Check if roc CLI exists
    if [ ! -f "$ROC_CLI" ]; then
        print_error "Roc CLI not found at $ROC_CLI"
        print_info "Please run 'zig build' first to build the Roc compiler"
        exit 1
    fi

    # Check if int app exists
    if [ ! -f "$INT_APP" ]; then
        print_error "Int test app not found at $INT_APP"
        exit 1
    fi
}

run_test() {
    local test_name="$1"
    local test_cmd="$2"
    local expected_output="$3"

    TESTS_RUN=$((TESTS_RUN + 1))

    print_info "Running: $test_name"
    echo "  Command: $test_cmd"

    if eval "$test_cmd" > "$TEST_OUTPUT_DIR/test_$TESTS_RUN.out" 2>&1; then
        if [ -n "$expected_output" ]; then
            # Check if expected output is present
            if grep -q "$expected_output" "$TEST_OUTPUT_DIR/test_$TESTS_RUN.out"; then
                print_success "$test_name"
                TESTS_PASSED=$((TESTS_PASSED + 1))
                return 0
            else
                print_error "$test_name - Expected output not found"
                echo "  Expected: $expected_output"
                echo "  Got (first 5 lines):"
                cat "$TEST_OUTPUT_DIR/test_$TESTS_RUN.out" | head -5
                echo "  NOTE: For complete output, run: cat $TEST_OUTPUT_DIR/test_$TESTS_RUN.out"
                TESTS_FAILED=$((TESTS_FAILED + 1))
                FAILED_TESTS+=("$test_name")
                return 1
            fi
        else
            print_success "$test_name"
            TESTS_PASSED=$((TESTS_PASSED + 1))
            return 0
        fi
    else
        print_error "$test_name - Command failed"
        
        # Show more complete output for arm64glibc debugging
        if [[ "$test_name" == *"arm64glibc"* ]]; then
            echo "  Complete error output for arm64glibc debugging:"
            cat "$TEST_OUTPUT_DIR/test_$TESTS_RUN.out"
        else
            echo "  Error output (first 10 lines):"
            cat "$TEST_OUTPUT_DIR/test_$TESTS_RUN.out" | head -10
            echo "  NOTE: This is a summary of the error output."
            echo "  For complete output, run: cat $TEST_OUTPUT_DIR/test_$TESTS_RUN.out"
        fi
        
        TESTS_FAILED=$((TESTS_FAILED + 1))
        FAILED_TESTS+=("$test_name")
        return 1
    fi
}

test_native_execution() {
    print_section "Testing Native Build and Execution"

    local native_output="$TEST_OUTPUT_DIR/int_app_native"

    # Test native build (should work on current platform)
    run_test "Native build" \
        "$ROC_CLI build --output=$native_output $INT_APP" \
        ""

    # Verify the executable was created
    if [ ! -f "$native_output" ]; then
        print_error "Native executable not created"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        FAILED_TESTS+=("native executable creation")
        return 1
    fi

    print_success "Native executable created"

    # Show executable info
    if command -v file >/dev/null 2>&1; then
        echo "  File type: $(file "$native_output")"
    fi

    # Make sure it's executable
    chmod +x "$native_output"

    # Test execution - the int platform should run the host which calls the app functions
    print_info "Testing native execution..."

    local exec_output="$TEST_OUTPUT_DIR/native_exec.out"
    if run_with_timeout 10 "$native_output" > "$exec_output" 2>&1; then
        local exit_code=$?
        if [ $exit_code -eq 0 ]; then
            print_success "Native executable runs and exits successfully"

            # Show what the executable outputs (useful for debugging)
            if [ -s "$exec_output" ]; then
                echo "  Output:"
                head -5 "$exec_output" | sed 's/^/    /'
            fi

            TESTS_PASSED=$((TESTS_PASSED + 1))
        else
            print_error "Native executable exited with code $exit_code"
            echo "  Output (first 10 lines):"
            head -10 "$exec_output" | sed 's/^/    /'
            echo "  NOTE: For complete output, run: cat $exec_output"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            FAILED_TESTS+=("native execution exit code")
        fi
    else
        print_error "Native executable timed out or crashed"
        echo "  Output (first 10 lines):"
        head -10 "$exec_output" | sed 's/^/    /'
        echo "  NOTE: For complete output, run: cat $exec_output"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        FAILED_TESTS+=("native execution timeout")
    fi

    TESTS_RUN=$((TESTS_RUN + 1))
}

test_cross_compilation() {
    print_section "Testing Cross-Compilation"

    for target in "${CROSS_TARGETS[@]}"; do
        local output_name="$TEST_OUTPUT_DIR/int_app_$target"

        # Test cross-compilation build
        run_test "Cross-compile to $target" \
            "$ROC_CLI build --target=$target --output=$output_name $INT_APP" \
            ""

        # Check if the executable was created
        if [ -f "$output_name" ]; then
            print_success "Executable created for $target"

            # Show some info about the generated executable
            if command -v file >/dev/null 2>&1; then
                echo "  File info: $(file "$output_name")"
            fi

            if command -v ldd >/dev/null 2>&1 && [[ "$target" == *"$(uname -m)"* ]]; then
                echo "  Dependencies:"
                ldd "$output_name" 2>/dev/null | head -5 || echo "    (static or incompatible)"
            fi
        else
            print_error "Executable not created for $target"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            FAILED_TESTS+=("$target executable creation")
        fi
    done
}

test_platform_build() {
    print_section "Testing Platform Build System"

    # Test that platform libraries are built
    run_test "Build platform libraries" \
        "zig build" \
        ""

    # Check that target directories exist with expected files
    for target in "${CROSS_TARGETS[@]}"; do
        local target_dir="test/int/platform/targets/$target"

        if [ -d "$target_dir" ]; then
            print_success "Target directory exists: $target"

            # Check for expected files
            local expected_files=("libhost.a")
            if [[ "$target" == *"glibc"* ]]; then
                expected_files+=("libc.so.6" "libc.so" "libc_stub.s")
            fi

            for file in "${expected_files[@]}"; do
                if [ -f "$target_dir/$file" ]; then
                    echo "    $file: present"
                else
                    print_error "  $file missing in $target"
                    TESTS_FAILED=$((TESTS_FAILED + 1))
                    FAILED_TESTS+=("$target/$file")
                fi
            done
        else
            print_error "Target directory missing: $target"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            FAILED_TESTS+=("$target directory")
        fi
    done
}

test_glibc_stubs() {
    print_section "Testing Glibc Stub Generation"

    for target in "x64glibc" "arm64glibc"; do
        local stub_file="test/int/platform/targets/$target/libc_stub.s"

        if [ -f "$stub_file" ]; then
            print_success "Glibc stub exists: $target"

            # Check that essential symbols are present
            local essential_symbols=("__libc_start_main" "abort" "getauxval" "_IO_stdin_used")
            local missing_symbols=0

            for symbol in "${essential_symbols[@]}"; do
                if grep -q "$symbol" "$stub_file"; then
                    echo "    $symbol: present"
                else
                    print_error "  Symbol $symbol missing from $target"
                    TESTS_FAILED=$((TESTS_FAILED + 1))
                    FAILED_TESTS+=("$target $symbol")
                    missing_symbols=$((missing_symbols + 1))
                fi
            done

            if [ $missing_symbols -eq 0 ]; then
                echo "    All essential symbols present"
            fi

            # Check architecture-specific instructions
            if [[ "$target" == "x64glibc" ]]; then
                if grep -q "xor %rax" "$stub_file"; then
                    echo "    x86_64 assembly: correct"
                else
                    print_error "  x86_64 assembly instructions missing from $target"
                fi
            elif [[ "$target" == "arm64glibc" ]]; then
                if grep -q "mov x0" "$stub_file"; then
                    echo "    ARM64 assembly: correct"
                else
                    print_error "  ARM64 assembly instructions missing from $target"
                fi
            fi
        else
            print_error "Glibc stub missing: $target"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            FAILED_TESTS+=("$target stub")
        fi
    done
}

print_summary() {
    echo
    print_section "Test Summary"
    echo "Total tests: $TESTS_RUN"
    echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
    echo -e "${RED}Failed: $TESTS_FAILED${NC}"

    if [ $TESTS_FAILED -gt 0 ]; then
        echo
        echo "Failed tests:"
        for failed_test in "${FAILED_TESTS[@]}"; do
            echo "  - $failed_test"
        done
        echo
        print_error "Some tests failed"
        return 1
    else
        echo
        print_success "All tests passed"
        return 0
    fi
}

main() {
    print_header

    # Setup
    setup
    trap cleanup EXIT

    # Run test suites
    test_platform_build
    test_glibc_stubs
    test_cross_compilation
    test_native_execution

    # Print summary and exit with appropriate code
    if print_summary; then
        exit 0
    else
        exit 1
    fi
}

# Handle command line arguments
case "${1:-}" in
    --help|-h)
        echo "Usage: $0 [--help]"
        echo
        echo "Test script for Roc's int platform cross-compilation."
        echo "This script tests:"
        echo "  - Platform build system"
        echo "  - Glibc stub generation"
        echo "  - Native execution"
        echo "  - Cross-compilation to all supported targets"
        echo
        echo "Make sure to run 'zig build' first to build the Roc compiler."
        exit 0
        ;;
    *)
        main "$@"
        ;;
esac
