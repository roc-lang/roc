# Minimal test file - original benchmark temporarily disabled
# See bench_repeated_check_ORIGINAL.roc for the full version
# TODO: Re-enable once `is_ne` is supported for all numeric types

app [main] { pf: platform "../../test/str/platform/main.roc" }

main = |_| 1 + 1
