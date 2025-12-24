# Minimal test file - original benchmark temporarily disabled
# See bench_repeated_check_ORIGINAL.roc for the full version
# TODO: Re-enable once `!=` (which desugars to is_eq().not()) is fully working

app [main] { pf: platform "../../test/str/platform/main.roc" }

main = |_| 1 + 1
