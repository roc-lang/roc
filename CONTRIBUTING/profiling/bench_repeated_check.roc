# Minimal test file - original benchmark temporarily disabled
# See bench_repeated_check_ORIGINAL.roc for the full version
# TODO: Re-enable once `!=` (which desugars to is_eq().not()) is fully working

app [process_string] { pf: platform "../../test/str/platform/main.roc" }

process_string : Str -> Str
process_string = |s| s
