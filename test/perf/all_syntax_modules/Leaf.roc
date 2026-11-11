# Imported only by Strings, so ci/perf/bench.roc edits it for the
# "warm, edit a leaf" scenario.
Leaf := [].{
	# ci/perf/bench.roc rewrites this number before each timed run.
	perf_edit : U64
	perf_edit = 0

	greet : Str -> Str
	greet = |name| "Hello, ${name}!"
}
