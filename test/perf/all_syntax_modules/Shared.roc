# Imported by every other module of this fixture, so ci/perf/bench.roc edits
# it for the "warm, edit widely used" scenario.
Shared := [].{
	# ci/perf/bench.roc rewrites this number before each timed run.
	perf_edit : U64
	perf_edit = 0

	label : Str, Str -> Str
	label = |name, value| "${name}: ${value}"

	sum : List(I64) -> I64
	sum = |nums| nums.fold(0, |acc, n| acc + n)

	is_small : U64 -> Bool
	is_small = |n| n < 10
}
