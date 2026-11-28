## Perf regression benchmarks for roc pull requests, driven by
## .github/workflows/ci_perf.yml. Always run with the Roc version pinned in the
## header, never with a roc built from the sources being measured.
##
## `screen` compares a base and a head roc on one machine. Each benchmark runs
## as base/head pairs, in random order within each pair and across benchmarks,
## so drift on a shared machine hits both sides alike. The median of the
## per-pair head/base wall-time ratios gets a distribution-free confidence
## interval from sign-test order statistics, at a Bonferroni-corrected level.
## Benchmarks that are not clearly fine get more pairs, up to `max_pair_count`.
##
## `confirm` re-measures what screening flagged on a fresh machine with a fixed
## number of pairs, `diagnose` explains confirmed regressions, `triage` picks
## the machines to confirm on, and `report` writes the job summary. It fails
## only when screening and confirmation both found a regression.
app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
	roc: "0.1.0-RC2",
}

import pf.Cmd
import pf.Env
import pf.Monotonic
import pf.OsStr
import pf.Path
import pf.Random
import pf.Stderr
import pf.Stdout
import pf.System

## A benchmark regresses when the whole confidence interval of its median
## head/base ratio is above 1 + threshold.
threshold : F64
threshold = 0.05

## Chance of at least one false regression per machine, split evenly across
## the benchmarks measured there.
family_alpha : F64
family_alpha = 0.05

## Rounds run before measuring and thrown away.
warmup_round_count : U64
warmup_round_count = 2

## Pairs every benchmark gets in screening.
screen_pair_count : U64
screen_pair_count = 30

## Pairs added at a time to a benchmark screening has not called fine yet.
batch_pair_count : U64
batch_pair_count = 20

## Screening adds `batch_pair_count` pairs at a time, so this must be
## `screen_pair_count` plus a whole number of batches for escalation to stop
## exactly here.
max_pair_count : U64
max_pair_count = 110

expect (max_pair_count - screen_pair_count) % batch_pair_count == 0

# Even with every benchmark measured on one machine, screening's pairs give
# each a confidence interval, so `measurement` never lacks one.
expect median_rank(screen_pair_count, family_alpha / benchmarks.len().to_f64()) > 0

## Every editable fixture file holds this line; benchmarks rewrite the number.
edit_marker : Str
edit_marker = "perf_edit = 0"

Scenario : [ColdNoCache, ColdEmptyCache, WarmNoChange, WarmEdit(Str)]

## `args` are roc's arguments, in which `{file}` becomes the scenario's cache
## flag, any extra flags, then the app's path, and `{out}` an `--output=` path
## of the run's own.
Benchmark : {
	name : Str,
	project : [InPlace(Str), Copied(Str)],
	scenario : Scenario,
	args : List(Str),
}

benchmarks : List(Benchmark)
benchmarks = {
	single_file = [
		{ cmd: "check", args: ["check", "{file}"] },
		{ cmd: "run-interpreter", args: ["--opt=interpreter", "{file}"] },
		{ cmd: "run-dev", args: ["--opt=dev", "{file}"] },
		{ cmd: "build-dev", args: ["build", "--opt=dev", "{out}", "{file}"] },
	].map(
		|c| {
			name: "all_syntax_test/cold-no-cache/${c.cmd}",
			project: InPlace("test/echo/all_syntax_test.roc"),
			scenario: ColdNoCache,
			args: c.args,
		},
	)

	scenarios = [
		{ label: "cold-no-cache", scenario: ColdNoCache },
		{ label: "cold-empty-cache", scenario: ColdEmptyCache },
		{ label: "warm-no-change", scenario: WarmNoChange },
		{ label: "warm-edit-leaf", scenario: WarmEdit("Leaf.roc") },
		{ label: "warm-edit-entry", scenario: WarmEdit("main.roc") },
		{ label: "warm-edit-widely-used", scenario: WarmEdit("Shared.roc") },
	]
	commands = [
		{ cmd: "check", args: ["check", "{file}"] },
		{ cmd: "run-dev", args: ["--opt=dev", "{file}"] },
	]
	modules = scenarios.join_map(
		|s|
			commands.map(
				|c| {
					name: "all_syntax_modules/${s.label}/${c.cmd}",
					project: Copied("test/perf/all_syntax_modules"),
					scenario: s.scenario,
					args: c.args,
				},
			),
	)

	List.concat(single_file, modules)
}

main! : List(OsStr) => Try({}, _)
main! = |raw_args| {
	args = raw_args.map(OsStr.display)

	match args {
		["screen", .. as rest] => measure!(Phase.Screen, rest)
		["confirm", .. as rest] => measure!(Phase.Confirm, rest)
		["diagnose", .. as rest] => diagnose!(rest)
		["triage", .. as rest] => triage!(rest)
		["report", .. as rest] => report!(rest)
		_ => {
			Stderr.line!(
				\\usage: roc ci/perf/bench.roc screen --base ROC --head ROC --out FILE [--only NAMES]
				\\       roc ci/perf/bench.roc confirm --base ROC --head ROC --only NAMES --out FILE
				\\       roc ci/perf/bench.roc diagnose --base ROC --head ROC --only NAMES --out FILE
				\\       roc ci/perf/bench.roc triage --dir DIR
				\\       roc ci/perf/bench.roc report --dir DIR --summary FILE
			)?

			Err(Exit(2))
		}
	}
}

# ---- arguments ----

option_value : List(Str), Str -> Try(Str, [MissingOption(Str)])
option_value = |args, name| match args {
	[flag, value, ..] if flag == name => Ok(value)
	[_, .. as rest] => option_value(rest, name)
	[] => Err(MissingOption(name))
}

expect option_value(["--base", "a", "--head", "b"], "--head") == Ok("b")
expect option_value(["--base"], "--base") == Err(MissingOption("--base"))

split_names : Str -> List(Str)
split_names = |names| names.split_on(",").map(Str.trim).drop_if(Str.is_empty)

## The benchmarks a comma-separated `--only` list names, in its order.
benchmarks_named : Str -> Try(List(Benchmark), [UnknownBenchmark(Str)])
benchmarks_named = |names|
	split_names(names).map_try(
		|name|
			benchmarks
				.find_first(|benchmark| benchmark.name == name)
				.map_err(|_| UnknownBenchmark(name)),
	)

expect benchmarks_named(" all_syntax_test/cold-no-cache/check, ").map_ok(List.len) == Ok(1)
expect benchmarks_named("nope") == Err(UnknownBenchmark("nope"))

## The base and head rocs that `--base` and `--head` name.
resolve_rocs! : List(Str) => Try(Sides(Path), _)
resolve_rocs! = |args| {
	base = Path.absolute!(Path.utf8(option_value(args, "--base")?))?
	head = Path.absolute!(Path.utf8(option_value(args, "--head")?))?
	Ok({ base, head })
}

## The machines the perf check runs on.
Os := [Linux, MacOS, Windows].{
	to_str : Os -> Str
	to_str = |os| match os {
		Linux => "linux"
		MacOS => "macos"
		Windows => "windows"
	}

	from_str : Str -> Try(Os, [UnknownOs(Str)])
	from_str = |name| match name {
		"linux" => Ok(Linux)
		"macos" => Ok(MacOS)
		"windows" => Ok(Windows)
		_ => Err(UnknownOs(name))
	}

	is_eq : _
}

## Every machine, in the order the report shows them.
all_oses : List(Os)
all_oses = [Os.Linux, Os.MacOS, Os.Windows]

expect all_oses.all(|os| Os.from_str(os.to_str()) == Ok(os))

detect_os! : () => Os
detect_os! = || match System.target!().os() {
	Linux(_) => Os.Linux
	MacOS(_) => Os.MacOS
	Windows(_) => Os.Windows
}

read_cpu_model! : Os => Try(Str, [CpuModelUnknown])
read_cpu_model! = |os| match os {
	Linux => {
		cpuinfo = Path.read_utf8!("/proc/cpuinfo") ? |_| CpuModelUnknown
		cpuinfo_model(cpuinfo)
	}
	MacOS =>
		Cmd.new("sysctl")
			.args(["-n", "machdep.cpu.brand_string"])
			.capture!()
			.map_err(|_| CpuModelUnknown)
	Windows => Env.var_str!("PROCESSOR_IDENTIFIER").map_err(|_| CpuModelUnknown)
}

## The model name in Linux's `/proc/cpuinfo`.
cpuinfo_model : Str -> Try(Str, [CpuModelUnknown])
cpuinfo_model = |cpuinfo| {
	line = cpuinfo.split_on("\n").find_first(|l| l.starts_with("model name")) ? |_| CpuModelUnknown
	parts = line.split_first(":") ? |_| CpuModelUnknown
	Ok(parts.after.trim())
}

expect cpuinfo_model("processor\t: 0\nmodel name\t: AMD EPYC 7763 64-Core Processor\n") == Ok("AMD EPYC 7763 64-Core Processor")
expect cpuinfo_model("processor\t: 0\n") == Err(CpuModelUnknown)

# ---- statistics ----

sort_f64 : List(F64) -> List(F64)
sort_f64 = |numbers|
	numbers.sort_with(
		|a, b|
			if a < b {
				Before
			} else if a > b {
				After
			} else {
				Same
			},
	)

median : List(F64) -> Try(F64, [ListWasEmpty])
median = |numbers| {
	sorted = sort_f64(numbers)
	half = sorted.len() // 2

	# An odd count has its middle first in the upper half; an even count
	# averages the last of the lower half and the first of the upper.
	match (sorted.take_first(half).last(), sorted.drop_first(half).first()) {
		(Ok(lower), Ok(upper)) if sorted.len() % 2 == 0 => Ok((lower + upper) / 2.0)
		(_, Ok(middle)) => Ok(middle)
		(_, Err(ListWasEmpty)) => Err(ListWasEmpty)
	}
}

expect median([3.0, 1.0, 2.0]) == Ok(2.0)
expect median([4.0, 1.0, 3.0, 2.0]) == Ok(2.5)
expect median([7.0]) == Ok(7.0)
expect median([]) == Err(ListWasEmpty)

## The 1-based rank `r` for which the order statistics x_(r) and x_(n+1-r)
## bound the median with probability at least 1 - alpha, from the sign test:
## the largest r with P(Binomial(n, 1/2) <= r - 1) <= alpha / 2.
## Zero when n is too small for that level.
median_rank : U64, F64 -> U64
median_rank = |n, alpha| {
	# P(Binomial(n, 1/2) = k), starting at k = 0.
	var $pmf = 1.0

	for _ in 0..<n {
		$pmf = $pmf * 0.5
	}

	var $cdf = 0.0
	var $rank = 0
	var $k = 0

	while $k < n {
		$cdf = $cdf + $pmf

		if $cdf > alpha / 2.0 {
			break
		} else {
			$rank = $k + 1
		}

		$pmf = $pmf * (n - $k).to_f64() / ($k + 1).to_f64()
		$k = $k + 1
	}

	$rank
}

# P(B <= 1) = 16/32768 and P(B <= 2) = 121/32768 for n = 15.
expect median_rank(15, 0.05 / 16.0) == 2
expect median_rank(15, 0.05) == 4
expect median_rank(5, 0.05) == 0
expect median_rank(60, 0.05 / 16.0) > 15

Interval : { low : F64, high : F64 }

ratio_interval : List(F64), F64 -> Try(Interval, [TooFewPairs])
ratio_interval = |ratios, alpha| {
	rank = median_rank(ratios.len(), alpha)

	if rank == 0 {
		Err(TooFewPairs)
	} else {
		sorted = sort_f64(ratios)

		match (sorted.get(rank - 1), sorted.get(sorted.len() - rank)) {
			(Ok(low), Ok(high)) => Ok({ low, high })
			_ => crash "ratio_interval: a median rank never passes the middle of the ratios"
		}
	}
}

## What a benchmark's confidence interval says about the head roc.
Verdict := [Regressed, Improved, Unchanged, Inconclusive].{
	to_str : Verdict -> Str
	to_str = |verdict| match verdict {
		Regressed => "regressed"
		Improved => "improved"
		Unchanged => "ok"
		Inconclusive => "inconclusive"
	}

	from_str : Str -> Try(Verdict, [UnknownVerdict(Str)])
	from_str = |word| match word {
		"regressed" => Ok(Regressed)
		"improved" => Ok(Improved)
		"ok" => Ok(Unchanged)
		"inconclusive" => Ok(Inconclusive)
		_ => Err(UnknownVerdict(word))
	}

	is_eq : _
}

expect
	[Verdict.Regressed, Verdict.Improved, Verdict.Unchanged, Verdict.Inconclusive].all(
		|verdict| Verdict.from_str(verdict.to_str()) == Ok(verdict),
	)

classify : Interval -> Verdict
classify = |interval|
	if interval.low > 1.0 + threshold {
		Regressed
	} else if interval.high < 1.0 - threshold {
		Improved
	} else if interval.high <= 1.0 + threshold {
		Unchanged
	} else {
		Inconclusive
	}

expect classify({ low: 1.06, high: 1.2 }) == Verdict.Regressed
expect classify({ low: 1.05, high: 1.2 }) == Verdict.Inconclusive
expect classify({ low: 0.97, high: 1.05 }) == Verdict.Unchanged
expect classify({ low: 0.8, high: 0.94 }) == Verdict.Improved

## Screening keeps measuring a benchmark until it is clearly fine or has
## `max_pair_count` pairs, so `regressed` is only ever decided at
## `max_pair_count`.
should_add_pairs : Verdict, U64 -> Bool
should_add_pairs = |verdict, pair_count|
	if pair_count >= max_pair_count {
		False
	} else {
		match verdict {
			Regressed | Inconclusive => True
			Unchanged | Improved => False
		}
	}

expect should_add_pairs(Verdict.Regressed, 30)
expect !should_add_pairs(Verdict.Regressed, 110)
expect should_add_pairs(Verdict.Inconclusive, 50)
expect !should_add_pairs(Verdict.Unchanged, 30)

## One base run and one head run of a benchmark, timed back to back.
Pair : { base_ms : F64, head_ms : F64 }

## What a benchmark's pairs say. `ratio` is the median per-pair head/base
## ratio, and `base_ms` and `head_ms` are each side's median time.
Measurement : {
	verdict : Verdict,
	pair_count : U64,
	ratio : F64,
	interval : Interval,
	base_ms : F64,
	head_ms : F64,
}

## Only for a measured benchmark: it has `screen_pair_count` pairs or more.
measurement : List(Pair), F64 -> Measurement
measurement = |pairs, alpha| {
	ratios = pairs.map(|pair| pair.head_ms / pair.base_ms)
	interval = ratio_interval(ratios, alpha) ?? crash "measurement: a measured benchmark has enough pairs for an interval"
	middle = |numbers| median(numbers) ?? crash "measurement: a measured benchmark has pairs"

	{
		verdict: classify(interval),
		pair_count: pairs.len(),
		ratio: middle(ratios),
		interval,
		base_ms: middle(pairs.map(|pair| pair.base_ms)),
		head_ms: middle(pairs.map(|pair| pair.head_ms)),
	}
}

expect {
	pairs = [1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6].map(|head_ms| { base_ms: 1.0, head_ms })

	measurement(pairs, 0.05)
		== {
			verdict: Verdict.Inconclusive,
			pair_count: 7,
			ratio: 1.3,
			interval: { low: 1.0, high: 1.6 },
			base_ms: 1.0,
			head_ms: 1.3,
		}
}

# ---- formatting ----

## `x` to one decimal place, rounded half away from zero.
one_decimal_text : F64 -> Str
one_decimal_text = |x|
	match (x * 10.0).round_to_i64_try() {
		Ok(tenths) => {
			sign =
				if tenths < 0 {
					"-"
				} else {
					""
				}

			"${sign}${(tenths.abs() // 10).to_str()}.${(tenths.abs() % 10).to_str()}"
		}
		# NaN and infinities.
		Err(OutOfRange) => x.to_str()
	}

expect one_decimal_text(12.34) == "12.3"
expect one_decimal_text(-0.05) == "-0.1"
expect one_decimal_text(0.0) == "0.0"

## A head/base ratio as a signed percent change, like `+5.2%`.
change_text : F64 -> Str
change_text = |ratio| {
	percent = one_decimal_text((ratio - 1.0) * 100.0)

	if percent.starts_with("-") or percent == "0.0" {
		"${percent}%"
	} else {
		"+${percent}%"
	}
}

expect change_text(1.052) == "+5.2%"
expect change_text(0.97) == "-3.0%"
expect change_text(1.0) == "0.0%"

## A measurement's change and its confidence interval, like
## `+5.2% [+3.0%, +7.5%]`.
change_and_interval_text : Measurement -> Str
change_and_interval_text = |measured| {
	low = change_text(measured.interval.low)
	high = change_text(measured.interval.high)
	"${change_text(measured.ratio)} [${low}, ${high}]"
}

## `text` on one line, so it fits in one results field or annotation.
single_line : Str -> Str
single_line = |text|
	text
		.replace_each("\t", " ")
		.replace_each("\n", " ")
		.replace_each("\r", " ")
		.trim()

last_lines : Str, U64 -> Str
last_lines = |text, count| Str.join_with(text.trim().split_on("\n").take_last(count), "\n")

expect last_lines("a\nb\nc\n", 2) == "b\nc"
expect last_lines("a", 5) == "a"

# ---- running roc ----

## The same thing for the base roc and the head roc.
Sides(a) : { base : a, head : a }

## Where one side of one benchmark runs: its compiler, its own copy of the
## app and cache, and the file its edits rewrite.
Slot : {
	roc : Path,
	app_path : Path,
	cache_dir : Path,
	output_prefix : Path,
	edit : [NoEdit, Edit(Path, Str)],
}

## Each side of each benchmark gets its own project copy and cache, so the two
## compilers never share cache entries, even when they are the same binary.
create_slots! : Benchmark, { rocs : Sides(Path), dir : Path } => Try(Sides(Slot), _)
create_slots! = |benchmark, { rocs, dir }| {
	base = create_slot!(benchmark, { roc: rocs.base, dir: dir.join("base") })?
	head = create_slot!(benchmark, { roc: rocs.head, dir: dir.join("head") })?
	Ok({ base, head })
}

create_slot! : Benchmark, { roc : Path, dir : Path } => Try(Slot, _)
create_slot! = |benchmark, { roc, dir }| {
	cache_dir = dir.join("cache")
	Path.create_all!(cache_dir)?
	output_prefix = dir.join("out")

	match benchmark.project {
		InPlace(file) =>
			Ok({
				roc,
				app_path: Path.utf8(file),
				cache_dir,
				output_prefix,
				edit: NoEdit,
			})
		Copied(source) => {
			project = dir.join("project")
			Path.copy_dir!(Path.utf8(source), project)?

			edit =
				match benchmark.scenario {
					WarmEdit(name) => {
						edited = project.join(name)
						original = Path.read_utf8!(edited)?

						if original.contains(edit_marker) {
							Edit(edited, original)
						} else {
							return Err(MissingEditMarker("${source}/${name} must contain `${edit_marker}`"))
						}
					}
					ColdNoCache | ColdEmptyCache | WarmNoChange => NoEdit
				}

			Ok({
				roc,
				app_path: project.join("main.roc"),
				cache_dir,
				output_prefix,
				edit,
			})
		}
	}
}

## Every run builds to its own path: Windows can keep a just-written
## executable locked, so overwriting the previous one fails.
output_path : Slot, U64 -> Path
output_path = |slot, round| Path.utf8("${slot.output_prefix.display()}-${round.to_str()}")

## The arguments after the roc executable for one run of `benchmark`.
command_args : Benchmark,
{
	slot : Slot,
	round : U64,
	flags : List(Str),
} -> List(OsStr)
command_args = |benchmark, { slot, round, flags }|
	benchmark.args.join_map(
		|arg|
			if arg == "{file}" {
				cache_flags =
					match benchmark.scenario {
						ColdNoCache => ["--no-cache"]
						ColdEmptyCache | WarmNoChange | WarmEdit(_) => []
					}

				List.concat(cache_flags, flags)
					.map(OsStr.from_str)
					.append(slot.app_path.to_os_str())
			} else if arg == "{out}" {
				[OsStr.from_str("--output=${output_path(slot, round).display()}")]
			} else {
				[OsStr.from_str(arg)]
			},
	)

expect {
	slot = {
		roc: Path.utf8("roc"),
		app_path: Path.utf8("app/main.roc"),
		cache_dir: Path.utf8("cache"),
		output_prefix: Path.utf8("out"),
		edit: NoEdit,
	}

	benchmark = {
		name: "build",
		project: InPlace("app/main.roc"),
		scenario: ColdNoCache,
		args: ["build", "{out}", "{file}"],
	}

	command_args(benchmark, { slot, round: 7, flags: ["--timings"] }).map(OsStr.display)
		== ["build", "--output=out-7", "--no-cache", "--timings", "app/main.roc"]
}

## The command for one run of `benchmark`. `round` keeps its output path unique,
## `flags` go before the app's path, and `wrapper` (such as valgrind) runs
## roc when it is not empty.
roc_command : Benchmark,
{
	slot : Slot,
	round : U64,
	flags : List(Str),
	wrapper : List(Str),
} -> Cmd.Command
roc_command = |benchmark, { slot, round, flags, wrapper }| {
	roc_args = command_args(benchmark, { slot, round, flags })

	command =
		match wrapper {
			[program, .. as wrapper_args] => {
				wrapped = wrapper_args.map(OsStr.from_str).append(slot.roc.to_os_str())
				Cmd.new(OsStr.from_str(program)).args(List.concat(wrapped, roc_args))
			}
			[] => Cmd.new(slot.roc.to_os_str()).args(roc_args)
		}

	command.env("ROC_CACHE_DIR", slot.cache_dir.to_os_str())
}

failure_details : Cmd.Output -> Str
failure_details = |output| {
	stderr_tail = last_lines(output.stderr_utf8_lossy(), 15)
	"${Str.inspect(output.status)}\n${stderr_tail}"
}

## Run untimed and report whether the benchmark works with this compiler.
try_run! : Benchmark, Slot => Try([Passed, Failed(Str)], _)
try_run! = |benchmark, slot| {
	output = roc_command(benchmark, { slot, round: 0, flags: [], wrapper: [] }).run!()?

	match output.status {
		Exited(0) => Ok(Passed)
		_ => Ok(Failed(failure_details(output)))
	}
}

time_run! : Benchmark, { slot : Slot, round : U64 } => Try(F64, _)
time_run! = |benchmark, { slot, round }| {
	command = roc_command(benchmark, { slot, round, flags: [], wrapper: [] }).stdout(Null)
	start = Monotonic.now!()
	output = command.run!()?
	elapsed = start.elapsed!()

	if benchmark.args.contains("{out}") {
		_ = Path.delete!(output_path(slot, round))
		{}
	} else {
		{}
	}

	match output.status {
		Exited(0) => Ok(elapsed.to_nanos().to_f64() / 1_000_000.0)
		_ => Err(RunFailed("${benchmark.name} failed with ${slot.roc.display()} mid-run: ${failure_details(output)}"))
	}
}

## Set up what both sides of a pair should start from.
prepare! : Benchmark, { slot : Slot, round : U64 } => Try({}, _)
prepare! = |benchmark, { slot, round }|
	match benchmark.scenario {
		ColdEmptyCache => {
			Path.delete_all!(slot.cache_dir)?
			Path.create_all!(slot.cache_dir)
		}
		WarmEdit(_) =>
			match slot.edit {
				# A number never used before, so the edit can not hit the cache.
				Edit(path, original) => {
					edited = original.replace_first(edit_marker, "perf_edit = ${round.to_str()}")
					Path.write_utf8!(path, edited)
				}
				NoEdit => Ok({})
			}
		ColdNoCache | WarmNoChange => Ok({})
	}

## Which side of a pair runs first, at random.
pick_order! : () => Try([BaseFirst, HeadFirst], _)
pick_order! = || {
	seed = Random.seed_u32!()?

	if seed % 2 == 0 {
		Ok(BaseFirst)
	} else {
		Ok(HeadFirst)
	}
}

shuffle! : List(a) => Try(List(a), _)
shuffle! = |items| {
	var $shuffled = items
	var $i = items.len()

	while $i > 1 {
		seed = Random.seed_u32!()?
		j = seed.to_u64() % $i
		$i = $i - 1
		$shuffled = $shuffled.swap($i, j) ?? crash "shuffle!: both indexes are below the list's length"
	}

	Ok($shuffled)
}

# ---- measuring ----

## A benchmark that works with both compilers. `position` is its place among
## the selected benchmarks, which its results keep.
TimedBenchmark : {
	position : U64,
	benchmark : Benchmark,
	slots : Sides(Slot),
	pairs : List(Pair),
}

## Measuring so far. The round number doubles as the value each edit writes,
## so it must keep growing across warm-up, screening and escalation.
Progress : { timed_benchmarks : List(TimedBenchmark), round : U64 }

measure! : Phase, List(Str) => Try({}, _)
measure! = |phase, args| {
	rocs = resolve_rocs!(args)?
	out = Path.utf8(option_value(args, "--out")?)

	selected =
		match option_value(args, "--only") {
			Ok(names) => benchmarks_named(names)?
			Err(missing) =>
				match phase {
					Screen => benchmarks
					Confirm => return Err(missing)
				}
		}

	os = detect_os!()
	root = Env.create_temp_dir_with_prefix!("roc-perf-")?

	Stdout.line!("Preflight: running ${selected.len().to_str()} benchmarks once per compiler.")?
	preflighted = run_preflight!(selected, { rocs, root })?
	alpha = family_alpha / U64.max(preflighted.measured.len(), 1).to_f64()

	Stdout.line!("Warm-up: ${warmup_round_count.to_str()} rounds.")?

	warmed_up = run_rounds!(
		{ timed_benchmarks: preflighted.measured, round: 0 },
		{ round_count: warmup_round_count, timings: Discard },
	)?

	first_pair_count =
		match phase {
			Screen => screen_pair_count
			Confirm => max_pair_count
		}

	Stdout.line!("Measuring ${first_pair_count.to_str()} pairs per benchmark.")?
	progress = run_rounds!(warmed_up, { round_count: first_pair_count, timings: Keep })?

	measured =
		match phase {
			Screen => escalate!(progress, alpha)?
			Confirm => progress.timed_benchmarks
		}

	measured_rows = measured.map(
		|timed| {
			position: timed.position,
			row: { name: timed.benchmark.name, result: Measured(measurement(timed.pairs, alpha)) },
		},
	)

	rows =
		List.concat(measured_rows, preflighted.skipped)
			.sort_by(|placed| placed.position)
			.map(|placed| placed.row)

	results = results_text({
		phase,
		os,
		cpu: read_cpu_model!(os),
		alpha,
		rows,
		timed_benchmarks: measured,
	})

	Path.write_utf8!(out, results)?
	Stdout.line!(rows_table(rows))?

	set_github_output!({ name: "regressed", value: Str.join_with(regressed_names(rows), ",") })?
	_ = Path.delete_all!(root)
	Ok({})
}

## What preflight found: the benchmarks to measure, and rows for the skipped
## ones.
Preflight : { measured : List(TimedBenchmark), skipped : List(PlacedRow) }

## Runs each benchmark once per compiler. One that only the head roc fails
## fails the check, and one the base roc fails is skipped.
run_preflight! : List(Benchmark), { rocs : Sides(Path), root : Path } => Try(Preflight, _)
run_preflight! = |selected, { rocs, root }| {
	var $measured = []
	var $skipped = []

	for (position, benchmark) in selected.map_with_index(|b, i| (i, b)) {
		slots = create_slots!(benchmark, { rocs, dir: root.join(position.to_str()) })?

		skipped_because = |reason| {
			position,
			row: { name: benchmark.name, result: Skipped(reason) },
		}

		match (try_run!(benchmark, slots.base)?, try_run!(benchmark, slots.head)?) {
			(Passed, Passed) => {
				$measured = $measured.append({ position, benchmark, slots, pairs: [] })
			}
			(Passed, Failed(details)) => {
				Stderr.line!("::error title=Perf benchmark failed::${benchmark.name} works with the base roc but fails with the head roc. If this PR changes test/echo/all_syntax_test.roc or test/perf/all_syntax_modules, keep them working.")?
				Stderr.line!(details)?
				return Err(Exit(2))
			}
			(Failed(details), Passed) => {
				Stdout.line!("Skipping ${benchmark.name}: the base roc fails on it.\n${details}")?
				$skipped = $skipped.append(skipped_because("the base roc fails on it: ${single_line(last_lines(details, 3))}"))
			}
			(Failed(details), Failed(_)) => {
				Stdout.line!("Skipping ${benchmark.name}: both the base and the head roc fail on it.\n${details}")?
				$skipped = $skipped.append(skipped_because("both the base and the head roc fail on it: ${single_line(last_lines(details, 3))}"))
			}
		}
	}

	Ok({ measured: $measured, skipped: $skipped })
}

## Runs `round_count` rounds of one pair per benchmark, in a new random order
## each round.
run_rounds! : Progress, { round_count : U64, timings : [Keep, Discard] } => Try(Progress, _)
run_rounds! = |progress, { round_count, timings }| {
	var $timed_benchmarks = progress.timed_benchmarks
	var $round = progress.round

	for _ in 0..<round_count {
		$round = $round + 1
		var $updated = []

		for timed in shuffle!($timed_benchmarks)? {
			pair = run_pair!(timed, $round)?

			updated =
				match timings {
					Keep => { ..timed, pairs: timed.pairs.append(pair) }
					Discard => timed
				}

			$updated = $updated.append(updated)
		}

		$timed_benchmarks = $updated.sort_by(|timed| timed.position)
	}

	Ok({ timed_benchmarks: $timed_benchmarks, round: $round })
}

run_pair! : TimedBenchmark, U64 => Try(Pair, _)
run_pair! = |timed, round| {
	base = { slot: timed.slots.base, round }
	head = { slot: timed.slots.head, round }
	prepare!(timed.benchmark, base)?
	prepare!(timed.benchmark, head)?

	match pick_order!()? {
		BaseFirst => {
			base_ms = time_run!(timed.benchmark, base)?
			head_ms = time_run!(timed.benchmark, head)?
			Ok({ base_ms, head_ms })
		}
		HeadFirst => {
			head_ms = time_run!(timed.benchmark, head)?
			base_ms = time_run!(timed.benchmark, base)?
			Ok({ base_ms, head_ms })
		}
	}
}

## Adds `batch_pair_count` pairs at a time to every benchmark that is not
## clearly fine yet, until none is left. Gives back every benchmark.
escalate! : Progress, F64 => Try(List(TimedBenchmark), _)
escalate! = |progress, alpha| {
	is_undecided = |timed| {
		verdict = measurement(timed.pairs, alpha).verdict
		should_add_pairs(verdict, timed.pairs.len())
	}

	var $decided = progress.timed_benchmarks.drop_if(is_undecided)
	var $undecided = progress.timed_benchmarks.keep_if(is_undecided)
	var $round = progress.round

	while !$undecided.is_empty() {
		names = $undecided.map(|timed| timed.benchmark.name) |> Str.join_with(", ")
		Stdout.line!("Adding ${batch_pair_count.to_str()} pairs for: ${names}")?

		batch = run_rounds!(
			{ timed_benchmarks: $undecided, round: $round },
			{ round_count: batch_pair_count, timings: Keep },
		)?

		$round = batch.round
		$decided = List.concat($decided, batch.timed_benchmarks.drop_if(is_undecided))
		$undecided = batch.timed_benchmarks.keep_if(is_undecided)
	}

	Ok($decided)
}

# ---- results files ----

## Which pass over the benchmarks a results file comes from.
Phase := [Screen, Confirm].{

	## The word that starts its results files' names.
	to_str : Phase -> Str
	to_str = |phase| match phase {
		Screen => "screen"
		Confirm => "confirm"
	}

	is_eq : _
}

## One benchmark's result on one machine.
Row : { name : Str, result : [Measured(Measurement), Skipped(Str)] }

## A row, and its benchmark's place among the selected ones.
PlacedRow : { position : U64, row : Row }

## A results file: its metadata, a line per benchmark, then each measured
## benchmark's times, which only people read.
results_text : {
	phase : Phase,
	os : Os,
	cpu : Try(Str, [CpuModelUnknown]),
	alpha : F64,
	rows : List(Row),
	timed_benchmarks : List(TimedBenchmark),
} -> Str
results_text = |{ phase, os, cpu, alpha, rows, timed_benchmarks }| {
	cpu_lines =
		match cpu {
			Ok(model) => ["meta\tcpu\t${single_line(model)}"]
			Err(CpuModelUnknown) => []
		}

	meta_lines = [
		["meta\tphase\t${phase.to_str()}", "meta\tos\t${os.to_str()}"],
		cpu_lines,
		["meta\tthreshold\t${threshold.to_str()}", "meta\talpha\t${alpha.to_str()}"],
	].join()

	times_text = |times| times.map(F64.to_str) |> Str.join_with(",")

	sample_lines = timed_benchmarks.join_map(
		|timed| [
			"base\t${timed.benchmark.name}\t${times_text(timed.pairs.map(|pair| pair.base_ms))}",
			"head\t${timed.benchmark.name}\t${times_text(timed.pairs.map(|pair| pair.head_ms))}",
		],
	)

	lines = [meta_lines, rows.map(row_line), sample_lines].join()
	"${Str.join_with(lines, "\n")}\n"
}

row_line : Row -> Str
row_line = |row|
	match row.result {
		Measured(measured) =>
			[
				"bench",
				row.name,
				measured.verdict.to_str(),
				measured.pair_count.to_str(),
				measured.ratio.to_str(),
				measured.interval.low.to_str(),
				measured.interval.high.to_str(),
				measured.base_ms.to_str(),
				measured.head_ms.to_str(),
			]
				|> Str.join_with("\t")
		Skipped(reason) => "skipped\t${row.name}\t${single_line(reason)}"
	}

## What one machine's results file says.
Results : {
	os : Os,
	cpu : Try(Str, [CpuModelUnknown]),
	rows : List(Row),
}

## Reads back what `results_text` wrote. Both ends are this program, so a
## line it can not read is a bug, and fails the whole file.
parse_results : Str -> Try(Results, [BadLine(Str), NoOsLine])
parse_results = |text| {
	var $os = Err(NoOsLine)
	var $cpu = Err(CpuModelUnknown)
	var $rows = []

	for line in text.split_on("\n") {
		match line.split_on("\t") {
			["meta", "os", name] => {
				os = Os.from_str(name) ? |_| BadLine(line)
				$os = Ok(os)
			}
			["meta", "cpu", model] => {
				$cpu = Ok(model)
			}
			["bench", name, verdict, pair_count, ratio, low, high, base_ms, head_ms] => {
				measured = {
					verdict: Verdict.from_str(verdict) ? |_| BadLine(line),
					pair_count: U64.from_str(pair_count) ? |_| BadLine(line),
					ratio: F64.from_str(ratio) ? |_| BadLine(line),
					interval: {
						low: F64.from_str(low) ? |_| BadLine(line),
						high: F64.from_str(high) ? |_| BadLine(line),
					},
					base_ms: F64.from_str(base_ms) ? |_| BadLine(line),
					head_ms: F64.from_str(head_ms) ? |_| BadLine(line),
				}

				$rows = $rows.append({ name, result: Measured(measured) })
			}
			["skipped", name, reason] => {
				$rows = $rows.append({ name, result: Skipped(reason) })
			}
			# For people reading the file.
			["meta", _, _] | ["base", ..] | ["head", ..] | [""] => {}
			_ => return Err(BadLine(line))
		}
	}

	Ok({
		os: $os?,
		cpu: $cpu,
		rows: $rows,
	})
}

sample_measurement : Measurement
sample_measurement = {
	verdict: Verdict.Regressed,
	pair_count: 110,
	ratio: 1.25,
	interval: { low: 1.125, high: 1.5 },
	base_ms: 100.5,
	head_ms: 125.0,
}

sample_row : Row
sample_row = { name: "all_syntax_test/cold-no-cache/check", result: Measured(sample_measurement) }

sample_row_with_verdict : Verdict -> Row
sample_row_with_verdict = |verdict| {
	measured = { ..sample_measurement, verdict }
	{ ..sample_row, result: Measured(measured) }
}

skipped_row : Row
skipped_row = { name: "all_syntax_modules/cold-no-cache/check", result: Skipped("parse error") }

sample_screen : Results
sample_screen = {
	os: Os.Linux,
	cpu: Ok("cpu"),
	rows: [sample_row],
}

expect {
	text = results_text({
		phase: Phase.Screen,
		os: Os.Linux,
		cpu: Ok("cpu"),
		alpha: 0.01,
		rows: [sample_row, skipped_row],
		timed_benchmarks: [],
	})

	parse_results(text)
		== Ok({
			os: Os.Linux,
			cpu: Ok("cpu"),
			rows: [sample_row, skipped_row],
		})
}

expect {
	text = results_text({
		phase: Phase.Confirm,
		os: Os.MacOS,
		cpu: Err(CpuModelUnknown),
		alpha: 0.01,
		rows: [],
		timed_benchmarks: [],
	})

	parse_results(text)
		== Ok({
			os: Os.MacOS,
			cpu: Err(CpuModelUnknown),
			rows: [],
		})
}

expect parse_results("meta\tos\tlinux\nbench\tname\n") == Err(BadLine("bench\tname"))
expect parse_results("meta\tos\tbeos\n") == Err(BadLine("meta\tos\tbeos"))
expect parse_results("") == Err(NoOsLine)

## The results for the job log, one benchmark per line.
rows_table : List(Row) -> Str
rows_table = |rows|
	rows.map(
		|row|
			match row.result {
				Measured(measured) => {
					times = "${one_decimal_text(measured.base_ms)} -> ${one_decimal_text(measured.head_ms)} ms"
					"${measured.verdict.to_str()}\t${change_and_interval_text(measured)}\t${measured.pair_count.to_str()} pairs\t${times}\t${row.name}"
				}
				Skipped(reason) => "skipped\t${row.name}: ${reason}"
			},
	)
		|> Str.join_with("\n")

## A row's measurement, when it regressed.
regression : Row -> Try(Measurement, [NotRegressed])
regression = |row|
	match row.result {
		Measured(measured) if measured.verdict == Verdict.Regressed => Ok(measured)
		Measured(_) | Skipped(_) => Err(NotRegressed)
	}

regressed_names : List(Row) -> List(Str)
regressed_names = |rows| rows.keep_oks(|row| regression(row).map_ok(|_| row.name))

expect
	regressed_names([sample_row, skipped_row, sample_row_with_verdict(Verdict.Unchanged)])
		== [sample_row.name]

## Sets a step output for later steps of the GitHub Actions job; does
## nothing outside one.
set_github_output! : { name : Str, value : Str } => Try({}, _)
set_github_output! = |{ name, value }|
	match Env.var_str!("GITHUB_OUTPUT") {
		Ok(file) => Path.append_utf8!(Path.utf8(file), "${name}=${value}\n")
		Err(_) => Ok({})
	}

## Every results file of `phase` in `dir`.
read_dir_results! : Path, Phase => Try(List(Results), _)
read_dir_results! = |dir, phase| {
	var $found = []

	for entry in Path.list!(dir)? {
		match entry.filename() {
			Ok(filename) => {
				name = filename.display()

				if name.starts_with("${phase.to_str()}-") and name.ends_with(".tsv") {
					text = Path.read_utf8!(entry)?
					results = parse_results(text) ? |problem| ResultsFile(name, problem)
					$found = $found.append(results)
				} else {
					{}
				}
			}
			Err(IsDirPath) | Err(EndsInDots) => {}
		}
	}

	Ok($found)
}

# ---- triage ----

## The machines whose screening flagged regressions, with the benchmarks to
## confirm there, as a GitHub Actions matrix.
triage_matrix : List(Results) -> Str
triage_matrix = |screens| {
	entries = screens.keep_oks(
		|screen|
			match regressed_names(screen.rows) {
				[] => Err(NothingFlagged)
				flagged => Ok({ os: screen.os.to_str(), benchmarks: Str.join_with(flagged, ",") })
			},
	)

	Json.to_str(entries)
}

expect triage_matrix([]) == "[]"
expect triage_matrix([sample_screen]) == "[{\"benchmarks\":\"all_syntax_test/cold-no-cache/check\",\"os\":\"linux\"}]"

triage! : List(Str) => Try({}, _)
triage! = |args| {
	screens = read_dir_results!(Path.utf8(option_value(args, "--dir")?), Phase.Screen)?
	matrix = triage_matrix(screens)
	Stdout.line!("Machines to confirm on: ${matrix}")?
	set_github_output!({ name: "matrix", value: matrix })
}

# ---- report ----

## What the report makes of one benchmark on one machine. A regression
## carries its confirmation, and an unconfirmed one its screening.
Outcome : [
	Regression(Measurement),
	NotConfirmed(Measurement),
	Missing,
	Skipped(Str),
	Noisy,
	Faster,
	Fine,
]

outcome : Row, Try(Row, [NotFound]) -> Outcome
outcome = |screen, confirm|
	match screen.result {
		Skipped(reason) => Skipped(reason)
		Measured(screened) =>
			match screened.verdict {
				Regressed =>
					match confirm {
						Ok(confirmation) =>
							match regression(confirmation) {
								Ok(confirmed) => Regression(confirmed)
								Err(NotRegressed) => NotConfirmed(screened)
							}
						Err(NotFound) => Missing
					}
				Inconclusive => Noisy
				Improved => Faster
				Unchanged => Fine
			}
	}

expect outcome(sample_row, Ok(sample_row)) == Regression(sample_measurement)
expect
	outcome(sample_row, Ok(sample_row_with_verdict(Verdict.Unchanged)))
		== NotConfirmed(sample_measurement)
expect outcome(sample_row, Ok(skipped_row)) == NotConfirmed(sample_measurement)
expect outcome(sample_row, Err(NotFound)) == Missing
expect outcome(skipped_row, Err(NotFound)) == Skipped("parse error")
expect outcome(sample_row_with_verdict(Verdict.Inconclusive), Err(NotFound)) == Noisy

outcome_label : Outcome -> Str
outcome_label = |o| match o {
	Regression(_) => ":x: regression"
	NotConfirmed(_) => ":warning: not confirmed"
	Missing => ":x: confirmation missing"
	Skipped(_) => ":warning: skipped"
	Noisy => ":grey_question: inconclusive"
	Faster => ":rocket: faster"
	Fine => ":white_check_mark: ok"
}

## Whether an outcome fails the check.
is_failing : Outcome -> Bool
is_failing = |o| match o {
	Regression(_) | Missing => True
	NotConfirmed(_) | Skipped(_) | Noisy | Faster | Fine => False
}

## The annotation an outcome prints, when it needs one.
annotation : Outcome, { os : Os, name : Str } -> Try(Str, [NoAnnotation])
annotation = |o, { os, name }| {
	subject = "${os.to_str()} ${name}"

	match o {
		Regression(confirmed) => Ok("::error title=Perf regression::${subject} is ${change_text(confirmed.ratio)} slower (confidence interval ${change_text(confirmed.interval.low)} to ${change_text(confirmed.interval.high)}), confirmed on a second machine.")
		Missing => Ok("::error title=Perf confirmation missing::${subject} regressed in screening but has no confirmation results.")
		NotConfirmed(screened) => Ok("::warning title=Perf regression not confirmed::${subject} looked ${change_text(screened.ratio)} slower in screening, but a second machine did not confirm it.")
		Skipped(reason) => Ok("::warning title=Perf benchmark skipped::${subject} was not measured: ${reason}")
		# Too noisy to decide is routine on shared runners and needs no
		# action, so it is only shown in the table.
		Noisy | Faster | Fine => Err(NoAnnotation)
	}
}

## A row's change, confidence interval and pair count; empty for a skipped
## benchmark.
result_cell : Row -> Str
result_cell = |row|
	match row.result {
		Measured(measured) => "${change_and_interval_text(measured)}, ${measured.pair_count.to_str()} pairs"
		Skipped(_) => ""
	}

## A benchmark's screening row, its confirmation if any, and what the report
## makes of them.
Assessment : {
	row : Row,
	confirmed : Try(Row, [NotFound]),
	outcome : Outcome,
}

table_line : Assessment -> Str
table_line = |{ row, confirmed, outcome: o }| {
	times =
		match row.result {
			Measured(measured) => {
				base: "${one_decimal_text(measured.base_ms)} ms",
				head: "${one_decimal_text(measured.head_ms)} ms",
			}
			Skipped(_) => { base: "", head: "" }
		}

	confirmed_cell =
		match confirmed {
			Ok(confirmation) => result_cell(confirmation)
			Err(NotFound) => ""
		}

	"| `${row.name}` | ${times.base} | ${times.head} | ${result_cell(row)} | ${confirmed_cell} | ${outcome_label(o)} |"
}

## The job summary, the annotations to print, and whether the check passes.
Report : {
	summary : Str,
	annotations : List(Str),
	check : [Pass, Fail],
}

## One machine's part of a report.
MachineReport : {
	section : Str,
	annotations : List(Str),
	skipped_lines : List(Str),
	check : [Pass, Fail],
}

## The `diagnose` markdown for one machine.
Diagnosis : { os : Os, text : Str }

build_report : {
	screens : List(Results),
	confirms : List(Results),
	diagnoses : List(Diagnosis),
} -> Report
build_report = |{ screens, confirms, diagnoses }| {
	machines = all_oses.map(
		|os|
			match screens.find_first(|screen| screen.os == os) {
				Ok(screen) => {
					confirm = confirms.find_first(|results| results.os == os)
					diagnosis = diagnoses.find_first(|d| d.os == os).map_ok(|d| d.text)
					machine_report(screen, { confirm, diagnosis })
				}
				Err(NotFound) => {
					section: "### ${os.to_str()}\n\nNo screening results.\n",
					annotations: ["::error title=Perf results missing::No screening results for ${os.to_str()}."],
					skipped_lines: [],
					check: Fail,
				}
			},
	)

	check =
		if machines.any(|machine| machine.check == Fail) {
			Fail
		} else {
			Pass
		}

	status =
		match check {
			Fail => "## :x: Perf regression check failed"
			Pass => "## :white_check_mark: No confirmed perf regressions"
		}

	skipped_lines = machines.join_map(|machine| machine.skipped_lines)

	skipped =
		if skipped_lines.is_empty() {
			""
		} else {
			"\n### :warning: Skipped benchmarks\n\n${Str.join_with(skipped_lines, "\n")}\n"
		}

	method =
		\\Each benchmark ran as interleaved base/head pairs on one machine. The change is the median head/base wall-time ratio,
		\\with a distribution-free confidence interval. A regression needs the whole confidence interval above +${one_decimal_text(threshold * 100.0)}%
		\\in screening and again on a second machine.

	sections = machines.map(|machine| machine.section)
	summary = List.concat([status, skipped, method, ""], sections) |> Str.join_with("\n")

	{
		summary,
		annotations: machines.join_map(|machine| machine.annotations),
		check,
	}
}

machine_report : Results,
{
	confirm : Try(Results, [NotFound]),
	diagnosis : Try(Str, [NotFound]),
} -> MachineReport
machine_report = |screen, { confirm, diagnosis }| {
	os = screen.os.to_str()

	assessments = screen.rows.map(
		|row| {
			confirmed =
				match confirm {
					Ok(results) => results.rows.find_first(|c| c.name == row.name)
					Err(NotFound) => Err(NotFound)
				}

			{
				row,
				confirmed,
				outcome: outcome(row, confirmed),
			}
		},
	)

	annotations = assessments.keep_oks(
		|a| annotation(a.outcome, { os: screen.os, name: a.row.name }),
	)

	skipped_lines = screen.rows.keep_oks(
		|row|
			match row.result {
				Skipped(reason) => Ok("- ${os} `${row.name}` was not measured: ${reason}")
				Measured(_) => Err(Measured)
			},
	)

	table_lines = Str.join_with(assessments.map(table_line), "\n")

	table =
		\\| benchmark | base | head | screening: change [confidence interval] | confirmation: change [confidence interval] | result |
		\\|---|---|---|---|---|---|
		\\${table_lines}

	title =
		match screen.cpu {
			Ok(cpu) => "${os} (${cpu})"
			Err(CpuModelUnknown) => os
		}

	expanded =
		\\### ${title}
		\\
		\\${table}
		\\
		\\

	section =
		match diagnosis {
			Ok(text) => "${expanded}${text}"
			# A machine with nothing to annotate folds its table away.
			Err(NotFound) if annotations.is_empty() => {
				benchmark_count = screen.rows.len()
				noisy_count = assessments.count_if(|a| a.outcome == Noisy)
				headline =
					if noisy_count == 0 {
						"all ${benchmark_count.to_str()} benchmarks fine"
					} else {
						"no regressions, ${noisy_count.to_str()} of ${benchmark_count.to_str()} benchmarks too noisy to decide"
					}
				\\<details><summary>${title}: ${headline}</summary>
				\\
				\\${table}
				\\
				\\</details>
				\\
			}
			Err(NotFound) => expanded
		}

	check =
		if assessments.any(|a| is_failing(a.outcome)) {
			Fail
		} else {
			Pass
		}

	{ section, annotations, skipped_lines, check }
}

## The report when every machine screened like `screen` and nothing needed
## confirming.
unconfirmed_report : Results -> Report
unconfirmed_report = |screen|
	build_report({
		screens: all_oses.map(|os| { ..screen, os }),
		confirms: [],
		diagnoses: [],
	})

# Linux's regression is not confirmed, macOS's is, and Windows ran nothing.
expect {
	report = build_report({
		screens: [
			sample_screen,
			{ ..sample_screen, os: Os.MacOS },
			{ ..sample_screen, os: Os.Windows, rows: [] },
		],
		confirms: [
			{ ..sample_screen, rows: [sample_row_with_verdict(Verdict.Unchanged)] },
			{ ..sample_screen, os: Os.MacOS },
		],
		diagnoses: [],
	})

	report.check == Fail and report.annotations.len() == 2
}

expect {
	report = unconfirmed_report({ ..sample_screen, rows: [skipped_row] })

	report.check == Pass
		and report.summary.contains("Skipped benchmarks")
		and report.annotations.len() == 3
}

expect {
	fine_row = sample_row_with_verdict(Verdict.Unchanged)
	report = unconfirmed_report({ ..sample_screen, rows: [fine_row] })

	report.check == Pass
		and report.annotations.is_empty()
		and report.summary.contains("<details><summary>linux (cpu): all 1 benchmarks fine")
}

expect {
	rows = [Verdict.Inconclusive, Verdict.Unchanged].map(sample_row_with_verdict)
	report = unconfirmed_report({ ..sample_screen, cpu: Err(CpuModelUnknown), rows })

	report.check == Pass
		and report.annotations.is_empty()
		and report.summary.contains("linux: no regressions, 1 of 2 benchmarks too noisy to decide")
}

expect {
	report = build_report({ screens: [], confirms: [], diagnoses: [] })
	report.check == Fail and report.annotations.len() == 3
}

report! : List(Str) => Try({}, _)
report! = |args| {
	dir = Path.utf8(option_value(args, "--dir")?)
	summary_path = Path.utf8(option_value(args, "--summary")?)
	screens = read_dir_results!(dir, Phase.Screen)?
	confirms = read_dir_results!(dir, Phase.Confirm)?
	var $diagnoses = []

	for os in all_oses {
		match Path.read_utf8!(dir.join("diagnose-${os.to_str()}.md")) {
			Ok(text) => {
				$diagnoses = $diagnoses.append({ os, text })
			}
			# Only machines that confirmed a regression are diagnosed.
			Err(_) => {}
		}
	}

	report = build_report({ screens, confirms, diagnoses: $diagnoses })
	Path.append_utf8!(summary_path, report.summary)?

	for line in report.annotations {
		Stdout.line!(line)?
	}

	match report.check {
		Pass => Ok({})
		Fail => Err(Exit(1))
	}
}

# ---- diagnose ----

diagnose! : List(Str) => Try({}, _)
diagnose! = |args| {
	rocs = resolve_rocs!(args)?
	out = Path.utf8(option_value(args, "--out")?)
	selected = benchmarks_named(option_value(args, "--only")?)?
	os = detect_os!()
	root = Env.create_temp_dir_with_prefix!("roc-perf-diag-")?

	has_valgrind = os == Os.Linux and Cmd.check_available!("valgrind")

	var $sections = []

	for (position, benchmark) in selected.map_with_index(|b, i| (i, b)) {
		slots = create_slots!(benchmark, { rocs, dir: root.join(position.to_str()) })?
		_ = try_run!(benchmark, slots.base)?
		_ = try_run!(benchmark, slots.head)?
		timings = time_phases!(benchmark, { slots, first_round: 1000 + position * 100 })?
		rss = measure_peak_rss!(benchmark, { os, slots, first_round: 3000 + position * 100 })?

		cachegrind =
			if has_valgrind {
				count_with_cachegrind!(benchmark, { slots, round: 2000 + position * 100 })?
			} else {
				""
			}

		section =
			\\<details><summary>Diagnostics for <code>${benchmark.name}</code></summary>
			\\
			\\${timings}${rss}${cachegrind}</details>
			\\

		$sections = $sections.append(section)
	}

	Path.write_utf8!(out, Str.join_with($sections, "\n"))?
	_ = Path.delete_all!(root)
	Ok({})
}

## One `--timings` row: the total or a phase, or a part of a phase.
Timing : {
	name : Str,
	ms : F64,
	level : [Phase, Part],
}

## `--timings` rows look like `  ✓ Type Checking   18ms, RSS 10MB - 58MB` for
## a phase and `      Parsing (summed)   0ms` for a part of one. The total
## (`roc check   50ms, peak RSS 58MB`) and phases are indented less than parts.
parse_timings : Str -> List(Timing)
parse_timings = |text|
	text.split_on("\n").keep_oks(
		|line| {
			indent = line.count_utf8_bytes() - line.trim_start().count_utf8_bytes()
			body = line.trim().drop_prefix("✓").trim()

			before_comma =
				match body.split_first(",") {
					Ok(parts) => parts.before
					Err(NotFound) => body
				}

			words = before_comma.split_on(" ").drop_if(Str.is_empty)
			last_word = words.last() ? |_| NotATiming
			ms = duration_ms(last_word) ? |_| NotATiming

			level =
				if indent < 4 {
					Phase
				} else {
					Part
				}

			name = Str.join_with(words.drop_last(1), " ")
			Ok({ name, ms, level })
		},
	)

duration_ms : Str -> Try(F64, [NotDuration])
duration_ms = |word| {
	if word.ends_with("ms") {
		F64.from_str(word.drop_suffix("ms")).map_err(|_| NotDuration)
	} else if word.ends_with("µs") {
		F64.from_str(word.drop_suffix("µs")).map_ok(|x| x / 1000.0).map_err(|_| NotDuration)
	} else if word.ends_with("s") {
		F64.from_str(word.drop_suffix("s")).map_ok(|x| x * 1000.0).map_err(|_| NotDuration)
	} else {
		Err(NotDuration)
	}
}

expect duration_ms("84.5ms") == Ok(84.5)
expect duration_ms("1.5s") == Ok(1500.0)
expect duration_ms("fast") == Err(NotDuration)
expect
	parse_timings("roc check   50ms, peak RSS 58MB\n  ✓ Type Checking   18ms, RSS 10MB - 58MB\n      Parsing (summed)   0.5ms\nnoise\n")
		== [
			{ name: "roc check", ms: 50.0, level: Phase },
			{ name: "Type Checking", ms: 18.0, level: Phase },
			{ name: "Parsing (summed)", ms: 0.5, level: Part },
		]

## `--timings` runs per side in a diagnosis.
timings_run_count : U64
timings_run_count = 5

## `/usr/bin/time` runs per side in a diagnosis.
rss_run_count : U64
rss_run_count = 3

## Median per phase of `timings_run_count` `--timings` runs per side. Parts of
## phases that take under a millisecond on both sides are left out.
time_phases! : Benchmark, { slots : Sides(Slot), first_round : U64 } => Try(Str, _)
time_phases! = |benchmark, { slots, first_round }| {
	run_timings! = |slot, round| {
		prepare!(benchmark, { slot, round })?
		output = roc_command(benchmark, { slot, round, flags: ["--timings"], wrapper: [] }).run!()?
		Ok(parse_timings(output.stderr_utf8_lossy()))
	}

	var $base_runs = []
	var $head_runs = []

	for round in first_round..<(first_round + timings_run_count) {
		$base_runs = $base_runs.append(run_timings!(slots.base, round)?)
		$head_runs = $head_runs.append(run_timings!(slots.head, round)?)
	}

	phase_median = |runs, name|
		median(runs.join_map(|run| run.keep_if(|t| t.name == name).map(|t| t.ms)))

	first_head_run = $head_runs.first() ?? crash "time_phases!: each side has `timings_run_count` runs"

	rows = first_head_run.join_map(
		|timing| {
			label =
				match timing.level {
					Phase => "**${timing.name}**"
					Part => timing.name
				}

			match (phase_median($base_runs, timing.name), phase_median($head_runs, timing.name)) {
				(Ok(base_ms), Ok(head_ms)) =>
					if timing.level == Part and base_ms < 1.0 and head_ms < 1.0 {
						[]
					} else {
						change =
							if base_ms > 0.0 {
								change_text(head_ms / base_ms)
							} else {
								""
							}

						["| ${label} | ${one_decimal_text(base_ms)} | ${one_decimal_text(head_ms)} | ${change} |"]
					}
				# Only the head roc reports it.
				(Err(ListWasEmpty), Ok(head_ms)) => ["| ${label} |  | ${one_decimal_text(head_ms)} |  |"]
				(_, Err(ListWasEmpty)) => crash "time_phases!: the first head run reports every row it lists"
			}
		},
	)

	if rows.is_empty() {
		Ok("No `--timings` phase rows found.\n\n")
	} else {
		table_lines = Str.join_with(rows, "\n")

		text =
			\\`--timings` (median of ${timings_run_count.to_str()} runs, ms)
			\\
			\\| phase | base | head | change |
			\\|---|---|---|---|
			\\${table_lines}
			\\
			\\

		Ok(text)
	}
}

## Peak resident memory from `/usr/bin/time`, median of `rss_run_count` runs
## per side.
measure_peak_rss! : Benchmark,
{
	os : Os,
	slots : Sides(Slot),
	first_round : U64,
} => Try(Str, _)
measure_peak_rss! = |benchmark, { os, slots, first_round }| {
	time_path = "/usr/bin/time"

	wrapper =
		match os {
			Linux => [time_path, "-v"]
			MacOS => [time_path, "-l"]
			Windows => return Ok("")
		}

	if Path.exists!(Path.utf8(time_path))? {
		sample_rss! = |slot| {
			var $samples = []

			for round in first_round..<(first_round + rss_run_count) {
				prepare!(benchmark, { slot, round })?
				command = roc_command(benchmark, { slot, round, flags: [], wrapper })
				output = command.run!()?

				match max_rss_kb(os, output.stderr_utf8_lossy()) {
					Ok(kb) => {
						$samples = $samples.append(kb)
					}
					Err(NoMaxRss) => {}
				}
			}

			Ok($samples)
		}

		base_kb = sample_rss!(slots.base)?
		head_kb = sample_rss!(slots.head)?

		match (median(base_kb), median(head_kb)) {
			(Ok(base), Ok(head)) => {
				base_mib = one_decimal_text(base / 1024.0)
				head_mib = one_decimal_text(head / 1024.0)
				Ok("Peak RSS: base ${base_mib} MiB, head ${head_mib} MiB (${change_text(head / base)}).\n\n")
			}
			_ => Ok("")
		}
	} else {
		Ok("")
	}
}

## GNU time prints `Maximum resident set size (kbytes): N`; macOS prints
## `N  maximum resident set size` in bytes.
max_rss_kb : Os, Str -> Try(F64, [NoMaxRss])
max_rss_kb = |os, text| {
	line = text.split_on("\n").find_first(|l| l.contains("aximum resident set size")) ? |_| NoMaxRss

	match os {
		Linux | Windows => {
			kilobytes = line.split_on(":").last() ? |_| NoMaxRss
			F64.from_str(kilobytes.trim()).map_err(|_| NoMaxRss)
		}
		MacOS => {
			bytes = line.trim().split_on(" ").first() ? |_| NoMaxRss
			F64.from_str(bytes).map_ok(|b| b / 1024.0).map_err(|_| NoMaxRss)
		}
	}
}

expect max_rss_kb(Os.Linux, "\tMaximum resident set size (kbytes): 2048\n") == Ok(2048.0)
expect max_rss_kb(Os.MacOS, "     2097152  maximum resident set size\n") == Ok(2048.0)
expect max_rss_kb(Os.Linux, "nothing here\n") == Err(NoMaxRss)

## One of cachegrind's summary counts.
Counter : { name : Str, value : F64 }

## Instruction and simulated cache and branch counts from cachegrind; these
## are exact, unlike hardware counters, which shared VMs rarely expose.
count_with_cachegrind! : Benchmark, { slots : Sides(Slot), round : U64 } => Try(Str, _)
count_with_cachegrind! = |benchmark, { slots, round }| {
	run_cachegrind! = |slot| {
		prepare!(benchmark, { slot, round })?
		log = slot.cache_dir.join("cachegrind.log")

		wrapper = [
			"valgrind",
			"--tool=cachegrind",
			"--cache-sim=yes",
			"--branch-sim=yes",
			"--cachegrind-out-file=/dev/null",
			"--log-file=${log.display()}",
		]

		_ =
			roc_command(benchmark, { slot, round, flags: ["--jobs=1"], wrapper })
				.stdout(Null)
				.stderr(Null)
				.run!()?

		match Path.read_utf8!(log) {
			Ok(text) => Ok(cachegrind_summary(text))
			# Valgrind did not get far enough to log; the section says so.
			Err(_) => Ok([])
		}
	}

	base = run_cachegrind!(slots.base)?
	head = run_cachegrind!(slots.head)?

	rows = base.map(
		|counter| {
			head_count = head.find_first(|h| h.name == counter.name).map_ok(|h| h.value)

			match head_count {
				Ok(head_value) => {
					change =
						if counter.value > 0.0 {
							change_text(head_value / counter.value)
						} else {
							""
						}

					"| ${counter.name} | ${count_text(counter.value)} | ${count_text(head_value)} | ${change} |"
				}
				Err(NotFound) => "| ${counter.name} | ${count_text(counter.value)} |  |  |"
			}
		},
	)

	if rows.is_empty() {
		Ok("Cachegrind produced no summary.\n\n")
	} else {
		table_lines = Str.join_with(rows, "\n")

		text =
			\\Cachegrind (`--jobs=1`)
			\\
			\\| counter | base | head | change |
			\\|---|---|---|---|
			\\${table_lines}
			\\
			\\

		Ok(text)
	}
}

## Lines like `==123== I refs:      1,234,567` or
## `==123== Mispredicts:      4,321  (  4,000 cond +  321 ind)`.
cachegrind_summary : Str -> List(Counter)
cachegrind_summary = |text| {
	wanted_names = [
		"I refs",
		"D refs",
		"I1  misses",
		"D1  misses",
		"LLi misses",
		"LLd misses",
		"LL misses",
		"Branches",
		"Mispredicts",
	]

	text.split_on("\n").keep_oks(
		|line| {
			body =
				match line.split_first("== ") {
					Ok(parts) => parts.after
					Err(NotFound) => line
				}

			parts = body.split_first(":") ? |_| NotACounter
			name = parts.before.trim()

			if wanted_names.contains(name) {
				digits = parts.after.trim().split_on(" ").first() ? |_| NotACounter
				value = F64.from_str(digits.replace_each(",", "")) ? |_| NotACounter
				Ok({ name, value })
			} else {
				Err(NotACounter)
			}
		},
	)
}

expect
	cachegrind_summary("==42== I refs:        1,234,567\n==42== Mispredicts:   4,321  (4,000 cond + 321 ind)\n")
		== [{ name: "I refs", value: 1234567.0 }, { name: "Mispredicts", value: 4321.0 }]

count_text : F64 -> Str
count_text = |count| {
	if count >= 1_000_000_000.0 {
		"${one_decimal_text(count / 1_000_000_000.0)}G"
	} else if count >= 1_000_000.0 {
		"${one_decimal_text(count / 1_000_000.0)}M"
	} else if count >= 1000.0 {
		"${one_decimal_text(count / 1000.0)}k"
	} else {
		one_decimal_text(count)
	}
}

expect count_text(1_234_567.0) == "1.2M"
expect count_text(999.0) == "999.0"
