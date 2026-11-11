## Perf regression benchmarks for roc pull requests, driven by
## .github/workflows/ci_perf.yml. Always run with the nightly pinned in the
## header, never with a roc built from the sources being measured.
##
## `screen` compares a base and a head roc on one machine. Each benchmark runs
## as base/head pairs, in random order within each pair and across benchmarks,
## so drift on a shared machine hits both sides alike. The median of the
## per-pair head/base wall-time ratios gets a distribution-free confidence
## interval from sign-test order statistics, at a Bonferroni-corrected level.
## Benchmarks that are not clearly fine get more pairs, up to `max_pairs`.
##
## `confirm` re-measures what screening flagged on a fresh machine with a fixed
## number of pairs, `diagnose` explains confirmed regressions, `triage` picks
## the machines to confirm on, and `report` writes the job summary. It fails
## only when screening and confirmation both found a regression.
app [main!] {
	pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.0/2yhxoR7cZPSYbyUzPWULFw8AA6QEcdsgFNRmzu7pZbst.tar.zst",
	roc: "nightly-2026-11-06-34f137a",
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

warmup_rounds : U64
warmup_rounds = 2

screen_pairs : U64
screen_pairs = 30

batch_pairs : U64
batch_pairs = 20

max_pairs : U64
max_pairs = 100

## Every editable fixture file holds this line; benchmarks rewrite the number.
edit_marker : Str
edit_marker = "perf_edit = 0"

Scenario : [ColdNoCache, ColdEmptyCache, WarmNoChange, WarmEdit(Str)]

Bench : {
	name : Str,
	project : [InPlace(Str), Copied(Str)],
	scenario : Scenario,
	args : List(Str),
}

benchmarks : List(Bench)
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

	single_file.concat(modules)
}

main! : List(OsStr) => Try({}, _)
main! = |raw_args| {
	args = raw_args.map(OsStr.display)
	match args {
		["screen", .. as rest] => measure!(Screen, rest)
		["confirm", .. as rest] => measure!(Confirm, rest)
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

option : List(Str), Str -> Try(Str, [MissingOption(Str)])
option = |args, name| match args {
	[flag, value, ..] if flag == name => Ok(value)
	[_, .. as rest] => option(rest, name)
	[] => Err(MissingOption(name))
}

expect option(["--base", "a", "--head", "b"], "--head") == Ok("b")
expect option(["--base"], "--base") == Err(MissingOption("--base"))

path_of : Str -> Path
path_of = |str| Path.from_raw(Utf8(str))

split_names : Str -> List(Str)
split_names = |names| names.split_on(",").map(|n| n.trim()).drop_if(|n| n.is_empty())

select : Str -> Try(List(Bench), [UnknownBenchmark(Str)])
select = |names| {
	var $selected = []
	for name in split_names(names) {
		match benchmarks.find_first(|b| b.name == name) {
			Ok(bench) => {
				$selected = $selected.append(bench)
			}
			Err(NotFound) => {
				return Err(UnknownBenchmark(name))
			}
		}
	}
	Ok($selected)
}

expect select("all_syntax_test/cold-no-cache/check").map_ok(|bs| bs.len()) == Ok(1)
expect select("nope") == Err(UnknownBenchmark("nope"))

os_name! : () => Str
os_name! = || match System.target!().os() {
	Linux(_) => "linux"
	MacOS(_) => "macos"
	Windows(_) => "windows"
}

cpu_model! : Str => Str
cpu_model! = |os| {
	model =
		if os == "linux" {
			match Path.read_utf8!(path_of("/proc/cpuinfo")) {
				Ok(text) =>
					match text.split_on("\n").find_first(|line| line.starts_with("model name")) {
						Ok(line) =>
							match line.split_first(":") {
								Ok(parts) => parts.after.trim()
								Err(_) => line
							}
						Err(_) => "unknown"
					}
				Err(_) => "unknown"
			}
		} else if os == "macos" {
			match Cmd.new("sysctl").args(["-n", "machdep.cpu.brand_string"]).capture!() {
				Ok(text) => text.trim()
				Err(_) => "unknown"
			}
		} else {
			Env.var_str!("PROCESSOR_IDENTIFIER") ?? "unknown"
		}
	sanitize(model)
}

# ---- statistics ----

sort_f64 : List(F64) -> List(F64)
sort_f64 = |xs| xs.sort_with(|a, b| if (a < b) Before else if (a > b) After else Same)

median : List(F64) -> F64
median = |xs| {
	sorted = sort_f64(xs)
	n = sorted.len()
	if n == 0 {
		0.0
	} else if n % 2 == 1 {
		sorted.get(n // 2) ?? 0.0
	} else {
		((sorted.get(n // 2 - 1) ?? 0.0) + (sorted.get(n // 2) ?? 0.0)) / 2.0
	}
}

expect median([3.0, 1.0, 2.0]) == 2.0
expect median([4.0, 1.0, 3.0, 2.0]) == 2.5
expect median([]) == 0.0

## The 1-based rank `r` for which the order statistics x_(r) and x_(n+1-r)
## bound the median with probability at least 1 - alpha, from the sign test:
## the largest r with P(Binomial(n, 1/2) <= r - 1) <= alpha / 2.
## Zero when n is too small for that level.
median_rank : U64, F64 -> U64
median_rank = |n, alpha| {
	var $pmf = 1.0
	var $i = 0
	while $i < n {
		$pmf = $pmf * 0.5
		$i = $i + 1
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
		$pmf = $pmf * U64.to_f64(n - $k) / U64.to_f64($k + 1)
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
	n = ratios.len()
	rank = median_rank(n, alpha)
	if rank == 0 {
		Err(TooFewPairs)
	} else {
		sorted = sort_f64(ratios)
		Ok({ low: sorted.get(rank - 1) ?? 0.0, high: sorted.get(n - rank) ?? 0.0 })
	}
}

Verdict : [Regressed, Improved, Unchanged, Inconclusive]

classify : Try(Interval, [TooFewPairs]) -> Verdict
classify = |interval| match interval {
	Ok(ci) =>
		if ci.low > 1.0 + threshold {
			Regressed
		} else if ci.high < 1.0 - threshold {
			Improved
		} else if ci.high <= 1.0 + threshold {
			Unchanged
		} else {
			Inconclusive
		}
	Err(TooFewPairs) => Inconclusive
}

expect classify(Ok({ low: 1.06, high: 1.2 })) == Regressed
expect classify(Ok({ low: 1.05, high: 1.2 })) == Inconclusive
expect classify(Ok({ low: 0.97, high: 1.05 })) == Unchanged
expect classify(Ok({ low: 0.8, high: 0.94 })) == Improved
expect classify(Err(TooFewPairs)) == Inconclusive

verdict_str : Verdict -> Str
verdict_str = |verdict| match verdict {
	Regressed => "regressed"
	Improved => "improved"
	Unchanged => "ok"
	Inconclusive => "inconclusive"
}

## Screening keeps measuring a benchmark until it is clearly fine or has
## `max_pairs` pairs, so `regressed` is only ever decided at `max_pairs`.
needs_more_pairs : Verdict, U64 -> Bool
needs_more_pairs = |verdict, pairs| {
	if pairs >= max_pairs {
		Bool.False
	} else {
		match verdict {
			Regressed | Inconclusive => Bool.True
			Unchanged | Improved => Bool.False
		}
	}
}

expect needs_more_pairs(Regressed, 30)
expect !needs_more_pairs(Regressed, 100)
expect needs_more_pairs(Inconclusive, 50)
expect !needs_more_pairs(Unchanged, 30)

ratios : List(F64), List(F64) -> List(F64)
ratios = |base_ms, head_ms| {
	var $out = []
	var $i = 0
	while $i < base_ms.len() {
		b = base_ms.get($i) ?? 1.0
		h = head_ms.get($i) ?? 1.0
		$out = $out.append(h / b)
		$i = $i + 1
	}
	$out
}

# ---- formatting ----

round_i64 : F64 -> I64
round_i64 = |x| if (x >= 0.0) F64.to_i64_wrap(x + 0.5) else F64.to_i64_wrap(x - 0.5)

fixed1 : F64 -> Str
fixed1 = |x| {
	tenths = round_i64(x * 10.0)
	sign = if (tenths < 0) "-" else ""
	whole = I64.abs(tenths) // 10
	frac = I64.abs(tenths) % 10
	"${sign}${whole.to_str()}.${frac.to_str()}"
}

expect fixed1(12.34) == "12.3"
expect fixed1(-0.05) == "-0.1"
expect fixed1(0.0) == "0.0"

change : F64 -> Str
change = |ratio| {
	pct = (ratio - 1.0) * 100.0
	if (round_i64(pct * 10.0) > 0) "+${fixed1(pct)}%" else "${fixed1(pct)}%"
}

expect change(1.052) == "+5.2%"
expect change(0.97) == "-3.0%"
expect change(1.0) == "0.0%"

sanitize : Str -> Str
sanitize = |str| str.replace_each("\t", " ").replace_each("\n", " ").replace_each("\r", " ").trim()

last_lines : Str, U64 -> Str
last_lines = |text, count| {
	lines = text.trim().split_on("\n")
	lines.drop_first(lines.len() - U64.min(count, lines.len())) |> Str.join_with("\n")
}

# ---- running roc ----

Slot : {
	file : Path,
	cache : Path,
	out : Path,
	edit : [NoEdit, Edit(Path, Str)],
}

## Give each side of each benchmark its own project copy and cache, so the two
## compilers never share cache entries, even when they are the same binary.
setup_slot! : Path, Str, U64, Bench => Try(Slot, _)
setup_slot! = |root, side, index, bench| {
	dir = root.join(side).join(index.to_str())
	cache = dir.join("cache")
	Path.create_all!(cache)?
	out = dir.join("out")
	match bench.project {
		InPlace(file) => Ok({ file: path_of(file), cache, out, edit: NoEdit })
		Copied(source) => {
			project = dir.join("project")
			Path.copy_dir!(path_of(source), project)?
			edit =
				match bench.scenario {
					WarmEdit(name) => {
						edited = project.join(name)
						original = Path.read_utf8!(edited)?
						if original.contains(edit_marker) {
							Edit(edited, original)
						} else {
							return Err(MissingEditMarker("${source}/${name} must contain `${edit_marker}`"))
						}
					}
					_ => NoEdit
				}
			Ok({ file: project.join("main.roc"), cache, out, edit })
		}
	}
}

## Every run builds to its own path: Windows can keep a just-written
## executable locked, so overwriting the previous one fails.
out_path : Slot, U64 -> Path
out_path = |slot, run| path_of("${slot.out.display()}-${run.to_str()}")

command_args : Bench, Slot, List(Str), U64 -> List(OsStr)
command_args = |bench, slot, extra, run| bench.args.join_map(
	|arg|
		if arg == "{file}" {
			no_cache =
				match bench.scenario {
					ColdNoCache => ["--no-cache"]
					_ => []
				}
			no_cache.concat(extra).map(OsStr.from_str).append(slot.file.to_os_str())
		} else if arg == "{out}" {
			[OsStr.from_str("--output=${out_path(slot, run).display()}")]
		} else {
			[OsStr.from_str(arg)]
		},
)

roc_command : Path, Bench, Slot, List(Str), U64 -> Cmd.Command
roc_command = |roc, bench, slot, extra, run|
	Cmd.new(roc.to_os_str())
		.args(command_args(bench, slot, extra, run))
		.env("ROC_CACHE_DIR", slot.cache.to_os_str())

failure_details : Cmd.Output -> Str
failure_details = |output| "${Str.inspect(output.status)}\n${last_lines(output.stderr_utf8_lossy(), 15)}"

## Run untimed and report whether the benchmark works with this compiler.
try_run! : Path, Bench, Slot => Try([Passed, Failed(Str)], _)
try_run! = |roc, bench, slot| {
	output = roc_command(roc, bench, slot, [], 0).run!()?
	match output.status {
		Exited(0) => Ok(Passed)
		_ => Ok(Failed(failure_details(output)))
	}
}

time_run! : Path, Bench, Slot, U64 => Try(F64, _)
time_run! = |roc, bench, slot, run| {
	cmd = roc_command(roc, bench, slot, [], run).stdout(Null)
	start = Monotonic.now!()
	output = cmd.run!()?
	elapsed = start.elapsed!()
	if bench.args.contains("{out}") {
		_ = Path.delete!(out_path(slot, run))
		{}
	} else {
		{}
	}
	match output.status {
		Exited(0) => Ok(U64.to_f64(elapsed.to_nanos()) / 1_000_000.0)
		_ => Err(RunFailed("${bench.name} failed with ${roc.display()} mid-run: ${failure_details(output)}"))
	}
}

## Set up the state both sides of a pair should start from.
prepare! : Bench, Slot, U64 => Try({}, _)
prepare! = |bench, slot, round|
	match bench.scenario {
		ColdEmptyCache => {
			Path.delete_all!(slot.cache)?
			Path.create_all!(slot.cache)
		}
		WarmEdit(_) =>
			match slot.edit {
				# A number never used before, so the edit can not hit the cache.
				Edit(path, original) => Path.write_utf8!(path, original.replace_first(edit_marker, "perf_edit = ${round.to_str()}"))
				NoEdit => Ok({})
			}
		ColdNoCache | WarmNoChange => Ok({})
	}

coin! : () => Try(Bool, _)
coin! = || Ok(Random.seed_u32!()? % 2 == 0)

shuffle! : List(a) => Try(List(a), _)
shuffle! = |items| {
	var $list = items
	var $i = items.len()
	while $i > 1 {
		j = U32.to_u64(Random.seed_u32!()?) % $i
		$i = $i - 1
		$list = $list.swap($i, j) ?? $list
	}
	Ok($list)
}

# ---- measuring ----

State : {
	bench : Bench,
	base : Slot,
	head : Slot,
	skipped : [Measured, Skipped(Str)],
	base_ms : List(F64),
	head_ms : List(F64),
}

Phase : [Screen, Confirm]

measure! : Phase, List(Str) => Try({}, _)
measure! = |phase, args| {
	base_roc = Path.absolute!(path_of(option(args, "--base")?))?
	head_roc = Path.absolute!(path_of(option(args, "--head")?))?
	out = path_of(option(args, "--out")?)
	selected =
		match (phase, option(args, "--only")) {
			(_, Ok(names)) => select(names)?
			(Screen, Err(_)) => benchmarks
			(Confirm, Err(missing)) => return Err(missing)
		}
	os = os_name!()
	root = Env.create_temp_dir_with_prefix!("roc-perf-")?

	Stdout.line!("Preflight: running ${selected.len().to_str()} benchmarks once per compiler.")?
	var $states = []
	var $index = 0
	for bench in selected {
		base = setup_slot!(root, "base", $index, bench)?
		head = setup_slot!(root, "head", $index, bench)?
		skipped =
			match (try_run!(base_roc, bench, base)?, try_run!(head_roc, bench, head)?) {
				(Passed, Passed) => Measured
				(Passed, Failed(details)) => {
					Stderr.line!("::error title=Perf benchmark failed::${bench.name} works with the base roc but fails with the head roc. If this PR changes test/echo/all_syntax_test.roc or test/perf/all_syntax_modules, keep them working.")?
					Stderr.line!(details)?
					return Err(Exit(2))
				}
				(Failed(details), Passed) => {
					Stdout.line!("Skipping ${bench.name}: the base roc fails on it.\n${details}")?
					Skipped("the base roc fails on it: ${sanitize(last_lines(details, 3))}")
				}
				(Failed(details), Failed(_)) => {
					Stdout.line!("Skipping ${bench.name}: both the base and the head roc fail on it.\n${details}")?
					Skipped("both the base and the head roc fail on it: ${sanitize(last_lines(details, 3))}")
				}
			}
		$states = $states.append({ bench, base, head, skipped, base_ms: [], head_ms: [] })
		$index = $index + 1
	}

	measured = $states.count_if(|s| s.skipped == Measured)
	alpha = family_alpha / U64.to_f64(U64.max(measured, 1))
	all_measured = indices_where($states, |s| s.skipped == Measured)
	sides = { base: base_roc, head: head_roc }

	Stdout.line!("Warm-up: ${warmup_rounds.to_str()} rounds.")?
	var $run = run_rounds!({ states: $states, round: 0 }, all_measured, warmup_rounds, Bool.False, sides)?

	first_pairs =
		match phase {
			Screen => screen_pairs
			Confirm => max_pairs
		}
	Stdout.line!("Measuring ${first_pairs.to_str()} pairs per benchmark.")?
	$run = run_rounds!($run, all_measured, first_pairs, Bool.True, sides)?

	match phase {
		Screen => {
			var $active = escalate($run.states, alpha)
			while !$active.is_empty() {
				names = $active.map(|i| ($run.states.get(i) ?? dummy_state).bench.name) |> Str.join_with(", ")
				Stdout.line!("Adding ${batch_pairs.to_str()} pairs for: ${names}")?
				$run = run_rounds!($run, $active, batch_pairs, Bool.True, sides)?
				$active = escalate($run.states, alpha)
			}
		}
		Confirm => {}
	}
	$states = $run.states

	rows = $states.map(|s| to_row(s, alpha))
	meta = [
		(
			"phase",
			match phase {
				Screen => "screen"
				Confirm => "confirm"
			},
		),
		("os", os),
		("cpu", cpu_model!(os)),
		("threshold", threshold.to_str()),
		("alpha", alpha.to_str()),
	]
	Path.write_utf8!(out, results_text(meta, rows, $states))?
	Stdout.line!(rows_table(rows))?

	flagged = rows.keep_if(|row| row.verdict == "regressed").map(|row| row.name) |> Str.join_with(",")
	github_output!("regressed", flagged)?
	_ = Path.delete_all!(root)
	Ok({})
}

dummy_state : State
dummy_state = {
	bench: { name: "", project: InPlace(""), scenario: ColdNoCache, args: [] },
	base: { file: path_of(""), cache: path_of(""), out: path_of(""), edit: NoEdit },
	head: { file: path_of(""), cache: path_of(""), out: path_of(""), edit: NoEdit },
	skipped: Measured,
	base_ms: [],
	head_ms: [],
}

indices_where : List(State), (State -> Bool) -> List(U64)
indices_where = |states, keep| {
	var $out = []
	var $i = 0
	for state in states {
		if keep(state) {
			$out = $out.append($i)
		} else {
			{}
		}
		$i = $i + 1
	}
	$out
}

escalate : List(State), F64 -> List(U64)
escalate = |states, alpha|
	indices_where(
		states,
		|s| s.skipped == Measured and needs_more_pairs(classify(ratio_interval(ratios(s.base_ms, s.head_ms), alpha)), s.base_ms.len()),
	)

Sides : { base : Path, head : Path }

## The round number doubles as the value each edit writes, so it must keep
## growing across warm-up, screening and escalation.
Run : { states : List(State), round : U64 }

run_rounds! : Run, List(U64), U64, Bool, Sides => Try(Run, _)
run_rounds! = |run, active, rounds, record, sides| {
	var $states = run.states
	var $round = run.round
	var $r = 0
	while $r < rounds {
		$round = $round + 1
		$states = run_round!($states, active, $round, record, sides)?
		$r = $r + 1
	}
	Ok({ states: $states, round: $round })
}

run_round! : List(State), List(U64), U64, Bool, Sides => Try(List(State), _)
run_round! = |states, active, round, record, sides| {
	var $states = states
	for index in shuffle!(active)? {
		state = $states.get(index) ?? dummy_state
		prepare!(state.bench, state.base, round)?
		prepare!(state.bench, state.head, round)?
		base_first = coin!()?
		times =
			if base_first {
				b = time_run!(sides.base, state.bench, state.base, round)?
				h = time_run!(sides.head, state.bench, state.head, round)?
				{ b, h }
			} else {
				h = time_run!(sides.head, state.bench, state.head, round)?
				b = time_run!(sides.base, state.bench, state.base, round)?
				{ b, h }
			}
		if record {
			updated = { ..state, base_ms: state.base_ms.append(times.b), head_ms: state.head_ms.append(times.h) }
			$states = $states.set(index, updated) ?? $states
		} else {
			{}
		}
	}
	Ok($states)
}

# ---- results files ----

Row : {
	os : Str,
	name : Str,
	verdict : Str,
	pairs : U64,
	ratio : F64,
	low : F64,
	high : F64,
	base_ms : F64,
	head_ms : F64,
	note : Str,
}

to_row : State, F64 -> Row
to_row = |state, alpha| {
	rs = ratios(state.base_ms, state.head_ms)
	interval = ratio_interval(rs, alpha)
	ci = interval ?? { low: 0.0, high: 0.0 }
	base_row = {
		os: "",
		name: state.bench.name,
		verdict: verdict_str(classify(interval)),
		pairs: rs.len(),
		ratio: median(rs),
		low: ci.low,
		high: ci.high,
		base_ms: median(state.base_ms),
		head_ms: median(state.head_ms),
		note: "",
	}
	match state.skipped {
		Measured => base_row
		Skipped(reason) => { ..base_row, verdict: "skipped", note: reason }
	}
}

results_text : List((Str, Str)), List(Row), List(State) -> Str
results_text = |meta, rows, states| {
	meta_lines = meta.map(|(key, value)| "meta\t${key}\t${sanitize(value)}")
	row_lines = rows.map(row_line)
	sample_lines = states.join_map(
		|s| [
			"base\t${s.bench.name}\t${s.base_ms.map(|x| x.to_str()) |> Str.join_with(",")}",
			"head\t${s.bench.name}\t${s.head_ms.map(|x| x.to_str()) |> Str.join_with(",")}",
		],
	)
	meta_lines.concat(row_lines).concat(sample_lines) |> Str.join_with("\n") |> Str.concat("\n")
}

row_line : Row -> Str
row_line = |r|
	[
		"bench",
		r.name,
		r.verdict,
		r.pairs.to_str(),
		r.ratio.to_str(),
		r.low.to_str(),
		r.high.to_str(),
		r.base_ms.to_str(),
		r.head_ms.to_str(),
		sanitize(r.note),
	]
		|> Str.join_with("\t")

Results : { phase : Str, os : Str, cpu : Str, rows : List(Row) }

parse_results : Str -> Results
parse_results = |text| {
	var $results = { phase: "", os: "", cpu: "", rows: [] }
	for line in text.split_on("\n") {
		match line.split_on("\t") {
			["meta", "phase", value] => {
				$results = { ..$results, phase: value }
			}
			["meta", "os", value] => {
				$results = { ..$results, os: value }
			}
			["meta", "cpu", value] => {
				$results = { ..$results, cpu: value }
			}
			["bench", name, verdict, pairs, ratio, low, high, base_ms, head_ms, note] => {
				row = {
					os: $results.os,
					name,
					verdict,
					pairs: U64.from_str(pairs) ?? 0,
					ratio: F64.from_str(ratio) ?? 0.0,
					low: F64.from_str(low) ?? 0.0,
					high: F64.from_str(high) ?? 0.0,
					base_ms: F64.from_str(base_ms) ?? 0.0,
					head_ms: F64.from_str(head_ms) ?? 0.0,
					note,
				}
				$results = { ..$results, rows: $results.rows.append(row) }
			}
			_ => {}
		}
	}
	$results
}

sample_row : Row
sample_row = {
	os: "linux",
	name: "all_syntax_test/cold-no-cache/check",
	verdict: "regressed",
	pairs: 100,
	ratio: 1.25,
	low: 1.125,
	high: 1.5,
	base_ms: 100.5,
	head_ms: 125.0,
	note: "",
}

expect {
	parsed = parse_results("meta\tphase\tscreen\nmeta\tos\tlinux\n${row_line(sample_row)}\n")
	parsed.phase == "screen" and parsed.rows == [sample_row]
}

rows_table : List(Row) -> Str
rows_table = |rows| {
	lines = rows.map(
		|r|
			if r.verdict == "skipped" {
				"skipped\t${r.name}: ${r.note}"
			} else {
				"${r.verdict}\t${change(r.ratio)} [${change(r.low)}, ${change(r.high)}]\t${r.pairs.to_str()} pairs\t${fixed1(r.base_ms)} -> ${fixed1(r.head_ms)} ms\t${r.name}"
			},
	)
	lines |> Str.join_with("\n")
}

github_output! : Str, Str => Try({}, _)
github_output! = |key, value|
	match Env.var_str!("GITHUB_OUTPUT") {
		Ok(file) => Path.append_utf8!(path_of(file), "${key}=${value}\n")
		Err(_) => Ok({})
	}

read_dir_results! : Path, Str => Try(List(Results), _)
read_dir_results! = |dir, prefix| {
	var $out = []
	for entry in Path.list!(dir)? {
		name = (entry.filename() ?? path_of("")).display()
		if name.starts_with(prefix) and name.ends_with(".tsv") {
			$out = $out.append(parse_results(Path.read_utf8!(entry)?))
		} else {
			{}
		}
	}
	Ok($out)
}

# ---- triage ----

## The machines whose screening flagged regressions, with the benchmarks to
## confirm there, as a GitHub Actions matrix.
triage_matrix : List(Results) -> Str
triage_matrix = |screens| {
	entries = screens.join_map(
		|s| {
			flagged = s.rows.keep_if(|r| r.verdict == "regressed").map(|r| r.name)
			if flagged.is_empty() {
				[]
			} else {
				["{\"os\":\"${s.os}\",\"benchmarks\":\"${flagged |> Str.join_with(",")}\"}"]
			}
		},
	)
	"[${entries |> Str.join_with(",")}]"
}

expect triage_matrix([]) == "[]"
expect
	triage_matrix([{ phase: "screen", os: "linux", cpu: "", rows: [sample_row, { ..sample_row, name: "x", verdict: "ok" }] }])
		== "[{\"os\":\"linux\",\"benchmarks\":\"all_syntax_test/cold-no-cache/check\"}]"

triage! : List(Str) => Try({}, _)
triage! = |args| {
	screens = read_dir_results!(path_of(option(args, "--dir")?), "screen-")?
	matrix = triage_matrix(screens)
	Stdout.line!("Machines to confirm on: ${matrix}")?
	github_output!("matrix", matrix)
}

# ---- report ----

Outcome : [Regression, NotConfirmed, Missing, Skipped, Noisy, Faster, Fine]

outcome : Row, Try(Row, [NotFound]) -> Outcome
outcome = |screen, confirm|
	if screen.verdict == "skipped" {
		Skipped
	} else if screen.verdict == "regressed" {
		match confirm {
			Ok(c) => if (c.verdict == "regressed") Regression else NotConfirmed
			Err(NotFound) => Missing
		}
	} else if screen.verdict == "inconclusive" {
		Noisy
	} else if screen.verdict == "improved" {
		Faster
	} else {
		Fine
	}

expect outcome(sample_row, Ok(sample_row)) == Regression
expect outcome(sample_row, Ok({ ..sample_row, verdict: "ok" })) == NotConfirmed
expect outcome(sample_row, Err(NotFound)) == Missing
expect outcome({ ..sample_row, verdict: "skipped" }, Err(NotFound)) == Skipped
expect outcome({ ..sample_row, verdict: "inconclusive" }, Err(NotFound)) == Noisy

outcome_label : Outcome -> Str
outcome_label = |o| match o {
	Regression => ":x: regression"
	NotConfirmed => ":warning: not confirmed"
	Missing => ":x: confirmation missing"
	Skipped => ":warning: skipped"
	Noisy => ":grey_question: inconclusive"
	Faster => ":rocket: faster"
	Fine => ":white_check_mark: ok"
}

measured_cell : Row -> Str
measured_cell = |r|
	if r.verdict == "skipped" {
		""
	} else {
		"${change(r.ratio)} [${change(r.low)}, ${change(r.high)}], ${r.pairs.to_str()} pairs"
	}

os_order : List(Str)
os_order = ["linux", "macos", "windows"]

## The markdown summary, the annotations to print and whether the check fails.
Report : { summary : Str, annotations : List(Str), failed : Bool }

build_report : List(Results), List(Results), List({ os : Str, text : Str }) -> Report
build_report = |screens, confirms, diagnostics| {
	var $annotations = []
	var $failed = Bool.False
	var $skipped_lines = []
	var $sections = []

	for os in os_order {
		match screens.find_first(|s| s.os == os) {
			Err(NotFound) => {
				$failed = Bool.True
				$annotations = $annotations.append("::error title=Perf results missing::No screening results for ${os}.")
				$sections = $sections.append("### ${os}\n\nNo screening results.\n")
			}
			Ok(screen) => {
				confirm_rows = confirms.find_first(|c| c.os == os).map_ok(|c| c.rows) ?? []
				var $lines = [
					"| benchmark | base | head | screening: change [confidence interval] | confirmation: change [confidence interval] | result |",
					"|---|---|---|---|---|---|",
				]
				var $notable = 0
				for row in screen.rows {
					confirm = confirm_rows.find_first(|c| c.name == row.name)
					o = outcome(row, confirm)
					$notable = $notable + (if (o == Fine or o == Faster) 0 else 1)
					confirm_cell =
						match confirm {
							Ok(c) => measured_cell(c)
							Err(NotFound) => ""
						}
					times = if (row.verdict == "skipped") { base: "", head: "" } else { base: "${fixed1(row.base_ms)} ms", head: "${fixed1(row.head_ms)} ms" }
					$lines = $lines.append("| `${row.name}` | ${times.base} | ${times.head} | ${measured_cell(row)} | ${confirm_cell} | ${outcome_label(o)} |")
					where_ = "${os} `${row.name}`"
					match o {
						Regression => {
							$failed = Bool.True
							c = confirm ?? row
							$annotations = $annotations.append("::error title=Perf regression::${os} ${row.name} is ${change(c.ratio)} slower (confidence interval ${change(c.low)} to ${change(c.high)}), confirmed on a second machine.")
						}
						Missing => {
							$failed = Bool.True
							$annotations = $annotations.append("::error title=Perf confirmation missing::${os} ${row.name} regressed in screening but has no confirmation results.")
						}
						NotConfirmed => {
							$annotations = $annotations.append("::warning title=Perf regression not confirmed::${os} ${row.name} looked ${change(row.ratio)} slower in screening, but a second machine did not confirm it.")
						}
						Skipped => {
							$skipped_lines = $skipped_lines.append("- ${where_} was not measured: ${row.note}")
							$annotations = $annotations.append("::warning title=Perf benchmark skipped::${os} ${row.name} was not measured: ${row.note}")
						}
						Noisy => {
							$annotations = $annotations.append("::notice title=Perf benchmark inconclusive::${os} ${row.name} was too noisy to decide after ${row.pairs.to_str()} pairs (${change(row.low)} to ${change(row.high)}).")
						}
						Faster | Fine => {}
					}
				}
				diagnostic = diagnostics.find_first(|d| d.os == os).map_ok(|d| d.text) ?? ""
				table = $lines |> Str.join_with("\n")
				title = "${os} (${screen.cpu})"
				section =
					if $notable == 0 and diagnostic.is_empty() {
						"<details><summary>${title}: all ${screen.rows.len().to_str()} benchmarks fine</summary>\n\n${table}\n\n</details>\n"
					} else {
						"### ${title}\n\n${table}\n\n${diagnostic}"
					}
				$sections = $sections.append(section)
			}
		}
	}

	status =
		if $failed {
			"## :x: Perf regression check failed"
		} else {
			"## :white_check_mark: No confirmed perf regressions"
		}
	skipped =
		if $skipped_lines.is_empty() {
			""
		} else {
			"\n### :warning: Skipped benchmarks\n\n${$skipped_lines |> Str.join_with("\n")}\n"
		}
	method =
		\\Each benchmark ran as interleaved base/head pairs on one machine. The change is the median head/base wall-time ratio,
		\\with a distribution-free confidence interval. A regression needs the whole confidence interval above +${fixed1(threshold * 100.0)}%
		\\in screening and again on a second machine.
	summary = [status, skipped, method, ""].concat($sections) |> Str.join_with("\n")
	{ summary, annotations: $annotations, failed: $failed }
}

expect {
	screen = { phase: "screen", os: "linux", cpu: "cpu", rows: [sample_row] }
	report = build_report(
		[screen, { ..screen, os: "macos" }, { ..screen, os: "windows", rows: [] }],
		[{ ..screen, phase: "confirm", rows: [{ ..sample_row, verdict: "ok" }] }, { ..screen, phase: "confirm", os: "macos" }],
		[],
	)
	report.failed and report.annotations.len() == 2
}

expect {
	screen = { phase: "screen", os: "linux", cpu: "cpu", rows: [{ ..sample_row, verdict: "skipped", note: "parse error" }] }
	report = build_report([screen, { ..screen, os: "macos" }, { ..screen, os: "windows" }], [], [])
	!report.failed and report.summary.contains("Skipped benchmarks") and report.annotations.len() == 3
}

expect {
	screen = { phase: "screen", os: "linux", cpu: "cpu", rows: [{ ..sample_row, verdict: "ok" }] }
	report = build_report([screen, { ..screen, os: "macos" }, { ..screen, os: "windows" }], [], [])
	!report.failed and report.annotations.is_empty() and report.summary.contains("<details><summary>linux (cpu): all 1 benchmarks fine")
}

report! : List(Str) => Try({}, _)
report! = |args| {
	dir = path_of(option(args, "--dir")?)
	summary_path = path_of(option(args, "--summary")?)
	screens = read_dir_results!(dir, "screen-")?
	confirms = read_dir_results!(dir, "confirm-")?
	var $diagnostics = []
	for os in os_order {
		match Path.read_utf8!(dir.join("diagnose-${os}.md")) {
			Ok(text) => {
				$diagnostics = $diagnostics.append({ os, text })
			}
			Err(_) => {}
		}
	}
	result = build_report(screens, confirms, $diagnostics)
	Path.append_utf8!(summary_path, result.summary)?
	for annotation in result.annotations {
		Stdout.line!(annotation)?
	}
	if (result.failed) Err(Exit(1)) else Ok({})
}

# ---- diagnose ----

diagnose! : List(Str) => Try({}, _)
diagnose! = |args| {
	base_roc = Path.absolute!(path_of(option(args, "--base")?))?
	head_roc = Path.absolute!(path_of(option(args, "--head")?))?
	out = path_of(option(args, "--out")?)
	selected = select(option(args, "--only")?)?
	os = os_name!()
	root = Env.create_temp_dir_with_prefix!("roc-perf-diag-")?
	has_valgrind = os == "linux" and succeeds!(Cmd.new("valgrind").arg("--version"))

	var $sections = []
	var $index = 0
	for bench in selected {
		base = setup_slot!(root, "base", $index, bench)?
		head = setup_slot!(root, "head", $index, bench)?
		_ = try_run!(base_roc, bench, base)?
		_ = try_run!(head_roc, bench, head)?
		timings = phase_timings!(bench, base_roc, base, head_roc, head, 1000 + $index * 100)?
		rss = peak_rss!(os, bench, base_roc, base, head_roc, head, 3000 + $index * 100)?
		cachegrind =
			if has_valgrind {
				cachegrind_counts!(bench, base_roc, base, head_roc, head, 2000 + $index * 100)?
			} else {
				""
			}
		$sections = $sections.append("#### Diagnostics for `${bench.name}`\n\n${timings}${rss}${cachegrind}")
		$index = $index + 1
	}
	Path.write_utf8!(out, $sections |> Str.join_with("\n"))?
	_ = Path.delete_all!(root)
	Ok({})
}

succeeds! : Cmd.Command => Bool
succeeds! = |cmd|
	match cmd.stdout(Null).stderr(Null).run!() {
		Ok(output) => output.status == Exited(0)
		Err(_) => Bool.False
	}

Timing : { phase : Str, ms : F64, top : Bool }

## `--timings` rows look like `  ✓ Type Checking   18ms, RSS 10MB - 58MB` for
## a phase and `      Parsing (summed)   0ms` for a part of one. The total
## (`roc check   50ms, peak RSS 58MB`) and phases are indented less than parts.
parse_timings : Str -> List(Timing)
parse_timings = |text|
	text.split_on("\n").join_map(
		|line| {
			indent = Str.count_utf8_bytes(line) - Str.count_utf8_bytes(line.trim_start())
			body = line.trim().drop_prefix("✓").trim()
			before_comma =
				match body.split_first(",") {
					Ok(parts) => parts.before
					Err(_) => body
				}
			words = before_comma.split_on(" ").drop_if(|w| w.is_empty())
			match words.last() {
				Ok(last) =>
					match duration_ms(last) {
						Ok(ms) => [{ phase: words.drop_last(1) |> Str.join_with(" "), ms, top: indent < 4 }]
						Err(_) => []
					}
				Err(_) => []
			}
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
			{ phase: "roc check", ms: 50.0, top: Bool.True },
			{ phase: "Type Checking", ms: 18.0, top: Bool.True },
			{ phase: "Parsing (summed)", ms: 0.5, top: Bool.False },
		]

## Median per phase of five `--timings` runs per side. Parts of phases that
## take under a millisecond on both sides are left out.
phase_timings! : Bench, Path, Slot, Path, Slot, U64 => Try(Str, _)
phase_timings! = |bench, base_roc, base, head_roc, head, first_round| {
	var $base_runs = []
	var $head_runs = []
	var $i = 0
	while $i < 5 {
		prepare!(bench, base, first_round + $i)?
		prepare!(bench, head, first_round + $i)?
		$base_runs = $base_runs.append(parse_timings(roc_command(base_roc, bench, base, ["--timings"], first_round + $i).run!()?.stderr_utf8_lossy()))
		$head_runs = $head_runs.append(parse_timings(roc_command(head_roc, bench, head, ["--timings"], first_round + $i).run!()?.stderr_utf8_lossy()))
		$i = $i + 1
	}
	phase_median = |runs, phase| median(runs.join_map(|run| run.keep_if(|t| t.phase == phase).map(|t| t.ms)))
	rows = ($head_runs.first() ?? []).join_map(
		|t| {
			b = phase_median($base_runs, t.phase)
			h = phase_median($head_runs, t.phase)
			if !t.top and b < 1.0 and h < 1.0 {
				[]
			} else {
				name = if (t.top) "**${t.phase}**" else t.phase
				ratio_text = if (b > 0.0) change(h / b) else ""
				["| ${name} | ${fixed1(b)} | ${fixed1(h)} | ${ratio_text} |"]
			}
		},
	)
	if rows.is_empty() {
		Ok("No `--timings` phase rows found.\n\n")
	} else {
		Ok(["`--timings` (median of 5 runs, ms)", "", "| phase | base | head | change |", "|---|---|---|---|"].concat(rows) |> Str.join_with("\n") |> Str.concat("\n\n"))
	}
}

## Peak resident memory from `/usr/bin/time`, median of three runs per side.
peak_rss! : Str, Bench, Path, Slot, Path, Slot, U64 => Try(Str, _)
peak_rss! = |os, bench, base_roc, base, head_roc, head, first_round| {
	time_bin = path_of("/usr/bin/time")
	if os == "windows" or !(Path.exists!(time_bin) ?? Bool.False) {
		Ok("")
	} else {
		flag = if (os == "macos") "-l" else "-v"
		measure_kb! = |roc, slot| {
			var $samples = []
			var $i = 0
			while $i < 3 {
				prepare!(bench, slot, first_round + $i)?
				output =
					Cmd.new(time_bin.to_os_str())
						.args([OsStr.from_str(flag), roc.to_os_str()].concat(command_args(bench, slot, [], first_round + $i)))
						.env("ROC_CACHE_DIR", slot.cache.to_os_str())
						.run!()?
				match max_rss_kb(os, output.stderr_utf8_lossy()) {
					Ok(kb) => {
						$samples = $samples.append(kb)
					}
					Err(_) => {}
				}
				$i = $i + 1
			}
			Ok($samples)
		}
		base_kb = measure_kb!(base_roc, base)?
		head_kb = measure_kb!(head_roc, head)?
		if base_kb.is_empty() or head_kb.is_empty() {
			Ok("")
		} else {
			b = median(base_kb) / 1024.0
			h = median(head_kb) / 1024.0
			Ok("Peak RSS: base ${fixed1(b)} MiB, head ${fixed1(h)} MiB (${change(h / b)}).\n\n")
		}
	}
}

## GNU time prints `Maximum resident set size (kbytes): N`; macOS prints
## `N  maximum resident set size` in bytes.
max_rss_kb : Str, Str -> Try(F64, [NotFound])
max_rss_kb = |os, text| {
	line = text.split_on("\n").find_first(|l| l.contains("aximum resident set size"))?
	if os == "macos" {
		bytes = line.trim().split_on(" ").first() ?? ""
		F64.from_str(bytes).map_ok(|x| x / 1024.0).map_err(|_| NotFound)
	} else {
		number = line.split_on(":").last() ?? ""
		F64.from_str(number.trim()).map_err(|_| NotFound)
	}
}

expect max_rss_kb("linux", "\tMaximum resident set size (kbytes): 2048\n") == Ok(2048.0)
expect max_rss_kb("macos", "     2097152  maximum resident set size\n") == Ok(2048.0)

## Instruction and simulated cache and branch counts from cachegrind; these
## are exact, unlike hardware counters, which shared VMs rarely expose.
cachegrind_counts! : Bench, Path, Slot, Path, Slot, U64 => Try(Str, _)
cachegrind_counts! = |bench, base_roc, base, head_roc, head, round| {
	count! = |roc, slot, log| {
		prepare!(bench, slot, round)?
		_ =
			Cmd.new("valgrind")
				.args(
					[
						OsStr.from_str("--tool=cachegrind"),
						OsStr.from_str("--cache-sim=yes"),
						OsStr.from_str("--branch-sim=yes"),
						OsStr.from_str("--cachegrind-out-file=/dev/null"),
						OsStr.from_str("--log-file=${log.display()}"),
						roc.to_os_str(),
					].concat(command_args(bench, slot, ["--jobs=1"], round)),
				)
				.env("ROC_CACHE_DIR", slot.cache.to_os_str())
				.stdout(Null)
				.stderr(Null)
				.run!()?
		Ok(cachegrind_summary(Path.read_utf8!(log) ?? ""))
	}
	b = count!(base_roc, base, base.cache.join("cachegrind.log"))?
	h = count!(head_roc, head, head.cache.join("cachegrind.log"))?
	rows = b.map(
		|entry| {
			hv = (h.find_first(|e| e.name == entry.name) ?? entry).value
			ratio_text = if (entry.value > 0.0) change(hv / entry.value) else ""
			"| ${entry.name} | ${count_str(entry.value)} | ${count_str(hv)} | ${ratio_text} |"
		},
	)
	if rows.is_empty() {
		Ok("Cachegrind produced no summary.\n\n")
	} else {
		Ok(["Cachegrind (`--jobs=1`)", "", "| counter | base | head | change |", "|---|---|---|---|"].concat(rows) |> Str.join_with("\n") |> Str.concat("\n\n"))
	}
}

## Lines like `==123== I refs:      1,234,567` or
## `==123== Mispredicts:      4,321  (  4,000 cond +  321 ind)`.
cachegrind_summary : Str -> List({ name : Str, value : F64 })
cachegrind_summary = |text| {
	wanted = ["I refs", "D refs", "I1  misses", "D1  misses", "LLi misses", "LLd misses", "LL misses", "Branches", "Mispredicts"]
	text.split_on("\n").join_map(
		|line| {
			body =
				match line.split_first("== ") {
					Ok(parts) => parts.after
					Err(_) => line
				}
			match body.split_first(":") {
				Ok(parts) => {
					name = parts.before.trim()
					if wanted.contains(name) {
						digits = (parts.after.trim().split_on(" ").first() ?? "").replace_each(",", "")
						match F64.from_str(digits) {
							Ok(value) => [{ name, value }]
							Err(_) => []
						}
					} else {
						[]
					}
				}
				Err(_) => []
			}
		},
	)
}

expect cachegrind_summary("==42== I refs:        1,234,567\n==42== Mispredicts:   4,321  (4,000 cond + 321 ind)\n") == [{ name: "I refs", value: 1234567.0 }, { name: "Mispredicts", value: 4321.0 }]

count_str : F64 -> Str
count_str = |value| {
	if value >= 1000000000.0 {
		"${fixed1(value / 1000000000.0)}G"
	} else if value >= 1000000.0 {
		"${fixed1(value / 1000000.0)}M"
	} else if value >= 1000.0 {
		"${fixed1(value / 1000.0)}k"
	} else {
		fixed1(value)
	}
}
