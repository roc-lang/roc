# Regression test for #12191: on AArch64 `apply_scalar` takes stack-passed
# arguments, and a value its caller reloaded right after calling it was read
# from the wrong stack slot.
app [main!] {
	pf: platform "./platform/main.roc",
	pkg: "./tailcc_stack_args_pkg/main.roc",
}

import pf.Stdin
import pf.Stdout
import pkg.Case

main! = || {
	Stdout.line!(
		match run_case(Stdin.line!()) {
			Ok({}) => "PASS"
			Err(_) => "FAIL"
		},
	)
}

run_case : Str -> Try({}, Str)
run_case = |line| {
	match line.split_on(" ") {
		[_, _, profile, source_text, ..] => {
			match invoke(profile, source_text) {
				Err(message) => Err(message)
				Ok(_result) => Ok({})
			}
		}
		_ => Err("malformed case row")
	}
}

invoke : Str, Str -> Try(Case.Result, Str)
invoke = |profile, source| {
	match mapping_profile(profile) {
		Err(message) => Err(message)
		Ok(value) => Case.to_lower(source, value, Case.unlimited_limits) |> Try.map_err(|_| "case err")
	}
}

mapping_profile : Str -> Try(Case.MappingProfile, Str)
mapping_profile = |profile| match profile {
	"default" => Ok(Case.unicode_default)
	_ => Err("unknown mapping profile")
}
