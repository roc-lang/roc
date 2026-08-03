# Control variant for issue 9968: the optional builder's value argument is
# I64 instead of {} while the phantom `to_action` record row stays the same.
# Guards against positional/slot-pairing regressions in specialization
# request pinning.
app [main!] {
	pf: platform "../../fx/platform/main.roc",
	repro: "./cli_repro_pkg/main.roc",
}

import pf.Stdout
import repro.Cli
import repro.Param

cli_parser =
	Cli.finish(
		{
			file: Param.maybe_str_i64(1),
			files: Param.str_list({}),
		}.Cli,
	)

describe = |result|
	match result {
		SuccessfullyParsed({ file, files }) => {
			file_str = match file {
				Ok(f) => f
				Err(NoValue) => "<none>"
			}
			joined = Str.join_with(files, ",")
			"i64 file=${file_str} files=${joined}"
		}
		IncorrectUsage(_) => "usage error"
	}

main! = || {
	Stdout.line!(describe((cli_parser.parser)(["prog", "alpha", "beta"])))
}
