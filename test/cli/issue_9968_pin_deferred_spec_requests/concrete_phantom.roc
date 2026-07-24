# Control variant for issue 9968: the shared phantom position is written
# concretely on both builders (`{ get_params : {} }` on maybe_str's
# to_action and on str_list_concrete's from_action). This never panicked
# and must keep working.
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
			file: Param.maybe_str({}),
			files: Param.str_list_concrete({}),
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
			"concrete file=${file_str} files=${joined}"
		}
		IncorrectUsage(_) => "usage error"
	}

main! = || {
	Stdout.line!(describe((cli_parser.parser)(["prog", "alpha", "beta"])))
}
