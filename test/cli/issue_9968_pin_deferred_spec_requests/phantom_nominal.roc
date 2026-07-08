# Phantom-row matrix variant for issue 9968: the phantom `to_action` type
# argument is a nested nominal type (`PhantomParams := { get_params : {} }`),
# threaded through a combinator chain with a shared type variable and
# consumed only at the type level.
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
			file: Param.maybe_str_nominal({}),
			files: Param.str_list_open({}),
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
			"nominal file=${file_str} files=${joined}"
		}
		IncorrectUsage(_) => "usage error"
	}

main! = || {
	Stdout.line!(describe(cli_parser.parser(["prog", "alpha", "beta"])))
}
