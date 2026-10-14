# Phantom-row matrix variant for issue 9968: the phantom `to_action` record
# row exists only in the callee's annotation, while the request's argument is
# a minted iterator. The deferred request therefore carries generated-private
# representation evidence, and its phantom row must still be pinned to the
# callee's checked root before the requester's graph seals.
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
			file: Param.maybe_str_prefixed(["x", "-"].iter().map(|s| s)),
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
			"private file=${file_str} files=${joined}"
		}
		IncorrectUsage(_) => "usage error"
	}

main! = || {
	Stdout.line!(describe((cli_parser.parser)(["prog", "alpha", "beta"])))
}
