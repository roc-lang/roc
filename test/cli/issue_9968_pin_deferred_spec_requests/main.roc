# Regression test for https://github.com/roc-lang/roc/issues/9968
#
# An optional parameter followed by a rest parameter, built from record
# combinators: the optional builder's phantom `to_action` type argument
# (`{ get_params : {} }`) exists only in the Param module's annotation and
# threads through a shared type variable, never as a value. Lowering used to
# seal the deferred `Param.maybe_str` specialization request from value-flow
# evidence alone, defaulting the phantom row to `{}` and panicking with
# "instantiation unified a non-empty record with an empty record" when the
# callee's checked annotation contradicted the sealed request.
#
# This is also the cross-module split case: the requester (this app) and the
# requested templates (the `repro` package modules) live in different modules.
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
			"file=${file_str} files=${joined}"
		}
		IncorrectUsage(_) => "usage error"
	}

main! = || {
	Stdout.line!(describe((cli_parser.parser)(["prog", "alpha", "beta"])))
}
