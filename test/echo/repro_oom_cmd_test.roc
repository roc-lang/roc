# Reproduces the OOM:
#
#   The Roc compiler ran out of memory trying to preallocate virtual
#   address space for compiling and running this program. Try using
#   `roc build` to build the executable separately, then run it
#   manually.
#
# Mirrors the structure of basic-cli2's cmd-test.roc: an opaque nominal
# `MyCmd` type with several static-dispatch methods that each return
# `Try` with a different open tag-union error type, and a polymorphic
# `expect_err` helper that calls `Str.inspect(err)` on the result.

IOErr := [
	AlreadyExists,
	BrokenPipe,
	Interrupted,
	NotFound,
	Other(Str),
	OutOfMemory,
	PermissionDenied,
	Unsupported,
]

MyCmd :: {
	program : Str,
	args : List(Str),
}.{
	new : Str -> MyCmd
	new = |program| { program, args: [] }

	arg : MyCmd, Str -> MyCmd
	arg = |cmd, a| { ..cmd, args: cmd.args.append(a) }

	args : MyCmd, List(Str) -> MyCmd
	args = |cmd, more| { ..cmd, args: cmd.args.concat(more) }

	exec! : Str, List(Str) => Try({}, [ExecFailed({ command : Str, exit_code : I32 }), FailedToGetExitCode({ command : Str, err : IOErr }), ..])
	exec! = |program, _arguments| Err(FailedToGetExitCode({ command: program, err: IOErr.NotFound }))

	exec_cmd! : MyCmd => Try({}, [ExecCmdFailed({ command : Str, exit_code : I32 }), FailedToGetExitCode({ command : Str, err : IOErr }), ..])
	exec_cmd! = |cmd| Err(FailedToGetExitCode({ command: cmd.program, err: IOErr.NotFound }))

	exec_output! : MyCmd => Try(
		{ stdout_utf8 : Str, stderr_utf8_lossy : Str },
		[
			StdoutContainsInvalidUtf8({ cmd_str : Str, err : [BadUtf8({ problem : _, index : U64 })] }),
			NonZeroExitCode({ command : Str, exit_code : I32, stdout_utf8_lossy : Str, stderr_utf8_lossy : Str }),
			FailedToGetExitCode({ command : Str, err : IOErr }),
			..
		]
	)
	exec_output! = |cmd| Err(FailedToGetExitCode({ command: cmd.program, err: IOErr.NotFound }))

	exec_output_bytes! : MyCmd => Try(
		{ stderr_bytes : List(U8), stdout_bytes : List(U8) },
		[
			NonZeroExitCodeB({ exit_code : I32, stdout_bytes : List(U8), stderr_bytes : List(U8) }),
			FailedToGetExitCodeB(IOErr),
			..
		]
	)
	exec_output_bytes! = |_cmd| Err(FailedToGetExitCodeB(IOErr.NotFound))

	exec_exit_code! : MyCmd => Try(I32, [FailedToGetExitCode({ command : Str, err : IOErr }), ..])
	exec_exit_code! = |cmd| Err(FailedToGetExitCode({ command: cmd.program, err: IOErr.NotFound }))
}

main! = |_args| {
	match run!() {
		Ok(_) => Ok({})
		Err(_) => Ok({})
	}
}

run! = || {
	expect_err(MyCmd.exec!("blablaXYZ", []), "x")?
	expect_err(MyCmd.exec!("cat", ["a"]), "x")?
	expect_err(MyCmd.new("blablaXYZ").exec_cmd!(), "x")?
	expect_err(MyCmd.new("cat").arg("a").exec_cmd!(), "x")?
	expect_err(MyCmd.new("blablaXYZ").exec_output!(), "x")?
	expect_err(MyCmd.new("cat").arg("a").exec_output!(), "x")?
	expect_err(MyCmd.new("printf").args(["x"]).exec_output!(), "x")?
	expect_err(MyCmd.new("blablaXYZ").exec_output_bytes!(), "x")?
	expect_err(MyCmd.new("cat").arg("a").exec_output_bytes!(), "x")?
	expect_err(MyCmd.new("blablaXYZ").exec_exit_code!(), "x")?
	expect_err(MyCmd.new("cat").arg("a").exec_exit_code!(), "x")?
	expect_err(MyCmd.exec!("rm", ["x"]), "x")?

	echo!("done")

	Ok({})
}

expect_err = |err, expected_str| {
	if Str.inspect(err) == expected_str {
		Ok({})
	} else {
		Err(FailedExpectation(Str.inspect(err)))
	}
}
