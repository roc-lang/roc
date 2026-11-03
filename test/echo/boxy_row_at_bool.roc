# A local function's constructed tag read at `Bool` in a running program
# (`test/cli/BoxyRowAtBool.roc` covers `roc test`).
Check : { key : Str, pass : Bool }

check : Str, Str -> Check
check = |key, value| {
	failed = |text| { key: text, pass: False }
	passed = |text| { key: text, pass: True }
	if value == "ok" passed(key) else failed(key)
}

main! = |_args| {
	echo!(Str.inspect(check("first", "ok")))
	echo!(Str.inspect(check("second", "no")))
	Ok({})
}
