# Repro for https://github.com/roc-lang/roc/issues/10474.
# The numeric status field is compared as a number and also interpolated into a
# Str, so checking should report a type mismatch instead of panicking later.
Send :: {}.{
	send! = |_| {
		Ok({ status: 200, body: [] })
	}
}

main! = |_| {
	response = Send.send!({})?
	if response.status == 200 {
		echo!("OK")
		Ok({})
	} else {
		echo!("HTTP status: ${response.status}")
		Err(Exit(1))
	}
}
