## `BoxyRowAtBool`'s re-tagging under a failing expectation: `roc test` reports
## the failure and keeps running the expectations after it.
BoxyRowAtBoolFails := [].{}

Check : { key : Str, pass : Bool }

check : Str, Str -> Check
check = |key, value| {
	failed = |text| { key: text, pass: False }
	if value == "ok" {
		{ key, pass: True }
	} else {
		failed(key)
	}
}

expect check("a", "no") == { key: "a", pass: True }
expect check("a", "no") == { key: "a", pass: False }
