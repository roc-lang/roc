app [main!] { pf: platform "./platform/main.roc" }

main! = || {
	_result = match "abc" {
		"${_}${_}" => 1
		_ => 0
	}

	{}
}
