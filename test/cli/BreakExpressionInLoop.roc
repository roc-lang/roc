app [main!] { pf: platform "../fx/platform/main.roc" }

main! = || {
	while True {
		_ = if True { break } else { 1 }
	}

	{}
}
