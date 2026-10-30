app [main!] {
	pf: platform "./platform/main.roc",
	pkg: "./issue_12693_pkg/main.roc",
}

import pf.Stdout
import pkg.Form

main! = || {
	_ = Form.submit
	_ = Form.adder
	Stdout.line!("hi")
}
