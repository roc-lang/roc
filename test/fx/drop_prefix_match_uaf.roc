app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Stderr
import pf.Host

main! = || {
    demo_input = Host.get_greeting!(Host.new("L000000000000000000000000"))
	match demo_input.drop_prefix("L") {
		_ => {}
	}
}
