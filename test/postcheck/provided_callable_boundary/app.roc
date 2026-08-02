app [main!] { pf: platform "./platform/main.roc" }

import pf.Host

main! : () => {}
main! = || {
	callable = Host.make!(41)
	Host.drop!(callable)
}
