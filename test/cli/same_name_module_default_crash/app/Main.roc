app [main!] { pf: platform "../platform/main.roc" }

import Cfg

main! = || {
	_ = Cfg.remote_cfg
	Ok({})
}
