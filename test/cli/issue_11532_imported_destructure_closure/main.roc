import Setup

label = Setup.Cfg.label(Setup.cfg)

main! = |_args| {
	echo!(label)
	Ok({})
}
