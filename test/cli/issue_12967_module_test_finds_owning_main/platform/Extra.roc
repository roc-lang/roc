# Counter.roc does not import this module, so testing Counter.roc neither
# checks it nor runs this failing expect.
Extra := [].{
	suffix : Str
	suffix = "!"
}

expect Extra.suffix == "?"
