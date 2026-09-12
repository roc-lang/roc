import Host

Files := {}.{
	new : {} -> Files
	new = |_| {}

	read! : Files, Str => Str
	read! = |_files, path|
		match Host.read_wait!(path) {
			Ok(contents) => contents
			Err(ReadFailed(_)) => ""
		}
}
