import Files exposing [Files]

Io := {}.{
	new : {} -> Io
	new = |_| {}

	files : Io -> Files
	files = |_io| Files.new({})
}
