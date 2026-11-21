Point := { marker : Str ?? "circle" }

marker = || {
	scatter : { point : Point }
	scatter = { point: Ok({})? }
	Ok(scatter.point.marker)
}

actual = marker() ?? "error"
expect actual == "circle"
