## State the host holds between calls without reading it. Its fields are
## functions that are not each boxed; the one `Box` around the whole state is
## what crosses the host boundary.
Counter(model) := {
	model : model,
	update : model, I64 -> model,
	read : model -> I64,
	scale : I64 -> I64,
}.{
	new : model, (model, I64 -> model), (model -> I64), (I64 -> I64) -> Counter(model)
	new = |model, update, read, scale| { model, update, read, scale }

	step : Counter(model), I64 -> Counter(model)
	step = |counter, event| { ..counter, model: (counter.update)(counter.model, (counter.scale)(event)) }

	total : Counter(model) -> I64
	total = |counter| (counter.read)(counter.model)
}
