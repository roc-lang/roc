## State the host holds between calls without reading it. Its fields are
## functions that are not each boxed; the one `Box` around the whole state is
## what crosses the host boundary. The app's model is boxed because glue
## cannot lay out a type variable held by value.
Counter(model) := {
	model : Box(model),
	update : model, I64 -> model,
	read : model -> I64,
	scale : I64 -> I64,
}.{
	new : model, (model, I64 -> model), (model -> I64), (I64 -> I64) -> Counter(model)
	new = |model, update, read, scale| { model: Box.box(model), update, read, scale }

	step : Counter(model), I64 -> Counter(model)
	step = |counter, event| {
		model = (counter.update)(Box.unbox(counter.model), (counter.scale)(event))
		{ ..counter, model: Box.box(model) }
	}

	total : Counter(model) -> I64
	total = |counter| (counter.read)(Box.unbox(counter.model))
}
