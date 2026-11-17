import Model

expect {
	model = { ..Model.empty, items: [{ name: "a", extra: Bool.False }] }
	model.items.len() == 1
}
