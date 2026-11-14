Hooks := [].{
	hooks = |_| { size: 7.U64, name: "seven" }

	nested = |_| { inner: { size: 3.U64, name: "three" }, items: [{ size: 4.U64, name: "four" }, { size: 5.U64, name: "five" }] }

	step = |_| { run: |n| n + 1, label: "inc" }
}
