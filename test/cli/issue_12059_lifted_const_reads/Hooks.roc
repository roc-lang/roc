Hooks := [].{
	hooks : { size : U64, name : Str }
	hooks = { size: 7, name: "seven" }

	nested : { inner : { size : U64, name : Str }, items : List({ size : U64, name : Str }) }
	nested = { inner: { size: 3, name: "three" }, items: [{ size: 4, name: "four" }, { size: 5, name: "five" }] }

	step : { run : U64 -> U64, label : Str }
	step = { run: |n| n + 1, label: "inc" }

	handler : { run : { size : U64, name : Str } -> U64, make : U64 -> { size : U64, name : Str } }
	handler = { run: |o| o.size + 1, make: |n| { size: n, name: "made" } }
}
