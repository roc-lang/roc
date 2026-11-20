app [main!] {}

Output(a) :: { key : Str, encoder : a -> Str }.{
	define : Str, (a -> Str) -> Output(a)
	define = |key, encoder| { key, encoder }

	encode : Output(a), a -> Str
	encode = |output, value| (output.encoder)(value)
}

choices : Output({ first : { label : Str, value : Str }, remaining : List({ label : Str, value : Str }) })
choices = Output.define(
	"choices",
	|data| Json.to_str({
		first: { label: data.first.label, value: data.first.value },
		remaining: data.remaining.map(|item| { label: item.label, value: item.value }),
	}),
)

main! = |_args| {
	echo!(choices.encode({ first: { label: "a", value: "b" }, remaining: [{ label: "c", value: "d" }] }))
	Ok({})
}
