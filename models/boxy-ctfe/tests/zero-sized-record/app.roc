# #13423: a zero-sized record with a field, inside a literal a function body keeps (a value-form expression), panics
# static data materialization under --specialize=no ("static initializer ZST struct had runtime fields").
# known.txt lists the configurations that fail while it is open.
show_rt = |z, v| if z != 0 {
	crash "rt"
} else {
	Str.inspect(v)
}

f : U64 -> List({ a : {} })
f = |_n| [{ a: {} }]

main! = |args| {
	z = args.len() * 0
	echo!(Str.concat(Str.concat("p0 ct: ", Str.inspect(f(0))), "\n"))
	echo!(Str.concat(Str.concat("p0 rt: ", show_rt(z, f(z))), "\n"))
	Ok({})
}
