# A call from another module into a generic cycle of tail calls (#13340's shape, closed by #13354 on dev).
import Loop

main! = |args| {
	n = 5 + args.len() * 0
	echo!(Str.concat(Loop.ping({ a: 1, b: "r" }, n, ">"), "\n"))
	Ok({})
}
