# A call from another module into a cycle of tail calls (#13356's shape).
import Parity

main! = |args| {
	n = 81 + args.len() * 0
	echo!(Str.concat(Parity.even(n, 0).to_str(), "\n"))
	Ok({})
}
