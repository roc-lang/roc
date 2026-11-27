# A self-recursive function in another module whose recursive call becomes a tail call only after an unused
# binding is dropped. dev wasm32 with boxy lowering traps (a #13356 variant).
import M0

main! = |args| {
	f = M0.nt(2 + args.len() * 0, "a")
	echo!(Str.concat(f(3).to_str(), "\n"))
	Ok({})
}
