app [main!] { pf: platform "../fx/platform/main.roc" }
import pf.Stdout

wrap : U64, a -> Box(({} => a))
wrap = |_handle, value| Box.box(|{}| value)

main! : () => {}
main! = || {
	handle : U64
	handle = 900
	run : U64 -> U64
	run = |n| {
		f = Box.unbox(wrap(handle, n))
		f({})
	}
	Stdout.line!("got ${run(42).to_str()}")
}
