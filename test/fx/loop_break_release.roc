app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

# Repro for https://github.com/roc-lang/roc/issues/12266
# A stream left with `break` must be released at the loop exit, just like a
# stream that runs to the end, on every backend.

# A stream of 0, 1, 2, ... 9 whose state owns a host box.
# The host counts how many such boxes have been dropped ("primitive=N").
counter! : () => Stream(I64)
counter! = || {
	handle = Host.boxed_add!(1)
	Stream.custom(
		{ handle, n: 0 },
		Unknown,
		|state| if state.n < 10 { Ok((state.n, { handle: state.handle, n: state.n + 1 })) } else { Err(NoMore) },
	)
}

dropped! : () => Str
dropped! = || if Host.boxed_drop_report!().starts_with("drops primitive=1") { "released" } else { "STILL ALIVE" }

named! : Str, I64 => {}
named! = |label, stop_at| {
	Host.reset_boxed_drop_report!()
	stream = counter!()
	for! item in stream {
		if item >= stop_at {
			break
		}
	}
	Stdout.line!("named, ${label}: ${dropped!()}")
}

inline! : Str, I64 => {}
inline! = |label, stop_at| {
	Host.reset_boxed_drop_report!()
	for! item in counter!() {
		if item >= stop_at {
			break
		}
	}
	Stdout.line!("inline, ${label}: ${dropped!()}")
}

main! = || {
	named!("runs to the end", 100)
	named!("break at 3", 3)
	inline!("runs to the end", 100)
	inline!("break at 3", 3)
}
