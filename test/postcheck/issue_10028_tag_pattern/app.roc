app [main!] { pf: platform "platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/10028
# Optimized build specialization must not panic on Draw.offset's tag match.
import pf.Draw

main! : () => Try({}, [Exit(I64), ..])
main! = || {
	_offset = Draw.offset(Draw.align_left)
	Ok({})
}
