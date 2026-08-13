# A generalized const with an omitted defaulted field, widened at two use
# sites — including a match branch for a payload-carrying tag that is never
# constructed anywhere. The omitted default must materialize at the field's
# live graph cell like any supplied field value (design.md "Polarity":
# Monotype's third row adaptation); an early resolved-view demand used to
# panic monotype postcheck here:
#   postcheck invariant violated: resolved Monotype view requested for an
#   unresolved instantiation node
# The scrutinees are runtime-selected so the matches are not compile-time
# unconditional.
cfg : { mode : [Fast, Slow] ?? Fast }
cfg = {}

use1 : U64 -> U64
use1 = |n| {
	v = if n > 90 { mode: Turbo } else cfg
	match v.mode {
		Fast => n
		Slow => n + 1
		Turbo => n + 2
	}
}

use2 : U64 -> U64
use2 = |n| {
	v = if n > 90 { mode: Slow } else cfg
	match v.mode {
		Fast => n
		Slow => n + 1
		Other(_) => n + 3
	}
}

main! = |args| {
	v = use1(List.len(args)) + use2(List.len(args))
	if v < 100 Ok({}) else Err(Exit(1))
}
