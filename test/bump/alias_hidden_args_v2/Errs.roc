# `Base` carries its polarity marker as a hidden argument; its public API
# arity and every reference to it use its declared arity, zero.
Errs :: [].{
	Base : [Other]

	fwd : U64 -> Base
	fwd = |_| Other
}
