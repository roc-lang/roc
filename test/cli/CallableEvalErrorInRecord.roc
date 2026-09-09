module [target]

Gen(a) : U64 -> a

map_fn : Gen(a), (a -> b) -> Gen(b)
map_fn = |generator, transform| |input| transform(generator(input))

generator : Gen({ value : U64 })
generator = map_fn(
	|input| input,
	|value| {
		value
	},
)

# Retain the rejected callable inside a data root. Its original diagnostic
# must suppress redundant compile-time evaluation crashes.
target : { generator : Gen({ value : U64 }) }
target = { generator: generator }
