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

# Keep this an eligible data root so finalization exercises restoration of the
# erroneous callable value inside the record.
target : { generator : Gen({ value : U64 }) }
target = { generator: generator }
