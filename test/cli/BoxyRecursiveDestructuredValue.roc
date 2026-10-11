# A destructured top-level callable value built by a block, recursing through
# its own destructured binder. Compile-time evaluation lowers the extraction
# under its result binding, so the stored closure captures that recursive
# binding along with `z`; a Boxy worker reaches the value through its own
# top-level reference instead of a capture slot.
BoxyRecursiveDestructuredValue :: [].{}

{ down, start } = {
	z = 0.U64
	{ down: |n| if n == z { z } else { down(n - 1) + 1 }, start: z }
}

expect down(4) == 4
expect down(0) == 0
