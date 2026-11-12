# Operators whose requirement is decided where the operand type becomes
# known, and equality on types that are closed all the way down.

use : U64 -> U64
use = |n| n

# `$step + 1`, `$step * 3` and the comparisons are checked before anything
# says what `$step` is; `use($step)` then makes it a U64.
walk : U64 -> Str
walk = |seed| {
	var $step = 0
	$step = $step + 1
	$step = $step * 3
	small = $step < 10
	exact = $step == 3
	different = $step != 4
	total = use($step) + seed
	"${total.to_str()} ${Str.inspect(small)} ${Str.inspect(exact)} ${Str.inspect(different)}"
}

# Nothing ever says what these are, so they are Dec.
seconds = 60 * 60

# Works for any number type.
double = |n| n * 2

Point : { x : U64, y : Str }

State : [Idle, Busy(U8), Done((Str, Bool))]

same_point : Point, Point -> Bool
same_point = |a, b| a == b

other_state : State, State -> Bool
other_state = |a, b| a != b

is_idle : State -> Bool
is_idle = |state| state == Idle

main! = |args| {
	# A runtime zero, so the calls below run when the program does.
	zero = args.len() * 0
	points = "${Str.inspect(same_point({ x: zero, y: "a" }, { x: 0, y: "a" }))} ${Str.inspect(same_point({ x: zero, y: "a" }, { x: 1, y: "a" }))}"
	busy : State
	busy = Busy(zero.to_u8_wrap() + 2)
	states = "${Str.inspect(other_state(busy, Busy(2)))} ${Str.inspect(other_state(busy, Done(("x", Bool.True))))} ${Str.inspect(is_idle(busy))} ${Str.inspect(is_idle(Idle))}"
	echo!("${walk(zero + 4)} | ${seconds.to_str()} ${double(zero + 21).to_str()} ${double(1.5).to_str()} | ${points} | ${states}")
	Ok({})
}
