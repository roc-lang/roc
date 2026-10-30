# A function read out of a record pattern and called into an annotated
# function's result widens that result exactly as one read with a field
# access does (design.md "Row Subsumption"), so a caller's match with an extra
# `C` branch is accepted. The record's field kind is a literal's still-flex
# kind or a call result's here, which the row coercion decides as a
# commitment point (design.md "Field Kinds"). A nested annotated function
# whose body's type is still a variable of the enclosing function relates to
# its annotation at the enclosing function's boundary, so a call inside that
# function widens the annotation too. `C` sorts between `A` and `B`, so a
# result numbered by the step row alone shows up as a wrong branch, on every
# backend.
DestructuredStepWidenedMatch := {}

Stepper :: { step : () -> [A, B] }.{
	by_literal : Stepper -> [A, B]
	by_literal = |s| {
		{ step } = { step: s.step }
		step()
	}

	by_call : Stepper -> [A, B]
	by_call = |s| {
		{ step } = wrap(s.step)
		step()
	}

	by_match : Stepper -> [A, B]
	by_match = |s| match { step: s.step } {
		{ step } => step()
	}

	# The captured record's kind belongs to the enclosing function, so the
	# annotated local function's binder stays at that function's rank until it
	# is bound there.
	by_capture : Stepper -> [A, B]
	by_capture = |s| {
		r = { step: s.step }
		inner : {} -> [A, B]
		inner = |_| {
			{ step } = r
			step()
		}
		inner({})
	}

	# A local function whose result is the captured function value's closed
	# row keeps that row when its call flows into the widened result.
	by_local_call : Stepper -> [A, B]
	by_local_call = |s| {
		r = { step: s.step }
		inner = |_| (r.step)()
		inner({})
	}

	# The annotated local function is called inside the enclosing function,
	# which has not yet bound the destructured binder when the body is
	# checked: the body's relation to the annotation waits for the enclosing
	# boundary, and the call instantiates the annotation as written.
	inner_destructure : Stepper -> Str
	inner_destructure = |s| {
		r = { step: s.step }
		inner : {} -> [A, B]
		inner = |_| {
			{ step } = r
			step()
		}
		match inner({}) {
			A => "a"
			B => "b"
			C => "c"
		}
	}

	inner_field : Stepper -> Str
	inner_field = |s| {
		r = { step: s.step }
		inner : {} -> [A, B]
		inner = |_| (r.step)()
		match inner({}) {
			A => "a"
			B => "b"
			C => "c"
		}
	}
}

# The body's type is the enclosing function's parameter, still a variable
# when the body is checked; the enclosing boundary relates it to the
# annotation, which closes it to `[A, B]`.
inner_param = |x| {
	inner : {} -> [A, B]
	inner = |_| x
	match inner({}) {
		A => "a"
		B => "b"
		C => "c"
	}
}

# A branch in result position relates like the body it is the value of.
inner_branch = |x, flag| {
	inner : {} -> [A, B]
	inner = |_| if flag x else A
	match inner({}) {
		A => "a"
		B => "b"
		C => "c"
	}
}


wrap = |f| { step: f }

stepper : [A, B] -> Stepper
stepper = |tag| { step: || tag }

literal : Stepper -> Str
literal = |s| match s.by_literal() {
	A => "a"
	B => "b"
	C => "c"
}

called : Stepper -> Str
called = |s| match s.by_call() {
	A => "a"
	B => "b"
	C => "c"
}

matched : Stepper -> Str
matched = |s| match s.by_match() {
	A => "a"
	B => "b"
	C => "c"
}

captured : Stepper -> Str
captured = |s| match s.by_capture() {
	A => "a"
	B => "b"
	C => "c"
}

local_call : Stepper -> Str
local_call = |s| match s.by_local_call() {
	A => "a"
	B => "b"
	C => "c"
}

expect literal(stepper(B)) == "b"
expect called(stepper(B)) == "b"
expect matched(stepper(A)) == "a"
expect captured(stepper(B)) == "b"
expect local_call(stepper(B)) == "b"
expect stepper(B).inner_destructure() == "b"
expect stepper(A).inner_field() == "a"
expect inner_param(B) == "b"
expect inner_branch(B, Bool.True) == "b"
expect inner_branch(B, Bool.False) == "a"
