NominalBoxPatternBinders :: [].{}

Wrapped := Box(Str)

expect {
	t = (Wrapped.(Box.box("x")), 1)
	match t {
		(Wrapped.(b), _) => Box.unbox(b) == "x"
	}
}

expect {
	r = { c: Wrapped.(Box.box("x")), n: 1 }
	match r {
		{ c: Wrapped.(b), n: _ } => Box.unbox(b) == "x"
	}
}

expect {
	l = [Wrapped.(Box.box("x")), Wrapped.(Box.box("y"))]
	match l {
		[Wrapped.(a), Wrapped.(b)] => Box.unbox(a) == "x" and Box.unbox(b) == "y"
		_ => False
	}
}
