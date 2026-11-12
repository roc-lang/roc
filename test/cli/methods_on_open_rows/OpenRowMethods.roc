# Functions whose records and tag unions end in a row variable, calling
# methods on the whole value through an operator and through a method
# named `foo`. Imported by the tests in this directory.

OpenRowMethods := [].{
	same = |l, r| {
		_ = l.id + r.id
		l == r
	}

	read_foo = |l, r| {
		_ = l.id + r.id
		l.foo(r)
	}

	is_red = |c| c == Red

	red_foo = |c| c.foo(Red)
}
