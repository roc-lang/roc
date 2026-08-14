# META
~~~ini
description=Match expression with nested list patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match nestedList {
    [[x], [y]] => x + y
    [[x, y]] => x - y
    [x, [y]] => x * y
}
~~~
# EXPECTED
MISSING METHOD - nested_list_scoping.md:4:17:4:22
POLYMORPHIC VALUE - nested_list_scoping.md:1:1:5:2
# PROBLEMS
── ✗ missing method ──────────────────────────────── nested_list_scoping.md:4:17

The value before this * operator has a type that doesn't have a times method.

[x, [y]] => x * y
            ^^^^^

The value's type, which does not have a method named times, is:

    List(a) where [a.minus : a, a -> a, a.plus : a, a -> a]

Hint: The * operator calls a method named times on the value preceding it,
passing the value after the operator as the one argument.

── ✗ polymorphic value ────────────────────────────── nested_list_scoping.md:1:1

This top-level value still has an unresolved polymorphic type.

match nestedList {
    [[x], [y]] => x + y
    [[x, y]] => x - y
    [x, [y]] => x * y
}

Its type is:
a where [a.minus : a, a -> a, a.plus : a, a -> a]
Add an annotation or use this value in a way that fixes its concrete type.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,OpenSquare,LowerIdent,CloseSquare,Comma,OpenSquare,LowerIdent,CloseSquare,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
OpenSquare,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,CloseSquare,OpFatArrow,LowerIdent,OpBinaryMinus,LowerIdent,
OpenSquare,LowerIdent,Comma,OpenSquare,LowerIdent,CloseSquare,CloseSquare,OpFatArrow,LowerIdent,OpStar,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "nestedList"))
	(branches
		(branch
			(p-list
				(p-list
					(p-ident (raw "x")))
				(p-list
					(p-ident (raw "y"))))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))
		(branch
			(p-list
				(p-list
					(p-ident (raw "x"))
					(p-ident (raw "y"))))
			(e-binop (op "-")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))
		(branch
			(p-list
				(p-ident (raw "x"))
				(p-list
					(p-ident (raw "y"))))
			(e-binop (op "*")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))))
~~~
# FORMATTED
~~~roc
match nestedList {
	[[x], [y]] => x + y
	[[x, y]] => x - y
	[x, [y]] => x * y
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-list
									(patterns
										(p-assign (ident "x"))))
								(p-list
									(patterns
										(p-assign (ident "y"))))))))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 232)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-lookup-local
								(p-assign (ident "y")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-list
									(patterns
										(p-assign (ident "x"))
										(p-assign (ident "y"))))))))
				(value
					(e-dispatch-call (method "minus") (constraint-fn-var 234)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-lookup-local
								(p-assign (ident "y")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))
								(p-list
									(patterns
										(p-assign (ident "y"))))))))
				(value
					(e-binop (op "mul")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y")))))))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.minus : a, a -> a, a.plus : a, a -> a]"))
~~~
