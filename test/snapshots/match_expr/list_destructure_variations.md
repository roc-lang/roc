# META
~~~ini
description=Match expression with various list destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match list {
    [] => 0
    [x] => x
    [first, second] => first + second
    [head, .. as tail] => head
    [One, Two, .. as rest] => 3
    [x, y, z, .. as more] => x + y + z
}
~~~
# EXPECTED
MISSING METHOD - list_destructure_variations.md:4:24:4:38
MISSING METHOD - list_destructure_variations.md:2:11:2:12
MISSING METHOD - list_destructure_variations.md:6:31:6:32
MISSING METHOD - list_destructure_variations.md:7:30:7:35
# PROBLEMS
── ✗ missing method ──────────────────────── list_destructure_variations.md:4:24

This plus method is being called on a value whose type doesn't have that method.

[first, second] => first + second
                   ^^^^^^^^^^^^^^

The value's type, which does not have a method named plus, is:

    [One, Two, ..]

── ✗ missing method ──────────────────────── list_destructure_variations.md:2:11

This from_numeral method is being called on a value whose type doesn't have
that method.

[] => 0
      ^

The value's type, which does not have a method named from_numeral, is:

    [One, Two, ..]

── ✗ missing method ──────────────────────── list_destructure_variations.md:6:31

This from_numeral method is being called on a value whose type doesn't have
that method.

[One, Two, .. as rest] => 3
                          ^

The value's type, which does not have a method named from_numeral, is:

    [One, Two, ..]

── ✗ missing method ──────────────────────── list_destructure_variations.md:7:30

This plus method is being called on a value whose type doesn't have that method.

[x, y, z, .. as more] => x + y + z
                         ^^^^^

The value's type, which does not have a method named plus, is:

    [One, Two, ..]

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,CloseSquare,OpFatArrow,Int,
OpenSquare,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
OpenSquare,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,UpperIdent,Comma,UpperIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,Int,
OpenSquare,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "list"))
	(branches
		(branch
			(p-list)
			(e-int (raw "0")))
		(branch
			(p-list
				(p-ident (raw "x")))
			(e-ident (raw "x")))
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-ident (raw "second")))
			(e-binop (op "+")
				(e-ident (raw "first"))
				(e-ident (raw "second"))))
		(branch
			(p-list
				(p-ident (raw "head"))
				(p-list-rest (name "tail")))
			(e-ident (raw "head")))
		(branch
			(p-list
				(p-tag (raw "One"))
				(p-tag (raw "Two"))
				(p-list-rest (name "rest")))
			(e-int (raw "3")))
		(branch
			(p-list
				(p-ident (raw "x"))
				(p-ident (raw "y"))
				(p-ident (raw "z"))
				(p-list-rest (name "more")))
			(e-binop (op "+")
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-ident (raw "y")))
				(e-ident (raw "z"))))))
~~~
# FORMATTED
~~~roc
match list {
	[] => 0
	[x] => x
	[first, second] => first + second
	[head, .. as tail] => head
	[One, Two, .. as rest] => 3
	[x, y, z, .. as more] => x + y + z
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
							(patterns))))
				(value
					(e-runtime-error (tag "erroneous_value_expr"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))))))
				(value
					(e-lookup-local
						(p-assign (ident "x")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first"))
								(p-assign (ident "second"))))))
				(value
					(e-runtime-error (tag "erroneous_value_expr"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "head")))
							(rest-at (index 1)
								(p-assign (ident "tail"))))))
				(value
					(e-lookup-local
						(p-assign (ident "head")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-applied-tag)
								(p-applied-tag))
							(rest-at (index 2)
								(p-assign (ident "rest"))))))
				(value
					(e-runtime-error (tag "erroneous_value_expr"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))
								(p-assign (ident "y"))
								(p-assign (ident "z")))
							(rest-at (index 3)
								(p-assign (ident "more"))))))
				(value
					(e-runtime-error (tag "erroneous_value_expr")))))))
~~~
# TYPES
~~~clojure
(expr (type "[One, Two, ..]"))
~~~
