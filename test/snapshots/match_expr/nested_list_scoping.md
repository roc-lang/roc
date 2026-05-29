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
 - :0:0:0:0
NON-EXHAUSTIVE MATCH - nested_list_scoping.md:1:1:5:2
# PROBLEMS
**MISSING METHOD**
The value before this ***** operator has a type that doesn't have a **times** method:
**nested_list_scoping.md:4:17:4:22:**
```roc
    [x, [y]] => x * y
```
                ^^^^^

The value's type, which does not have a method named **times**, is:

    List(a) where [a.minus : a, a -> a, a.plus : a, a -> a]

**Hint:** The ***** operator calls a method named **times** on the value preceding it, passing the value after the operator as the one argument.

**NON-EXHAUSTIVE MATCH**
This `match` expression doesn't cover all possible cases:
**nested_list_scoping.md:1:1:5:2:**
```roc
match nestedList {
    [[x], [y]] => x + y
    [[x, y]] => x - y
    [x, [y]] => x * y
}
```

The value being matched on has type:
        _List(Error)_

Missing patterns:
        []

Hint: Add branches to handle these cases, or use `_` to match anything.

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
			(e-lookup-local
				(p-assign (ident "nestedList"))))
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
					(e-dispatch-call (method "plus") (constraint-fn-var 43)
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
					(e-dispatch-call (method "minus") (constraint-fn-var 49)
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
(expr (type "Error"))
~~~
