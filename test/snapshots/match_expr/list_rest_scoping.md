# META
~~~ini
description=Match expression with list rest patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, ..rest] => first + 1
    [..rest, last] => last + 2
    [x, ..rest, y] => x + y
}
~~~
# EXPECTED
BAD LIST REST PATTERN SYNTAX - list_rest_scoping.md:2:13:2:19
BAD LIST REST PATTERN SYNTAX - list_rest_scoping.md:3:6:3:12
BAD LIST REST PATTERN SYNTAX - list_rest_scoping.md:4:9:4:15
UNUSED VARIABLE - list_rest_scoping.md:2:15:2:15
UNUSED VARIABLE - list_rest_scoping.md:3:8:3:8
UNUSED VARIABLE - list_rest_scoping.md:4:11:4:11
NON-EXHAUSTIVE MATCH - list_rest_scoping.md:1:1:5:2
REDUNDANT PATTERN - list_rest_scoping.md:1:1:5:2
REDUNDANT PATTERN - list_rest_scoping.md:1:1:5:2
# PROBLEMS
**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

**list_rest_scoping.md:2:13:2:19:**
```roc
    [first, ..rest] => first + 1
```
            ^^^^^^


**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

**list_rest_scoping.md:3:6:3:12:**
```roc
    [..rest, last] => last + 2
```
     ^^^^^^


**BAD LIST REST PATTERN SYNTAX**
List rest patterns should use the `.. as name` syntax, not `..name`.
For example, use `[first, .. as rest]` instead of `[first, ..rest]`.

**list_rest_scoping.md:4:9:4:15:**
```roc
    [x, ..rest, y] => x + y
```
        ^^^^^^


**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_rest_scoping.md:2:15:2:15:**
```roc
    [first, ..rest] => first + 1
```
              ^


**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_rest_scoping.md:3:8:3:8:**
```roc
    [..rest, last] => last + 2
```
       ^


**UNUSED VARIABLE**
Variable `rest` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_rest` to suppress this warning.
The unused variable is declared here:
**list_rest_scoping.md:4:11:4:11:**
```roc
    [x, ..rest, y] => x + y
```
          ^


**NON-EXHAUSTIVE MATCH**
This `match` expression doesn't cover all possible cases:
**list_rest_scoping.md:1:1:5:2:**
```roc
match items {
    [first, ..rest] => first + 1
    [..rest, last] => last + 2
    [x, ..rest, y] => x + y
}
```

The value being matched on has type:
        _List(a) where [a.plus : a, a -> a]_

Missing patterns:
        []

Hint: Add branches to handle these cases, or use `_` to match anything.

**REDUNDANT PATTERN**
The second branch of this `match` is redundant:
**list_rest_scoping.md:1:1:5:2:**
```roc
match items {
    [first, ..rest] => first + 1
    [..rest, last] => last + 2
    [x, ..rest, y] => x + y
}
```

This pattern can never match because earlier patterns already cover all the values it would match.

**REDUNDANT PATTERN**
The third branch of this `match` is redundant:
**list_rest_scoping.md:1:1:5:2:**
```roc
match items {
    [first, ..rest] => first + 1
    [..rest, last] => last + 2
    [x, ..rest, y] => x + y
}
```

This pattern can never match because earlier patterns already cover all the values it would match.

# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,Int,
OpenSquare,DoubleDot,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,Int,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "items"))
	(branches
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-list-rest (name "rest")))
			(e-binop (op "+")
				(e-ident (raw "first"))
				(e-int (raw "1"))))
		(branch
			(p-list
				(p-list-rest (name "rest"))
				(p-ident (raw "last")))
			(e-binop (op "+")
				(e-ident (raw "last"))
				(e-int (raw "2"))))
		(branch
			(p-list
				(p-ident (raw "x"))
				(p-list-rest (name "rest"))
				(p-ident (raw "y")))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))))
~~~
# FORMATTED
~~~roc
match items {
	[first, .. as rest] => first + 1
	[.. as rest, last] => last + 2
	[x, .. as rest, y] => x + y
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-lookup-local
				(p-assign (ident "items"))))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first")))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "first")))
						(e-num (value "1")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "last")))
							(rest-at (index 0)
								(p-assign (ident "rest"))))))
				(value
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "last")))
						(e-num (value "2")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))
								(p-assign (ident "y")))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y")))))))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.plus : a, a -> a]"))
~~~
