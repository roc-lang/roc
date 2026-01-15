# META
~~~ini
description=Test return statement in block
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    if x > 10 then
        return 0
    else
        x
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - can_return_in_block.md:5:5:5:9
UNDEFINED VARIABLE - can_return_in_block.md:3:15:3:19
UNRECOGNIZED SYNTAX - can_return_in_block.md:5:5:5:9
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **else** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_return_in_block.md:5:5:5:9:**
```roc
    else
```
    ^^^^


**UNDEFINED VARIABLE**
Nothing is named `then` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_return_in_block.md:3:15:3:19:**
```roc
    if x > 10 then
```
              ^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**can_return_in_block.md:5:5:5:9:**
```roc
    else
```
    ^^^^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Int,
KwIf,LowerIdent,OpGreaterThan,Int,LowerIdent,
KwReturn,Int,
KwElse,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))
		(e-if-without-else
			(e-binop (op ">")
				(e-ident (raw "x"))
				(e-int (raw "10")))
			(e-ident (raw "then")))
		(s-return
			(e-int (raw "0")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-ident (raw "x"))))
~~~
# FORMATTED
~~~roc
{
	x = 5
	if x > 10 then
	return 0
	
	x
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(s-expr
		(e-if
			(if-branches
				(if-branch
					(e-binop (op "gt")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-num (value "10")))
					(e-runtime-error (tag "ident_not_in_scope"))))
			(if-else
				(e-empty_record))))
	(s-return
		(e-num (value "0")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(e-lookup-local
		(p-assign (ident "x"))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
