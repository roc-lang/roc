# META
~~~ini
description=Test if without else in expression position
type=expr
~~~
# SOURCE
~~~roc
{
    x = if Bool.true then 5
    x
}
~~~
# EXPECTED
DOES NOT EXIST - can_if_no_else_expr_error.md:2:12:2:21
UNDEFINED VARIABLE - can_if_no_else_expr_error.md:2:22:2:26
MISSING METHOD - can_if_no_else_expr_error.md:2:27:2:28
# PROBLEMS
**DOES NOT EXIST**
`Bool.true` does not exist.

`Bool` is in scope, but it has no associated `true`.

It's referenced here:
**can_if_no_else_expr_error.md:2:12:2:21:**
```roc
    x = if Bool.true then 5
```
           ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `then` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_if_no_else_expr_error.md:2:22:2:26:**
```roc
    x = if Bool.true then 5
```
                     ^^^^


**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**can_if_no_else_expr_error.md:2:27:2:28:**
```roc
    x = if Bool.true then 5
```
                          ^

The value's type, which does not have a method named**from_numeral**, is:

    {}

# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,KwIf,UpperIdent,NoSpaceDotLowerIdent,LowerIdent,Int,
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
			(e-if-without-else
				(e-ident (raw "Bool.true"))
				(e-ident (raw "then"))))
		(e-int (raw "5"))
		(e-ident (raw "x"))))
~~~
# FORMATTED
~~~roc
{
	x = if Bool.true then
	5
	x
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-if
			(if-branches
				(if-branch
					(e-runtime-error (tag "nested_value_not_found"))
					(e-runtime-error (tag "ident_not_in_scope"))))
			(if-else
				(e-empty_record))))
	(s-expr
		(e-num (value "5")))
	(e-lookup-local
		(p-assign (ident "x"))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
