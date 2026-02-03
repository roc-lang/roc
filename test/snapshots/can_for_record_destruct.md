# META
~~~ini
description=Test for loop with record destructure pattern
type=expr
~~~
# SOURCE
~~~roc
{
    result = 0
    for { x, y } in [{ x: 1, y: 2 }] {
        result
    }
    result
}
~~~
# EXPECTED
UNUSED VARIABLE - can_for_record_destruct.md:3:11:3:12
UNUSED VARIABLE - can_for_record_destruct.md:3:14:3:15
MISSING METHOD - can_for_record_destruct.md:2:14:2:15
# PROBLEMS
**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**can_for_record_destruct.md:3:11:3:12:**
```roc
    for { x, y } in [{ x: 1, y: 2 }] {
```
          ^


**UNUSED VARIABLE**
Variable `y` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:
**can_for_record_destruct.md:3:14:3:15:**
```roc
    for { x, y } in [{ x: 1, y: 2 }] {
```
             ^


**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**can_for_record_destruct.md:2:14:2:15:**
```roc
    result = 0
```
             ^

The value's type, which does not have a method named**from_numeral**, is:

    {}

# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Int,
KwFor,OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,KwIn,OpenSquare,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseSquare,OpenCurly,
LowerIdent,
CloseCurly,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "result"))
			(e-int (raw "0")))
		(s-for
			(p-record
				(field (name "x") (rest false))
				(field (name "y") (rest false)))
			(e-list
				(e-record
					(field (field "x")
						(e-int (raw "1")))
					(field (field "y")
						(e-int (raw "2")))))
			(e-block
				(statements
					(e-ident (raw "result")))))
		(e-ident (raw "result"))))
~~~
# FORMATTED
~~~roc
{
	result = 0
	for { x, y } in [{ x: 1, y: 2 }] {
		result
	}
	result
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "result"))
		(e-num (value "0")))
	(s-for
		(p-record-destructure
			(destructs
				(record-destruct (label "x") (ident "x")
					(required
						(p-assign (ident "x"))))
				(record-destruct (label "y") (ident "y")
					(required
						(p-assign (ident "y"))))))
		(e-list
			(elems
				(e-record
					(fields
						(field (name "x")
							(e-num (value "1")))
						(field (name "y")
							(e-num (value "2")))))))
		(e-block
			(e-lookup-local
				(p-assign (ident "result")))))
	(e-lookup-local
		(p-assign (ident "result"))))
~~~
# TYPES
~~~clojure
(expr (type "{}"))
~~~
