# META
~~~ini
description=Test basic for loop in block
type=expr
~~~
# SOURCE
~~~roc
{
    total = 0
    for x in [1, 2, 3] {
        total = total + x
    }
    total
}
~~~
# EXPECTED
DUPLICATE DEFINITION - can_for_loop_basic.md:4:9:4:14
INVALID ASSIGNMENT TO ITSELF - can_for_loop_basic.md:4:17:4:22
UNUSED VARIABLE - can_for_loop_basic.md:4:9:4:14
# PROBLEMS
**DUPLICATE DEFINITION**
The name `total` is being redeclared in this scope.

The redeclaration is here:
**can_for_loop_basic.md:4:9:4:14:**
```roc
        total = total + x
```
        ^^^^^

But `total` was already defined here:
**can_for_loop_basic.md:2:5:2:10:**
```roc
    total = 0
```
    ^^^^^


**INVALID ASSIGNMENT TO ITSELF**
The value `total` is assigned to itself, which would cause an infinite loop at runtime.

Only functions can reference themselves (for recursion). For non-function values, the right-hand side must be fully computable without referring to the value being assigned.

**can_for_loop_basic.md:4:17:4:22:**
```roc
        total = total + x
```
                ^^^^^


**UNUSED VARIABLE**
Variable `total` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_total` to suppress this warning.
The unused variable is declared here:
**can_for_loop_basic.md:4:9:4:14:**
```roc
        total = total + x
```
        ^^^^^


# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Int,
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
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
			(p-ident (raw "total"))
			(e-int (raw "0")))
		(s-for
			(p-ident (raw "x"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))
				(e-int (raw "3")))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "total"))
						(e-binop (op "+")
							(e-ident (raw "total"))
							(e-ident (raw "x")))))))
		(e-ident (raw "total"))))
~~~
# FORMATTED
~~~roc
{
	total = 0
	for x in [1, 2, 3] {
		total = total + x
	}
	total
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "total"))
		(e-num (value "0")))
	(s-for
		(p-assign (ident "x"))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2"))
				(e-num (value "3"))))
		(e-block
			(s-let
				(p-assign (ident "total"))
				(e-binop (op "add")
					(e-runtime-error (tag "self_referential_definition"))
					(e-lookup-local
						(p-assign (ident "x")))))
			(e-empty_record)))
	(e-lookup-local
		(p-assign (ident "total"))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
