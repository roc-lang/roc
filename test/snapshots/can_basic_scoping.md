# META
~~~ini
description=Basic variable scoping behavior
type=snippet
~~~
# SOURCE
~~~roc
# Top-level variables
x = 5
y = 10

# Function that shadows outer variable
outerFunc = |_| {
    x = 20  # Should shadow top-level x
    innerResult = {
        # Block scope
        z = x + y  # x should resolve to 20, y to 10
        z + 1
    }
    innerResult
}
~~~
# EXPECTED
DUPLICATE DEFINITION - can_basic_scoping.md:7:5:7:6
# PROBLEMS
**DUPLICATE DEFINITION**
The name `x` is being redeclared in this scope.

The redeclaration is here:
**can_basic_scoping.md:7:5:7:6:**
```roc
    x = 20  # Should shadow top-level x
```
    ^

But `x` was already defined here:
**can_basic_scoping.md:2:1:2:2:**
```roc
x = 5
```
^


# TOKENS
~~~zig
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))
		(s-decl
			(p-ident (raw "y"))
			(e-int (raw "10")))
		(s-decl
			(p-ident (raw "outerFunc"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "x"))
							(e-int (raw "20")))
						(s-decl
							(p-ident (raw "innerResult"))
							(e-block
								(statements
									(s-decl
										(p-ident (raw "z"))
										(e-binop (op "+")
											(e-ident (raw "x"))
											(e-ident (raw "y"))))
									(e-binop (op "+")
										(e-ident (raw "z"))
										(e-int (raw "1"))))))
						(e-ident (raw "innerResult"))))))))
~~~
# FORMATTED
~~~roc
# Top-level variables
x = 5
y = 10

# Function that shadows outer variable
outerFunc = |_| {
	x = 20 # Should shadow top-level x
	innerResult = {
		# Block scope
		z = x + y # x should resolve to 20, y to 10
		z + 1
	}
	innerResult
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "y"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "outerFunc"))
		(e-closure
			(captures
				(capture (ident "y")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "x"))
						(e-num (value "20")))
					(s-let
						(p-assign (ident "innerResult"))
						(e-block
							(s-let
								(p-assign (ident "z"))
								(e-binop (op "add")
									(e-lookup-local
										(p-assign (ident "x")))
									(e-lookup-local
										(p-assign (ident "y")))))
							(e-binop (op "add")
								(e-lookup-local
									(p-assign (ident "z")))
								(e-num (value "1")))))
					(e-lookup-local
						(p-assign (ident "innerResult"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "_arg -> Num(_size)")))
	(expressions
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "_arg -> Num(_size)"))))
~~~
