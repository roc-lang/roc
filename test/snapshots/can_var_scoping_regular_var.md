# META
~~~ini
description=Variable scoping with var keyword
type=snippet
~~~
# SOURCE
~~~roc
# Regular function with var usage
processItems = |items| {
	var count_ = 0
	var total_ = 0

	# Reassign vars within same function - should work
	count_ = count_ + 1
	total_ = total_ + 10

	# Nested function - var reassignment should fail across function boundary
	nestedFunc = |_| {
		count_ = count_ + 5 # Should cause error - different function
		total_ = total_ * 2 # Should cause error - different function
		count_
	}

	result = nestedFunc({})
	total_ + result
}
~~~
# EXPECTED
VAR REASSIGNMENT ERROR - :0:0:0:0
VAR REASSIGNMENT ERROR - :0:0:0:0
UNUSED VARIABLE - can_var_scoping_regular_var.md:2:17:2:22
# PROBLEMS

VAR REASSIGNMENT ERROR

Cannot reassign a `var` from outside the function where it was declared.
Variables declared with `var` can only be reassigned within the same function scope.


VAR REASSIGNMENT ERROR

Cannot reassign a `var` from outside the function where it was declared.
Variables declared with `var` can only be reassigned within the same function scope.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `items` is defined here and then never used. ───┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  processItems = |items| {                                                  │
 │                  ‾‾‾‾‾                                                     │
 └─────────────────────────────────────── can_var_scoping_regular_var.md:2:17 ┘

    If you don't need this variable, prefix it with an underscore like `_items`
    to suppress this warning.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwVar,LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "processItems"))
			(e-lambda
				(args
					(p-ident (raw "items")))
				(e-block
					(statements
						(s-var (name "count_")
							(e-int (raw "0")))
						(s-var (name "total_")
							(e-int (raw "0")))
						(s-decl
							(p-ident (raw "count_"))
							(e-binop (op "+")
								(e-ident (raw "count_"))
								(e-int (raw "1"))))
						(s-decl
							(p-ident (raw "total_"))
							(e-binop (op "+")
								(e-ident (raw "total_"))
								(e-int (raw "10"))))
						(s-decl
							(p-ident (raw "nestedFunc"))
							(e-lambda
								(args
									(p-underscore))
								(e-block
									(statements
										(s-decl
											(p-ident (raw "count_"))
											(e-binop (op "+")
												(e-ident (raw "count_"))
												(e-int (raw "5"))))
										(s-decl
											(p-ident (raw "total_"))
											(e-binop (op "*")
												(e-ident (raw "total_"))
												(e-int (raw "2"))))
										(e-ident (raw "count_"))))))
						(s-decl
							(p-ident (raw "result"))
							(e-apply
								(e-ident (raw "nestedFunc"))
								(e-record)))
						(e-binop (op "+")
							(e-ident (raw "total_"))
							(e-ident (raw "result")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "processItems"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> Error")))
	(expressions
		(expr (type "_arg -> Error"))))
~~~
