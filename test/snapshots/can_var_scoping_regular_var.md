# META
~~~ini
description=Variable scoping with var keyword
type=snippet
~~~
# SOURCE
~~~roc
# Regular function with var usage
processItems = |items| {
	var $count = 0
	var $total = 0

	# Reassign vars within same function - should work
	$count = $count + 1
	$total = $total + 10

	# Nested function - var reassignment should fail across function boundary
	nestedFunc = |_| {
		$count = $count + 5 # Should cause error - different function
		$total = $total * 2 # Should cause error - different function
		$count
	}

	result = nestedFunc({})
	$total + result
}
~~~
# EXPECTED
VAR REASSIGNMENT ERROR - :0:0:0:0
VAR REASSIGNMENT ERROR - :0:0:0:0
UNUSED VARIABLE - can_var_scoping_regular_var.md:2:17:2:22
# PROBLEMS
**VAR REASSIGNMENT ERROR**
Cannot reassign a `var` from outside the function where it was declared.
Variables declared with `var` can only be reassigned within the same function scope.

**VAR REASSIGNMENT ERROR**
Cannot reassign a `var` from outside the function where it was declared.
Variables declared with `var` can only be reassigned within the same function scope.

**UNUSED VARIABLE**
Variable `items` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_items` to suppress this warning.
The unused variable is declared here:
**can_var_scoping_regular_var.md:2:17:2:22:**
```roc
processItems = |items| {
```
                ^^^^^


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
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "processItems"))
			(e-lambda
				(args
					(p-ident (raw "items")))
				(e-block
					(statements
						(s-var (name "$count")
							(e-int (raw "0")))
						(s-var (name "$total")
							(e-int (raw "0")))
						(s-decl
							(p-ident (raw "$count"))
							(e-binop (op "+")
								(e-ident (raw "$count"))
								(e-int (raw "1"))))
						(s-decl
							(p-ident (raw "$total"))
							(e-binop (op "+")
								(e-ident (raw "$total"))
								(e-int (raw "10"))))
						(s-decl
							(p-ident (raw "nestedFunc"))
							(e-lambda
								(args
									(p-underscore))
								(e-block
									(statements
										(s-decl
											(p-ident (raw "$count"))
											(e-binop (op "+")
												(e-ident (raw "$count"))
												(e-int (raw "5"))))
										(s-decl
											(p-ident (raw "$total"))
											(e-binop (op "*")
												(e-ident (raw "$total"))
												(e-int (raw "2"))))
										(e-ident (raw "$count"))))))
						(s-decl
							(p-ident (raw "result"))
							(e-apply
								(e-ident (raw "nestedFunc"))
								(e-record)))
						(e-binop (op "+")
							(e-ident (raw "$total"))
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
		(e-lambda
			(args
				(p-assign (ident "items")))
			(e-block
				(s-var
					(p-assign (ident "$count"))
					(e-num (value "0")))
				(s-var
					(p-assign (ident "$total"))
					(e-num (value "0")))
				(s-reassign
					(p-assign (ident "$count"))
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "$count")))
						(e-num (value "1"))))
				(s-reassign
					(p-assign (ident "$total"))
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "$total")))
						(e-num (value "10"))))
				(s-let
					(p-assign (ident "nestedFunc"))
					(e-closure
						(captures
							(capture (ident "$count")))
						(e-lambda
							(args
								(p-underscore))
							(e-block
								(s-reassign
									(p-assign (ident "$count"))
									(e-runtime-error (tag "var_across_function_boundary")))
								(s-reassign
									(p-assign (ident "$total"))
									(e-runtime-error (tag "var_across_function_boundary")))
								(e-lookup-local
									(p-assign (ident "$count")))))))
				(s-let
					(p-assign (ident "result"))
					(e-call
						(e-lookup-local
							(p-assign (ident "nestedFunc")))
						(e-empty_record)))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "$total")))
					(e-lookup-local
						(p-assign (ident "result"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> Error")))
	(expressions
		(expr (type "_arg -> Error"))))
~~~
