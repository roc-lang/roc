# META
~~~ini
description=Variable scoping with var keyword
type=file
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
MISSING MAIN! FUNCTION - can_var_scoping_regular_var.md:2:1:19:2
VAR REASSIGNMENT ERROR - :0:0:0:0
VAR REASSIGNMENT ERROR - :0:0:0:0
UNUSED VARIABLE - can_var_scoping_regular_var.md:2:17:2:22
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**can_var_scoping_regular_var.md:2:1:19:2:**
```roc
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
```


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
LowerIdent(2:1-2:13),OpAssign(2:14-2:15),OpBar(2:16-2:17),LowerIdent(2:17-2:22),OpBar(2:22-2:23),OpenCurly(2:24-2:25),
KwVar(3:2-3:5),LowerIdent(3:6-3:12),OpAssign(3:13-3:14),Int(3:15-3:16),
KwVar(4:2-4:5),LowerIdent(4:6-4:12),OpAssign(4:13-4:14),Int(4:15-4:16),
LowerIdent(7:2-7:8),OpAssign(7:9-7:10),LowerIdent(7:11-7:17),OpPlus(7:18-7:19),Int(7:20-7:21),
LowerIdent(8:2-8:8),OpAssign(8:9-8:10),LowerIdent(8:11-8:17),OpPlus(8:18-8:19),Int(8:20-8:22),
LowerIdent(11:2-11:12),OpAssign(11:13-11:14),OpBar(11:15-11:16),Underscore(11:16-11:17),OpBar(11:17-11:18),OpenCurly(11:19-11:20),
LowerIdent(12:3-12:9),OpAssign(12:10-12:11),LowerIdent(12:12-12:18),OpPlus(12:19-12:20),Int(12:21-12:22),
LowerIdent(13:3-13:9),OpAssign(13:10-13:11),LowerIdent(13:12-13:18),OpStar(13:19-13:20),Int(13:21-13:22),
LowerIdent(14:3-14:9),
CloseCurly(15:2-15:3),
LowerIdent(17:2-17:8),OpAssign(17:9-17:10),LowerIdent(17:11-17:21),NoSpaceOpenRound(17:21-17:22),OpenCurly(17:22-17:23),CloseCurly(17:23-17:24),CloseRound(17:24-17:25),
LowerIdent(18:2-18:8),OpPlus(18:9-18:10),LowerIdent(18:11-18:17),
CloseCurly(19:1-19:2),
EndOfFile(20:1-20:1),
~~~
# PARSE
~~~clojure
(file @2.1-19.2
	(type-module @2.1-2.13)
	(statements
		(s-decl @2.1-19.2
			(p-ident @2.1-2.13 (raw "processItems"))
			(e-lambda @2.16-19.2
				(args
					(p-ident @2.17-2.22 (raw "items")))
				(e-block @2.24-19.2
					(statements
						(s-var @3.2-3.16 (name "count_")
							(e-int @3.15-3.16 (raw "0")))
						(s-var @4.2-4.16 (name "total_")
							(e-int @4.15-4.16 (raw "0")))
						(s-decl @7.2-7.21
							(p-ident @7.2-7.8 (raw "count_"))
							(e-binop @7.11-7.21 (op "+")
								(e-ident @7.11-7.17 (raw "count_"))
								(e-int @7.20-7.21 (raw "1"))))
						(s-decl @8.2-8.22
							(p-ident @8.2-8.8 (raw "total_"))
							(e-binop @8.11-8.22 (op "+")
								(e-ident @8.11-8.17 (raw "total_"))
								(e-int @8.20-8.22 (raw "10"))))
						(s-decl @11.2-15.3
							(p-ident @11.2-11.12 (raw "nestedFunc"))
							(e-lambda @11.15-15.3
								(args
									(p-underscore))
								(e-block @11.19-15.3
									(statements
										(s-decl @12.3-12.22
											(p-ident @12.3-12.9 (raw "count_"))
											(e-binop @12.12-12.22 (op "+")
												(e-ident @12.12-12.18 (raw "count_"))
												(e-int @12.21-12.22 (raw "5"))))
										(s-decl @13.3-13.22
											(p-ident @13.3-13.9 (raw "total_"))
											(e-binop @13.12-13.22 (op "*")
												(e-ident @13.12-13.18 (raw "total_"))
												(e-int @13.21-13.22 (raw "2"))))
										(e-ident @14.3-14.9 (raw "count_"))))))
						(s-decl @17.2-17.25
							(p-ident @17.2-17.8 (raw "result"))
							(e-apply @17.11-17.25
								(e-ident @17.11-17.21 (raw "nestedFunc"))
								(e-record @17.22-17.24)))
						(e-binop @18.2-18.17 (op "+")
							(e-ident @18.2-18.8 (raw "total_"))
							(e-ident @18.11-18.17 (raw "result")))))))))
~~~
# FORMATTED
~~~roc
# Regular function with var usage
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
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.13 (ident "processItems"))
		(e-lambda @2.16-19.2
			(args
				(p-assign @2.17-2.22 (ident "items")))
			(e-block @2.24-19.2
				(s-var @3.2-3.16
					(p-assign @3.2-3.16 (ident "count_"))
					(e-int @3.15-3.16 (value "0")))
				(s-var @4.2-4.16
					(p-assign @4.2-4.16 (ident "total_"))
					(e-int @4.15-4.16 (value "0")))
				(s-reassign @7.2-7.8
					(p-assign @3.2-3.16 (ident "count_"))
					(e-binop @7.11-7.21 (op "add")
						(e-lookup-local @7.11-7.17
							(p-assign @3.2-3.16 (ident "count_")))
						(e-int @7.20-7.21 (value "1"))))
				(s-reassign @8.2-8.8
					(p-assign @4.2-4.16 (ident "total_"))
					(e-binop @8.11-8.22 (op "add")
						(e-lookup-local @8.11-8.17
							(p-assign @4.2-4.16 (ident "total_")))
						(e-int @8.20-8.22 (value "10"))))
				(s-let @11.2-15.3
					(p-assign @11.2-11.12 (ident "nestedFunc"))
					(e-closure @11.15-15.3
						(captures
							(capture @3.2-3.16 (ident "count_")))
						(e-lambda @11.15-15.3
							(args
								(p-underscore @11.16-11.17))
							(e-block @11.19-15.3
								(s-reassign @12.3-12.9
									(p-assign @3.2-3.16 (ident "count_"))
									(e-runtime-error (tag "var_across_function_boundary")))
								(s-reassign @13.3-13.9
									(p-assign @4.2-4.16 (ident "total_"))
									(e-runtime-error (tag "var_across_function_boundary")))
								(e-lookup-local @14.3-14.9
									(p-assign @3.2-3.16 (ident "count_")))))))
				(s-let @17.2-17.25
					(p-assign @17.2-17.8 (ident "result"))
					(e-call @17.11-17.25
						(e-lookup-local @17.11-17.21
							(p-assign @11.2-11.12 (ident "nestedFunc")))
						(e-empty_record @17.22-17.24)))
				(e-binop @18.2-18.17 (op "add")
					(e-lookup-local @18.2-18.8
						(p-assign @4.2-4.16 (ident "total_")))
					(e-lookup-local @18.11-18.17
						(p-assign @17.2-17.8 (ident "result"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.13 (type "_arg -> Num(_size)")))
	(expressions
		(expr @2.16-19.2 (type "_arg -> Num(_size)"))))
~~~
