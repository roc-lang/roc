# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

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
# PROBLEMS
**VAR REASSIGNMENT ERROR**
Cannot reassign a `var` from outside the function where it was declared.
Variables declared with `var` can only be reassigned within the same function scope.

**VAR REASSIGNMENT ERROR**
Cannot reassign a `var` from outside the function where it was declared.
Variables declared with `var` can only be reassigned within the same function scope.

**UNUSED VARIABLE**
Variable ``items`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_items` to suppress this warning.
The unused variable is declared here:
**can_var_scoping_regular_var.md:4:17:4:22:**
```roc
processItems = |items| {
```
                ^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:34),
LowerIdent(4:1-4:13),OpAssign(4:14-4:15),OpBar(4:16-4:17),LowerIdent(4:17-4:22),OpBar(4:22-4:23),OpenCurly(4:24-4:25),Newline(1:1-1:1),
KwVar(5:2-5:5),LowerIdent(5:6-5:12),OpAssign(5:13-5:14),Int(5:15-5:16),Newline(1:1-1:1),
KwVar(6:2-6:5),LowerIdent(6:6-6:12),OpAssign(6:13-6:14),Int(6:15-6:16),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(8:3-8:52),
LowerIdent(9:2-9:8),OpAssign(9:9-9:10),LowerIdent(9:11-9:17),OpPlus(9:18-9:19),Int(9:20-9:21),Newline(1:1-1:1),
LowerIdent(10:2-10:8),OpAssign(10:9-10:10),LowerIdent(10:11-10:17),OpPlus(10:18-10:19),Int(10:20-10:22),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(12:3-12:75),
LowerIdent(13:2-13:12),OpAssign(13:13-13:14),OpBar(13:15-13:16),Underscore(13:16-13:17),OpBar(13:17-13:18),OpenCurly(13:19-13:20),Newline(1:1-1:1),
LowerIdent(14:3-14:9),OpAssign(14:10-14:11),LowerIdent(14:12-14:18),OpPlus(14:19-14:20),Int(14:21-14:22),Newline(14:24-14:64),
LowerIdent(15:3-15:9),OpAssign(15:10-15:11),LowerIdent(15:12-15:18),OpStar(15:19-15:20),Int(15:21-15:22),Newline(15:24-15:64),
LowerIdent(16:3-16:9),Newline(1:1-1:1),
CloseCurly(17:2-17:3),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(19:2-19:8),OpAssign(19:9-19:10),LowerIdent(19:11-19:21),NoSpaceOpenRound(19:21-19:22),OpenCurly(19:22-19:23),CloseCurly(19:23-19:24),CloseRound(19:24-19:25),Newline(1:1-1:1),
LowerIdent(20:2-20:8),OpPlus(20:9-20:10),LowerIdent(20:11-20:17),Newline(1:1-1:1),
CloseCurly(21:1-21:2),EndOfFile(21:2-21:2),
~~~
# PARSE
~~~clojure
(file @1-1-21-2
	(module @1-1-1-10
		(exposes @1-8-1-10))
	(statements
		(s-decl @4-1-21-2
			(p-ident @4-1-4-13 (raw "processItems"))
			(e-lambda @4-16-21-2
				(args
					(p-ident @4-17-4-22 (raw "items")))
				(e-block @4-24-21-2
					(statements
						(s-var @5-2-6-5 (name "count_")
							(e-int @5-15-5-16 (raw "0")))
						(s-var @6-2-9-8 (name "total_")
							(e-int @6-15-6-16 (raw "0")))
						(s-decl @9-2-10-8
							(p-ident @9-2-9-8 (raw "count_"))
							(e-binop @9-11-10-8 (op "+")
								(e-ident @9-11-9-17 (qaul "") (raw "count_"))
								(e-int @9-20-9-21 (raw "1"))))
						(s-decl @10-2-13-12
							(p-ident @10-2-10-8 (raw "total_"))
							(e-binop @10-11-13-12 (op "+")
								(e-ident @10-11-10-17 (qaul "") (raw "total_"))
								(e-int @10-20-10-22 (raw "10"))))
						(s-decl @13-2-17-3
							(p-ident @13-2-13-12 (raw "nestedFunc"))
							(e-lambda @13-15-17-3
								(args
									(p-underscore))
								(e-block @13-19-17-3
									(statements
										(s-decl @14-3-15-9
											(p-ident @14-3-14-9 (raw "count_"))
											(e-binop @14-12-15-9 (op "+")
												(e-ident @14-12-14-18 (qaul "") (raw "count_"))
												(e-int @14-21-14-22 (raw "5"))))
										(s-decl @15-3-16-9
											(p-ident @15-3-15-9 (raw "total_"))
											(e-binop @15-12-16-9 (op "*")
												(e-ident @15-12-15-18 (qaul "") (raw "total_"))
												(e-int @15-21-15-22 (raw "2"))))
										(e-ident @16-3-16-9 (qaul "") (raw "count_"))))))
						(s-decl @19-2-19-25
							(p-ident @19-2-19-8 (raw "result"))
							(e-apply @19-11-19-25
								(e-ident @19-11-19-21 (qaul "") (raw "nestedFunc"))
								(e-record @19-22-19-24)))
						(e-binop @20-2-21-2 (op "+")
							(e-ident @20-2-20-8 (qaul "") (raw "total_"))
							(e-ident @20-11-20-17 (qaul "") (raw "result")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 114)
		(p-assign @4-1-4-13 (ident "processItems") (id 72))
		(e-lambda @4-16-21-2 (id 112)
			(args
				(p-assign @4-17-4-22 (ident "items") (id 73)))
			(e-block @4-24-21-2
				(s-var @5-2-6-5
					(p-assign @5-2-6-5 (ident "count_") (id 75))
					(e-int @5-15-5-16 (value "0") (id 74)))
				(s-var @6-2-9-8
					(p-assign @6-2-9-8 (ident "total_") (id 78))
					(e-int @6-15-6-16 (value "0") (id 77)))
				(s-reassign @9-2-9-8
					(p-assign @5-2-6-5 (ident "count_") (id 75))
					(e-binop @9-11-10-8 (op "add") (id 82)
						(e-lookup-local @9-11-9-17
							(pattern (id 75)))
						(e-int @9-20-9-21 (value "1"))))
				(s-reassign @10-2-10-8
					(p-assign @6-2-9-8 (ident "total_") (id 78))
					(e-binop @10-11-13-12 (op "add") (id 86)
						(e-lookup-local @10-11-10-17
							(pattern (id 78)))
						(e-int @10-20-10-22 (value "10"))))
				(s-let @13-2-17-3
					(p-assign @13-2-13-12 (ident "nestedFunc") (id 88))
					(e-lambda @13-15-17-3 (id 99)
						(args
							(p-underscore @13-16-13-17 (id 89)))
						(e-block @13-19-17-3
							(s-reassign @14-3-14-9
								(p-assign @5-2-6-5 (ident "count_") (id 75))
								(e-runtime-error (tag "var_across_function_boundary") (id 91)))
							(s-reassign @15-3-15-9
								(p-assign @6-2-9-8 (ident "total_") (id 78))
								(e-runtime-error (tag "var_across_function_boundary") (id 94)))
							(e-lookup-local @16-3-16-9
								(pattern (id 75))))))
				(s-let @19-2-19-25
					(p-assign @19-2-19-8 (ident "result") (id 101))
					(e-call @19-11-19-25 (id 105)
						(e-lookup-local @19-11-19-21
							(pattern (id 88)))
						(e-empty_record @19-22-19-24)))
				(e-binop @20-2-21-2 (op "add")
					(e-lookup-local @20-2-20-8
						(pattern (id 78)))
					(e-lookup-local @20-11-20-17
						(pattern (id 101))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "processItems") (def_var 114) (type "* ? *")))
	(expressions
		(expr @4-16-21-2 (type "* ? *"))))
~~~
