# META
~~~ini
description=Basic variable scoping behavior
type=file
~~~
# SOURCE
~~~roc
module []

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
# PROBLEMS
**DUPLICATE DEFINITION**
The name `x` is being redeclared in this scope.

The redeclaration is here:
**can_basic_scoping.md:9:5:9:6:**
```roc
    x = 20  # Should shadow top-level x
```

But `x` was already defined here:
**can_basic_scoping.md:4:1:4:2:**
```roc
x = 5
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:22),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Int(4:5-4:6),Newline(1:1-1:1),
LowerIdent(5:1-5:2),OpAssign(5:3-5:4),Int(5:5-5:7),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(7:2-7:39),
LowerIdent(8:1-8:10),OpAssign(8:11-8:12),OpBar(8:13-8:14),Underscore(8:14-8:15),OpBar(8:15-8:16),OpenCurly(8:17-8:18),Newline(1:1-1:1),
LowerIdent(9:5-9:6),OpAssign(9:7-9:8),Int(9:9-9:11),Newline(9:14-9:40),
LowerIdent(10:5-10:16),OpAssign(10:17-10:18),OpenCurly(10:19-10:20),Newline(1:1-1:1),
Newline(11:10-11:22),
LowerIdent(12:9-12:10),OpAssign(12:11-12:12),LowerIdent(12:13-12:14),OpPlus(12:15-12:16),LowerIdent(12:17-12:18),Newline(12:21-12:53),
LowerIdent(13:9-13:10),OpPlus(13:11-13:12),Int(13:13-13:14),Newline(1:1-1:1),
CloseCurly(14:5-14:6),Newline(1:1-1:1),
LowerIdent(15:5-15:16),Newline(1:1-1:1),
CloseCurly(16:1-16:2),EndOfFile(16:2-16:2),
~~~
# PARSE
~~~clojure
(file @1-1-16-2
	(module @1-1-1-10
		(exposes @1-8-1-10))
	(statements
		(s-decl @4-1-4-6
			(p-ident @4-1-4-2 (raw "x"))
			(e-int @4-5-4-6 (raw "5")))
		(s-decl @5-1-5-7
			(p-ident @5-1-5-2 (raw "y"))
			(e-int @5-5-5-7 (raw "10")))
		(s-decl @8-1-16-2
			(p-ident @8-1-8-10 (raw "outerFunc"))
			(e-lambda @8-13-16-2
				(args
					(p-underscore))
				(e-block @8-17-16-2
					(statements
						(s-decl @9-5-9-11
							(p-ident @9-5-9-6 (raw "x"))
							(e-int @9-9-9-11 (raw "20")))
						(s-decl @10-5-14-6
							(p-ident @10-5-10-16 (raw "innerResult"))
							(e-block @10-19-14-6
								(statements
									(s-decl @12-9-13-10
										(p-ident @12-9-12-10 (raw "z"))
										(e-binop @12-13-13-10 (op "+")
											(e-ident @12-13-12-14 (qaul "") (raw "x"))
											(e-ident @12-17-12-18 (qaul "") (raw "y"))))
									(e-binop @13-9-14-6 (op "+")
										(e-ident @13-9-13-10 (qaul "") (raw "z"))
										(e-int @13-13-13-14 (raw "1"))))))
						(e-ident @15-5-15-16 (qaul "") (raw "innerResult"))))))))
~~~
# FORMATTED
~~~roc
module []

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
	(d-let (id 74)
		(p-assign @4-1-4-2 (ident "x") (id 72))
		(e-int @4-5-4-6 (value "5") (id 73)))
	(d-let (id 77)
		(p-assign @5-1-5-2 (ident "y") (id 75))
		(e-int @5-5-5-7 (value "10") (id 76)))
	(d-let (id 98)
		(p-assign @8-1-8-10 (ident "outerFunc") (id 78))
		(e-lambda @8-13-16-2 (id 97)
			(args
				(p-underscore @8-14-8-15 (id 79)))
			(e-block @8-17-16-2
				(s-let @9-5-9-11
					(p-assign @9-5-9-6 (ident "x") (id 80))
					(e-int @9-9-9-11 (value "20") (id 82)))
				(s-let @10-5-14-6
					(p-assign @10-5-10-16 (ident "innerResult") (id 84))
					(e-block @10-19-14-6 (id 93)
						(s-let @12-9-13-10
							(p-assign @12-9-12-10 (ident "z") (id 85))
							(e-binop @12-13-13-10 (op "add") (id 88)
								(e-lookup-local @12-13-12-14
									(pattern (id 80)))
								(e-lookup-local @12-17-12-18
									(pattern (id 75)))))
						(e-binop @13-9-14-6 (op "add")
							(e-lookup-local @13-9-13-10
								(pattern (id 85)))
							(e-int @13-13-13-14 (value "1")))))
				(e-lookup-local @15-5-15-16
					(pattern (id 84)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "x") (type "Num(*)"))
		(def (name "y") (type "Num(*)"))
		(def (name "outerFunc") (type "*")))
	(expressions
		(expr @4-5-4-6 (type "Num(*)"))
		(expr @5-5-5-7 (type "Num(*)"))
		(expr @8-13-16-2 (type "*"))))
~~~