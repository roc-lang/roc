# META
~~~ini
description=Nested if-then-else chain demonstrating flattening
type=file
~~~
# SOURCE
~~~roc
module [checkNumber]

checkNumber = |num| {
	if num < 0 {
		"negative"
	} else if num == 0 {
		"zero"
	} else if num > 100 {
		"large"
	} else {
		"positive"
	}
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:20),CloseSquare(1:20-1:21),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:12),OpAssign(3:13-3:14),OpBar(3:15-3:16),LowerIdent(3:16-3:19),OpBar(3:19-3:20),OpenCurly(3:21-3:22),Newline(1:1-1:1),
KwIf(4:2-4:4),LowerIdent(4:5-4:8),OpLessThan(4:9-4:10),Int(4:11-4:12),OpenCurly(4:13-4:14),Newline(1:1-1:1),
StringStart(5:3-5:4),StringPart(5:4-5:12),StringEnd(5:12-5:13),Newline(1:1-1:1),
CloseCurly(6:2-6:3),KwElse(6:4-6:8),KwIf(6:9-6:11),LowerIdent(6:12-6:15),OpEquals(6:16-6:18),Int(6:19-6:20),OpenCurly(6:21-6:22),Newline(1:1-1:1),
StringStart(7:3-7:4),StringPart(7:4-7:8),StringEnd(7:8-7:9),Newline(1:1-1:1),
CloseCurly(8:2-8:3),KwElse(8:4-8:8),KwIf(8:9-8:11),LowerIdent(8:12-8:15),OpGreaterThan(8:16-8:17),Int(8:18-8:21),OpenCurly(8:22-8:23),Newline(1:1-1:1),
StringStart(9:3-9:4),StringPart(9:4-9:9),StringEnd(9:9-9:10),Newline(1:1-1:1),
CloseCurly(10:2-10:3),KwElse(10:4-10:8),OpenCurly(10:9-10:10),Newline(1:1-1:1),
StringStart(11:3-11:4),StringPart(11:4-11:12),StringEnd(11:12-11:13),Newline(1:1-1:1),
CloseCurly(12:2-12:3),Newline(1:1-1:1),
CloseCurly(13:1-13:2),EndOfFile(13:2-13:2),
~~~
# PARSE
~~~clojure
(file @1-1-13-2
	(module @1-1-1-21
		(exposes @1-8-1-21
			(exposed-lower-ident (text "checkNumber"))))
	(statements
		(s-decl @3-1-13-2
			(p-ident @3-1-3-12 (raw "checkNumber"))
			(e-lambda @3-15-13-2
				(args
					(p-ident @3-16-3-19 (raw "num")))
				(e-block @3-21-13-2
					(statements
						(e-if-then-else @4-2-13-2
							(e-binop @4-5-4-14 (op "<")
								(e-ident @4-5-4-8 (qaul "") (raw "num"))
								(e-int @4-11-4-12 (raw "0")))
							(e-block @4-13-6-3
								(statements
									(e-string @5-3-5-13
										(e-string-part @5-4-5-12 (raw "negative")))))
							(e-if-then-else @6-9-13-2
								(e-binop @6-12-6-22 (op "==")
									(e-ident @6-12-6-15 (qaul "") (raw "num"))
									(e-int @6-19-6-20 (raw "0")))
								(e-block @6-21-8-3
									(statements
										(e-string @7-3-7-9
											(e-string-part @7-4-7-8 (raw "zero")))))
								(e-if-then-else @8-9-13-2
									(e-binop @8-12-8-23 (op ">")
										(e-ident @8-12-8-15 (qaul "") (raw "num"))
										(e-int @8-18-8-21 (raw "100")))
									(e-block @8-22-10-3
										(statements
											(e-string @9-3-9-10
												(e-string-part @9-4-9-9 (raw "large")))))
									(e-block @10-9-12-3
										(statements
											(e-string @11-3-11-13
												(e-string-part @11-4-11-12 (raw "positive"))))))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 106)
		(p-assign @3-1-3-12 (ident "checkNumber") (id 72))
		(e-lambda @3-15-13-2 (id 105)
			(args
				(p-assign @3-16-3-19 (ident "num") (id 73)))
			(e-block @3-21-13-2
				(e-if @4-2-13-2 (cond-var 0) (branch-var 0)
					(if-branches
						(if-branch
							(e-binop @4-5-4-14 (op "lt")
								(e-lookup-local @4-5-4-8
									(pattern (id 73)))
								(e-int @4-11-4-12 (num-var 76) (value "0")))
							(e-block @4-13-6-3
								(e-string @5-3-5-13
									(e-literal @5-4-5-12 (string "negative")))))
						(if-branch
							(e-binop @6-12-6-22 (op "eq")
								(e-lookup-local @6-12-6-15
									(pattern (id 73)))
								(e-int @6-19-6-20 (num-var 84) (value "0")))
							(e-block @6-21-8-3
								(e-string @7-3-7-9
									(e-literal @7-4-7-8 (string "zero")))))
						(if-branch
							(e-binop @8-12-8-23 (op "gt")
								(e-lookup-local @8-12-8-15
									(pattern (id 73)))
								(e-int @8-18-8-21 (num-var 92) (value "100")))
							(e-block @8-22-10-3
								(e-string @9-3-9-10
									(e-literal @9-4-9-9 (string "large"))))))
					(if-else
						(e-block @10-9-12-3
							(e-string @11-3-11-13
								(e-literal @11-4-11-12 (string "positive"))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "checkNumber") (type "*")))
	(expressions
		(expr @3-15-13-2 (type "*"))))
~~~