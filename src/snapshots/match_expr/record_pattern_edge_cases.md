# META
~~~ini
description=Edge cases for nested record patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
    { a: { b: { c } } } => "deeply nested: ${c}"
    { x, y: {} } => "mixed with empty: ${x}"
    { outer: { inner }, simple } => "mixed: ${inner} and ${simple}"
    { a: { b }, c: { d } } => "multiple nested: ${b}, ${d}"
    { name: x } => "renamed: ${x}"
    { person: { name: firstName, age: userAge } } => "renamed nested: ${firstName} (${userAge.to_str()})"
    {} => "empty record"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch(1:1-1:6),TripleDot(1:7-1:10),OpenCurly(1:11-1:12),Newline(1:1-1:1),
OpenCurly(2:5-2:6),LowerIdent(2:7-2:8),OpColon(2:8-2:9),OpenCurly(2:10-2:11),LowerIdent(2:12-2:13),OpColon(2:13-2:14),OpenCurly(2:15-2:16),LowerIdent(2:17-2:18),CloseCurly(2:19-2:20),CloseCurly(2:21-2:22),CloseCurly(2:23-2:24),OpFatArrow(2:25-2:27),StringStart(2:28-2:29),StringPart(2:29-2:44),OpenStringInterpolation(2:44-2:46),LowerIdent(2:46-2:47),CloseStringInterpolation(2:47-2:48),StringPart(2:48-2:48),StringEnd(2:48-2:49),Newline(1:1-1:1),
OpenCurly(3:5-3:6),LowerIdent(3:7-3:8),Comma(3:8-3:9),LowerIdent(3:10-3:11),OpColon(3:11-3:12),OpenCurly(3:13-3:14),CloseCurly(3:14-3:15),CloseCurly(3:16-3:17),OpFatArrow(3:18-3:20),StringStart(3:21-3:22),StringPart(3:22-3:40),OpenStringInterpolation(3:40-3:42),LowerIdent(3:42-3:43),CloseStringInterpolation(3:43-3:44),StringPart(3:44-3:44),StringEnd(3:44-3:45),Newline(1:1-1:1),
OpenCurly(4:5-4:6),LowerIdent(4:7-4:12),OpColon(4:12-4:13),OpenCurly(4:14-4:15),LowerIdent(4:16-4:21),CloseCurly(4:22-4:23),Comma(4:23-4:24),LowerIdent(4:25-4:31),CloseCurly(4:32-4:33),OpFatArrow(4:34-4:36),StringStart(4:37-4:38),StringPart(4:38-4:45),OpenStringInterpolation(4:45-4:47),LowerIdent(4:47-4:52),CloseStringInterpolation(4:52-4:53),StringPart(4:53-4:58),OpenStringInterpolation(4:58-4:60),LowerIdent(4:60-4:66),CloseStringInterpolation(4:66-4:67),StringPart(4:67-4:67),StringEnd(4:67-4:68),Newline(1:1-1:1),
OpenCurly(5:5-5:6),LowerIdent(5:7-5:8),OpColon(5:8-5:9),OpenCurly(5:10-5:11),LowerIdent(5:12-5:13),CloseCurly(5:14-5:15),Comma(5:15-5:16),LowerIdent(5:17-5:18),OpColon(5:18-5:19),OpenCurly(5:20-5:21),LowerIdent(5:22-5:23),CloseCurly(5:24-5:25),CloseCurly(5:26-5:27),OpFatArrow(5:28-5:30),StringStart(5:31-5:32),StringPart(5:32-5:49),OpenStringInterpolation(5:49-5:51),LowerIdent(5:51-5:52),CloseStringInterpolation(5:52-5:53),StringPart(5:53-5:55),OpenStringInterpolation(5:55-5:57),LowerIdent(5:57-5:58),CloseStringInterpolation(5:58-5:59),StringPart(5:59-5:59),StringEnd(5:59-5:60),Newline(1:1-1:1),
OpenCurly(6:5-6:6),LowerIdent(6:7-6:11),OpColon(6:11-6:12),LowerIdent(6:13-6:14),CloseCurly(6:15-6:16),OpFatArrow(6:17-6:19),StringStart(6:20-6:21),StringPart(6:21-6:30),OpenStringInterpolation(6:30-6:32),LowerIdent(6:32-6:33),CloseStringInterpolation(6:33-6:34),StringPart(6:34-6:34),StringEnd(6:34-6:35),Newline(1:1-1:1),
OpenCurly(7:5-7:6),LowerIdent(7:7-7:13),OpColon(7:13-7:14),OpenCurly(7:15-7:16),LowerIdent(7:17-7:21),OpColon(7:21-7:22),LowerIdent(7:23-7:32),Comma(7:32-7:33),LowerIdent(7:34-7:37),OpColon(7:37-7:38),LowerIdent(7:39-7:46),CloseCurly(7:47-7:48),CloseCurly(7:49-7:50),OpFatArrow(7:51-7:53),StringStart(7:54-7:55),StringPart(7:55-7:71),OpenStringInterpolation(7:71-7:73),LowerIdent(7:73-7:82),CloseStringInterpolation(7:82-7:83),StringPart(7:83-7:85),OpenStringInterpolation(7:85-7:87),LowerIdent(7:87-7:94),NoSpaceDotLowerIdent(7:94-7:101),NoSpaceOpenRound(7:101-7:102),CloseRound(7:102-7:103),CloseStringInterpolation(7:103-7:104),StringPart(7:104-7:105),StringEnd(7:105-7:106),Newline(1:1-1:1),
OpenCurly(8:5-8:6),CloseCurly(8:6-8:7),OpFatArrow(8:8-8:10),StringStart(8:11-8:12),StringPart(8:12-8:24),StringEnd(8:24-8:25),Newline(1:1-1:1),
CloseCurly(9:1-9:2),EndOfFile(9:2-9:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ellipsis)
	(branches
		(branch @1.1-1.1
			(p-record @2.5-2.24
				(field @2.7-2.24 (name "a") (rest false)
					(p-record @2.10-2.22
						(field @2.12-2.22 (name "b") (rest false)
							(p-record @2.15-2.20
								(field @2.17-2.20 (name "c") (rest false)))))))
			(e-string @2.28-2.49
				(e-string-part @2.29-2.44 (raw "deeply nested: "))
				(e-ident @2.46-2.47 (raw "c"))
				(e-string-part @2.48-2.48 (raw ""))))
		(branch @1.1-1.1
			(p-record @3.5-3.17
				(field @3.7-3.9 (name "x") (rest false))
				(field @3.10-3.17 (name "y") (rest false)
					(p-record @3.13-3.15)))
			(e-string @3.21-3.45
				(e-string-part @3.22-3.40 (raw "mixed with empty: "))
				(e-ident @3.42-3.43 (raw "x"))
				(e-string-part @3.44-3.44 (raw ""))))
		(branch @1.1-1.1
			(p-record @4.5-4.33
				(field @4.7-4.24 (name "outer") (rest false)
					(p-record @4.14-4.23
						(field @4.16-4.23 (name "inner") (rest false))))
				(field @4.25-4.33 (name "simple") (rest false)))
			(e-string @4.37-4.68
				(e-string-part @4.38-4.45 (raw "mixed: "))
				(e-ident @4.47-4.52 (raw "inner"))
				(e-string-part @4.53-4.58 (raw " and "))
				(e-ident @4.60-4.66 (raw "simple"))
				(e-string-part @4.67-4.67 (raw ""))))
		(branch @1.1-1.1
			(p-record @5.5-5.27
				(field @5.7-5.16 (name "a") (rest false)
					(p-record @5.10-5.15
						(field @5.12-5.15 (name "b") (rest false))))
				(field @5.17-5.27 (name "c") (rest false)
					(p-record @5.20-5.25
						(field @5.22-5.25 (name "d") (rest false)))))
			(e-string @5.31-5.60
				(e-string-part @5.32-5.49 (raw "multiple nested: "))
				(e-ident @5.51-5.52 (raw "b"))
				(e-string-part @5.53-5.55 (raw ", "))
				(e-ident @5.57-5.58 (raw "d"))
				(e-string-part @5.59-5.59 (raw ""))))
		(branch @1.1-1.1
			(p-record @6.5-6.16
				(field @6.7-6.16 (name "name") (rest false)
					(p-ident @6.13-6.14 (raw "x"))))
			(e-string @6.20-6.35
				(e-string-part @6.21-6.30 (raw "renamed: "))
				(e-ident @6.32-6.33 (raw "x"))
				(e-string-part @6.34-6.34 (raw ""))))
		(branch @1.1-1.1
			(p-record @7.5-7.50
				(field @7.7-7.50 (name "person") (rest false)
					(p-record @7.15-7.48
						(field @7.17-7.33 (name "name") (rest false)
							(p-ident @7.23-7.32 (raw "firstName")))
						(field @7.34-7.48 (name "age") (rest false)
							(p-ident @7.39-7.46 (raw "userAge"))))))
			(e-string @7.54-7.106
				(e-string-part @7.55-7.71 (raw "renamed nested: "))
				(e-ident @7.73-7.82 (raw "firstName"))
				(e-string-part @7.83-7.85 (raw " ("))
				(e-field-access @7.87-7.104
					(e-ident @7.87-7.94 (raw "userAge"))
					(e-apply @7.94-7.103
						(e-ident @7.94-7.101 (raw "to_str"))))
				(e-string-part @7.104-7.105 (raw ")"))))
		(branch @1.1-1.1
			(p-record @8.5-8.7)
			(e-string @8.11-8.25
				(e-string-part @8.12-8.24 (raw "empty record"))))))
~~~
# FORMATTED
~~~roc
match ... {
	{ a: { b: { c } } } => "deeply nested: ${c}"
	{ x, y: {} } => "mixed with empty: ${x}"
	{ outer: { inner }, simple } => "mixed: ${inner} and ${simple}"
	{ a: { b }, c: { d } } => "multiple nested: ${b}, ${d}"
	{ name: x } => "renamed: ${x}"
	{ person: { name: firstName, age: userAge } } => "renamed nested: ${firstName} (${userAge.to_str()})"
	{} => "empty record"
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-9.2
	(match @1.1-9.2
		(cond
			(e-not-implemented @1.7-1.10))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @2.5-2.24
							(destructs
								(record-destruct @2.7-2.24 (label "a") (ident "a")
									(sub-pattern
										(p-record-destructure @2.10-2.22
											(destructs
												(record-destruct @2.12-2.22 (label "b") (ident "b")
													(sub-pattern
														(p-record-destructure @2.15-2.20
															(destructs
																(record-destruct @2.17-2.20 (label "c") (ident "c")
																	(required))))))))))))))
				(value
					(e-string @2.28-2.49
						(e-literal @2.29-2.44 (string "deeply nested: "))
						(e-lookup-local @2.46-2.47
							(p-assign @2.17-2.20 (ident "c")))
						(e-literal @2.48-2.48 (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @3.5-3.17
							(destructs
								(record-destruct @3.7-3.9 (label "x") (ident "x")
									(required))
								(record-destruct @3.10-3.17 (label "y") (ident "y")
									(sub-pattern
										(p-record-destructure @3.13-3.15
											(destructs))))))))
				(value
					(e-string @3.21-3.45
						(e-literal @3.22-3.40 (string "mixed with empty: "))
						(e-lookup-local @3.42-3.43
							(p-assign @3.7-3.9 (ident "x")))
						(e-literal @3.44-3.44 (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @4.5-4.33
							(destructs
								(record-destruct @4.7-4.24 (label "outer") (ident "outer")
									(sub-pattern
										(p-record-destructure @4.14-4.23
											(destructs
												(record-destruct @4.16-4.23 (label "inner") (ident "inner")
													(required))))))
								(record-destruct @4.25-4.33 (label "simple") (ident "simple")
									(required))))))
				(value
					(e-string @4.37-4.68
						(e-literal @4.38-4.45 (string "mixed: "))
						(e-lookup-local @4.47-4.52
							(p-assign @4.16-4.23 (ident "inner")))
						(e-literal @4.53-4.58 (string " and "))
						(e-lookup-local @4.60-4.66
							(p-assign @4.25-4.33 (ident "simple")))
						(e-literal @4.67-4.67 (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @5.5-5.27
							(destructs
								(record-destruct @5.7-5.16 (label "a") (ident "a")
									(sub-pattern
										(p-record-destructure @5.10-5.15
											(destructs
												(record-destruct @5.12-5.15 (label "b") (ident "b")
													(required))))))
								(record-destruct @5.17-5.27 (label "c") (ident "c")
									(sub-pattern
										(p-record-destructure @5.20-5.25
											(destructs
												(record-destruct @5.22-5.25 (label "d") (ident "d")
													(required))))))))))
				(value
					(e-string @5.31-5.60
						(e-literal @5.32-5.49 (string "multiple nested: "))
						(e-lookup-local @5.51-5.52
							(p-assign @5.12-5.15 (ident "b")))
						(e-literal @5.53-5.55 (string ", "))
						(e-lookup-local @5.57-5.58
							(p-assign @5.22-5.25 (ident "d")))
						(e-literal @5.59-5.59 (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @6.5-6.16
							(destructs
								(record-destruct @6.7-6.16 (label "name") (ident "name")
									(sub-pattern
										(p-assign @6.13-6.14 (ident "x"))))))))
				(value
					(e-string @6.20-6.35
						(e-literal @6.21-6.30 (string "renamed: "))
						(e-lookup-local @6.32-6.33
							(p-assign @6.13-6.14 (ident "x")))
						(e-literal @6.34-6.34 (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @7.5-7.50
							(destructs
								(record-destruct @7.7-7.50 (label "person") (ident "person")
									(sub-pattern
										(p-record-destructure @7.15-7.48
											(destructs
												(record-destruct @7.17-7.33 (label "name") (ident "name")
													(sub-pattern
														(p-assign @7.23-7.32 (ident "firstName"))))
												(record-destruct @7.34-7.48 (label "age") (ident "age")
													(sub-pattern
														(p-assign @7.39-7.46 (ident "userAge"))))))))))))
				(value
					(e-string @7.54-7.106
						(e-literal @7.55-7.71 (string "renamed nested: "))
						(e-lookup-local @7.73-7.82
							(p-assign @7.23-7.32 (ident "firstName")))
						(e-literal @7.83-7.85 (string " ("))
						(e-dot-access @7.87-7.104 (field "to_str")
							(receiver
								(e-lookup-local @7.87-7.94
									(p-assign @7.39-7.46 (ident "userAge"))))
							(args))
						(e-literal @7.104-7.105 (string ")")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @8.5-8.7
							(destructs))))
				(value
					(e-string @8.11-8.25
						(e-literal @8.12-8.24 (string "empty record"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-9.2 (type "Str"))
~~~
