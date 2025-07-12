# META
~~~ini
description=Match expression with record destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
    { name, age } => "${name} is ${age.to_str()} years old"
    { name, address: { city } } => "${city} is the city of ${name}"
    {} => "empty"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch(1:1-1:6),TripleDot(1:7-1:10),OpenCurly(1:11-1:12),
OpenCurly(2:5-2:6),LowerIdent(2:7-2:11),Comma(2:11-2:12),LowerIdent(2:13-2:16),CloseCurly(2:17-2:18),OpFatArrow(2:19-2:21),StringStart(2:22-2:23),StringPart(2:23-2:23),OpenStringInterpolation(2:23-2:25),LowerIdent(2:25-2:29),CloseStringInterpolation(2:29-2:30),StringPart(2:30-2:34),OpenStringInterpolation(2:34-2:36),LowerIdent(2:36-2:39),NoSpaceDotLowerIdent(2:39-2:46),NoSpaceOpenRound(2:46-2:47),CloseRound(2:47-2:48),CloseStringInterpolation(2:48-2:49),StringPart(2:49-2:59),StringEnd(2:59-2:60),
OpenCurly(3:5-3:6),LowerIdent(3:7-3:11),Comma(3:11-3:12),LowerIdent(3:13-3:20),OpColon(3:20-3:21),OpenCurly(3:22-3:23),LowerIdent(3:24-3:28),CloseCurly(3:29-3:30),CloseCurly(3:31-3:32),OpFatArrow(3:33-3:35),StringStart(3:36-3:37),StringPart(3:37-3:37),OpenStringInterpolation(3:37-3:39),LowerIdent(3:39-3:43),CloseStringInterpolation(3:43-3:44),StringPart(3:44-3:60),OpenStringInterpolation(3:60-3:62),LowerIdent(3:62-3:66),CloseStringInterpolation(3:66-3:67),StringPart(3:67-3:67),StringEnd(3:67-3:68),
OpenCurly(4:5-4:6),CloseCurly(4:6-4:7),OpFatArrow(4:8-4:10),StringStart(4:11-4:12),StringPart(4:12-4:17),StringEnd(4:17-4:18),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ellipsis)
	(branches
		(branch @2.5-2.60
			(p-record @2.5-2.18
				(field @2.7-2.11 (name "name") (rest false))
				(field @2.13-2.16 (name "age") (rest false)))
			(e-string @2.22-2.60
				(e-string-part @2.23-2.23 (raw ""))
				(e-ident @2.25-2.29 (raw "name"))
				(e-string-part @2.30-2.34 (raw " is "))
				(e-field-access @2.36-2.48
					(e-ident @2.36-2.39 (raw "age"))
					(e-apply @2.39-2.48
						(e-ident @2.39-2.46 (raw "to_str"))))
				(e-string-part @2.49-2.59 (raw " years old"))))
		(branch @3.5-3.68
			(p-record @3.5-3.32
				(field @3.7-3.11 (name "name") (rest false))
				(field @3.13-3.30 (name "address") (rest false)
					(p-record @3.22-3.30
						(field @3.24-3.28 (name "city") (rest false)))))
			(e-string @3.36-3.68
				(e-string-part @3.37-3.37 (raw ""))
				(e-ident @3.39-3.43 (raw "city"))
				(e-string-part @3.44-3.60 (raw " is the city of "))
				(e-ident @3.62-3.66 (raw "name"))
				(e-string-part @3.67-3.67 (raw ""))))
		(branch @4.5-4.18
			(p-record @4.5-4.7)
			(e-string @4.11-4.18
				(e-string-part @4.12-4.17 (raw "empty"))))))
~~~
# FORMATTED
~~~roc
match ... {
	{ name, age } => "${name} is ${age.to_str()} years old"
	{ name, address: { city } } => "${city} is the city of ${name}"
	{} => "empty"
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-5.2
	(match @1.1-5.2
		(cond
			(e-not-implemented @1.1-1.1))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @2.5-2.18
							(destructs
								(record-destruct @2.7-2.11 (label "name") (ident "name")
									(required))
								(record-destruct @2.13-2.16 (label "age") (ident "age")
									(required))))))
				(value
					(e-string @2.22-2.60
						(e-literal @2.23-2.23 (string ""))
						(e-lookup-local @2.25-2.29
							(p-assign @2.7-2.11 (ident "name")))
						(e-literal @2.30-2.34 (string " is "))
						(e-dot-access @2.36-2.48 (field "to_str")
							(receiver
								(e-lookup-local @2.36-2.39
									(p-assign @2.13-2.16 (ident "age"))))
							(args))
						(e-literal @2.49-2.59 (string " years old")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @3.5-3.32
							(destructs
								(record-destruct @3.7-3.11 (label "name") (ident "name")
									(required))
								(record-destruct @3.13-3.30 (label "address") (ident "address")
									(sub-pattern
										(p-record-destructure @3.22-3.30
											(destructs
												(record-destruct @3.24-3.28 (label "city") (ident "city")
													(required))))))))))
				(value
					(e-string @3.36-3.68
						(e-literal @3.37-3.37 (string ""))
						(e-lookup-local @3.39-3.43
							(p-assign @3.24-3.28 (ident "city")))
						(e-literal @3.44-3.60 (string " is the city of "))
						(e-lookup-local @3.62-3.66
							(p-assign @3.7-3.11 (ident "name")))
						(e-literal @3.67-3.67 (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @4.5-4.7
							(destructs))))
				(value
					(e-string @4.11-4.18
						(e-literal @4.12-4.17 (string "empty"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Str"))
~~~
