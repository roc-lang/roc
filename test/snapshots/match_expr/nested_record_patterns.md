# META
~~~ini
description=Match expression with deeply nested record patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
    { name, address: { city, country } } => "${name} lives in ${city}, ${country}"
    { person: { name, age }, location: { city } } => "${name} (${age.to_str()}) from ${city}"
    { data: { info: { value } } } => "Deep nested: ${value}"
    { simple } => "Simple: ${simple}"
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
OpenCurly(2:5-2:6),LowerIdent(2:7-2:11),Comma(2:11-2:12),LowerIdent(2:13-2:20),OpColon(2:20-2:21),OpenCurly(2:22-2:23),LowerIdent(2:24-2:28),Comma(2:28-2:29),LowerIdent(2:30-2:37),CloseCurly(2:38-2:39),CloseCurly(2:40-2:41),OpFatArrow(2:42-2:44),StringStart(2:45-2:46),StringPart(2:46-2:46),OpenStringInterpolation(2:46-2:48),LowerIdent(2:48-2:52),CloseStringInterpolation(2:52-2:53),StringPart(2:53-2:63),OpenStringInterpolation(2:63-2:65),LowerIdent(2:65-2:69),CloseStringInterpolation(2:69-2:70),StringPart(2:70-2:72),OpenStringInterpolation(2:72-2:74),LowerIdent(2:74-2:81),CloseStringInterpolation(2:81-2:82),StringPart(2:82-2:82),StringEnd(2:82-2:83),
OpenCurly(3:5-3:6),LowerIdent(3:7-3:13),OpColon(3:13-3:14),OpenCurly(3:15-3:16),LowerIdent(3:17-3:21),Comma(3:21-3:22),LowerIdent(3:23-3:26),CloseCurly(3:27-3:28),Comma(3:28-3:29),LowerIdent(3:30-3:38),OpColon(3:38-3:39),OpenCurly(3:40-3:41),LowerIdent(3:42-3:46),CloseCurly(3:47-3:48),CloseCurly(3:49-3:50),OpFatArrow(3:51-3:53),StringStart(3:54-3:55),StringPart(3:55-3:55),OpenStringInterpolation(3:55-3:57),LowerIdent(3:57-3:61),CloseStringInterpolation(3:61-3:62),StringPart(3:62-3:64),OpenStringInterpolation(3:64-3:66),LowerIdent(3:66-3:69),NoSpaceDotLowerIdent(3:69-3:76),NoSpaceOpenRound(3:76-3:77),CloseRound(3:77-3:78),CloseStringInterpolation(3:78-3:79),StringPart(3:79-3:86),OpenStringInterpolation(3:86-3:88),LowerIdent(3:88-3:92),CloseStringInterpolation(3:92-3:93),StringPart(3:93-3:93),StringEnd(3:93-3:94),
OpenCurly(4:5-4:6),LowerIdent(4:7-4:11),OpColon(4:11-4:12),OpenCurly(4:13-4:14),LowerIdent(4:15-4:19),OpColon(4:19-4:20),OpenCurly(4:21-4:22),LowerIdent(4:23-4:28),CloseCurly(4:29-4:30),CloseCurly(4:31-4:32),CloseCurly(4:33-4:34),OpFatArrow(4:35-4:37),StringStart(4:38-4:39),StringPart(4:39-4:52),OpenStringInterpolation(4:52-4:54),LowerIdent(4:54-4:59),CloseStringInterpolation(4:59-4:60),StringPart(4:60-4:60),StringEnd(4:60-4:61),
OpenCurly(5:5-5:6),LowerIdent(5:7-5:13),CloseCurly(5:14-5:15),OpFatArrow(5:16-5:18),StringStart(5:19-5:20),StringPart(5:20-5:28),OpenStringInterpolation(5:28-5:30),LowerIdent(5:30-5:36),CloseStringInterpolation(5:36-5:37),StringPart(5:37-5:37),StringEnd(5:37-5:38),
OpenCurly(6:5-6:6),CloseCurly(6:6-6:7),OpFatArrow(6:8-6:10),StringStart(6:11-6:12),StringPart(6:12-6:17),StringEnd(6:17-6:18),
CloseCurly(7:1-7:2),EndOfFile(7:2-7:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ellipsis)
	(branches
		(branch @2.5-2.83
			(p-record @2.5-2.41
				(field @2.7-2.11 (name "name") (rest false))
				(field @2.13-2.39 (name "address") (rest false)
					(p-record @2.22-2.39
						(field @2.24-2.28 (name "city") (rest false))
						(field @2.30-2.37 (name "country") (rest false)))))
			(e-string @2.45-2.83
				(e-string-part @2.46-2.46 (raw ""))
				(e-ident @2.48-2.52 (raw "name"))
				(e-string-part @2.53-2.63 (raw " lives in "))
				(e-ident @2.65-2.69 (raw "city"))
				(e-string-part @2.70-2.72 (raw ", "))
				(e-ident @2.74-2.81 (raw "country"))
				(e-string-part @2.82-2.82 (raw ""))))
		(branch @3.5-3.94
			(p-record @3.5-3.50
				(field @3.7-3.28 (name "person") (rest false)
					(p-record @3.15-3.28
						(field @3.17-3.21 (name "name") (rest false))
						(field @3.23-3.26 (name "age") (rest false))))
				(field @3.30-3.48 (name "location") (rest false)
					(p-record @3.40-3.48
						(field @3.42-3.46 (name "city") (rest false)))))
			(e-string @3.54-3.94
				(e-string-part @3.55-3.55 (raw ""))
				(e-ident @3.57-3.61 (raw "name"))
				(e-string-part @3.62-3.64 (raw " ("))
				(e-field-access @3.66-3.78
					(e-ident @3.66-3.69 (raw "age"))
					(e-apply @3.69-3.78
						(e-ident @3.69-3.76 (raw "to_str"))))
				(e-string-part @3.79-3.86 (raw ") from "))
				(e-ident @3.88-3.92 (raw "city"))
				(e-string-part @3.93-3.93 (raw ""))))
		(branch @4.5-4.61
			(p-record @4.5-4.34
				(field @4.7-4.32 (name "data") (rest false)
					(p-record @4.13-4.32
						(field @4.15-4.30 (name "info") (rest false)
							(p-record @4.21-4.30
								(field @4.23-4.28 (name "value") (rest false)))))))
			(e-string @4.38-4.61
				(e-string-part @4.39-4.52 (raw "Deep nested: "))
				(e-ident @4.54-4.59 (raw "value"))
				(e-string-part @4.60-4.60 (raw ""))))
		(branch @5.5-5.38
			(p-record @5.5-5.15
				(field @5.7-5.13 (name "simple") (rest false)))
			(e-string @5.19-5.38
				(e-string-part @5.20-5.28 (raw "Simple: "))
				(e-ident @5.30-5.36 (raw "simple"))
				(e-string-part @5.37-5.37 (raw ""))))
		(branch @6.5-6.18
			(p-record @6.5-6.7)
			(e-string @6.11-6.18
				(e-string-part @6.12-6.17 (raw "empty"))))))
~~~
# FORMATTED
~~~roc
match ... {
	{ name, address: { city, country } } => "${name} lives in ${city}, ${country}"
	{ person: { name, age }, location: { city } } => "${name} (${age.to_str()}) from ${city}"
	{ data: { info: { value } } } => "Deep nested: ${value}"
	{ simple } => "Simple: ${simple}"
	{} => "empty"
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-7.2
	(match @1.1-7.2
		(cond
			(e-not-implemented @1.1-1.1))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @2.5-2.41
							(destructs
								(record-destruct @2.7-2.11 (label "name") (ident "name")
									(required
										(p-assign @2.7-2.11 (ident "name"))))
								(record-destruct @2.13-2.39 (label "address") (ident "address")
									(sub-pattern
										(p-record-destructure @2.22-2.39
											(destructs
												(record-destruct @2.24-2.28 (label "city") (ident "city")
													(required
														(p-assign @2.24-2.28 (ident "city"))))
												(record-destruct @2.30-2.37 (label "country") (ident "country")
													(required
														(p-assign @2.30-2.37 (ident "country"))))))))))))
				(value
					(e-string @2.45-2.83
						(e-literal @2.46-2.46 (string ""))
						(e-lookup-local @2.48-2.52
							(p-assign @2.7-2.11 (ident "name")))
						(e-literal @2.53-2.63 (string " lives in "))
						(e-lookup-local @2.65-2.69
							(p-assign @2.24-2.28 (ident "city")))
						(e-literal @2.70-2.72 (string ", "))
						(e-lookup-local @2.74-2.81
							(p-assign @2.30-2.37 (ident "country")))
						(e-literal @2.82-2.82 (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @3.5-3.50
							(destructs
								(record-destruct @3.7-3.28 (label "person") (ident "person")
									(sub-pattern
										(p-record-destructure @3.15-3.28
											(destructs
												(record-destruct @3.17-3.21 (label "name") (ident "name")
													(required
														(p-assign @3.17-3.21 (ident "name"))))
												(record-destruct @3.23-3.26 (label "age") (ident "age")
													(required
														(p-assign @3.23-3.26 (ident "age"))))))))
								(record-destruct @3.30-3.48 (label "location") (ident "location")
									(sub-pattern
										(p-record-destructure @3.40-3.48
											(destructs
												(record-destruct @3.42-3.46 (label "city") (ident "city")
													(required
														(p-assign @3.42-3.46 (ident "city"))))))))))))
				(value
					(e-string @3.54-3.94
						(e-literal @3.55-3.55 (string ""))
						(e-lookup-local @3.57-3.61
							(p-assign @3.17-3.21 (ident "name")))
						(e-literal @3.62-3.64 (string " ("))
						(e-dot-access @3.66-3.78 (field "to_str")
							(receiver
								(e-lookup-local @3.66-3.69
									(p-assign @3.23-3.26 (ident "age"))))
							(args))
						(e-literal @3.79-3.86 (string ") from "))
						(e-lookup-local @3.88-3.92
							(p-assign @3.42-3.46 (ident "city")))
						(e-literal @3.93-3.93 (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @4.5-4.34
							(destructs
								(record-destruct @4.7-4.32 (label "data") (ident "data")
									(sub-pattern
										(p-record-destructure @4.13-4.32
											(destructs
												(record-destruct @4.15-4.30 (label "info") (ident "info")
													(sub-pattern
														(p-record-destructure @4.21-4.30
															(destructs
																(record-destruct @4.23-4.28 (label "value") (ident "value")
																	(required
																		(p-assign @4.23-4.28 (ident "value"))))))))))))))))
				(value
					(e-string @4.38-4.61
						(e-literal @4.39-4.52 (string "Deep nested: "))
						(e-lookup-local @4.54-4.59
							(p-assign @4.23-4.28 (ident "value")))
						(e-literal @4.60-4.60 (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @5.5-5.15
							(destructs
								(record-destruct @5.7-5.13 (label "simple") (ident "simple")
									(required
										(p-assign @5.7-5.13 (ident "simple"))))))))
				(value
					(e-string @5.19-5.38
						(e-literal @5.20-5.28 (string "Simple: "))
						(e-lookup-local @5.30-5.36
							(p-assign @5.7-5.13 (ident "simple")))
						(e-literal @5.37-5.37 (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @6.5-6.7
							(destructs))))
				(value
					(e-string @6.11-6.18
						(e-literal @6.12-6.17 (string "empty"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-7.2 (type "Str"))
~~~
