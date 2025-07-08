# META
~~~ini
description=Let-polymorphism with empty list in expression
type=expr
~~~
# SOURCE
~~~roc
match [] {
    empty => { ints: [1, 2, 3], strs: ["a", "b", "c"], empty: empty }
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch(1:1-1:6),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),OpenCurly(1:10-1:11),Newline(1:1-1:1),
LowerIdent(2:5-2:10),OpFatArrow(2:11-2:13),OpenCurly(2:14-2:15),LowerIdent(2:16-2:20),OpColon(2:20-2:21),OpenSquare(2:22-2:23),Int(2:23-2:24),Comma(2:24-2:25),Int(2:26-2:27),Comma(2:27-2:28),Int(2:29-2:30),CloseSquare(2:30-2:31),Comma(2:31-2:32),LowerIdent(2:33-2:37),OpColon(2:37-2:38),OpenSquare(2:39-2:40),StringStart(2:40-2:41),StringPart(2:41-2:42),StringEnd(2:42-2:43),Comma(2:43-2:44),StringStart(2:45-2:46),StringPart(2:46-2:47),StringEnd(2:47-2:48),Comma(2:48-2:49),StringStart(2:50-2:51),StringPart(2:51-2:52),StringEnd(2:52-2:53),CloseSquare(2:53-2:54),Comma(2:54-2:55),LowerIdent(2:56-2:61),OpColon(2:61-2:62),LowerIdent(2:63-2:68),CloseCurly(2:69-2:70),Newline(1:1-1:1),
CloseCurly(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-list @1.7-1.9)
	(branches
		(branch @2.5-3.2
			(p-ident @2.5-2.10 (raw "empty"))
			(e-record @2.14-2.70
				(field (field "ints")
					(e-list @2.22-2.31
						(e-int @2.23-2.24 (raw "1"))
						(e-int @2.26-2.27 (raw "2"))
						(e-int @2.29-2.30 (raw "3"))))
				(field (field "strs")
					(e-list @2.39-2.54
						(e-string @2.40-2.43
							(e-string-part @2.41-2.42 (raw "a")))
						(e-string @2.45-2.48
							(e-string-part @2.46-2.47 (raw "b")))
						(e-string @2.50-2.53
							(e-string-part @2.51-2.52 (raw "c")))))
				(field (field "empty")
					(e-ident @2.63-2.68 (raw "empty")))))))
~~~
# FORMATTED
~~~roc
match [] {
	empty => {ints: [1, 2, 3], strs: ["a", "b", "c"], empty: empty}
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-3.2
	(match
		(cond
			(e-empty-list @1.7-1.9))
		(branches
			(branch @2.14-2.70
				(patterns
					(pattern (degenerate false)
						(p-assign @2.5-2.10 (ident "empty"))))
				(value
					(e-record @2.14-2.70
						(fields
							(field (name "ints")
								(e-list @2.22-2.31
									(elems
										(e-int @2.23-2.24 (value "1"))
										(e-int @2.26-2.27 (value "2"))
										(e-int @2.29-2.30 (value "3")))))
							(field (name "strs")
								(e-list @2.39-2.54
									(elems
										(e-str @2.40-2.43
											(e-literal @2.41-2.42 (string "a")))
										(e-str @2.45-2.48
											(e-literal @2.46-2.47 (string "b")))
										(e-str @2.50-2.53
											(e-literal @2.51-2.52 (string "c"))))))
							(field (name "empty")
								(e-lookup-local @2.63-2.68
									(p-assign @2.5-2.10 (ident "empty")))))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "{ ints: List(Num(*)), strs: List(Str), empty: List(*) }"))
~~~
