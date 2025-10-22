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
KwMatch,OpenSquare,CloseSquare,OpenCurly,
LowerIdent,OpFatArrow,OpenCurly,LowerIdent,OpColon,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,Comma,LowerIdent,OpColon,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-list)
	(branches
		(branch
			(p-ident (raw "empty"))
			(e-record
				(field (field "ints")
					(e-list
						(e-int (raw "1"))
						(e-int (raw "2"))
						(e-int (raw "3"))))
				(field (field "strs")
					(e-list
						(e-string
							(e-string-part (raw "a")))
						(e-string
							(e-string-part (raw "b")))
						(e-string
							(e-string-part (raw "c")))))
				(field (field "empty")
					(e-ident (raw "empty")))))))
~~~
# FORMATTED
~~~roc
match [] {
	empty => { ints: [1, 2, 3], strs: ["a", "b", "c"], empty: empty }
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-empty_list))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign (ident "empty"))))
				(value
					(e-record
						(fields
							(field (name "ints")
								(e-list
									(elems
										(e-num (value "1"))
										(e-num (value "2"))
										(e-num (value "3")))))
							(field (name "strs")
								(e-list
									(elems
										(e-string
											(e-literal (string "a")))
										(e-string
											(e-literal (string "b")))
										(e-string
											(e-literal (string "c"))))))
							(field (name "empty")
								(e-lookup-local
									(p-assign (ident "empty")))))))))))
~~~
# TYPES
~~~clojure
(expr (type "{ empty: List(_elem), ints: List(Num(_size)), strs: List(Error) }"))
~~~
