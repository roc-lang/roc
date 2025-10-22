# META
~~~ini
description=Pattern alternatives with mixed pattern types
type=expr
~~~
# SOURCE
~~~roc
match ... {
	1 | 2 | 3 => "small numbers"
	"hello" | "world" => "greetings"
	Ok(_) | Some(_) => "success value"
	[] | [_] => "short list"
	(0, _) | (_, 0) => "has zero"
	_ => "other"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch,TripleDot,OpenCurly,
Int,OpBar,Int,OpBar,Int,OpFatArrow,StringStart,StringPart,StringEnd,
StringStart,StringPart,StringEnd,OpBar,StringStart,StringPart,StringEnd,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpBar,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,StringStart,StringPart,StringEnd,
OpenSquare,CloseSquare,OpBar,OpenSquare,Underscore,CloseSquare,OpFatArrow,StringStart,StringPart,StringEnd,
OpenRound,Int,Comma,Underscore,CloseRound,OpBar,OpenRound,Underscore,Comma,Int,CloseRound,OpFatArrow,StringStart,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ellipsis)
	(branches
		(branch
			(p-alternatives
				(p-int (raw "1"))
				(p-int (raw "2"))
				(p-int (raw "3")))
			(e-string
				(e-string-part (raw "small numbers"))))
		(branch
			(p-alternatives
				(p-string (raw """))
				(p-string (raw """)))
			(e-string
				(e-string-part (raw "greetings"))))
		(branch
			(p-alternatives
				(p-tag (raw "Ok")
					(p-underscore))
				(p-tag (raw "Some")
					(p-underscore)))
			(e-string
				(e-string-part (raw "success value"))))
		(branch
			(p-alternatives
				(p-list)
				(p-list
					(p-underscore)))
			(e-string
				(e-string-part (raw "short list"))))
		(branch
			(p-alternatives
				(p-tuple
					(p-int (raw "0"))
					(p-underscore))
				(p-tuple
					(p-underscore)
					(p-int (raw "0"))))
			(e-string
				(e-string-part (raw "has zero"))))
		(branch
			(p-underscore)
			(e-string
				(e-string-part (raw "other"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-not-implemented))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-num (value "1")))
					(pattern (degenerate false)
						(p-num (value "2")))
					(pattern (degenerate false)
						(p-num (value "3"))))
				(value
					(e-string
						(e-literal (string "small numbers")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-str (text """)))
					(pattern (degenerate false)
						(p-str (text """))))
				(value
					(e-string
						(e-literal (string "greetings")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag))
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-string
						(e-literal (string "success value")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns)))
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-underscore)))))
				(value
					(e-string
						(e-literal (string "short list")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-tuple
							(patterns
								(p-num (value "0"))
								(p-underscore))))
					(pattern (degenerate false)
						(p-tuple
							(patterns
								(p-underscore)
								(p-num (value "0"))))))
				(value
					(e-string
						(e-literal (string "has zero")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-underscore)))
				(value
					(e-string
						(e-literal (string "other"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
