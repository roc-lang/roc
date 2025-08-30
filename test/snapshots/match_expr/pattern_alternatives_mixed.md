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
# TOKENS
~~~text
KwMatch TripleDot OpenCurly Int OpBar Int OpBar Int OpFatArrow String String OpBar String OpFatArrow String UpperIdent OpenRound Underscore CloseRound OpBar UpperIdent OpenRound Underscore CloseRound OpFatArrow String OpenSquare CloseSquare OpBar OpenSquare Underscore CloseSquare OpFatArrow String OpenRound Int Comma Underscore CloseRound OpBar OpenRound Underscore Comma Int CloseRound OpFatArrow String Underscore OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
2 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:2 to 2:6

**Parse Error**
at 2:6 to 2:8

**Unsupported Node**
at 2:6 to 2:8

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
