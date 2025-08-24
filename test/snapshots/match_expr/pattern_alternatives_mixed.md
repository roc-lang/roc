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
(match <39 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
INCOMPATIBLE MATCH PATTERNS - pattern_alternatives_mixed.md:1:1:1:1
# PROBLEMS
**Parse Error**
at 1:1 to 1:11

**Parse Error**
at 2:12 to 2:12

**Parse Error**
at 3:20 to 3:20

**Parse Error**
at 4:18 to 4:18

**Parse Error**
at 5:11 to 5:11

**Parse Error**
at 6:18 to 6:18

**Parse Error**
at 7:4 to 7:4

**Parse Error**
at 1:1 to 8:2

**Parse Error**
at 8:2 to 8:2

**Unsupported Node**
at 1:1 to 8:2

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
