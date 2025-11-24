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
INCOMPATIBLE MATCH PATTERNS - pattern_alternatives_mixed.md:1:1:1:1
MISSING METHOD - pattern_alternatives_mixed.md:2:10:2:11
# PROBLEMS
**INCOMPATIBLE MATCH PATTERNS**
The pattern first pattern in this third`match` differs from previous ones:
**pattern_alternatives_mixed.md:1:1:**
```roc
match ... {
	1 | 2 | 3 => "small numbers"
	"hello" | "world" => "greetings"
	Ok(_) | Some(_) => "success value"
	[] | [_] => "short list"
	(0, _) | (_, 0) => "has zero"
	_ => "other"
}
```
 ^^^^^

The third pattern has this type:
    _[Ok(_a)][ProvidedByCompiler]_

But all the previous patterns have this type: 
    _Str_

All patterns in an `match` must have compatible types.



**MISSING METHOD**
This **from_numeral** method is being called on the type **Str**, which has no method with that name:
**pattern_alternatives_mixed.md:2:10:2:11:**
```roc
	1 | 2 | 3 => "small numbers"
```
	        ^


**Hint: **For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

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
(expr (type "a where [a.try_from_str : Str -> Try(a, [InvalidStr(Str)])]"))
~~~
