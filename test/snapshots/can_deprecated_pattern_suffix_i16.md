# META
~~~ini
description=Test deprecated i16 suffix in match pattern
type=expr
~~~
# SOURCE
~~~roc
|x| match x {
    -100i16 => "negative hundred"
    _ => "other"
}
~~~
# EXPECTED
DEPRECATED NUMBER SUFFIX - can_deprecated_pattern_suffix_i16.md:2:5:2:12
# PROBLEMS
**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**can_deprecated_pattern_suffix_i16.md:2:5:2:12:**
```roc
    -100i16 => "negative hundred"
```
    ^^^^^^^

The `i16` suffix is no longer supported. Use `-100.I16` instead.

# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
Int,OpFatArrow,StringStart,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "x")))
	(e-match
		(e-ident (raw "x"))
		(branches
			(branch
				(p-int (raw "-100i16"))
				(e-string
					(e-string-part (raw "negative hundred"))))
			(branch
				(p-underscore)
				(e-string
					(e-string-part (raw "other")))))))
~~~
# FORMATTED
~~~roc
|x| match x {
	-100i16 => "negative hundred"
	_ => "other"
}
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "x")))
	(e-match
		(match
			(cond
				(e-lookup-local
					(p-assign (ident "x"))))
			(branches
				(branch
					(patterns
						(pattern (degenerate false)
							(p-num (value "-100"))))
					(value
						(e-string
							(e-literal (string "negative hundred")))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-underscore)))
					(value
						(e-string
							(e-literal (string "other")))))))))
~~~
# TYPES
~~~clojure
(expr (type "I16 -> Str"))
~~~
