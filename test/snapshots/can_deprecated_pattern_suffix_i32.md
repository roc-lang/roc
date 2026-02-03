# META
~~~ini
description=Test deprecated signed integer suffix in match pattern
type=expr
~~~
# SOURCE
~~~roc
|x| match x {
    -50i32 => "negative"
    0i32 => "zero"
    _ => "positive"
}
~~~
# EXPECTED
DEPRECATED NUMBER SUFFIX - can_deprecated_pattern_suffix_i32.md:2:5:2:11
DEPRECATED NUMBER SUFFIX - can_deprecated_pattern_suffix_i32.md:3:5:3:9
# PROBLEMS
**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**can_deprecated_pattern_suffix_i32.md:2:5:2:11:**
```roc
    -50i32 => "negative"
```
    ^^^^^^

The `i32` suffix is no longer supported. Use `-50.I32` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**can_deprecated_pattern_suffix_i32.md:3:5:3:9:**
```roc
    0i32 => "zero"
```
    ^^^^

The `i32` suffix is no longer supported. Use `0.I32` instead.

# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
Int,OpFatArrow,StringStart,StringPart,StringEnd,
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
				(p-int (raw "-50i32"))
				(e-string
					(e-string-part (raw "negative"))))
			(branch
				(p-int (raw "0i32"))
				(e-string
					(e-string-part (raw "zero"))))
			(branch
				(p-underscore)
				(e-string
					(e-string-part (raw "positive")))))))
~~~
# FORMATTED
~~~roc
|x| match x {
	-50i32 => "negative"
	0i32 => "zero"
	_ => "positive"
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
							(p-num (value "-50"))))
					(value
						(e-string
							(e-literal (string "negative")))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-num (value "0"))))
					(value
						(e-string
							(e-literal (string "zero")))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-underscore)))
					(value
						(e-string
							(e-literal (string "positive")))))))))
~~~
# TYPES
~~~clojure
(expr (type "I32 -> Str"))
~~~
