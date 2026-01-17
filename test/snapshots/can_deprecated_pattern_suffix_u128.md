# META
~~~ini
description=Test deprecated u128 suffix in match pattern
type=expr
~~~
# SOURCE
~~~roc
|x| match x {
    999u128 => "big"
    _ => "other"
}
~~~
# EXPECTED
DEPRECATED NUMBER SUFFIX - can_deprecated_pattern_suffix_u128.md:2:5:2:12
# PROBLEMS
**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**can_deprecated_pattern_suffix_u128.md:2:5:2:12:**
```roc
    999u128 => "big"
```
    ^^^^^^^

The `u128` suffix is no longer supported. Use `999.U128` instead.

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
				(p-int (raw "999u128"))
				(e-string
					(e-string-part (raw "big"))))
			(branch
				(p-underscore)
				(e-string
					(e-string-part (raw "other")))))))
~~~
# FORMATTED
~~~roc
|x| match x {
	999u128 => "big"
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
							(p-num (value "999"))))
					(value
						(e-string
							(e-literal (string "big")))))
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
(expr (type "U128 -> Str"))
~~~
