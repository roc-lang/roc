# META
~~~ini
description=Test integer pattern too large for u128
type=expr
~~~
# SOURCE
~~~roc
|x| match x {
    999999999999999999999999999999999999999999 => "too big"
    _ => "other"
}
~~~
# EXPECTED
INVALID NUMBER - can_pattern_int_overflow.md:2:5:2:47
# PROBLEMS
**INVALID NUMBER**
This number literal is not valid: `999999999999999999999999999999999999999999`

**can_pattern_int_overflow.md:2:5:2:47:**
```roc
    999999999999999999999999999999999999999999 => "too big"
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Check that the number is correctly formatted. Valid examples include: `42`, `3.14`, `0x1A`, or `1_000_000`.

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
				(p-int (raw "999999999999999999999999999999999999999999"))
				(e-string
					(e-string-part (raw "too big"))))
			(branch
				(p-underscore)
				(e-string
					(e-string-part (raw "other")))))))
~~~
# FORMATTED
~~~roc
|x| match x {
	999999999999999999999999999999999999999999 => "too big"
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
							(p-runtime-error (tag "invalid_num_literal"))))
					(value
						(e-string
							(e-literal (string "too big")))))
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
(expr (type "Error -> Str"))
~~~
