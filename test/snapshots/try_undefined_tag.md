# META
~~~ini
description=Try operator on undefined tag identifier
type=expr
~~~
# SOURCE
~~~roc
A?
~~~
# EXPECTED
NON-EXHAUSTIVE MATCH - try_undefined_tag.md:1:1:1:3
# PROBLEMS
**NON-EXHAUSTIVE MATCH**
This `match` expression doesn't cover all possible cases:
**try_undefined_tag.md:1:1:1:3:**
```roc
A?
```
^^

The value being matched on has type:
        _[A, Ok(_a), Err(_b), .._others]_

Missing patterns:
        A

Hint: Add branches to handle these cases, or use `_` to match anything.

# TOKENS
~~~zig
UpperIdent,NoSpaceOpQuestion,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-question-suffix
	(e-tag (raw "A")))
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
			(e-tag (name "A")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-lookup-local
						(p-assign (ident "#ok")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-return
						(e-tag (name "Err")
							(args
								(e-lookup-local
									(p-assign (ident "#err")))))))))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
