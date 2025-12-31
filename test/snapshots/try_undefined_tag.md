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
TYPE MISMATCH - try_undefined_tag.md:1:1:1:3
# PROBLEMS
**TYPE MISMATCH**
The `?` operator expects a _Try_ value (`Ok(...)` or `Err(...)`), but this expression has a different type:
**try_undefined_tag.md:1:1:1:3:**
```roc
A?
```
^^

The expression has type:
        _[A, Ok(_a), Err(_b), .._others]_

**Hint:** The `?` operator unwraps `Ok` values and returns early on `Err`. Make sure your expression evaluates to a _Try_ type.

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
