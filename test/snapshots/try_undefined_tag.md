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
EXPECTED TRY TYPE - try_undefined_tag.md:1:1:1:1
# PROBLEMS
**TRY OPERATOR OUTSIDE FUNCTION**
The `?` operator can only be used inside function bodies because it can cause an early return.

**try_undefined_tag.md:1:1:1:3:**
```roc
A?
```
^^


**EXPECTED TRY TYPE**
The `?` operator expects a _Try_ type (a tag union containing ONLY _Ok_ and _Err_ tags),
but I found:
**try_undefined_tag.md:1:1:**
```roc
A?
```
^

This expression has type:

_[A, Ok(Error), Err(_a), .._others]_

Tip: Maybe wrap a value using _Ok(value)_ or _Err(value)_.

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
					(e-runtime-error (tag "return_outside_fn")))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
