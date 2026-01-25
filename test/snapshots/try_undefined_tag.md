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
TRY OPERATOR OUTSIDE FUNCTION - try_undefined_tag.md:1:1:1:3
TYPE MISMATCH - try_undefined_tag.md:1:1:1:2
# PROBLEMS
**TRY OPERATOR OUTSIDE FUNCTION**
The `?` operator can only be used inside function bodies because it can cause an early return.

**try_undefined_tag.md:1:1:1:3:**
```roc
A?
```
^^


**TYPE MISMATCH**
The `?` operator expects a `Try` type (a tag union containing ONLY `Ok` and `Err` tags), but I found:
**try_undefined_tag.md:1:1:1:2:**
```roc
A?
```
^

This expression has type:

    [A, .._others]

__Tip:__ Maybe wrap a value using `Ok(value)` or `Err(value)`.

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
						(p-nominal-external (builtin)
							(p-applied-tag))))
				(value
					(e-lookup-local
						(p-assign (ident "#ok")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-nominal-external (builtin)
							(p-applied-tag))))
				(value
					(e-runtime-error (tag "return_outside_fn")))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
