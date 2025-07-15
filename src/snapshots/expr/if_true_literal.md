# META
~~~ini
description=If expression with True boolean literal
type=expr
~~~
# SOURCE
~~~roc
if True 1 else 2
~~~
# EXPECTED
INVALID IF CONDITION - if_true_literal.md:1:4:1:4
# PROBLEMS
**INVALID IF CONDITION**
This `if` condition needs to be a _Bool_:
**if_true_literal.md:1:4:**
```roc
if True 1 else 2
```
   ^^^^

Right now, it has the type:
    _[True]_others_

Every `if` condition must evaluate to a _Bool_–either `True` or `False`.

# TOKENS
~~~zig
KwIf(1:1-1:3),UpperIdent(1:4-1:8),Int(1:9-1:10),KwElse(1:11-1:15),Int(1:16-1:17),EndOfFile(1:17-1:17),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-1.17
	(e-tag @1.4-1.8 (raw "True"))
	(e-int @1.9-1.10 (raw "1"))
	(e-int @1.16-1.17 (raw "2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-1.17
	(if-branches
		(if-branch
			(e-tag @1.4-1.8 (name "True"))
			(e-int @1.9-1.10 (value "1"))))
	(if-else
		(e-int @1.16-1.17 (value "2"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.17 (type "Num(_size)"))
~~~
