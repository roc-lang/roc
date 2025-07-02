# META
~~~ini
description=Example if-then-else statement with a tag expression
type=expr
~~~
# SOURCE
~~~roc
if True Ok(0) else Err(1)
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwIf(1:1-1:3),UpperIdent(1:4-1:8),UpperIdent(1:9-1:11),NoSpaceOpenRound(1:11-1:12),Int(1:12-1:13),CloseRound(1:13-1:14),KwElse(1:15-1:19),UpperIdent(1:20-1:23),NoSpaceOpenRound(1:23-1:24),Int(1:24-1:25),CloseRound(1:25-1:26),EndOfFile(1:26-1:26),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-1.26
	(e-tag @1.4-1.8 (raw "True"))
	(e-apply @1.9-1.14
		(e-tag @1.9-1.11 (raw "Ok"))
		(e-int @1.12-1.13 (raw "0")))
	(e-apply @1.20-1.26
		(e-tag @1.20-1.23 (raw "Err"))
		(e-int @1.24-1.25 (raw "1"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-1.26
	(if-branches
		(if-branch
			(e-tag @1.4-1.8 (name "True") (args "TODO"))
			(e-call @1.9-1.14
				(e-tag @1.9-1.11 (name "Ok") (args "TODO"))
				(e-int @1.12-1.13 (value "0")))))
	(if-else
		(e-call @1.20-1.26
			(e-tag @1.20-1.23 (name "Err") (args "TODO"))
			(e-int @1.24-1.25 (value "1")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.26 (type "*"))
~~~
