# META
~~~ini
description=Example if-then-else statement with a tag expression
type=expr
~~~
# SOURCE
~~~roc
if Bool.True Ok(0) else Err(1)
~~~
# EXPECTED
INVALID NOMINAL TAG - if_then_else_simple_tag.md:1:4:1:13
# PROBLEMS
**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**if_then_else_simple_tag.md:1:4:1:13:**
```roc
if Bool.True Ok(0) else Err(1)
```
   ^^^^^^^^^

The tag is:
    _True_

But the nominal type needs it to be:
    _EmptyDict_

# TOKENS
~~~zig
KwIf(1:1-1:3),UpperIdent(1:4-1:8),NoSpaceDotUpperIdent(1:8-1:13),UpperIdent(1:14-1:16),NoSpaceOpenRound(1:16-1:17),Int(1:17-1:18),CloseRound(1:18-1:19),KwElse(1:20-1:24),UpperIdent(1:25-1:28),NoSpaceOpenRound(1:28-1:29),Int(1:29-1:30),CloseRound(1:30-1:31),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-1.31
	(e-tag @1.4-1.13 (raw "Bool.True"))
	(e-apply @1.14-1.19
		(e-tag @1.14-1.16 (raw "Ok"))
		(e-int @1.17-1.18 (raw "0")))
	(e-apply @1.25-1.31
		(e-tag @1.25-1.28 (raw "Err"))
		(e-int @1.29-1.30 (raw "1"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-1.31
	(if-branches
		(if-branch
			(e-nominal-external @1.4-1.13
				(module-idx "2")
				(target-node-idx "1")
				(e-tag @1.4-1.13 (name "True")))
			(e-tag @1.14-1.19 (name "Ok")
				(args
					(e-num @1.17-1.18 (value "0"))))))
	(if-else
		(e-tag @1.25-1.31 (name "Err")
			(args
				(e-num @1.29-1.30 (value "1"))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.31 (type "[Ok(Num(_size)), Err(Num(_size2))]_others"))
~~~
