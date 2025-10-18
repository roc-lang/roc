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
UNDECLARED TYPE - if_then_else_simple_tag.md:1:4:1:8
# PROBLEMS
**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**if_then_else_simple_tag.md:1:4:1:8:**
```roc
if Bool.True Ok(0) else Err(1)
```
   ^^^^


# TOKENS
~~~zig
KwIf,UpperIdent,NoSpaceDotUpperIdent,UpperIdent,NoSpaceOpenRound,Int,CloseRound,KwElse,UpperIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-tag (raw "Bool.True"))
	(e-apply
		(e-tag (raw "Ok"))
		(e-int (raw "0")))
	(e-apply
		(e-tag (raw "Err"))
		(e-int (raw "1"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if
	(if-branches
		(if-branch
			(e-runtime-error (tag "undeclared_type"))
			(e-tag (name "Ok")
				(args
					(e-num (value "0"))))))
	(if-else
		(e-tag (name "Err")
			(args
				(e-num (value "1"))))))
~~~
# TYPES
~~~clojure
(expr (type "[Ok(Num(_size)), Err(Num(_size2))]_others"))
~~~
