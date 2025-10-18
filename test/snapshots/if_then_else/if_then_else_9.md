# META
~~~ini
description=if_then_else (9)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	1
} else if 10 { # Comment after else open
	A
} else { # Comment after else open
	3
}
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_9.md:1:4:1:8
INVALID IF CONDITION - if_then_else_9.md:3:11:3:11
INCOMPATIBLE IF BRANCHES - if_then_else_9.md:1:1:1:1
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `bool` in this scope.
Is there an `import` or `exposing` missing up-top?

**if_then_else_9.md:1:4:1:8:**
```roc
if bool {
```
   ^^^^


**INVALID IF CONDITION**
This `if` condition needs to be a _Bool_:
**if_then_else_9.md:3:11:**
```roc
} else if 10 { # Comment after else open
```
          ^^

Right now, it has the type:
    _Num(_size)_

Every `if` condition must evaluate to a _Bool_–either `True` or `False`.

**INCOMPATIBLE IF BRANCHES**
The type of the second branch of this `if` does not match the previous branches:
**if_then_else_9.md:1:1:**
```roc
if bool {
	1
} else if 10 { # Comment after else open
	A
} else { # Comment after else open
	3
}
```
 ^

The second branch has this type:
    _[A]_others_

But the previous branch has this type:
    _Num(_size)_

All branches in an `if` must have compatible types.

Note: You can wrap branches in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

# TOKENS
~~~zig
KwIf,LowerIdent,OpenCurly,
Int,
CloseCurly,KwElse,KwIf,Int,OpenCurly,
UpperIdent,
CloseCurly,KwElse,OpenCurly,
Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-ident (raw "bool"))
	(e-block
		(statements
			(e-int (raw "1"))))
	(e-if-then-else
		(e-int (raw "10"))
		(e-block
			(statements
				(e-tag (raw "A"))))
		(e-block
			(statements
				(e-int (raw "3"))))))
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
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-block
				(e-num (value "1"))))
		(if-branch
			(e-num (value "10"))
			(e-block
				(e-tag (name "A")))))
	(if-else
		(e-block
			(e-num (value "3")))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
