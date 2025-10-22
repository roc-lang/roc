# META
~~~ini
description=Example if-then-else statement
type=snippet
~~~
# SOURCE
~~~roc
foo = if 1 A

    else {
	"hello"
    }
~~~
# EXPECTED
INVALID IF CONDITION - if_then_else_simple_file.md:1:10:1:10
# PROBLEMS
**INVALID IF CONDITION**
This `if` condition needs to be a _Bool_:
**if_then_else_simple_file.md:1:10:**
```roc
foo = if 1 A
```
         ^

Right now, it has the type:
    _Num(_size)_

Every `if` condition must evaluate to a _Bool_–either `True` or `False`.

# TOKENS
~~~zig
LowerIdent,OpAssign,KwIf,Int,UpperIdent,
KwElse,OpenCurly,
StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-if-then-else
				(e-int (raw "1"))
				(e-tag (raw "A"))
				(e-block
					(statements
						(e-string
							(e-string-part (raw "hello")))))))))
~~~
# FORMATTED
~~~roc
foo = if 1 A

	else {
		"hello"
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-if
			(if-branches
				(if-branch
					(e-num (value "1"))
					(e-tag (name "A"))))
			(if-else
				(e-block
					(e-string
						(e-literal (string "hello"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
