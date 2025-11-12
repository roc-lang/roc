# META
~~~ini
description=if_then_else (3)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	A
} else 2
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_simple_block_formatting.md:1:4:1:8
TYPE DOES NOT HAVE METHODS - if_then_else_simple_block_formatting.md:3:8:3:9
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `bool` in this scope.
Is there an `import` or `exposing` missing up-top?

**if_then_else_simple_block_formatting.md:1:4:1:8:**
```roc
if bool {
```
   ^^^^


**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_int_digits` on a type that doesn't support methods:
**if_then_else_simple_block_formatting.md:3:8:3:9:**
```roc
} else 2
```
       ^

This type doesn't support methods:
    _[A]_others_



# TOKENS
~~~zig
KwIf,LowerIdent,OpenCurly,
UpperIdent,
CloseCurly,KwElse,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-ident (raw "bool"))
	(e-block
		(statements
			(e-tag (raw "A"))))
	(e-int (raw "2")))
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
				(e-tag (name "A")))))
	(if-else
		(e-num (value "2"))))
~~~
# TYPES
~~~clojure
(expr (type "[A]_others"))
~~~
