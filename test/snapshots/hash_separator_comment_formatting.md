# META
~~~ini
description=Triple hash ### is a normal comment, not a doc comment
type=file:Foo.roc
~~~
# SOURCE
~~~roc
### This is NOT a doc comment
x = 5
~~~
# EXPECTED
MISSING MAIN! FUNCTION - hash_separator_comment_formatting.md:2:1:2:6
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**hash_separator_comment_formatting.md:2:1:2:6:**
```roc
x = 5
```
^^^^^


# TOKENS
~~~zig
LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))))
~~~
# FORMATTED
~~~roc
# ## This is NOT a doc comment
x = 5
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-num (value "5"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
