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
MISSING METHOD - if_then_else_simple_file.md:1:10:1:11
TYPE DOES NOT HAVE METHODS - if_then_else_simple_file.md:4:3:4:8
# PROBLEMS
**MISSING METHOD**
This **from_numeral** method is being called on the type **Bool**, which has no method with that name:
**if_then_else_simple_file.md:1:10:1:11:**
```roc
foo = if 1 A
```
         ^


**Hint: **For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

**TYPE DOES NOT HAVE METHODS**
You're calling the method `try_from_str` on a type that doesn't support methods:
**if_then_else_simple_file.md:4:3:4:8:**
```roc
	"hello"
```
	 ^^^^^

This type doesn't support methods:
    _[A]_others_



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
		(patt (type "[A]_others")))
	(expressions
		(expr (type "[A]_others"))))
~~~
