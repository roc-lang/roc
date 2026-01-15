# META
~~~ini
description=Test error for for statement at module top level
type=file
~~~
# SOURCE
~~~roc
module [x]

for item in [1, 2, 3] {
    item
}

x = 1
~~~
# EXPECTED
MODULE HEADER DEPRECATED - can_error_invalid_top_level_for.md:1:1:1:11
INVALID STATEMENT - can_error_invalid_top_level_for.md:3:1:5:2
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**can_error_invalid_top_level_for.md:1:1:1:11:**
```roc
module [x]
```
^^^^^^^^^^


**INVALID STATEMENT**
The statement `for` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**can_error_invalid_top_level_for.md:3:1:5:2:**
```roc
for item in [1, 2, 3] {
    item
}
```


# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,OpenCurly,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-lower-ident
				(text "x"))))
	(statements
		(s-for
			(p-ident (raw "item"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))
				(e-int (raw "3")))
			(e-block
				(statements
					(e-ident (raw "item")))))
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "1")))))
~~~
# FORMATTED
~~~roc
module [x]

for item in [1, 2, 3] {
	item
}

x = 1
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-num (value "1"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
