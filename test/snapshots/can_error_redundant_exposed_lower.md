# META
~~~ini
description=Test error for duplicate lower identifier in exposed list
type=file
~~~
# SOURCE
~~~roc
module [foo, foo]

foo = 42
~~~
# EXPECTED
MODULE HEADER DEPRECATED - can_error_redundant_exposed_lower.md:1:1:1:18
REDUNDANT EXPOSED - can_error_redundant_exposed_lower.md:1:14:1:17
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**can_error_redundant_exposed_lower.md:1:1:1:18:**
```roc
module [foo, foo]
```
^^^^^^^^^^^^^^^^^


**REDUNDANT EXPOSED**
The identifier `foo` is exposed multiple times in the module header.

**can_error_redundant_exposed_lower.md:1:14:1:17:**
```roc
module [foo, foo]
```
             ^^^
You can remove the duplicate entry to fix this warning.

# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,
LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-lower-ident
				(text "foo"))
			(exposed-lower-ident
				(text "foo"))))
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-int (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-num (value "42"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
