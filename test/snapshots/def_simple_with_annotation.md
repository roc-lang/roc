# META
~~~ini
description=Simple definition with type annotation
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo : Str
foo = "one"
~~~
# EXPECTED
MODULE HEADER DEPRECATED - def_simple_with_annotation.md:1:1:1:13
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**def_simple_with_annotation.md:1:1:1:13:**
```roc
module [foo]
```
^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),UpperIdent(3:7-3:10),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),StringStart(4:7-4:8),StringPart(4:8-4:11),StringEnd(4:11-4:12),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.12
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident @1.9-1.12
				(text "foo"))))
	(statements
		(s-type-anno @3.1-3.10 (name "foo")
			(ty @3.7-3.10 (name "Str")))
		(s-decl @4.1-4.12
			(p-ident @4.1-4.4 (raw "foo"))
			(e-string @4.7-4.12
				(e-string-part @4.8-4.11 (raw "one"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.4 (ident "foo"))
		(e-string @4.7-4.12
			(e-literal @4.8-4.11 (string "one")))
		(annotation @4.1-4.4
			(declared-type
				(ty-lookup @3.7-3.10 (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "Str")))
	(expressions
		(expr @4.7-4.12 (type "Str"))))
~~~
