# META
~~~ini
description=Import not at top level
type=file
~~~
# SOURCE
~~~roc
module [main]

main = {
    import "foo"
}
~~~
# EXPECTED
IMPORT MUST BE TOP LEVEL - parse_error_import_not_top.md:4:5:4:11
MODULE HEADER DEPRECATED - parse_error_import_not_top.md:1:1:1:14
# PROBLEMS
**IMPORT MUST BE TOP LEVEL**
Import statements must appear at the top level of a module.
Move this import to the top of the file, after the module header but before any definitions.

**parse_error_import_not_top.md:4:5:4:11:**
```roc
    import "foo"
```
    ^^^^^^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**parse_error_import_not_top.md:1:1:1:14:**
```roc
module [main]
```
^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpenCurly,
KwImport,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-lower-ident
				(text "main"))))
	(statements
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-malformed (tag "import_must_be_top_level"))
					(e-string
						(e-string-part (raw "foo"))))))))
~~~
# FORMATTED
~~~roc
module [main]

main = {
		"foo"
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-block
			(e-string
				(e-literal (string "foo"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str")))
	(expressions
		(expr (type "Str"))))
~~~
