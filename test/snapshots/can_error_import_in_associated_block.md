# META
~~~ini
description=Test error for import inside associated block
type=file
~~~
# SOURCE
~~~roc
Foo := U64.{
    import Bar
}
~~~
# EXPECTED
IMPORT MUST BE TOP LEVEL - can_error_import_in_associated_block.md:2:5:2:11
PARSE ERROR - can_error_import_in_associated_block.md:3:1:3:2
PARSE ERROR - can_error_import_in_associated_block.md:4:1:4:1
TYPE MODULE MISSING MATCHING TYPE - can_error_import_in_associated_block.md:1:1:3:2
# PROBLEMS
**IMPORT MUST BE TOP LEVEL**
Import statements must appear at the top level of a module.
Move this import to the top of the file, after the module header but before any definitions.

**can_error_import_in_associated_block.md:2:5:2:11:**
```roc
    import Bar
```
    ^^^^^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Try(a, Str)`
    `Maybe(List(U64))`

**can_error_import_in_associated_block.md:3:1:3:2:**
```roc
}
```
^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly`
This is an unexpected parsing error. Please check your syntax.

**can_error_import_in_associated_block.md:4:1:4:1:**
```roc

```
^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a nominal type declaration matching the module name.

This file is named `can_error_import_in_associated_block`.roc, but no top-level nominal type named `can_error_import_in_associated_block` was found.

Add a nominal type like:
`can_error_import_in_associated_block := ...`
or:
`can_error_import_in_associated_block :: ...` (opaque nominal type)
**can_error_import_in_associated_block.md:1:1:3:2:**
```roc
Foo := U64.{
    import Bar
}
```


# TOKENS
~~~zig
UpperIdent,OpColonEqual,UpperIdent,Dot,OpenCurly,
KwImport,UpperIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty (name "U64"))
			(associated
				(s-malformed (tag "import_must_be_top_level"))
				(s-malformed (tag "expected_colon_after_type_annotation"))))))
~~~
# FORMATTED
~~~roc
Foo := U64.{
	
	
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-lookup (name "U64") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions))
~~~
