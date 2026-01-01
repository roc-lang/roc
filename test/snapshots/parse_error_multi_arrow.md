# META
~~~ini
description=Function with multiple arrows needing parens
type=file
~~~
# SOURCE
~~~roc
module [main]

foo : A -> B -> C
~~~
# EXPECTED
PARSE ERROR - parse_error_multi_arrow.md:3:14:3:16
PARSE ERROR - parse_error_multi_arrow.md:4:1:4:1
MODULE HEADER DEPRECATED - parse_error_multi_arrow.md:1:1:1:14
UNDECLARED TYPE - parse_error_multi_arrow.md:3:7:3:8
UNDECLARED TYPE - parse_error_multi_arrow.md:3:12:3:13
EXPOSED BUT NOT DEFINED - parse_error_multi_arrow.md:1:9:1:13
# PROBLEMS
**PARSE ERROR**
Function types with multiple arrows need parentheses.

Instead of writing **a -> b -> c**, use parentheses to clarify which you mean:
        a -> (b -> c) for a **curried** function (a function that **returns** another function)
        (a -> b) -> c for a **higher-order** function (a function that **takes** another function)

**parse_error_multi_arrow.md:3:14:3:16:**
```roc
foo : A -> B -> C
```
             ^^


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

**parse_error_multi_arrow.md:4:1:4:1:**
```roc

```
^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**parse_error_multi_arrow.md:1:1:1:14:**
```roc
module [main]
```
^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _A_ is not declared in this scope.

This type is referenced here:
**parse_error_multi_arrow.md:3:7:3:8:**
```roc
foo : A -> B -> C
```
      ^


**UNDECLARED TYPE**
The type _B_ is not declared in this scope.

This type is referenced here:
**parse_error_multi_arrow.md:3:12:3:13:**
```roc
foo : A -> B -> C
```
           ^


**EXPOSED BUT NOT DEFINED**
The module header says that `main` is exposed, but it is not defined anywhere in this module.

**parse_error_multi_arrow.md:1:9:1:13:**
```roc
module [main]
```
        ^^^^
You can fix this by either defining `main` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,OpArrow,UpperIdent,
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
		(s-type-anno (name "foo")
			(ty-fn
				(ty (name "A"))
				(ty (name "B"))))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
module [main]

foo : A -> B

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-malformed)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Error")))
	(expressions
		(expr (type "Error -> Error"))))
~~~
