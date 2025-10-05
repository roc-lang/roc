# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
me = "luc"
foo = "hello ${namF
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_017.md:2:7:2:8
TYPE MODULE MISSING MATCHING TYPE - fuzz_crash_017.md:1:1:2:20
UNRECOGNIZED SYNTAX - fuzz_crash_017.md:2:7:2:20
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_017.md:2:7:2:8:**
```roc
foo = "hello ${namF
```
      ^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `fuzz_crash_017`.roc, but no top-level type declaration named `fuzz_crash_017` was found.

Add either:
`fuzz_crash_017 := ...` (nominal type)
or:
`fuzz_crash_017 : ...` (type alias)
**fuzz_crash_017.md:1:1:2:20:**
```roc
me = "luc"
foo = "hello ${namF
```


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**fuzz_crash_017.md:2:7:2:20:**
```roc
foo = "hello ${namF
```
      ^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
LowerIdent(1:1-1:3),OpAssign(1:4-1:5),StringStart(1:6-1:7),StringPart(1:7-1:10),StringEnd(1:10-1:11),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),StringStart(2:7-2:8),StringPart(2:8-2:14),OpenStringInterpolation(2:14-2:16),LowerIdent(2:16-2:20),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.20
	(type-module @1.1-1.3)
	(statements
		(s-decl @1.1-1.11
			(p-ident @1.1-1.3 (raw "me"))
			(e-string @1.6-1.11
				(e-string-part @1.7-1.10 (raw "luc"))))
		(s-decl @2.1-2.20
			(p-ident @2.1-2.4 (raw "foo"))
			(e-malformed @2.7-2.20 (reason "string_expected_close_interpolation")))))
~~~
# FORMATTED
~~~roc
me = "luc"
foo = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.3 (ident "me"))
		(e-string @1.6-1.11
			(e-literal @1.7-1.10 (string "luc"))))
	(d-let
		(p-assign @2.1-2.4 (ident "foo"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.3 (type "Str"))
		(patt @2.1-2.4 (type "Error")))
	(expressions
		(expr @1.6-1.11 (type "Str"))
		(expr @2.7-2.20 (type "Error"))))
~~~
