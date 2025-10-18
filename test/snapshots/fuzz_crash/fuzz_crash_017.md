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
UNRECOGNIZED SYNTAX - fuzz_crash_017.md:2:7:2:20
MISSING MAIN! FUNCTION - fuzz_crash_017.md:1:1:2:20
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `string_expected_close_interpolation`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_017.md:2:7:2:8:**
```roc
foo = "hello ${namF
```
      ^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**fuzz_crash_017.md:2:7:2:20:**
```roc
foo = "hello ${namF
```
      ^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_017.md:1:1:2:20:**
```roc
me = "luc"
foo = "hello ${namF
```


# TOKENS
~~~zig
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "me"))
			(e-string
				(e-string-part (raw "luc"))))
		(s-decl
			(p-ident (raw "foo"))
			(e-malformed (reason "string_expected_close_interpolation")))))
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
		(p-assign (ident "me"))
		(e-string
			(e-literal (string "luc"))))
	(d-let
		(p-assign (ident "foo"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str"))
		(patt (type "Error")))
	(expressions
		(expr (type "Str"))
		(expr (type "Error"))))
~~~
