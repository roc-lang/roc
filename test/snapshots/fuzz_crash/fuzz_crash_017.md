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
EXPECTED INTERPOLATION END - fuzz_crash_017.md:2:7:2:8
UNRECOGNIZED SYNTAX - fuzz_crash_017.md:2:7:2:20
# PROBLEMS

┌────────────────────────────┐
│ EXPECTED INTERPOLATION END ├─ I was parsing a string interpolation, and I ──┐
└┬───────────────────────────┘  expected `}` before returning to the string.  │
 │                                                                            │
 │  foo = "hello ${namF                                                       │
 │        ‾                                                                   │
 └───────────────────────────────────────────────────── fuzz_crash_017.md:2:7 ┘

    String interpolations start with `${` and must close with `}` after the
    embedded expression.

    For example:
        "Hello, ${name}!"

    I found `"` here.


┌─────────────────────┐
│ UNRECOGNIZED SYNTAX ├─ I don't recognize this syntax. ──────────────────────┐
└┬────────────────────┘                                                       │
 │                                                                            │
 │  foo = "hello ${namF                                                       │
 │        ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_017.md:2:7 ┘

    This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
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
