# META
~~~ini
description=A simple mod with no blanks
type=snippet
~~~
# SOURCE
~~~roc
import pf.Stdout
hello! = Stdout.line!("Hello")

world = "World"
~~~
# EXPECTED
DOES NOT EXIST - simple_mod_no_blanks.md:2:17:2:22
# PROBLEMS

┌────────────────┐
│ DOES NOT EXIST ├─ `line!` was not found in `Stdout`. ───────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  hello! = Stdout.line!("Hello")                                            │
 │                  ‾‾‾‾‾                                                     │
 └─────────────────────────────────────────── simple_mod_no_blanks.md:2:17 ┘

    Check that `line!` is spelled correctly and that `Stdout` exposes it.

# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "pf.Stdout"))
		(s-decl
			(p-ident (raw "hello!"))
			(e-apply
				(e-ident (raw "Stdout.line!"))
				(e-string
					(e-string-part (raw "Hello")))))
		(s-decl
			(p-ident (raw "world"))
			(e-string
				(e-string-part (raw "World"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "hello!"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "world"))
		(e-string
			(e-literal (string "World"))))
	(s-import (mod "pf.Stdout")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Str")))
	(expressions
		(expr (type "Error"))
		(expr (type "Str"))))
~~~
