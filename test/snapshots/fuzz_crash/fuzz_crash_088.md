# META
~~~ini
description=canonicalize invariant panic: constant root has no top-level value entry
type=file
source_escapes=true
~~~
# SOURCE
~~~roc
main!=|0|""_=""
~~~
# EXPECTED
NON EXHAUSTIVE DESTRUCTURE - fuzz_crash_088.md:1:8:1:9
# PROBLEMS

┌────────────────────────────┐
│ NON EXHAUSTIVE DESTRUCTURE ├─ This destructuring pattern doesn't cover ─────┐
└┬───────────────────────────┘  all possible cases.                           │
 │                                                                            │
 │  main!=|0|""_=""                                                           │
 │         ‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_088.md:1:8 ┘

    The value being destructured has type:
            a
      where [
        a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
        a.is_eq : a, a -> Bool,
      ]

    Missing patterns:
            _

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,Int,OpBar,StringStart,StringPart,StringEnd,Underscore,OpAssign,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-int (raw "0")))
				(e-string
					(e-string-part (raw "")))))
		(s-decl
			(p-underscore)
			(e-string
				(e-string-part (raw ""))))))
~~~
# FORMATTED
~~~roc
main! = |0| ""

_ = ""
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "echo!"))
		(e-hosted-lambda (symbol "echo!")
			(args
				(p-assign (ident "_echo_arg"))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-record))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-num (value "0")))
			(e-string
				(e-literal (string "")))))
	(d-let
		(p-underscore)
		(e-string
			(e-literal (string "")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "a -> b where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_eq : a, a -> Bool, b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)])]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "a -> b where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_eq : a, a -> Bool, b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)])]"))
		(expr (type "Str"))))
~~~
