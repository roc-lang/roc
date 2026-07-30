# META
~~~ini
description=canonicalize crash: missing checked pattern for exhaustiveness site
type=file
~~~
# SOURCE
~~~roc
main!=|0|||"".P
~~~
# EXPECTED
NON EXHAUSTIVE DESTRUCTURE - fuzz_crash_108.md:1:8:1:9
# PROBLEMS

┌────────────────────────────┐
│ NON EXHAUSTIVE DESTRUCTURE ├─ This destructuring pattern doesn't cover ─────┐
└┬───────────────────────────┘  all possible cases.                           │
 │                                                                            │
 │  main!=|0|||"".P                                                           │
 │         ‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_108.md:1:8 ┘

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
LowerIdent,OpAssign,OpBar,Int,OpBar,OpBar,OpBar,StringStart,StringPart,StringEnd,NoSpaceDotUpperIdent,
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
				(e-lambda
					(args)
					(e-typed-string (type "P")
						(e-string-part (raw ""))))))))
~~~
# FORMATTED
~~~roc
main! = |0| || "".P
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
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "a -> (({}) -> Error) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_eq : a, a -> Bool]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "a -> (({}) -> Error) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_eq : a, a -> Bool]"))))
~~~
