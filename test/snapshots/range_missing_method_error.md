# META
~~~ini
description=Range over non-numeric type reports a missing range_exclusive method
type=snippet
~~~
# SOURCE
~~~roc
r = "a"..<"z"
~~~
# EXPECTED
MISSING METHOD - range_missing_method_error.md:1:5:1:14
# PROBLEMS

┌────────────────┐
│ MISSING METHOD ├─ The value before this `..<` operator has a type that ─────┐
└┬───────────────┘  doesn't have a `range_exclusive` method.                  │
 │                                                                            │
 │  r = "a"..<"z"                                                             │
 │      ‾‾‾‾‾‾‾‾‾                                                             │
 └───────────────────────────────────────── range_missing_method_error.md:1:5 ┘

    The value's type, which does not have a method named `range_exclusive`, is:

        Str

    Hint: The `..<` operator calls a method named `range_exclusive` on the
    value preceding it, passing the value after the operator as the one
    argument.

# TOKENS
~~~zig
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,OpDoubleDotLessThan,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "r"))
			(e-binop (op "..<")
				(e-string
					(e-string-part (raw "a")))
				(e-string
					(e-string-part (raw "z")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
		(e-dispatch-call (method "range_exclusive") (constraint-fn-var 198)
			(receiver
				(e-string
					(e-literal (string "a"))))
			(args
				(e-string
					(e-literal (string "z")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
