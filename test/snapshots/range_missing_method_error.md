# META
~~~ini
description=Range over non-numeric type reports missing constructor constraints
type=snippet
~~~
# SOURCE
~~~roc
r = "a"..<"z"
~~~
# EXPECTED
MISSING METHOD - range_missing_method_error.md:1:5:1:14
MISSING METHOD - range_missing_method_error.md:1:5:1:14
MISSING METHOD - range_missing_method_error.md:1:5:1:14
MISSING METHOD - range_missing_method_error.md:1:5:1:14
# PROBLEMS

┌────────────────┐
│ MISSING METHOD ├─ This `add_try` method is being called on a value whose ───┐
└┬───────────────┘  type doesn't have that method.                            │
 │                                                                            │
 │  r = "a"..<"z"                                                             │
 │      ‾‾‾‾‾‾‾‾‾                                                             │
 └───────────────────────────────────────── range_missing_method_error.md:1:5 ┘

    The value's type, which does not have a method named `add_try`, is:

        Str

    Hint: For this to work, the type would need to have a method named
    `add_try` associated with it in the type's declaration.


┌────────────────┐
│ MISSING METHOD ├─ This `from_numeral` method is being called on a value ────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  r = "a"..<"z"                                                             │
 │      ‾‾‾‾‾‾‾‾‾                                                             │
 └───────────────────────────────────────── range_missing_method_error.md:1:5 ┘

    The value's type, which does not have a method named `from_numeral`, is:

        Str

    Hint: For this to work, the type would need to have a method named
    `from_numeral` associated with it in the type's declaration.


┌────────────────┐
│ MISSING METHOD ├─ This `is_lt` method is being called on a value whose ─────┐
└┬───────────────┘  type doesn't have that method.                            │
 │                                                                            │
 │  r = "a"..<"z"                                                             │
 │      ‾‾‾‾‾‾‾‾‾                                                             │
 └───────────────────────────────────────── range_missing_method_error.md:1:5 ┘

    The value's type, which does not have a method named `is_lt`, is:

        Str

    Hint: For this to work, the type would need to have a method named `is_lt`
    associated with it in the type's declaration.


┌────────────────┐
│ MISSING METHOD ├─ This `steps_between` method is being called on a value ───┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  r = "a"..<"z"                                                             │
 │      ‾‾‾‾‾‾‾‾‾                                                             │
 └───────────────────────────────────────── range_missing_method_error.md:1:5 ┘

    The value's type, which does not have a method named `steps_between`, is:

        Str

    Hint: For this to work, the type would need to have a method named
    `steps_between` associated with it in the type's declaration.

# TOKENS
~~~zig
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,OpDoubleDotLessThan,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
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
		(e-call (constraint-fn-var 170)
			(e-lookup-external
				(builtin))
			(e-string
				(e-literal (string "a")))
			(e-string
				(e-literal (string "z"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Iter(Str)")))
	(expressions
		(expr (type "Iter(Str)"))))
~~~
