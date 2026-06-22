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
┌─ This add_checked method is being called on a value whose ──┤ MISSING METHOD │
│  type doesn't have that method:                             └───────────────┬┘
│                                                                             │
│  r = "a"..<"z"                                                              │
│      ‾‾‾‾‾‾‾‾‾                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
    range_missing_method_error.md:1:5

    The value's type, which does not have a method named add_checked, is:

        Str

    Hint: For this to work, the type would need to have a method named
    add_checked associated with it in the type's declaration.
                                                              ┌────────────────┐
┌─ This from_numeral method is being called on a value whose ─┤ MISSING METHOD │
│  type doesn't have that method:                             └───────────────┬┘
│                                                                             │
│  r = "a"..<"z"                                                              │
│      ‾‾‾‾‾‾‾‾‾                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
    range_missing_method_error.md:1:5

    The value's type, which does not have a method named from_numeral, is:

        Str

    Hint: For this to work, the type would need to have a method named
    from_numeral associated with it in the type's declaration.
                                                              ┌────────────────┐
┌─ This is_lt method is being called on a value whose type ───┤ MISSING METHOD │
│  doesn't have that method:                                  └───────────────┬┘
│                                                                             │
│  r = "a"..<"z"                                                              │
│      ‾‾‾‾‾‾‾‾‾                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
    range_missing_method_error.md:1:5

    The value's type, which does not have a method named is_lt, is:

        Str

    Hint: For this to work, the type would need to have a method named is_lt
    associated with it in the type's declaration.
                                                              ┌────────────────┐
┌─ This steps_between method is being called on a value ──────┤ MISSING METHOD │
│  whose type doesn't have that method:                       └───────────────┬┘
│                                                                             │
│  r = "a"..<"z"                                                              │
│      ‾‾‾‾‾‾‾‾‾                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
    range_missing_method_error.md:1:5

    The value's type, which does not have a method named steps_between, is:

        Str

    Hint: For this to work, the type would need to have a method named
    steps_between associated with it in the type's declaration.
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
		(e-call (constraint-fn-var 164)
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
