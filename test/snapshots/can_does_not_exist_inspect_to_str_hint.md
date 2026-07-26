# META
~~~ini
description=Hint for Inspect.to_str renamed to Str.inspect
type=snippet
~~~
# SOURCE
~~~roc
result = Inspect.to_str("hello")
~~~
# EXPECTED
DOES NOT EXIST - can_does_not_exist_inspect_to_str_hint.md:1:10:1:24
# PROBLEMS

┌────────────────┐
│ DOES NOT EXIST ├─ `Inspect.to_str` does not exist. ─────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  result = Inspect.to_str("hello")                                          │
 │           ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                   │
 └──────────────────────────── can_does_not_exist_inspect_to_str_hint.md:1:10 ┘

    Hint: `Inspect.to_str` has been renamed to `Str.inspect`.

# TOKENS
~~~zig
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "Inspect.to_str"))
				(e-string
					(e-string-part (raw "hello")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "result"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
