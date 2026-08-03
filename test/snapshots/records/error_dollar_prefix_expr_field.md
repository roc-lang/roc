# META
~~~ini
description=Dollar-prefixed expression record field names are rejected
type=expr
~~~
# SOURCE
~~~roc
{ $name: "Ada" }
~~~
# EXPECTED
INVALID RECORD FIELD NAME - error_dollar_prefix_expr_field.md:1:3:1:8
# PROBLEMS

┌───────────────────────────┐
│ INVALID RECORD FIELD NAME ├─ Record field names cannot start with a ────────┐
└┬──────────────────────────┘  dollar sign.                                   │
 │                                                                            │
 │  { $name: "Ada" }                                                          │
 │    ‾‾‾‾‾                                                                   │
 └───────────────────────────────────── error_dollar_prefix_expr_field.md:1:3 ┘

    Names that start with `$` are reassignable variables declared with the
    `var` keyword, so they cannot be used as record field names.

# TOKENS
~~~zig
OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "$name")
		(e-string
			(e-string-part (raw "Ada")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "$name")
			(e-string
				(e-literal (string "Ada"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ $name: Str }"))
~~~
