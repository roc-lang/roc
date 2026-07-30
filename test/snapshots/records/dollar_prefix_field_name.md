# META
~~~ini
description=Dollar-prefixed record field names are rejected
type=expr
~~~
# SOURCE
~~~roc
{ $field : "value" }
~~~
# EXPECTED
INVALID RECORD FIELD NAME - dollar_prefix_field_name.md:1:3:1:9
# PROBLEMS

┌───────────────────────────┐
│ INVALID RECORD FIELD NAME ├─ Record field names cannot start with a ────────┐
└┬──────────────────────────┘  dollar sign.                                   │
 │                                                                            │
 │  { $field : "value" }                                                      │
 │    ‾‾‾‾‾‾                                                                  │
 └─────────────────────────────────────────── dollar_prefix_field_name.md:1:3 ┘

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
	(field (field "$field")
		(e-string
			(e-string-part (raw "value")))))
~~~
# FORMATTED
~~~roc
{ $field: "value" }
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "$field")
			(e-string
				(e-literal (string "value"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ $field: Str }"))
~~~
