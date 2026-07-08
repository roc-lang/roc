# META
~~~ini
description=Dollar-prefixed type record field names are rejected
type=statement
~~~
# SOURCE
~~~roc
Person : { $name : Str }
~~~
# EXPECTED
INVALID RECORD FIELD NAME - error_dollar_prefix_type_field.md:1:12:1:17
# PROBLEMS

┌───────────────────────────┐
│ INVALID RECORD FIELD NAME ├─ Record field names cannot start with a ────────┐
└┬──────────────────────────┘  dollar sign.                                   │
 │                                                                            │
 │  Person : { $name : Str }                                                  │
 │             ‾‾‾‾‾                                                          │
 └──────────────────────────────────── error_dollar_prefix_type_field.md:1:12 ┘

    Names that start with `$` are reassignable variables declared with the
    `var` keyword, so they cannot be used as record field names.

# TOKENS
~~~zig
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-type-decl
	(header (name "Person")
		(args))
	(ty-record
		(anno-record-field (name "$name")
			(ty (name "Str")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "Person"))
		(ty-record
			(field (field "$name")
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Person")
			(ty-header (name "Person"))))
	(expressions))
~~~
