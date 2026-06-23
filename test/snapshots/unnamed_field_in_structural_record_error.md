# META
~~~ini
description=Unnamed field in a structural record type is rejected
type=snippet
~~~
# SOURCE
~~~roc
Bad : { x : U8, _ : U8 }
~~~
# EXPECTED
UNNAMED FIELD NOT ALLOWED IN STRUCTURAL RECORD - unnamed_field_in_structural_record_error.md:1:17:1:23
# PROBLEMS

┌────────────────────────────────────────────────┐
│ UNNAMED FIELD NOT ALLOWED IN STRUCTURAL RECORD ├─ Unnamed fields (written ──┐
└┬───────────────────────────────────────────────┘  `_` or `_name`) are       │
 │                                                  only allowed in nominal   │
 │                                                  record type               │
 │                                                  declarations, not in      │
 │                                                  structural record types.  │
 │                                                                            │
 │  Bad : { x : U8, _ : U8 }                                                  │
 │                  ‾‾‾‾‾‾                                                    │
 └────────────────────────── unnamed_field_in_structural_record_error.md:1:17 ┘

    Hint: Unnamed fields reserve layout padding for a nominal type (declared
    with `:=`). Give the field a name, or move it into a nominal type
    declaration.

# TOKENS
~~~zig
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,Underscore,OpColon,UpperIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Bad")
				(args))
			(ty-record
				(anno-record-field (name "x")
					(ty (name "U8")))
				(anno-record-field (name "_")
					(ty (name "U8")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "Bad"))
		(ty-record
			(field (field "x")
				(ty-lookup (name "U8") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Bad")
			(ty-header (name "Bad"))))
	(expressions))
~~~
