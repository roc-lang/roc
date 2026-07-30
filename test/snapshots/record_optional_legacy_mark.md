# META
~~~ini
description=Legacy optional field marker (:?) recovers with a did-you-mean diagnostic and formats to ?:
type=snippet
~~~
# SOURCE
~~~roc
value : { x : U32, y :? U32, z : ? U32 }
value = { x: 1 }
~~~
# EXPECTED
INVALID OPTIONAL FIELD SYNTAX - record_optional_legacy_mark.md:1:23:1:24
INVALID OPTIONAL FIELD SYNTAX - record_optional_legacy_mark.md:1:34:1:35
# PROBLEMS

┌───────────────────────────────┐
│ INVALID OPTIONAL FIELD SYNTAX ├─ I was parsing a record type, and this ─────┐
└┬──────────────────────────────┘  optional field puts the `?` after the      │
 │                                 `:`.                                       │
 │                                                                            │
 │  value : { x : U32, y :? U32, z : ? U32 }                                  │
 │                        ‾                                                   │
 └─────────────────────────────────────── record_optional_legacy_mark.md:1:23 ┘

    Optional fields are written with the `?` before the `:`: `?:` declares the
    field optional.

    For example:
        { name ?: Str }


┌───────────────────────────────┐
│ INVALID OPTIONAL FIELD SYNTAX ├─ I was parsing a record type, and this ─────┐
└┬──────────────────────────────┘  optional field puts the `?` after the      │
 │                                 `:`.                                       │
 │                                                                            │
 │  value : { x : U32, y :? U32, z : ? U32 }                                  │
 │                                   ‾                                        │
 └─────────────────────────────────────── record_optional_legacy_mark.md:1:34 ┘

    Optional fields are written with the `?` before the `:`: `?:` declares the
    field optional.

    For example:
        { name ?: Str }

# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,NoSpaceOpQuestion,UpperIdent,Comma,LowerIdent,OpColon,OpQuestion,UpperIdent,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "value")
			(ty-record
				(anno-record-field (name "x")
					(ty (name "U32")))
				(anno-record-field (name "y") (optional true)
					(ty (name "U32")))
				(anno-record-field (name "z") (optional true)
					(ty (name "U32")))))
		(s-decl
			(p-ident (raw "value"))
			(e-record
				(field (field "x")
					(e-int (raw "1")))))))
~~~
# FORMATTED
~~~roc
value : { x : U32, y ?: U32, z ?: U32 }
value = { x: 1 }
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "value"))
		(e-record
			(fields
				(field (name "x")
					(e-num (value "1")))))
		(annotation
			(ty-record
				(field (field "x")
					(ty-lookup (name "U32") (builtin)))
				(field (field "y") (optional true)
					(ty-lookup (name "U32") (builtin)))
				(field (field "z") (optional true)
					(ty-lookup (name "U32") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ x: U32, y ?: U32, z ?: U32 }")))
	(expressions
		(expr (type "{ x: U32, y ?: U32, z ?: U32 }"))))
~~~
