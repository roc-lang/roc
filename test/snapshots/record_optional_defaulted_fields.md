# META
~~~ini
description=Optional (?:) and defaulted (??) record fields through the full pipeline
type=snippet
~~~
# SOURCE
~~~roc
Config : { host : Str, port ?: U16, retries : U8 ?? 3 }

get_port : Config -> Try(U16, [MissingField])
get_port = |c| c.?port

mk_config : Str -> Config
mk_config = |host| { host: host }

bad_direct_access : Config -> U16
bad_direct_access = |c| c.port
~~~
# EXPECTED
TYPE MISMATCH - record_optional_defaulted_fields.md:10:26:10:31
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ The `port` field is optional, but it is being accessed ────┐
└┬──────────────┘  as if it is always present.                                │
 │                                                                            │
 │  bad_direct_access = |c| c.port                                            │
 │                           ‾‾‾‾‾                                            │
 └───────────────────────────────── record_optional_defaulted_fields.md:10:26 ┘

    An optional field may be missing from the record. Use `.?` to access
    it—that produces a `Try` you can match on or default with `??`.

# TOKENS
~~~zig
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpQuestion,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotQuestionLowerIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Config")
				(args))
			(ty-record
				(anno-record-field (name "host")
					(ty (name "Str")))
				(anno-record-field (name "port") (optional true)
					(ty (name "U16")))
				(anno-record-field (name "retries")
					(ty (name "U8"))
					(default
						(e-int (raw "3"))))))
		(s-type-anno (name "get_port")
			(ty-fn
				(ty (name "Config"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "U16"))
					(ty-tag-union
						(tags
							(ty (name "MissingField")))))))
		(s-decl
			(p-ident (raw "get_port"))
			(e-lambda
				(args
					(p-ident (raw "c")))
				(e-field-access
					(receiver
						(e-ident (raw "c")))
					(segment (mode "optional") (field "port")))))
		(s-type-anno (name "mk_config")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Config"))))
		(s-decl
			(p-ident (raw "mk_config"))
			(e-lambda
				(args
					(p-ident (raw "host")))
				(e-record
					(field (field "host")
						(e-ident (raw "host"))))))
		(s-type-anno (name "bad_direct_access")
			(ty-fn
				(ty (name "Config"))
				(ty (name "U16"))))
		(s-decl
			(p-ident (raw "bad_direct_access"))
			(e-lambda
				(args
					(p-ident (raw "c")))
				(e-field-access
					(receiver
						(e-ident (raw "c")))
					(segment (mode "required") (field "port")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "get_port"))
		(e-lambda
			(args
				(p-assign (ident "c")))
			(e-field-access
				(receiver
					(e-lookup-local
						(p-assign (ident "c"))))
				(segments
					(segment (name "port") (mode "optional")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Config") (local))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "U16") (builtin))
					(ty-tag-union
						(ty-tag-name (name "MissingField")))))))
	(d-let
		(p-assign (ident "mk_config"))
		(e-lambda
			(args
				(p-assign (ident "host")))
			(e-record
				(fields
					(field (name "host")
						(e-lookup-local
							(p-assign (ident "host")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Config") (local)))))
	(d-let
		(p-assign (ident "bad_direct_access"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Config") (local))
				(ty-lookup (name "U16") (builtin)))))
	(s-alias-decl
		(ty-header (name "Config"))
		(ty-record
			(field (field "host")
				(ty-lookup (name "Str") (builtin)))
			(field (field "port") (optional true)
				(ty-lookup (name "U16") (builtin)))
			(field (field "retries") (defaulted true)
				(ty-lookup (name "U8") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Config -> Try(U16, [MissingField])"))
		(patt (type "Str -> Config"))
		(patt (type "Config -> U16")))
	(type_decls
		(alias (type "Config")
			(ty-header (name "Config"))))
	(expressions
		(expr (type "Config -> Try(U16, [MissingField])"))
		(expr (type "Str -> Config"))
		(expr (type "Config -> U16"))))
~~~
