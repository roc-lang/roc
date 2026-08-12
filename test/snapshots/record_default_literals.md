# META
~~~ini
description=Defaulted field literals-only rule from both sides: literal defaults accepted, a def-referencing default and a non-concrete default rejected
type=snippet
~~~
# SOURCE
~~~roc
accepted : { n : U8 ?? 10, m : I8 ?? -1, s : Str ?? "hi", t : [Some(U8), None] ?? Some(1) }
accepted = {}

ten : U8
ten = 10

bad_ref : { a : U8 ?? ten }
bad_ref = { a: 1 }

Pair(x) : { items : List(x) ?? [] }

bad_list : Pair(U8)
bad_list = {}
~~~
# EXPECTED
DEFAULT VALUE MUST BE A LITERAL - record_default_literals.md:7:23:7:26
DEFAULT VALUE NOT CONCRETE - record_default_literals.md:10:32:10:34
# PROBLEMS

┌─────────────────────────────────┐
│ DEFAULT VALUE MUST BE A LITERAL ├─ The default value for the `a` field is ──┐
└┬────────────────────────────────┘  not a literal.                           │
 │                                                                            │
 │  bad_ref : { a : U8 ?? ten }                                               │
 │                        ‾‾‾                                                 │
 └─────────────────────────────────────────── record_default_literals.md:7:23 ┘

    A field default (`??`) is materialized by the compiler at every
    construction site that omits the field, so it must be a literal: a number,
    an interpolation-free string, a tag, or a list, record, or tuple built only
    from literals. Anything that refers to another value could form an
    evaluation cycle the compiler will not chase.


┌────────────────────────────┐
│ DEFAULT VALUE NOT CONCRETE ├─ The default value for the `items` field ──────┐
└┬───────────────────────────┘  does not have a concrete type.                │
 │                                                                            │
 │  Pair(x) : { items : List(x) ?? [] }                                       │
 │                                 ‾‾                                         │
 └────────────────────────────────────────── record_default_literals.md:10:32 ┘

    A default is evaluated once at compile time and filled in wherever
    construction omits the field, so it must have exactly one runtime
    representation. Annotate the field (or the default) with a concrete type.

# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,Comma,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,Comma,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,CloseSquare,OpDoubleQuestion,UpperIdent,NoSpaceOpenRound,Int,CloseRound,CloseCurly,
LowerIdent,OpAssign,OpenCurly,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,LowerIdent,CloseCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpDoubleQuestion,OpenSquare,CloseSquare,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "accepted")
			(ty-record
				(anno-record-field (name "n")
					(ty (name "U8"))
					(default
						(e-int (raw "10"))))
				(anno-record-field (name "m")
					(ty (name "I8"))
					(default
						(e-int (raw "-1"))))
				(anno-record-field (name "s")
					(ty (name "Str"))
					(default
						(e-string
							(e-string-part (raw "hi")))))
				(anno-record-field (name "t")
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "Some"))
								(ty (name "U8")))
							(ty (name "None"))))
					(default
						(e-apply
							(e-tag (raw "Some"))
							(e-int (raw "1")))))))
		(s-decl
			(p-ident (raw "accepted"))
			(e-record))
		(s-type-anno (name "ten")
			(ty (name "U8")))
		(s-decl
			(p-ident (raw "ten"))
			(e-int (raw "10")))
		(s-type-anno (name "bad_ref")
			(ty-record
				(anno-record-field (name "a")
					(ty (name "U8"))
					(default
						(e-ident (raw "ten"))))))
		(s-decl
			(p-ident (raw "bad_ref"))
			(e-record
				(field (field "a")
					(e-int (raw "1")))))
		(s-type-decl
			(header (name "Pair")
				(args
					(ty-var (raw "x"))))
			(ty-record
				(anno-record-field (name "items")
					(ty-apply
						(ty (name "List"))
						(ty-var (raw "x")))
					(default
						(e-list)))))
		(s-type-anno (name "bad_list")
			(ty-apply
				(ty (name "Pair"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "bad_list"))
			(e-record))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "accepted"))
		(e-empty_record)
		(annotation
			(ty-record
				(field (field "n") (defaulted true)
					(ty-lookup (name "U8") (builtin)))
				(field (field "m") (defaulted true)
					(ty-lookup (name "I8") (builtin)))
				(field (field "s") (defaulted true)
					(ty-lookup (name "Str") (builtin)))
				(field (field "t") (defaulted true)
					(ty-tag-union
						(ty-tag-name (name "Some")
							(ty-lookup (name "U8") (builtin)))
						(ty-tag-name (name "None")))))))
	(d-let
		(p-assign (ident "ten"))
		(e-num (value "10"))
		(annotation
			(ty-lookup (name "U8") (builtin))))
	(d-let
		(p-assign (ident "bad_ref"))
		(e-record
			(fields
				(field (name "a")
					(e-num (value "1")))))
		(annotation
			(ty-record
				(field (field "a")
					(ty-lookup (name "U8") (builtin))))))
	(d-let
		(p-assign (ident "bad_list"))
		(e-empty_record)
		(annotation
			(ty-apply (name "Pair") (local)
				(ty-lookup (name "U8") (builtin)))))
	(s-alias-decl
		(ty-header (name "Pair")
			(ty-args
				(ty-rigid-var (name "x"))))
		(ty-record
			(field (field "items") (defaulted true)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var-lookup (ty-rigid-var (name "x"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ m: I8 ?? -1, n: U8 ?? 10, s: Str ?? "hi", t: [None, Some(U8)] ?? Some(1) }"))
		(patt (type "U8"))
		(patt (type "{ a: U8 }"))
		(patt (type "Pair(U8)")))
	(type_decls
		(alias (type "Pair(x)")
			(ty-header (name "Pair")
				(ty-args
					(ty-rigid-var (name "x"))))))
	(expressions
		(expr (type "{ m: I8 ?? -1, n: U8 ?? 10, s: Str ?? "hi", t: [None, Some(U8)] ?? Some(1) }"))
		(expr (type "U8"))
		(expr (type "{ a: U8 }"))
		(expr (type "Pair(U8)"))))
~~~
