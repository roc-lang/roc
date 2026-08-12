# META
~~~ini
description=An explicit nominal constructor rejects a nominal record as its backing
type=snippet
~~~
# SOURCE
~~~roc
Wrap :: { a : U8, b : U8 }.{
	inc_a : Wrap -> Wrap

	inc_a = |wrap| Wrap.{ ..wrap, a: wrap.a + 1 }
}
~~~
# EXPECTED
INVALID NOMINAL RECORD - issue_10195_nominal_record_update_rewrapped.md:4:22:4:47
# PROBLEMS

┌────────────────────────┐
│ INVALID NOMINAL RECORD ├─ I'm having trouble with this nominal type that ───┐
└┬───────────────────────┘  wraps a record.                                   │
 │                                                                            │
 │  inc_a = |wrap| Wrap.{ ..wrap, a: wrap.a + 1 }                             │
 │                      ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                             │
 └─────────────────────── issue_10195_nominal_record_update_rewrapped.md:4:22 ┘

    The record I found is:

        Wrap

    But the nominal type expects:

        { a: U8, b: U8 }

# TOKENS
~~~zig
UpperIdent,OpDoubleColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,Dot,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,OpPlus,Int,CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Wrap")
				(args))
			(ty-record
				(anno-record-field (name "a")
					(ty (name "U8")))
				(anno-record-field (name "b")
					(ty (name "U8"))))
			(associated
				(s-type-anno (name "inc_a")
					(ty-fn
						(ty (name "Wrap"))
						(ty (name "Wrap"))))
				(s-decl
					(p-ident (raw "inc_a"))
					(e-lambda
						(args
							(p-ident (raw "wrap")))
						(e-nominal-record
							(mapper (e-tag (raw "Wrap")))
							(backing (e-record
									(ext
										(e-ident (raw "wrap")))
									(field (field "a")
										(e-binop (op "+")
											(e-field-access
												(receiver
													(e-ident (raw "wrap")))
												(segment (mode "required") (field "a")))
											(e-int (raw "1")))))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "issue_10195_nominal_record_update_rewrapped.Wrap.inc_a"))
		(e-lambda
			(args
				(p-assign (ident "wrap")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Wrap") (local))
				(ty-lookup (name "Wrap") (local)))))
	(s-nominal-decl
		(ty-header (name "Wrap"))
		(ty-record
			(field (field "a")
				(ty-lookup (name "U8") (builtin)))
			(field (field "b")
				(ty-lookup (name "U8") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Wrap -> Wrap")))
	(type_decls
		(nominal (type "Wrap")
			(ty-header (name "Wrap"))))
	(expressions
		(expr (type "Wrap -> Wrap"))))
~~~
