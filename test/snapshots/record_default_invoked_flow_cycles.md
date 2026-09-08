# META
~~~ini
description=Invoked-ness flows through result positions and call arguments: a block-produced closure cycle and a higher-order argument cycle are rejected as default value cycles, while an uncalled function-valued def reference stays accepted
type=snippet
~~~
# SOURCE
~~~roc
Blocky := { a : U8 ?? f({}) }

f = {
    n = 1
    |_| Blocky.{}.a + n
}

Hof := { a : U8 ?? apply(make) }

apply = |g| g({})

make = |_| Hof.{}.a

Keeps := { a : U8 ?? 0, h : ({} -> U8) ?? make_handler }

make_handler = |_| Keeps.{}.a

keeps : Keeps
keeps = Keeps.{}
~~~
# EXPECTED
DEFAULT VALUE CYCLE - record_default_invoked_flow_cycles.md:1:23:1:28
DEFAULT VALUE CYCLE - record_default_invoked_flow_cycles.md:8:20:8:31
INVALID NOMINAL RECORD - record_default_invoked_flow_cycles.md:5:16:5:18
INVALID NOMINAL RECORD - record_default_invoked_flow_cycles.md:12:16:12:18
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Default Value Cycle")
		(region (start 1 23) (end 1 28))
		(headline
			(reflow "The default value for the ")
			(annotated record-field "a")
			(reflow " field depends on itself."))
		(document
			(source-region (file "record_default_invoked_flow_cycles.md") (start 1 23) (end 1 28) (annotation error) (line-text "Blocky := { a : U8 ?? f({}) }"))
			(line-break)
			(reflow "A field default (")
			(annotated code "??")
			(reflow ") is materialized at every construction site that omits the field. This default reaches itself again—through values it references, or through constructions that omit the field and would materialize it—so there is no value to start from. Break the cycle by supplying the field at one of the constructions involved, or by removing the self-dependent reference from the default.")))
	(report
		(severity runtime_error)
		(title "Default Value Cycle")
		(region (start 8 20) (end 8 31))
		(headline
			(reflow "The default value for the ")
			(annotated record-field "a")
			(reflow " field depends on itself."))
		(document
			(source-region (file "record_default_invoked_flow_cycles.md") (start 8 20) (end 8 31) (annotation error) (line-text "Hof := { a : U8 ?? apply(make) }"))
			(line-break)
			(reflow "A field default (")
			(annotated code "??")
			(reflow ") is materialized at every construction site that omits the field. This default reaches itself again—through values it references, or through constructions that omit the field and would materialize it—so there is no value to start from. Break the cycle by supplying the field at one of the constructions involved, or by removing the self-dependent reference from the default.")))
	(report
		(severity runtime_error)
		(title "Invalid Nominal Record")
		(region (start 5 16) (end 5 18))
		(headline
			(reflow "I'm having trouble with this nominal type that wraps a record."))
		(document
			(source-region (file "record_default_invoked_flow_cycles.md") (start 5 16) (end 5 18) (annotation error) (line-text "    |_| Blocky.{}.a + n"))
			(line-break)
			(text "The record I found is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "But the nominal type expects:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ a: U8 }")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Invalid Nominal Record")
		(region (start 12 16) (end 12 18))
		(headline
			(reflow "I'm having trouble with this nominal type that wraps a record."))
		(document
			(source-region (file "record_default_invoked_flow_cycles.md") (start 12 16) (end 12 18) (annotation error) (line-text "make = |_| Hof.{}.a"))
			(line-break)
			(text "The record I found is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{}")
			(annotation-end)
			(line-break)
			(line-break)
			(text "But the nominal type expects:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ a: U8 }")
			(annotation-end))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,Int,
OpBar,Underscore,OpBar,UpperIdent,Dot,OpenCurly,CloseCurly,NoSpaceDotLowerIdent,OpPlus,LowerIdent,
CloseCurly,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,Dot,OpenCurly,CloseCurly,NoSpaceDotLowerIdent,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,Comma,LowerIdent,OpColon,OpenRound,OpenCurly,CloseCurly,OpArrow,UpperIdent,CloseRound,OpDoubleQuestion,LowerIdent,CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,Dot,OpenCurly,CloseCurly,NoSpaceDotLowerIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Blocky")
				(args))
			(ty-record
				(anno-record-field (name "a")
					(ty (name "U8"))
					(default
						(e-apply
							(e-ident (raw "f"))
							(e-record))))))
		(s-decl
			(p-ident (raw "f"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "n"))
						(e-int (raw "1")))
					(e-lambda
						(args
							(p-underscore))
						(e-binop (op "+")
							(e-field-access
								(receiver
									(e-nominal-record
										(mapper (e-tag (raw "Blocky")))
										(backing (e-record))))
								(segment (mode "required") (field "a")))
							(e-ident (raw "n")))))))
		(s-type-decl
			(header (name "Hof")
				(args))
			(ty-record
				(anno-record-field (name "a")
					(ty (name "U8"))
					(default
						(e-apply
							(e-ident (raw "apply"))
							(e-ident (raw "make")))))))
		(s-decl
			(p-ident (raw "apply"))
			(e-lambda
				(args
					(p-ident (raw "g")))
				(e-apply
					(e-ident (raw "g"))
					(e-record))))
		(s-decl
			(p-ident (raw "make"))
			(e-lambda
				(args
					(p-underscore))
				(e-field-access
					(receiver
						(e-nominal-record
							(mapper (e-tag (raw "Hof")))
							(backing (e-record))))
					(segment (mode "required") (field "a")))))
		(s-type-decl
			(header (name "Keeps")
				(args))
			(ty-record
				(anno-record-field (name "a")
					(ty (name "U8"))
					(default
						(e-int (raw "0"))))
				(anno-record-field (name "h")
					(ty-fn
						(ty-record)
						(ty (name "U8")))
					(default
						(e-ident (raw "make_handler"))))))
		(s-decl
			(p-ident (raw "make_handler"))
			(e-lambda
				(args
					(p-underscore))
				(e-field-access
					(receiver
						(e-nominal-record
							(mapper (e-tag (raw "Keeps")))
							(backing (e-record))))
					(segment (mode "required") (field "a")))))
		(s-type-anno (name "keeps")
			(ty (name "Keeps")))
		(s-decl
			(p-ident (raw "keeps"))
			(e-nominal-record
				(mapper (e-tag (raw "Keeps")))
				(backing (e-record))))))
~~~
# FORMATTED
~~~roc
Blocky := { a : U8 ?? f({}) }

f = {
	n = 1
	|_| Blocky.{}.a + n
}

Hof := { a : U8 ?? apply(make) }

apply = |g| g({})

make = |_| Hof.{}.a

Keeps := { a : U8 ?? 0, h : ({} -> U8) ?? make_handler }

make_handler = |_| Keeps.{}.a

keeps : Keeps
keeps = Keeps.{}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-block
			(s-let
				(p-assign (ident "n"))
				(e-num (value "1")))
			(e-closure
				(captures
					(capture (ident "n")))
				(e-lambda
					(args
						(p-underscore))
					(e-dispatch-call (method "plus") (constraint-fn-var 296)
						(receiver
							(e-field-access
								(receiver
									(e-runtime-error (tag "erroneous_value_expr")))
								(segments
									(segment (name "a") (mode "required")))))
						(args
							(e-lookup-local
								(p-assign (ident "n")))))))))
	(d-let
		(p-assign (ident "apply"))
		(e-lambda
			(args
				(p-assign (ident "g")))
			(e-call (constraint-fn-var 301)
				(e-lookup-local
					(p-assign (ident "g")))
				(e-empty_record))))
	(d-let
		(p-assign (ident "make"))
		(e-lambda
			(args
				(p-underscore))
			(e-field-access
				(receiver
					(e-runtime-error (tag "erroneous_value_expr")))
				(segments
					(segment (name "a") (mode "required"))))))
	(d-let
		(p-assign (ident "make_handler"))
		(e-lambda
			(args
				(p-underscore))
			(e-field-access
				(receiver
					(e-nominal (nominal "Keeps")
						(e-empty_record)))
				(segments
					(segment (name "a") (mode "required"))))))
	(d-let
		(p-assign (ident "keeps"))
		(e-nominal (nominal "Keeps")
			(e-empty_record))
		(annotation
			(ty-lookup (name "Keeps") (local))))
	(s-nominal-decl
		(ty-header (name "Blocky"))
		(ty-record
			(field (field "a")
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Hof"))
		(ty-record
			(field (field "a")
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Keeps"))
		(ty-record
			(field (field "a") (defaulted true)
				(ty-lookup (name "U8") (builtin)))
			(field (field "h") (defaulted true)
				(ty-parens
					(ty-fn (effectful false)
						(ty-record)
						(ty-lookup (name "U8") (builtin))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> b where [b.plus : b, Dec -> b]"))
		(patt (type "({} -> b) -> b"))
		(patt (type "_arg -> _ret"))
		(patt (type "_arg -> U8"))
		(patt (type "Keeps")))
	(type_decls
		(nominal (type "Blocky")
			(ty-header (name "Blocky")))
		(nominal (type "Hof")
			(ty-header (name "Hof")))
		(nominal (type "Keeps")
			(ty-header (name "Keeps"))))
	(expressions
		(expr (type "_arg -> b where [b.plus : b, Dec -> b]"))
		(expr (type "({} -> b) -> b"))
		(expr (type "_arg -> _ret"))
		(expr (type "_arg -> U8"))
		(expr (type "Keeps"))))
~~~
