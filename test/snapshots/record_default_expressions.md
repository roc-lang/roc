# META
~~~ini
description=Defaulted fields accept any pure expression: literals, def references, calls, and control flow accepted (parametric defaults included); an effectful default, a parametric literal default, and a materialization cycle rejected
type=snippet
~~~
# SOURCE
~~~roc
base : U8
base = 7

pick : U8 -> U8
pick = |n| if n > 5 n else 0

Accepted := { n : U8 ?? base + 3, s : Str ?? "hi", picked : U8 ?? pick(base), items : List(U8) ?? [] }

accepted : Accepted
accepted = Accepted.{}

Pair(x) := { items : List(x) ?? [] }

pair : Pair(U8)
pair = Pair.{}

eff! : {} => U8
eff! = |_| 5

BadEffect := { a : U8 ?? eff!({}) }

bad_effect : BadEffect
bad_effect = BadEffect.{ a: 1 }

BadLiteral(t) := { value : t ?? 0 }

bad_literal : BadLiteral(Str)
bad_literal = BadLiteral.{}

Cycle := { a : U8 ?? cycle_val.a }

cycle_val : Cycle
cycle_val = Cycle.{}
~~~
# EXPECTED
DEFAULT VALUE CYCLE - record_default_expressions.md:30:22:30:33
INVALID NOMINAL RECORD - record_default_expressions.md:33:19:33:21
EFFECTFUL DEFAULT VALUE - record_default_expressions.md:20:26:20:34
DEFAULT LITERAL NEEDS A CONCRETE TYPE - record_default_expressions.md:25:33:25:34
# PROBLEMS
── ✗ default value cycle ─────────────────── record_default_expressions.md:30:22

The default value for the a field depends on itself.

Cycle := { a : U8 ?? cycle_val.a }
                     ^^^^^^^^^^^

A field default (??) is materialized at every construction site that omits the
field. This default reaches itself again—through values it references, or
through constructions that omit the field and would materialize it—so there is
no value to start from. Break the cycle by supplying the field at one of the
constructions involved, or by removing the self-dependent reference from the
default.

── ✗ invalid nominal record ──────────────── record_default_expressions.md:33:19

I'm having trouble with this nominal type that wraps a record.

cycle_val = Cycle.{}
                  ^^

The record I found is:

    {}

But the nominal type expects:

    { a: U8 }

── ✗ effectful default value ─────────────── record_default_expressions.md:20:26

The default value for the a field performs effects, but a field default must be
pure.

BadEffect := { a : U8 ?? eff!({}) }
                         ^^^^^^^^

A default is filled in by the compiler wherever construction omits the field,
so running effects here would happen at unpredictable times. Compute the value
with an effectful function first, then pass it explicitly.

── ✗ default literal needs a concrete type ─ record_default_expressions.md:25:33

This literal in the default value for the value field does not have a concrete
type.

BadLiteral(t) := { value : t ?? 0 }
                                ^

A default is materialized at every construction site that omits the field, at
that site's own type—and a literal is converted through the type it lands in,
so a literal whose type stays parametric could land in a type with no literal
conversion at all. Give the literal a concrete type, or use an expression that
does not need literal conversion.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,OpGreaterThan,Int,LowerIdent,KwElse,Int,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,LowerIdent,OpPlus,Int,Comma,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpDoubleQuestion,OpenSquare,CloseSquare,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpDoubleQuestion,OpenSquare,CloseSquare,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,CloseCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpFatArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,Int,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenCurly,LowerIdent,OpColon,LowerIdent,OpDoubleQuestion,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,CloseCurly,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,LowerIdent,NoSpaceDotLowerIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "base")
			(ty (name "U8")))
		(s-decl
			(p-ident (raw "base"))
			(e-int (raw "7")))
		(s-type-anno (name "pick")
			(ty-fn
				(ty (name "U8"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "pick"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-if-then-else
					(e-binop (op ">")
						(e-ident (raw "n"))
						(e-int (raw "5")))
					(e-ident (raw "n"))
					(e-int (raw "0")))))
		(s-type-decl
			(header (name "Accepted")
				(args))
			(ty-record
				(anno-record-field (name "n")
					(ty (name "U8"))
					(default
						(e-binop (op "+")
							(e-ident (raw "base"))
							(e-int (raw "3")))))
				(anno-record-field (name "s")
					(ty (name "Str"))
					(default
						(e-string
							(e-string-part (raw "hi")))))
				(anno-record-field (name "picked")
					(ty (name "U8"))
					(default
						(e-apply
							(e-ident (raw "pick"))
							(e-ident (raw "base")))))
				(anno-record-field (name "items")
					(ty-apply
						(ty (name "List"))
						(ty (name "U8")))
					(default
						(e-list)))))
		(s-type-anno (name "accepted")
			(ty (name "Accepted")))
		(s-decl
			(p-ident (raw "accepted"))
			(e-nominal-record
				(mapper (e-tag (raw "Accepted")))
				(backing (e-record))))
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
		(s-type-anno (name "pair")
			(ty-apply
				(ty (name "Pair"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "pair"))
			(e-nominal-record
				(mapper (e-tag (raw "Pair")))
				(backing (e-record))))
		(s-type-anno (name "eff!")
			(ty-fn
				(ty-record)
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "eff!"))
			(e-lambda
				(args
					(p-underscore))
				(e-int (raw "5"))))
		(s-type-decl
			(header (name "BadEffect")
				(args))
			(ty-record
				(anno-record-field (name "a")
					(ty (name "U8"))
					(default
						(e-apply
							(e-ident (raw "eff!"))
							(e-record))))))
		(s-type-anno (name "bad_effect")
			(ty (name "BadEffect")))
		(s-decl
			(p-ident (raw "bad_effect"))
			(e-nominal-record
				(mapper (e-tag (raw "BadEffect")))
				(backing (e-record
						(field (field "a")
							(e-int (raw "1")))))))
		(s-type-decl
			(header (name "BadLiteral")
				(args
					(ty-var (raw "t"))))
			(ty-record
				(anno-record-field (name "value")
					(ty-var (raw "t"))
					(default
						(e-int (raw "0"))))))
		(s-type-anno (name "bad_literal")
			(ty-apply
				(ty (name "BadLiteral"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "bad_literal"))
			(e-nominal-record
				(mapper (e-tag (raw "BadLiteral")))
				(backing (e-record))))
		(s-type-decl
			(header (name "Cycle")
				(args))
			(ty-record
				(anno-record-field (name "a")
					(ty (name "U8"))
					(default
						(e-field-access
							(receiver
								(e-ident (raw "cycle_val")))
							(segment (mode "required") (field "a")))))))
		(s-type-anno (name "cycle_val")
			(ty (name "Cycle")))
		(s-decl
			(p-ident (raw "cycle_val"))
			(e-nominal-record
				(mapper (e-tag (raw "Cycle")))
				(backing (e-record))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "base"))
		(e-num (value "7"))
		(annotation
			(ty-lookup (name "U8") (builtin))))
	(d-let
		(p-assign (ident "pick"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-if
				(if-branches
					(if-branch
						(e-dispatch-call (method "is_gt") (constraint-fn-var 378)
							(receiver
								(e-lookup-local
									(p-assign (ident "n"))))
							(args
								(e-num (value "5"))))
						(e-lookup-local
							(p-assign (ident "n")))))
				(if-else
					(e-num (value "0")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin)))))
	(d-let
		(p-assign (ident "accepted"))
		(e-nominal (nominal "Accepted")
			(e-empty_record))
		(annotation
			(ty-lookup (name "Accepted") (local))))
	(d-let
		(p-assign (ident "pair"))
		(e-nominal (nominal "Pair")
			(e-empty_record))
		(annotation
			(ty-apply (name "Pair") (local)
				(ty-lookup (name "U8") (builtin)))))
	(d-let
		(p-assign (ident "eff!"))
		(e-lambda
			(args
				(p-underscore))
			(e-num (value "5")))
		(annotation
			(ty-fn (effectful true)
				(ty-record)
				(ty-lookup (name "U8") (builtin)))))
	(d-let
		(p-assign (ident "bad_effect"))
		(e-nominal (nominal "BadEffect")
			(e-record
				(fields
					(field (name "a")
						(e-num (value "1"))))))
		(annotation
			(ty-lookup (name "BadEffect") (local))))
	(d-let
		(p-assign (ident "bad_literal"))
		(e-nominal (nominal "BadLiteral")
			(e-empty_record))
		(annotation
			(ty-apply (name "BadLiteral") (local)
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "cycle_val"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-lookup (name "Cycle") (local))))
	(s-nominal-decl
		(ty-header (name "Accepted"))
		(ty-record
			(field (field "n") (defaulted true)
				(ty-lookup (name "U8") (builtin)))
			(field (field "s") (defaulted true)
				(ty-lookup (name "Str") (builtin)))
			(field (field "picked") (defaulted true)
				(ty-lookup (name "U8") (builtin)))
			(field (field "items") (defaulted true)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "U8") (builtin))))))
	(s-nominal-decl
		(ty-header (name "Pair")
			(ty-args
				(ty-rigid-var (name "x"))))
		(ty-record
			(field (field "items") (defaulted true)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var-lookup (ty-rigid-var (name "x")))))))
	(s-nominal-decl
		(ty-header (name "BadEffect"))
		(ty-record
			(field (field "a") (defaulted true)
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "BadLiteral")
			(ty-args
				(ty-rigid-var (name "t"))))
		(ty-record
			(field (field "value") (defaulted true)
				(ty-rigid-var-lookup (ty-rigid-var (name "t"))))))
	(s-nominal-decl
		(ty-header (name "Cycle"))
		(ty-record
			(field (field "a")
				(ty-lookup (name "U8") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U8"))
		(patt (type "U8 -> U8"))
		(patt (type "Accepted"))
		(patt (type "Pair(U8)"))
		(patt (type "{} => U8"))
		(patt (type "BadEffect"))
		(patt (type "BadLiteral(Str)"))
		(patt (type "Cycle")))
	(type_decls
		(nominal (type "Accepted")
			(ty-header (name "Accepted")))
		(nominal (type "Pair(x)")
			(ty-header (name "Pair")
				(ty-args
					(ty-rigid-var (name "x")))))
		(nominal (type "BadEffect")
			(ty-header (name "BadEffect")))
		(nominal (type "BadLiteral(t)")
			(ty-header (name "BadLiteral")
				(ty-args
					(ty-rigid-var (name "t")))))
		(nominal (type "Cycle")
			(ty-header (name "Cycle"))))
	(expressions
		(expr (type "U8"))
		(expr (type "U8 -> U8"))
		(expr (type "Accepted"))
		(expr (type "Pair(U8)"))
		(expr (type "{} => U8"))
		(expr (type "BadEffect"))
		(expr (type "BadLiteral(Str)"))
		(expr (type "Cycle"))))
~~~
