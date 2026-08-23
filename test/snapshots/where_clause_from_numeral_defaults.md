# META
~~~ini
description=A where-clause from_numeral contract makes an unpinned instantiated result defaultable, exactly like the from_numeral constraint a literal creates, so annotating a def with its own inferred constrained type does not change whether callers check
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

sum : a -> b where [
	a.iter : a -> Iter(item),
	b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]),
	b.plus : b, item -> b,
]
sum = |iter| {
	var $s = 0
	for value in iter {
		$s = $s + value
	}
	$s
}

total = (1..=10)->sum()

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,KwWhere,OpenSquare,
LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,
LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,Comma,
LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,LowerIdent,Comma,
CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpenRound,Int,OpDoubleDotEquals,Int,CloseRound,OpArrow,LowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno (name "sum")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b")))
			(where
				(method (mod-of "a") (name "iter")
					(args
						(ty-var (raw "a")))
					(ty-apply
						(ty (name "Iter"))
						(ty-var (raw "item"))))
				(method (mod-of "b") (name "from_numeral")
					(args
						(ty (name "Numeral")))
					(ty-apply
						(ty (name "Try"))
						(ty-var (raw "b"))
						(ty-tag-union
							(tags
								(ty-apply
									(ty (name "InvalidNumeral"))
									(ty (name "Str")))))))
				(method (mod-of "b") (name "plus")
					(args
						(ty-var (raw "b"))
						(ty-var (raw "item")))
					(ty-var (raw "b")))))
		(s-decl
			(p-ident (raw "sum"))
			(e-lambda
				(args
					(p-ident (raw "iter")))
				(e-block
					(statements
						(s-var (name "$s")
							(e-int (raw "0")))
						(s-for
							(p-ident (raw "value"))
							(e-ident (raw "iter"))
							(e-block
								(statements
									(s-decl
										(p-ident (raw "$s"))
										(e-binop (op "+")
											(e-ident (raw "$s"))
											(e-ident (raw "value")))))))
						(e-ident (raw "$s"))))))
		(s-decl
			(p-ident (raw "total"))
			(e-arrow-call
				(e-tuple
					(e-binop (op "..=")
						(e-int (raw "1"))
						(e-int (raw "10"))))
				(e-apply
					(e-ident (raw "sum")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-record)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

sum : a -> b
	where [
		a.iter : a -> Iter(item),
		b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]),
		b.plus : b, item -> b,
	]
sum = |iter| {
	var $s = 0
	for value in iter {
		$s = $s + value
	}
	$s
}

total = (1..=10) |> sum

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "sum"))
		(e-lambda
			(args
				(p-assign (ident "iter")))
			(e-block
				(s-var
					(p-assign (ident "$s"))
					(e-num (value "0")))
				(s-for
					(p-assign (ident "value"))
					(e-lookup-local
						(p-assign (ident "iter")))
					(e-block
						(s-reassign
							(p-assign (ident "$s"))
							(e-dispatch-call (method "plus") (constraint-fn-var 347)
								(receiver
									(e-lookup-local
										(p-assign (ident "$s"))))
								(args
									(e-lookup-local
										(p-assign (ident "value"))))))
						(e-empty_record)))
				(e-lookup-local
					(p-assign (ident "$s")))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "iter")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-apply (name "Iter") (builtin)
						(ty-rigid-var (name "item"))))
				(method (ty-rigid-var-lookup (ty-rigid-var (name "b"))) (name "from_numeral")
					(args
						(ty-lookup (name "Numeral") (builtin)))
					(ty-apply (name "Try") (builtin)
						(ty-rigid-var-lookup (ty-rigid-var (name "b")))
						(ty-tag-union
							(ty-tag-name (name "InvalidNumeral")
								(ty-lookup (name "Str") (builtin))))))
				(method (ty-rigid-var-lookup (ty-rigid-var (name "b"))) (name "plus")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "b")))
						(ty-rigid-var-lookup (ty-rigid-var (name "item"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))))))
	(d-let
		(p-assign (ident "total"))
		(e-call (constraint-fn-var 383)
			(e-lookup-local
				(p-assign (ident "sum")))
			(e-dispatch-call (method "range_inclusive_to") (constraint-fn-var 381)
				(receiver
					(e-num (value "1")))
				(args
					(e-num (value "10"))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> b where [a.iter : a -> Iter(item), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.plus : b, item -> b]"))
		(patt (type "Dec"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "a -> b where [a.iter : a -> Iter(item), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.plus : b, item -> b]"))
		(expr (type "Dec"))
		(expr (type "_arg -> {}"))))
~~~
