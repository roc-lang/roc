# META
~~~ini
description=Closure variable capture and scope behavior
type=snippet
~~~
# SOURCE
~~~roc
# Simple closure capturing outer variable
outer = 42
captureSimple = |_| outer

# Closure capturing multiple variables
x = 1
y = 2
captureMultiple = |_| x + y

# Nested closures with multi-level capture
level1 = 10
outerFn = |_| {
    level2 = 20
    innerFn = |_| level1 + level2
    innerFn
}

# Closure capturing closure
makeClosure = |n| |_| n
useClosure = makeClosure(100)

main = (captureSimple, captureMultiple, outerFn, useClosure)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpBar,Underscore,OpBar,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "outer"))
			(e-int (raw "42")))
		(s-decl
			(p-ident (raw "captureSimple"))
			(e-lambda
				(args
					(p-underscore))
				(e-ident (raw "outer"))))
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "1")))
		(s-decl
			(p-ident (raw "y"))
			(e-int (raw "2")))
		(s-decl
			(p-ident (raw "captureMultiple"))
			(e-lambda
				(args
					(p-underscore))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-ident (raw "y")))))
		(s-decl
			(p-ident (raw "level1"))
			(e-int (raw "10")))
		(s-decl
			(p-ident (raw "outerFn"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "level2"))
							(e-int (raw "20")))
						(s-decl
							(p-ident (raw "innerFn"))
							(e-lambda
								(args
									(p-underscore))
								(e-binop (op "+")
									(e-ident (raw "level1"))
									(e-ident (raw "level2")))))
						(e-ident (raw "innerFn"))))))
		(s-decl
			(p-ident (raw "makeClosure"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-lambda
					(args
						(p-underscore))
					(e-ident (raw "n")))))
		(s-decl
			(p-ident (raw "useClosure"))
			(e-apply
				(e-ident (raw "makeClosure"))
				(e-int (raw "100"))))
		(s-decl
			(p-ident (raw "main"))
			(e-tuple
				(e-ident (raw "captureSimple"))
				(e-ident (raw "captureMultiple"))
				(e-ident (raw "outerFn"))
				(e-ident (raw "useClosure"))))))
~~~
# FORMATTED
~~~roc
# Simple closure capturing outer variable
outer = 42
captureSimple = |_| outer

# Closure capturing multiple variables
x = 1
y = 2
captureMultiple = |_| x + y

# Nested closures with multi-level capture
level1 = 10
outerFn = |_| {
	level2 = 20
	innerFn = |_| level1 + level2
	innerFn
}

# Closure capturing closure
makeClosure = |n| |_| n
useClosure = makeClosure(100)

main = (captureSimple, captureMultiple, outerFn, useClosure)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "outer"))
		(e-num (value "42")))
	(d-let
		(p-assign (ident "captureSimple"))
		(e-closure
			(captures
				(capture (ident "outer")))
			(e-lambda
				(args
					(p-underscore))
				(e-lookup-local
					(p-assign (ident "outer"))))))
	(d-let
		(p-assign (ident "x"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "y"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "captureMultiple"))
		(e-closure
			(captures
				(capture (ident "x"))
				(capture (ident "y")))
			(e-lambda
				(args
					(p-underscore))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "x")))
					(e-lookup-local
						(p-assign (ident "y")))))))
	(d-let
		(p-assign (ident "level1"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "outerFn"))
		(e-closure
			(captures
				(capture (ident "level1")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "level2"))
						(e-num (value "20")))
					(s-let
						(p-assign (ident "innerFn"))
						(e-closure
							(captures
								(capture (ident "level1"))
								(capture (ident "level2")))
							(e-lambda
								(args
									(p-underscore))
								(e-binop (op "add")
									(e-lookup-local
										(p-assign (ident "level1")))
									(e-lookup-local
										(p-assign (ident "level2")))))))
					(e-lookup-local
						(p-assign (ident "innerFn")))))))
	(d-let
		(p-assign (ident "makeClosure"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-closure
				(captures
					(capture (ident "n")))
				(e-lambda
					(args
						(p-underscore))
					(e-lookup-local
						(p-assign (ident "n")))))))
	(d-let
		(p-assign (ident "useClosure"))
		(e-call
			(e-lookup-local
				(p-assign (ident "makeClosure")))
			(e-num (value "100"))))
	(d-let
		(p-assign (ident "main"))
		(e-tuple
			(elems
				(e-lookup-local
					(p-assign (ident "captureSimple")))
				(e-lookup-local
					(p-assign (ident "captureMultiple")))
				(e-lookup-local
					(p-assign (ident "outerFn")))
				(e-lookup-local
					(p-assign (ident "useClosure")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "_arg -> (_arg2 -> a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "a -> (_arg -> a)"))
		(patt (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "(_arg -> a, _arg2 -> b, _arg3 -> (_arg4 -> c), _arg5 -> d) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.plus : b, e -> b, c.from_numeral : Numeral -> Try(g, [InvalidNumeral(Str)]), c.plus : c, f -> c, d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]), e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)]), f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)]), g.plus : g, h -> g, h.from_numeral : Numeral -> Try(h, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "_arg -> (_arg2 -> a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "a -> (_arg -> a)"))
		(expr (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "(_arg -> a, _arg2 -> b, _arg3 -> (_arg4 -> c), _arg5 -> d) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.plus : b, e -> b, c.from_numeral : Numeral -> Try(g, [InvalidNumeral(Str)]), c.plus : c, f -> c, d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]), e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)]), f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)]), g.plus : g, h -> g, h.from_numeral : Numeral -> Try(h, [InvalidNumeral(Str)])]"))))
~~~
