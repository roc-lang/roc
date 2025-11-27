# META
~~~ini
description=Fold-style lambda with for loop
type=snippet
~~~
# SOURCE
~~~roc
fold : List(item), state, (state, item -> state) -> state
fold = |list, init, step| {
    var $state = init

    for item in list {
        $state = step($state, item)
    }

    $state
}

sumResult : U64
sumResult = fold([1, 2, 3, 4], 0, |acc, x| acc + x)

expect sumResult == 10
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**list_fold.md:13:19:13:20:**
```roc
sumResult = fold([1, 2, 3, 4], 0, |acc, x| acc + x)
```
                  ^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**list_fold.md:13:19:13:20:**
```roc
sumResult = fold([1, 2, 3, 4], 0, |acc, x| acc + x)
```
                  ^

It has the type:
    _Try(U64, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(U64, [InvalidNumeral(Str)])_

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,Comma,OpenRound,LowerIdent,Comma,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,LowerIdent,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,Int,Comma,Int,Comma,Int,Comma,Int,CloseSquare,Comma,Int,Comma,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,CloseRound,
KwExpect,LowerIdent,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "fold")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "item")))
				(ty-var (raw "state"))
				(ty-fn
					(ty-var (raw "state"))
					(ty-var (raw "item"))
					(ty-var (raw "state")))
				(ty-var (raw "state"))))
		(s-decl
			(p-ident (raw "fold"))
			(e-lambda
				(args
					(p-ident (raw "list"))
					(p-ident (raw "init"))
					(p-ident (raw "step")))
				(e-block
					(statements
						(s-var (name "$state")
							(e-ident (raw "init")))
						(s-for
							(p-ident (raw "item"))
							(e-ident (raw "list"))
							(e-block
								(statements
									(s-decl
										(p-ident (raw "$state"))
										(e-apply
											(e-ident (raw "step"))
											(e-ident (raw "$state"))
											(e-ident (raw "item")))))))
						(e-ident (raw "$state"))))))
		(s-type-anno (name "sumResult")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "sumResult"))
			(e-apply
				(e-ident (raw "fold"))
				(e-list
					(e-int (raw "1"))
					(e-int (raw "2"))
					(e-int (raw "3"))
					(e-int (raw "4")))
				(e-int (raw "0"))
				(e-lambda
					(args
						(p-ident (raw "acc"))
						(p-ident (raw "x")))
					(e-binop (op "+")
						(e-ident (raw "acc"))
						(e-ident (raw "x"))))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "sumResult"))
				(e-int (raw "10"))))))
~~~
# FORMATTED
~~~roc
fold : List(item), state, (state, item -> state) -> state
fold = |list, init, step| {
	var $state = init

	for item in list {
		$state = step($state, item)
	}

	$state
}

sumResult : U64
sumResult = fold([1, 2, 3, 4], 0, |acc, x| acc + x)

expect sumResult == 10
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "fold"))
		(e-lambda
			(args
				(p-assign (ident "list"))
				(p-assign (ident "init"))
				(p-assign (ident "step")))
			(e-block
				(s-var
					(p-assign (ident "$state"))
					(e-lookup-local
						(p-assign (ident "init"))))
				(s-for
					(p-assign (ident "item"))
					(e-lookup-local
						(p-assign (ident "list")))
					(e-block
						(s-reassign
							(p-assign (ident "$state"))
							(e-call
								(e-lookup-local
									(p-assign (ident "step")))
								(e-lookup-local
									(p-assign (ident "$state")))
								(e-lookup-local
									(p-assign (ident "item")))))
						(e-empty_record)))
				(e-lookup-local
					(p-assign (ident "$state")))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "item")))
				(ty-rigid-var (name "state"))
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "state")))
						(ty-rigid-var-lookup (ty-rigid-var (name "item")))
						(ty-rigid-var-lookup (ty-rigid-var (name "state")))))
				(ty-rigid-var-lookup (ty-rigid-var (name "state"))))))
	(d-let
		(p-assign (ident "sumResult"))
		(e-call
			(e-lookup-local
				(p-assign (ident "fold")))
			(e-list
				(elems
					(e-num (value "1"))
					(e-num (value "2"))
					(e-num (value "3"))
					(e-num (value "4"))))
			(e-num (value "0"))
			(e-lambda
				(args
					(p-assign (ident "acc"))
					(p-assign (ident "x")))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "acc")))
					(e-lookup-local
						(p-assign (ident "x"))))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "sumResult")))
			(e-num (value "10")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(item), state, (state, item -> state) -> state"))
		(patt (type "Error")))
	(expressions
		(expr (type "List(item), state, (state, item -> state) -> state"))
		(expr (type "Error"))))
~~~
