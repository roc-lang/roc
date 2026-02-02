# META
~~~ini
description=Arrow syntax with qualified functions and formatter dropping empty parens
type=snippet
~~~
# SOURCE
~~~roc
# Test qualified function calls with arrow syntax
test1 = "hello"->Str.is_empty
test2 = "hello"->Str.is_empty()
test3 = "hello"->Str.concat("bar")

# Test unqualified function calls
fn0 = |a| a
test4 = 10->fn0
test5 = 10->fn0()

# Test tag syntax
test6 = 42->Ok
test7 = 42->Ok()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,OpArrow,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,OpArrow,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,OpArrow,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,Int,OpArrow,LowerIdent,
LowerIdent,OpAssign,Int,OpArrow,LowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,Int,OpArrow,UpperIdent,
LowerIdent,OpAssign,Int,OpArrow,UpperIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "test1"))
			(e-local-dispatch
				(e-string
					(e-string-part (raw "hello")))
				(e-ident (raw "Str.is_empty"))))
		(s-decl
			(p-ident (raw "test2"))
			(e-local-dispatch
				(e-string
					(e-string-part (raw "hello")))
				(e-apply
					(e-ident (raw "Str.is_empty")))))
		(s-decl
			(p-ident (raw "test3"))
			(e-local-dispatch
				(e-string
					(e-string-part (raw "hello")))
				(e-apply
					(e-ident (raw "Str.concat"))
					(e-string
						(e-string-part (raw "bar"))))))
		(s-decl
			(p-ident (raw "fn0"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-ident (raw "a"))))
		(s-decl
			(p-ident (raw "test4"))
			(e-local-dispatch
				(e-int (raw "10"))
				(e-ident (raw "fn0"))))
		(s-decl
			(p-ident (raw "test5"))
			(e-local-dispatch
				(e-int (raw "10"))
				(e-apply
					(e-ident (raw "fn0")))))
		(s-decl
			(p-ident (raw "test6"))
			(e-local-dispatch
				(e-int (raw "42"))
				(e-tag (raw "Ok"))))
		(s-decl
			(p-ident (raw "test7"))
			(e-local-dispatch
				(e-int (raw "42"))
				(e-apply
					(e-tag (raw "Ok")))))))
~~~
# FORMATTED
~~~roc
# Test qualified function calls with arrow syntax
test1 = "hello"->Str.is_empty()
test2 = "hello"->Str.is_empty()
test3 = "hello"->Str.concat("bar")

# Test unqualified function calls
fn0 = |a| a
test4 = 10->fn0()
test5 = 10->fn0()

# Test tag syntax
test6 = 42->Ok
test7 = 42->Ok()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "test1"))
		(e-call
			(e-lookup-external
				(builtin))
			(e-string
				(e-literal (string "hello")))))
	(d-let
		(p-assign (ident "test2"))
		(e-call
			(e-lookup-external
				(builtin))
			(e-string
				(e-literal (string "hello")))))
	(d-let
		(p-assign (ident "test3"))
		(e-call
			(e-lookup-external
				(builtin))
			(e-string
				(e-literal (string "hello")))
			(e-string
				(e-literal (string "bar")))))
	(d-let
		(p-assign (ident "fn0"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-lookup-local
				(p-assign (ident "a")))))
	(d-let
		(p-assign (ident "test4"))
		(e-call
			(e-lookup-local
				(p-assign (ident "fn0")))
			(e-num (value "10"))))
	(d-let
		(p-assign (ident "test5"))
		(e-call
			(e-lookup-local
				(p-assign (ident "fn0")))
			(e-num (value "10"))))
	(d-let
		(p-assign (ident "test6"))
		(e-tag (name "Ok")
			(args
				(e-num (value "42")))))
	(d-let
		(p-assign (ident "test7"))
		(e-tag (name "Ok")
			(args
				(e-num (value "42"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Bool"))
		(patt (type "Bool"))
		(patt (type "Str"))
		(patt (type "b -> b"))
		(patt (type "b where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "b where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "[Ok(b), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "[Ok(b), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "Bool"))
		(expr (type "Bool"))
		(expr (type "Str"))
		(expr (type "b -> b"))
		(expr (type "b where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "b where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "[Ok(b), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "[Ok(b), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))))
~~~
