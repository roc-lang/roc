# META
~~~ini
description=Record creation with comments
type=expr
~~~
# SOURCE
~~~roc
{
    make_pair = |x, y| { first: x, second: y }
    pair1 = make_pair(1, "a")
    pair2 = make_pair("b", 42)
    pair3 = make_pair(True, False)
    { pair1, pair2, pair3 }.to_str()
}
~~~
# EXPECTED
TYPE DOES NOT HAVE METHODS - polymorphism.md:6:5:6:37
# PROBLEMS
**TYPE DOES NOT HAVE METHODS**
You're calling the method `to_str` on a type that doesn't support methods:
**polymorphism.md:6:5:6:37:**
```roc
    { pair1, pair2, pair3 }.to_str()
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This type doesn't support methods:
    _{ pair1: { first: Num(_size), second: Str }, pair2: { first: Str, second: Num(_size2) }, pair3: { first: [True]_others, second: [False]_others2 } }_



# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,Comma,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
OpenCurly,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseCurly,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "make_pair"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-record
					(field (field "first")
						(e-ident (raw "x")))
					(field (field "second")
						(e-ident (raw "y"))))))
		(s-decl
			(p-ident (raw "pair1"))
			(e-apply
				(e-ident (raw "make_pair"))
				(e-int (raw "1"))
				(e-string
					(e-string-part (raw "a")))))
		(s-decl
			(p-ident (raw "pair2"))
			(e-apply
				(e-ident (raw "make_pair"))
				(e-string
					(e-string-part (raw "b")))
				(e-int (raw "42"))))
		(s-decl
			(p-ident (raw "pair3"))
			(e-apply
				(e-ident (raw "make_pair"))
				(e-tag (raw "True"))
				(e-tag (raw "False"))))
		(e-field-access
			(e-record
				(field (field "pair1"))
				(field (field "pair2"))
				(field (field "pair3")))
			(e-apply
				(e-ident (raw "to_str"))))))
~~~
# FORMATTED
~~~roc
{
	make_pair = |x, y| { first: x, second: y }
	pair1 = make_pair(1, "a")
	pair2 = make_pair("b", 42)
	pair3 = make_pair(True, False)
	{ pair1, pair2, pair3 }.to_str()
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "make_pair"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-record
				(fields
					(field (name "first")
						(e-lookup-local
							(p-assign (ident "x"))))
					(field (name "second")
						(e-lookup-local
							(p-assign (ident "y"))))))))
	(s-let
		(p-assign (ident "pair1"))
		(e-call
			(e-lookup-local
				(p-assign (ident "make_pair")))
			(e-num (value "1"))
			(e-string
				(e-literal (string "a")))))
	(s-let
		(p-assign (ident "pair2"))
		(e-call
			(e-lookup-local
				(p-assign (ident "make_pair")))
			(e-string
				(e-literal (string "b")))
			(e-num (value "42"))))
	(s-let
		(p-assign (ident "pair3"))
		(e-call
			(e-lookup-local
				(p-assign (ident "make_pair")))
			(e-tag (name "True"))
			(e-tag (name "False"))))
	(e-dot-access (field "to_str")
		(receiver
			(e-record
				(fields
					(field (name "pair1")
						(e-lookup-local
							(p-assign (ident "pair1"))))
					(field (name "pair2")
						(e-lookup-local
							(p-assign (ident "pair2"))))
					(field (name "pair3")
						(e-lookup-local
							(p-assign (ident "pair3")))))))
		(args)))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
