# META
~~~ini
description=Tuple pattern matching tests
type=expr
~~~
# SOURCE
~~~roc
{

    # Simple tuple destructuring
    (x, y) = (1, 2)

    # Nested tuple patterns
    ((a, b), (c, d)) = ((10, 20), (30, 40))

    # Mixed patterns with literals
    (first, second, third) = (100, 42, 200)

    # Tuple with string and tag patterns
    (name, string, boolean) = ("Alice", "fixed", True)

    # Tuple with list pattern
    (list, hello) = ([1, 2, 3], "hello")

    {}
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,OpenRound,Int,Comma,Int,CloseRound,
OpenRound,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,OpAssign,OpenRound,NoSpaceOpenRound,Int,Comma,Int,CloseRound,Comma,OpenRound,Int,Comma,Int,CloseRound,CloseRound,
OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,OpenRound,Int,Comma,Int,Comma,Int,CloseRound,
OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,OpenRound,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,Comma,UpperIdent,CloseRound,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,OpenRound,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,Comma,StringStart,StringPart,StringEnd,CloseRound,
OpenCurly,CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-tuple
				(p-ident (raw "x"))
				(p-ident (raw "y")))
			(e-tuple
				(e-int (raw "1"))
				(e-int (raw "2"))))
		(s-decl
			(p-tuple
				(p-tuple
					(p-ident (raw "a"))
					(p-ident (raw "b")))
				(p-tuple
					(p-ident (raw "c"))
					(p-ident (raw "d"))))
			(e-tuple
				(e-tuple
					(e-int (raw "10"))
					(e-int (raw "20")))
				(e-tuple
					(e-int (raw "30"))
					(e-int (raw "40")))))
		(s-decl
			(p-tuple
				(p-ident (raw "first"))
				(p-ident (raw "second"))
				(p-ident (raw "third")))
			(e-tuple
				(e-int (raw "100"))
				(e-int (raw "42"))
				(e-int (raw "200"))))
		(s-decl
			(p-tuple
				(p-ident (raw "name"))
				(p-ident (raw "string"))
				(p-ident (raw "boolean")))
			(e-tuple
				(e-string
					(e-string-part (raw "Alice")))
				(e-string
					(e-string-part (raw "fixed")))
				(e-tag (raw "True"))))
		(s-decl
			(p-tuple
				(p-ident (raw "list"))
				(p-ident (raw "hello")))
			(e-tuple
				(e-list
					(e-int (raw "1"))
					(e-int (raw "2"))
					(e-int (raw "3")))
				(e-string
					(e-string-part (raw "hello")))))
		(e-record)))
~~~
# FORMATTED
~~~roc
{

	# Simple tuple destructuring
	(x, y) = (1, 2)

	# Nested tuple patterns
	((a, b), (c, d)) = ((10, 20), (30, 40))

	# Mixed patterns with literals
	(first, second, third) = (100, 42, 200)

	# Tuple with string and tag patterns
	(name, string, boolean) = ("Alice", "fixed", True)

	# Tuple with list pattern
	(list, hello) = ([1, 2, 3], "hello")

	{}
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-tuple
			(patterns
				(p-assign (ident "x"))
				(p-assign (ident "y"))))
		(e-tuple
			(elems
				(e-num (value "1"))
				(e-num (value "2")))))
	(s-let
		(p-tuple
			(patterns
				(p-tuple
					(patterns
						(p-assign (ident "a"))
						(p-assign (ident "b"))))
				(p-tuple
					(patterns
						(p-assign (ident "c"))
						(p-assign (ident "d"))))))
		(e-tuple
			(elems
				(e-tuple
					(elems
						(e-num (value "10"))
						(e-num (value "20"))))
				(e-tuple
					(elems
						(e-num (value "30"))
						(e-num (value "40")))))))
	(s-let
		(p-tuple
			(patterns
				(p-assign (ident "first"))
				(p-assign (ident "second"))
				(p-assign (ident "third"))))
		(e-tuple
			(elems
				(e-num (value "100"))
				(e-num (value "42"))
				(e-num (value "200")))))
	(s-let
		(p-tuple
			(patterns
				(p-assign (ident "name"))
				(p-assign (ident "string"))
				(p-assign (ident "boolean"))))
		(e-tuple
			(elems
				(e-string
					(e-literal (string "Alice")))
				(e-string
					(e-literal (string "fixed")))
				(e-tag (name "True")))))
	(s-let
		(p-tuple
			(patterns
				(p-assign (ident "list"))
				(p-assign (ident "hello"))))
		(e-tuple
			(elems
				(e-list
					(elems
						(e-num (value "1"))
						(e-num (value "2"))
						(e-num (value "3"))))
				(e-string
					(e-literal (string "hello"))))))
	(e-empty_record))
~~~
# TYPES
~~~clojure
(expr (type "{}"))
~~~
