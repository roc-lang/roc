# META
~~~ini
description=Record with mixed field types
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30, active: Bool.true, scores: [95, 87, 92], balance: 1250.75 }
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `true` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:7),OpColon(1:7-1:8),StringStart(1:9-1:10),StringPart(1:10-1:15),StringEnd(1:15-1:16),Comma(1:16-1:17),LowerIdent(1:18-1:21),OpColon(1:21-1:22),Int(1:23-1:25),Comma(1:25-1:26),LowerIdent(1:27-1:33),OpColon(1:33-1:34),UpperIdent(1:35-1:39),NoSpaceDotLowerIdent(1:39-1:44),Comma(1:44-1:45),LowerIdent(1:46-1:52),OpColon(1:52-1:53),OpenSquare(1:54-1:55),Int(1:55-1:57),Comma(1:57-1:58),Int(1:59-1:61),Comma(1:61-1:62),Int(1:63-1:65),CloseSquare(1:65-1:66),Comma(1:66-1:67),LowerIdent(1:68-1:75),OpColon(1:75-1:76),Float(1:77-1:84),CloseCurly(1:85-1:86),EndOfFile(1:86-1:86),
~~~
# PARSE
~~~clojure
(e-record @1.1-1.86
	(field (field "name") (optional false)
		(e-string @1.9-1.16
			(e-string-part @1.10-1.15 (raw "Alice"))))
	(field (field "age") (optional false)
		(e-int @1.23-1.25 (raw "30")))
	(field (field "active") (optional false)
		(e-ident @1.35-1.44 (qaul "Bool") (raw ".true")))
	(field (field "scores") (optional false)
		(e-list @1.54-1.66
			(e-int @1.55-1.57 (raw "95"))
			(e-int @1.59-1.61 (raw "87"))
			(e-int @1.63-1.65 (raw "92"))))
	(field (field "balance") (optional false)
		(e-frac @1.77-1.84 (raw "1250.75"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-1.86 (ext-var 89) (id 90)
	(fields
		(field (name "name")
			(e-string @1.9-1.16
				(e-literal @1.10-1.15 (string "Alice"))))
		(field (name "age")
			(e-int @1.23-1.25 (value "30")))
		(field (name "active")
			(e-runtime-error (tag "ident_not_in_scope")))
		(field (name "scores")
			(e-list @1.54-1.66 (elem-var 84)
				(elems
					(e-int @1.55-1.57 (value "95"))
					(e-int @1.59-1.61 (value "87"))
					(e-int @1.63-1.65 (value "92")))))
		(field (name "balance")
			(e-frac-dec @1.77-1.84 (value "1250.75")))))
~~~
# TYPES
~~~clojure
(expr (id 90) (type "{ name: Str, age: Num(*), active: Error, scores: List(Num(*)), balance: Frac(*) }"))
~~~
