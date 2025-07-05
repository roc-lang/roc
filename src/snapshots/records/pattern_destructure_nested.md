# META
~~~ini
description=Nested record destructuring pattern in a match expression
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name, address: { street, city, zipCode } } => "${name} lives on ${street} in ${city}"
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: record pattern with sub-patterns
Let us know if you want to help!

**UNDEFINED VARIABLE**
Nothing is named `street` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `city` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:13),OpenCurly(1:14-1:15),Newline(1:1-1:1),
OpenCurly(2:5-2:6),LowerIdent(2:7-2:11),Comma(2:11-2:12),LowerIdent(2:13-2:20),OpColon(2:20-2:21),OpenCurly(2:22-2:23),LowerIdent(2:24-2:30),Comma(2:30-2:31),LowerIdent(2:32-2:36),Comma(2:36-2:37),LowerIdent(2:38-2:45),CloseCurly(2:46-2:47),CloseCurly(2:48-2:49),OpFatArrow(2:50-2:52),StringStart(2:53-2:54),StringPart(2:54-2:54),OpenStringInterpolation(2:54-2:56),LowerIdent(2:56-2:60),CloseStringInterpolation(2:60-2:61),StringPart(2:61-2:71),OpenStringInterpolation(2:71-2:73),LowerIdent(2:73-2:79),CloseStringInterpolation(2:79-2:80),StringPart(2:80-2:84),OpenStringInterpolation(2:84-2:86),LowerIdent(2:86-2:90),CloseStringInterpolation(2:90-2:91),StringPart(2:91-2:91),StringEnd(2:91-2:92),Newline(1:1-1:1),
CloseCurly(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.13 (raw "person"))
	(branches
		(branch @1.1-1.1
			(p-record @2.5-2.49
				(field @2.7-2.12 (name "name") (rest false))
				(field @2.13-2.49 (name "address") (rest false)
					(p-record @2.22-2.47
						(field @2.24-2.31 (name "street") (rest false))
						(field @2.32-2.37 (name "city") (rest false))
						(field @2.38-2.47 (name "zipCode") (rest false)))))
			(e-string @2.53-2.92
				(e-string-part @2.54-2.54 (raw ""))
				(e-ident @2.56-2.60 (raw "name"))
				(e-string-part @2.61-2.71 (raw " lives on "))
				(e-ident @2.73-2.79 (raw "street"))
				(e-string-part @2.80-2.84 (raw " in "))
				(e-ident @2.86-2.90 (raw "city"))
				(e-string-part @2.91-2.91 (raw ""))))))
~~~
# FORMATTED
~~~roc
match person {
	{ name, address: { street, city, zipCode } } => "${name} lives on ${street} in ${city}"
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-3.2
	(match @1.1-3.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(p-runtime-error @2.13-2.49 (tag "not_implemented") (degenerate false)))
				(value
					(e-string @2.53-2.92
						(e-literal @2.54-2.54 (string ""))
						(e-lookup-local @2.56-2.60
							(pattern @2.7-2.12))
						(e-literal @2.61-2.71 (string " lives on "))
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-literal @2.80-2.84 (string " in "))
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-literal @2.91-2.91 (string ""))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "Str"))
~~~
