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
UNDEFINED VARIABLE - pattern_destructure_nested.md:1:7:1:13
UNUSED VARIABLE - pattern_destructure_nested.md:2:38:2:47
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**pattern_destructure_nested.md:1:7:1:13:**
```roc
match person {
```
      ^^^^^^


**UNUSED VARIABLE**
Variable ``zipCode`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_zipCode` to suppress this warning.

**pattern_destructure_nested.md:2:38:2:47:**
```roc
    { name, address: { street, city, zipCode } } => "${name} lives on ${street} in ${city}"
```
                                     ^^^^^^^^^


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
					(pattern (degenerate false)
						(p-record-destructure @2.5-2.49
							(destructs
								(record-destruct @2.7-2.12 (label "name") (ident "name")
									(required))
								(record-destruct @2.13-2.49 (label "address") (ident "address")
									(sub-pattern
										(p-record-destructure @2.22-2.47
											(destructs
												(record-destruct @2.24-2.31 (label "street") (ident "street")
													(required))
												(record-destruct @2.32-2.37 (label "city") (ident "city")
													(required))
												(record-destruct @2.38-2.47 (label "zipCode") (ident "zipCode")
													(required))))))))))
				(value
					(e-string @2.53-2.92
						(e-literal @2.54-2.54 (string ""))
						(e-lookup-local @2.56-2.60
							(p-assign @2.7-2.12 (ident "name")))
						(e-literal @2.61-2.71 (string " lives on "))
						(e-lookup-local @2.73-2.79
							(p-assign @2.24-2.31 (ident "street")))
						(e-literal @2.80-2.84 (string " in "))
						(e-lookup-local @2.86-2.90
							(p-assign @2.32-2.37 (ident "city")))
						(e-literal @2.91-2.91 (string ""))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "Str"))
~~~
