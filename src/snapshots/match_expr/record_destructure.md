# META
~~~ini
description=Match expression with record destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name, age } => name
    { name, address: { city } } => city
    {} => "empty"
}
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNUSED VARIABLE**
Variable ``age`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_age` to suppress this warning.
The unused variable is declared here:
**record_destructure.md:2:13:2:18:**
```roc
    { name, age } => name
```
            ^^^^^


**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: record pattern with sub-patterns
Let us know if you want to help!

**UNDEFINED VARIABLE**
Nothing is named `city` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNUSED VARIABLE**
Variable ``name`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_name` to suppress this warning.
The unused variable is declared here:
**record_destructure.md:3:7:3:12:**
```roc
    { name, address: { city } } => city
```
      ^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:13),OpenCurly(1:14-1:15),Newline(1:1-1:1),
OpenCurly(2:5-2:6),LowerIdent(2:7-2:11),Comma(2:11-2:12),LowerIdent(2:13-2:16),CloseCurly(2:17-2:18),OpFatArrow(2:19-2:21),LowerIdent(2:22-2:26),Newline(1:1-1:1),
OpenCurly(3:5-3:6),LowerIdent(3:7-3:11),Comma(3:11-3:12),LowerIdent(3:13-3:20),OpColon(3:20-3:21),OpenCurly(3:22-3:23),LowerIdent(3:24-3:28),CloseCurly(3:29-3:30),CloseCurly(3:31-3:32),OpFatArrow(3:33-3:35),LowerIdent(3:36-3:40),Newline(1:1-1:1),
OpenCurly(4:5-4:6),CloseCurly(4:6-4:7),OpFatArrow(4:8-4:10),StringStart(4:11-4:12),StringPart(4:12-4:17),StringEnd(4:17-4:18),Newline(1:1-1:1),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.13 (qaul "") (raw "person"))
	(branches
		(branch @2.5-3.6
			(p-record @2.5-2.18
				(field @2.7-2.12 (name "name") (rest false))
				(field @2.13-2.18 (name "age") (rest false)))
			(e-ident @2.22-2.26 (qaul "") (raw "name")))
		(branch @3.5-4.6
			(p-record @3.5-3.32
				(field @3.7-3.12 (name "name") (rest false))
				(field @3.13-3.32 (name "address") (rest false)
					(p-record @3.22-3.30
						(field @3.24-3.30 (name "city") (rest false)))))
			(e-ident @3.36-3.40 (qaul "") (raw "city")))
		(branch @1.1-1.1
			(p-record @4.5-4.7)
			(e-string @4.11-4.18
				(e-string-part @4.12-4.17 (raw "empty"))))))
~~~
# FORMATTED
~~~roc
match person {
	{ name, age } => name
	{ name, address: { city } } => city
	{} => "empty"
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-5.2
	(match @1.1-5.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(p-record-destructure @2.5-2.18 (degenerate false)
						(destructs
							(record-destruct @2.7-2.12 (label "name") (ident "name")
								(required))
							(record-destruct @2.13-2.18 (label "age") (ident "age")
								(required)))))
				(value
					(e-lookup-local @2.22-2.26
						(pattern @2.7-2.12))))
			(branch
				(patterns
					(p-runtime-error @3.13-3.32 (tag "not_implemented") (degenerate false)))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(p-record-destructure @4.5-4.7 (degenerate false)
						(destructs)))
				(value
					(e-string @4.11-4.18
						(e-literal @4.12-4.17 (string "empty"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "*"))
~~~
