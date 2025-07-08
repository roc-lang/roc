# META
~~~ini
description=Simple record destructuring in match expression
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name } => name
    { age } => age
}
~~~
# EXPECTED
UNDEFINED VARIABLE - simple_record.md:1:7:1:13
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**simple_record.md:1:7:1:13:**
```roc
match person {
```
      ^^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:13),OpenCurly(1:14-1:15),Newline(1:1-1:1),
OpenCurly(2:5-2:6),LowerIdent(2:7-2:11),CloseCurly(2:12-2:13),OpFatArrow(2:14-2:16),LowerIdent(2:17-2:21),Newline(1:1-1:1),
OpenCurly(3:5-3:6),LowerIdent(3:7-3:10),CloseCurly(3:11-3:12),OpFatArrow(3:13-3:15),LowerIdent(3:16-3:19),Newline(1:1-1:1),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.13 (raw "person"))
	(branches
		(branch @2.5-3.6
			(p-record @2.5-2.13
				(field @2.7-2.13 (name "name") (rest false)))
			(e-ident @2.17-2.21 (raw "name")))
		(branch @3.5-4.2
			(p-record @3.5-3.12
				(field @3.7-3.12 (name "age") (rest false)))
			(e-ident @3.16-3.19 (raw "age")))))
~~~
# FORMATTED
~~~roc
match person {
	{ name } => name
	{ age } => age
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-4.2
	(match
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch @2.17-2.21
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @2.5-2.13
							(destructs
								(record-destruct @2.7-2.13 (label "name") (ident "name")
									(required))))))
				(value
					(e-lookup-local @2.17-2.21
						(p-assign @2.7-2.13 (ident "name")))))
			(branch @3.16-3.19
				(patterns
					(pattern (degenerate false)
						(p-record-destructure @3.5-3.12
							(destructs
								(record-destruct @3.7-3.12 (label "age") (ident "age")
									(required))))))
				(value
					(e-lookup-local @3.16-3.19
						(p-assign @3.7-3.12 (ident "age"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "*"))
~~~
