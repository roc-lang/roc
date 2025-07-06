# META
~~~ini
description=record_literal_field_bang
type=expr
~~~
# SOURCE
~~~roc
{
    answer: 42,
    launchTheNukes!: |{}| boom,
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - record_literal_field_bang.md:3:22:3:24
PARSE ERROR - record_literal_field_bang.md:3:23:3:25
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `boom` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_literal_field_bang.md:3:27:3:31:**
```roc
    launchTheNukes!: |{}| boom,
```
                          ^^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:11),OpColon(2:11-2:12),Int(2:13-2:15),Comma(2:15-2:16),Newline(1:1-1:1),
LowerIdent(3:5-3:20),OpColon(3:20-3:21),OpBar(3:22-3:23),OpenCurly(3:23-3:24),CloseCurly(3:24-3:25),OpBar(3:25-3:26),LowerIdent(3:27-3:31),Comma(3:31-3:32),Newline(1:1-1:1),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-record @1.1-4.2
	(field (field "answer") (optional false)
		(e-int @2.13-2.15 (raw "42")))
	(field (field "launchTheNukes!") (optional false)
		(e-lambda @3.22-3.31
			(args
				(p-record @3.23-3.25))
			(e-ident @3.27-3.31 (raw "boom")))))
~~~
# FORMATTED
~~~roc
{

	answer: 42,
	launchTheNukes!: |{}| boom

}
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-4.2
	(fields
		(field (name "answer")
			(e-int @2.13-2.15 (value "42")))
		(field (name "launchTheNukes!")
			(e-lambda @3.22-3.31
				(args
					(p-record-destructure @3.23-3.25
						(destructs)))
				(e-runtime-error (tag "ident_not_in_scope"))))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "{ answer: Num(*), launchTheNukes!: * -> Error }"))
~~~
