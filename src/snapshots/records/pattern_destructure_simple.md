# META
~~~ini
description=Simple record destructuring pattern
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name, age } => name
}
~~~
# EXPECTED
UNDEFINED VARIABLE - pattern_destructure_simple.md:1:7:1:13
UNUSED VARIABLE - pattern_destructure_simple.md:2:13:2:18
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**pattern_destructure_simple.md:1:7:1:13:**
```roc
match person {
```
      ^^^^^^


**UNUSED VARIABLE**
Variable ``age`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_age` to suppress this warning.

**pattern_destructure_simple.md:2:13:2:18:**
```roc
    { name, age } => name
```
            ^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:13),OpenCurly(1:14-1:15),Newline(1:1-1:1),
OpenCurly(2:5-2:6),LowerIdent(2:7-2:11),Comma(2:11-2:12),LowerIdent(2:13-2:16),CloseCurly(2:17-2:18),OpFatArrow(2:19-2:21),LowerIdent(2:22-2:26),Newline(1:1-1:1),
CloseCurly(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.13 (raw "person"))
	(branches
		(branch @2.5-3.2
			(p-record @2.5-2.18
				(field @2.7-2.12 (name "name") (rest false))
				(field @2.13-2.18 (name "age") (rest false)))
			(e-ident @2.22-2.26 (raw "name")))))
~~~
# FORMATTED
~~~roc
match person {
	{ name, age } => name
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
						(p-record-destructure @2.5-2.18
							(destructs
								(record-destruct @2.7-2.12 (label "name") (ident "name")
									(required))
								(record-destruct @2.13-2.18 (label "age") (ident "age")
									(required))))))
				(value
					(e-lookup-local @2.22-2.26
						(p-assign @2.7-2.12 (ident "name"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "*"))
~~~
