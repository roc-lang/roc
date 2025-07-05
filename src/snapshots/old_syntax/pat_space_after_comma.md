# META
~~~ini
description=pat_space_after_comma
type=expr
~~~
# SOURCE
~~~roc
{i
,p}=5
Q
~~~
# EXPECTED
UNDEFINED VARIABLE - pat_space_after_comma.md:1:2:2:2
UNDEFINED VARIABLE - pat_space_after_comma.md:2:2:2:4
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

**pat_space_after_comma.md:1:2:2:2:**
```roc
{i
,p}=5
```


**UNDEFINED VARIABLE**
Nothing is named `p` in this scope.
Is there an `import` or `exposing` missing up-top?

**pat_space_after_comma.md:2:2:2:4:**
```roc
,p}=5
```
 ^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:2-1:3),Newline(1:1-1:1),
Comma(2:1-2:2),LowerIdent(2:2-2:3),CloseCurly(2:3-2:4),OpAssign(2:4-2:5),Int(2:5-2:6),Newline(1:1-1:1),
UpperIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-record @1.1-2.4
	(field (field "i") (optional false))
	(field (field "p") (optional false)))
~~~
# FORMATTED
~~~roc
{
	i,
	p
}
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-2.4
	(fields
		(field (name "i")
			(e-runtime-error (tag "ident_not_in_scope")))
		(field (name "p")
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(expr @1.1-2.4 (type "{ i: Error, p: Error }"))
~~~
