# META
~~~ini
description=opt_field_newline_in_pat
type=expr
~~~
# SOURCE
~~~roc
{i
?
Y}=p
Q
~~~
# EXPECTED
UNDEFINED VARIABLE - opt_field_newline_in_pat.md:1:2:1:3
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

**opt_field_newline_in_pat.md:1:2:1:3:**
```roc
{i
```
 ^


**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: binop
Let us know if you want to help!

# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:2-1:3),Newline(1:1-1:1),
OpQuestion(2:1-2:2),Newline(1:1-1:1),
UpperIdent(3:1-3:2),CloseCurly(3:2-3:3),OpAssign(3:3-3:4),LowerIdent(3:4-3:5),Newline(1:1-1:1),
UpperIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-3.3
	(statements
		(e-binop @1.2-3.3 (op "?")
			(e-ident @1.2-1.3 (raw "i"))
			(e-tag @3.1-3.2 (raw "Y")))))
~~~
# FORMATTED
~~~roc
{
	i
		?
		Y
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-3.3
	(e-runtime-error (tag "not_implemented")))
~~~
# TYPES
~~~clojure
(expr @1.1-3.3 (type "Error"))
~~~
