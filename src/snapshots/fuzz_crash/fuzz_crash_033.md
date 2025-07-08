# META
~~~ini
description=minimal reproduction of record parsing index out of bounds crash
type=expr
~~~
# SOURCE
~~~roc
{ i, Complete]
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_033.md:1:6:1:15
UNDEFINED VARIABLE - fuzz_crash_033.md:1:3:1:5
# PROBLEMS
**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**PARSE ERROR**
A parsing error occurred: `expected_expr_record_field_name`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_033.md:1:6:1:15:**
```roc
{ i, Complete]
```
     ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

**fuzz_crash_033.md:1:3:1:5:**
```roc
{ i, Complete]
```
  ^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:4),Comma(1:4-1:5),UpperIdent(1:6-1:14),CloseCurly(1:14-1:15),EndOfFile(1:15-1:15),
~~~
# PARSE
~~~clojure
(e-record @1.1-1.15
	(field (field "i"))
	(field (field "{")))
~~~
# FORMATTED
~~~roc
{i, {}
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-1.15
	(fields
		(field (name "i")
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.15 (type "{ i: Error }"))
~~~
