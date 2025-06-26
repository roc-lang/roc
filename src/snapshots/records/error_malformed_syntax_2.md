# META
~~~ini
description=Malformed record syntax (error case)
type=expr
~~~
# SOURCE
~~~roc
{ name = "Alice" }
~~~
# PROBLEMS
**UNUSED VARIABLE**
Variable ``name`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_name` to suppress this warning.
The unused variable is declared here:
**error_malformed_syntax_2.md:1:3:1:7:**
```roc
{ name = "Alice" }
```


# TOKENS
~~~zig
OpenCurly(1:1-1:2),LowerIdent(1:3-1:7),OpAssign(1:8-1:9),StringStart(1:10-1:11),StringPart(1:11-1:16),StringEnd(1:16-1:17),CloseCurly(1:18-1:19),EndOfFile(1:19-1:19),
~~~
# PARSE
~~~clojure
(e-block @1-1-1-19
	(statements
		(s-decl @1-3-1-17
			(p-ident @1-3-1-7 (raw "name"))
			(e-string @1-10-1-17
				(e-string-part @1-11-1-16 (raw "Alice"))))))
~~~
# FORMATTED
~~~roc
name = "Alice"
~~~
# TYPES
~~~clojure
(expr (id 76) (type "*"))
~~~