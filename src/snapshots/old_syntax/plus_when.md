# META
~~~ini
description=plus_when
type=expr
~~~
# SOURCE
~~~roc
1 +
    when Foo is
        Foo -> 2
        Bar -> 3
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
Int(1:1-1:2),OpPlus(1:3-1:4),Newline(1:1-1:1),
LowerIdent(2:5-2:9),UpperIdent(2:10-2:13),LowerIdent(2:14-2:16),Newline(1:1-1:1),
UpperIdent(3:9-3:12),OpArrow(3:13-3:15),Int(3:16-3:17),Newline(1:1-1:1),
UpperIdent(4:9-4:12),OpArrow(4:13-4:15),Int(4:16-4:17),EndOfFile(4:17-4:17),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.13 (op "+")
	(e-int @1.1-1.2 (raw "1"))
	(e-ident @2.5-2.9 (raw "when")))
~~~
# FORMATTED
~~~roc
1 +
	when
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.13 (op "add")
	(e-int @1.1-1.2 (value "1"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.13 (type "*"))
~~~
