# META
~~~ini
description=pattern_comma_newlines
type=expr
~~~
# SOURCE
~~~roc
1(i,p#
):f
n
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `p` in this scope.
Is there an `import` or `exposing` missing up-top?

**TYPE MISMATCH**
This expression is used in an unexpected way:
**pattern_comma_newlines.md:1:1:1:2:**
```roc
1(i,p#
```
^

It is of type:
    _Num(*)_

But you are trying to use it as:
    _Error, Error -> *_

# TOKENS
~~~zig
Int(1:1-1:2),NoSpaceOpenRound(1:2-1:3),LowerIdent(1:3-1:4),Comma(1:4-1:5),LowerIdent(1:5-1:6),Newline(1:7-1:7),
CloseRound(2:1-2:2),OpColon(2:2-2:3),LowerIdent(2:3-2:4),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-apply @1.1-2.2
	(e-int @1.1-1.2 (raw "1"))
	(e-ident @1.3-1.4 (qaul "") (raw "i"))
	(e-ident @1.5-1.6 (qaul "") (raw "p")))
~~~
# FORMATTED
~~~roc
1(
	i,
	p,
)
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-2.2
	(e-int @1.1-1.2 (value "1"))
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "*"))
~~~
