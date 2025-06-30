# META
~~~ini
description=parse_as_ann
type=expr
~~~
# SOURCE
~~~roc
foo : Foo.Bar.Baz x y as Blah a b

42
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpColon(1:5-1:6),UpperIdent(1:7-1:10),NoSpaceDotUpperIdent(1:10-1:14),NoSpaceDotUpperIdent(1:14-1:18),LowerIdent(1:19-1:20),LowerIdent(1:21-1:22),KwAs(1:23-1:25),UpperIdent(1:26-1:30),LowerIdent(1:31-1:32),LowerIdent(1:33-1:34),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(3:1-3:3),EndOfFile(3:3-3:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.4 (qaul "") (raw "foo"))
~~~
# FORMATTED
~~~roc
foo
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Error"))
~~~
