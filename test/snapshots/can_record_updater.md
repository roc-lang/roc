# META
~~~ini
description=Test record updater expression (not yet implemented)
type=expr
~~~
# SOURCE
~~~roc
{& foo: 1 }
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - can_record_updater.md:1:2:1:3
UNEXPECTED TOKEN IN TYPE ANNOTATION - can_record_updater.md:1:9:1:10
UNRECOGNIZED SYNTAX - can_record_updater.md:1:2:1:3
MALFORMED TYPE - can_record_updater.md:1:9:1:10
UNUSED VARIABLE - can_record_updater.md:1:4:1:10
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **&** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_record_updater.md:1:2:1:3:**
```roc
{& foo: 1 }
```
 ^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **1** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**can_record_updater.md:1:9:1:10:**
```roc
{& foo: 1 }
```
        ^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**can_record_updater.md:1:2:1:3:**
```roc
{& foo: 1 }
```
 ^

This might be a syntax error, an unsupported language feature, or a typo.

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**can_record_updater.md:1:9:1:10:**
```roc
{& foo: 1 }
```
        ^


**UNUSED VARIABLE**
Variable `foo` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_foo` to suppress this warning.
The unused variable is declared here:
**can_record_updater.md:1:4:1:10:**
```roc
{& foo: 1 }
```
   ^^^^^^


# TOKENS
~~~zig
OpenCurly,OpAmpersand,LowerIdent,OpColon,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(e-malformed (reason "expr_unexpected_token"))
		(s-type-anno (name "foo")
			(ty-malformed (tag "ty_anno_unexpected_token")))))
~~~
# FORMATTED
~~~roc
{
		foo : 
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "foo"))
		(e-anno-only))
	(e-empty_record))
~~~
# TYPES
~~~clojure
(expr (type "{}"))
~~~
