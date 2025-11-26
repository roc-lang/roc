# META
~~~ini
description=if_then_else (9)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	1
} else if 10 { # Comment after else open
	A
} else { # Comment after else open
	3
}
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_9.md:1:4:1:8
MISSING METHOD - if_then_else_9.md:3:11:3:13
MISSING METHOD - if_then_else_9.md:2:2:2:3
MISSING METHOD - if_then_else_9.md:6:2:6:3
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `bool` in this scope.
Is there an `import` or `exposing` missing up-top?

**if_then_else_9.md:1:4:1:8:**
```roc
if bool {
```
   ^^^^


**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**if_then_else_9.md:3:11:3:13:**
```roc
} else if 10 { # Comment after else open
```
          ^^

The value's type, which does not have a method named **from_numeral**, is:

    _Bool_

**Hint: **For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**if_then_else_9.md:2:2:2:3:**
```roc
	1
```
	^

The value's type, which does not have a method named **from_numeral**, is:

    _[A]_others_


**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**if_then_else_9.md:6:2:6:3:**
```roc
	3
```
	^

The value's type, which does not have a method named **from_numeral**, is:

    _[A]_others_


# TOKENS
~~~zig
KwIf,LowerIdent,OpenCurly,
Int,
CloseCurly,KwElse,KwIf,Int,OpenCurly,
UpperIdent,
CloseCurly,KwElse,OpenCurly,
Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-ident (raw "bool"))
	(e-block
		(statements
			(e-int (raw "1"))))
	(e-if-then-else
		(e-int (raw "10"))
		(e-block
			(statements
				(e-tag (raw "A"))))
		(e-block
			(statements
				(e-int (raw "3"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if
	(if-branches
		(if-branch
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-block
				(e-num (value "1"))))
		(if-branch
			(e-num (value "10"))
			(e-block
				(e-tag (name "A")))))
	(if-else
		(e-block
			(e-num (value "3")))))
~~~
# TYPES
~~~clojure
(expr (type "[A]_others"))
~~~
