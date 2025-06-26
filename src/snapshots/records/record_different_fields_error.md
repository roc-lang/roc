# META
~~~ini
description=Record with special character fields (error cases)
type=expr
~~~
# SOURCE
~~~roc
{
    _privateField: "leading underscore",
    field_: "trailing underscore",
    PascalCase: "pascal",
    kebab-case: "kebab",
    field$special: "dollar",
    field@symbol: "at symbol",
}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"leading underscore** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_different_fields_error.md:2:20:2:39:**
```roc
    _privateField: "leading underscore",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **leading underscore"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:2:21:2:40:**
```roc
    _privateField: "leading underscore",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **",** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:2:39:2:41:**
```roc
    _privateField: "leading underscore",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:2:40:2:40:**
```roc
    _privateField: "leading underscore",
```


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"trailing underscore** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_different_fields_error.md:3:13:3:33:**
```roc
    field_: "trailing underscore",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **trailing underscore"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:3:14:3:34:**
```roc
    field_: "trailing underscore",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **",** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:3:33:3:35:**
```roc
    field_: "trailing underscore",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:3:34:3:34:**
```roc
    field_: "trailing underscore",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:4:15:4:18:**
```roc
    PascalCase: "pascal",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:4:25:4:25:**
```roc
    PascalCase: "pascal",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:5:15:5:18:**
```roc
    kebab-case: "kebab",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:5:24:5:24:**
```roc
    kebab-case: "kebab",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **$special** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:6:10:6:18:**
```roc
    field$special: "dollar",
```


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"dollar** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_different_fields_error.md:6:20:6:27:**
```roc
    field$special: "dollar",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dollar"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:6:21:6:28:**
```roc
    field$special: "dollar",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **",** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:6:27:6:29:**
```roc
    field$special: "dollar",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:6:28:6:28:**
```roc
    field$special: "dollar",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **@symbol:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:7:10:7:18:**
```roc
    field@symbol: "at symbol",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:7:17:7:20:**
```roc
    field@symbol: "at symbol",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_error.md:7:30:7:30:**
```roc
    field@symbol: "at symbol",
```


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**UNDEFINED VARIABLE**
Nothing is named `kebab` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `case` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `field` in this scope.
Is there an `import` or `exposing` missing up-top?

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**UNDEFINED VARIABLE**
Nothing is named `field` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
NamedUnderscore(2:5-2:18),OpColon(2:18-2:19),StringStart(2:20-2:21),StringPart(2:21-2:39),StringEnd(2:39-2:40),Comma(2:40-2:41),Newline(1:1-1:1),
LowerIdent(3:5-3:11),OpColon(3:11-3:12),StringStart(3:13-3:14),StringPart(3:14-3:33),StringEnd(3:33-3:34),Comma(3:34-3:35),Newline(1:1-1:1),
UpperIdent(4:5-4:15),OpColon(4:15-4:16),StringStart(4:17-4:18),StringPart(4:18-4:24),StringEnd(4:24-4:25),Comma(4:25-4:26),Newline(1:1-1:1),
LowerIdent(5:5-5:10),OpBinaryMinus(5:10-5:11),LowerIdent(5:11-5:15),OpColon(5:15-5:16),StringStart(5:17-5:18),StringPart(5:18-5:23),StringEnd(5:23-5:24),Comma(5:24-5:25),Newline(1:1-1:1),
LowerIdent(6:5-6:10),MalformedUnknownToken(6:10-6:11),LowerIdent(6:11-6:18),OpColon(6:18-6:19),StringStart(6:20-6:21),StringPart(6:21-6:27),StringEnd(6:27-6:28),Comma(6:28-6:29),Newline(1:1-1:1),
LowerIdent(7:5-7:10),OpaqueName(7:10-7:17),OpColon(7:17-7:18),StringStart(7:19-7:20),StringPart(7:20-7:29),StringEnd(7:29-7:30),Comma(7:30-7:31),Newline(1:1-1:1),
CloseCurly(8:1-8:2),EndOfFile(8:2-8:2),
~~~
# PARSE
~~~clojure
(e-block @1-1-8-2
	(statements
		(s-type-anno @2-5-2-39 (name "_privateField")
			(ty-malformed @2-20-2-39 (tag "ty_anno_unexpected_token")))
		(e-malformed @2-21-2-40 (reason "expr_unexpected_token"))
		(e-malformed @2-39-2-41 (reason "expr_unexpected_token"))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))
		(s-type-anno @3-5-3-33 (name "field_")
			(ty-malformed @3-13-3-33 (tag "ty_anno_unexpected_token")))
		(e-malformed @3-14-3-34 (reason "expr_unexpected_token"))
		(e-malformed @3-33-3-35 (reason "expr_unexpected_token"))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))
		(e-tag @4-5-4-15 (raw "PascalCase"))
		(e-malformed @4-15-4-18 (reason "expr_unexpected_token"))
		(e-string @4-17-4-25
			(e-string-part @4-18-4-24 (raw "pascal")))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))
		(e-binop @5-5-5-16 (op "-")
			(e-ident @5-5-5-10 (qaul "") (raw "kebab"))
			(e-ident @5-11-5-15 (qaul "") (raw "case")))
		(e-malformed @5-15-5-18 (reason "expr_unexpected_token"))
		(e-string @5-17-5-24
			(e-string-part @5-18-5-23 (raw "kebab")))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))
		(e-ident @6-5-6-10 (qaul "") (raw "field"))
		(e-malformed @6-10-6-18 (reason "expr_unexpected_token"))
		(s-type-anno @6-11-6-27 (name "special")
			(ty-malformed @6-20-6-27 (tag "ty_anno_unexpected_token")))
		(e-malformed @6-21-6-28 (reason "expr_unexpected_token"))
		(e-malformed @6-27-6-29 (reason "expr_unexpected_token"))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))
		(e-ident @7-5-7-10 (qaul "") (raw "field"))
		(e-malformed @7-10-7-18 (reason "expr_unexpected_token"))
		(e-malformed @7-17-7-20 (reason "expr_unexpected_token"))
		(e-string @7-19-7-30
			(e-string-part @7-20-7-29 (raw "at symbol")))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
{
	_privateField : 
	
	
	
	field_ : 
	
	
	
	PascalCase
	
	"pascal"
	
	kebab - case
	
	"kebab"
	
	field
	
	special : 
	
	
	
	field
	
	
	"at symbol"
	
}
~~~
# TYPES
~~~clojure
(expr (id 109) (type "*"))
~~~