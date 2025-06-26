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
**record_different_fileds_error.md:1:18:1:37:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **leading underscore"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:19:1:38:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **",** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:37:1:39:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, field_** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:38:1:46:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"trailing underscore** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_different_fileds_error.md:1:48:1:68:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **trailing underscore"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:49:1:69:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **",** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:68:1:70:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, PascalCase** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:69:1:81:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:81:1:84:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, kebab** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:91:1:98:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:103:1:106:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, field** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:112:1:119:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **$special** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:119:1:127:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"dollar** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_different_fileds_error.md:1:129:1:136:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **dollar"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:130:1:137:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **",** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:136:1:138:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, field** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:137:1:144:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **@symbol:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:144:1:152:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fileds_error.md:1:151:1:154:**
```roc
{ _privateField: "leading underscore", field_: "trailing underscore", PascalCase: "pascal", kebab-case: "kebab", field$special: "dollar", field@symbol: "at symbol" }
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
OpenCurly(1:1-1:2),NamedUnderscore(1:3-1:16),OpColon(1:16-1:17),StringStart(1:18-1:19),StringPart(1:19-1:37),StringEnd(1:37-1:38),Comma(1:38-1:39),LowerIdent(1:40-1:46),OpColon(1:46-1:47),StringStart(1:48-1:49),StringPart(1:49-1:68),StringEnd(1:68-1:69),Comma(1:69-1:70),UpperIdent(1:71-1:81),OpColon(1:81-1:82),StringStart(1:83-1:84),StringPart(1:84-1:90),StringEnd(1:90-1:91),Comma(1:91-1:92),LowerIdent(1:93-1:98),OpBinaryMinus(1:98-1:99),LowerIdent(1:99-1:103),OpColon(1:103-1:104),StringStart(1:105-1:106),StringPart(1:106-1:111),StringEnd(1:111-1:112),Comma(1:112-1:113),LowerIdent(1:114-1:119),MalformedUnknownToken(1:119-1:120),LowerIdent(1:120-1:127),OpColon(1:127-1:128),StringStart(1:129-1:130),StringPart(1:130-1:136),StringEnd(1:136-1:137),Comma(1:137-1:138),LowerIdent(1:139-1:144),OpaqueName(1:144-1:151),OpColon(1:151-1:152),StringStart(1:153-1:154),StringPart(1:154-1:163),StringEnd(1:163-1:164),CloseCurly(1:165-1:166),EndOfFile(1:166-1:166),
~~~
# PARSE
~~~clojure
(e-block @1-1-1-166
	(statements
		(s-type-anno @1-3-1-37 (name "_privateField")
			(ty-malformed @1-18-1-37 (tag "ty_anno_unexpected_token")))
		(e-malformed @1-19-1-38 (reason "expr_unexpected_token"))
		(e-malformed @1-37-1-39 (reason "expr_unexpected_token"))
		(e-malformed @1-38-1-46 (reason "expr_unexpected_token"))
		(s-type-anno @1-40-1-68 (name "field_")
			(ty-malformed @1-48-1-68 (tag "ty_anno_unexpected_token")))
		(e-malformed @1-49-1-69 (reason "expr_unexpected_token"))
		(e-malformed @1-68-1-70 (reason "expr_unexpected_token"))
		(e-malformed @1-69-1-81 (reason "expr_unexpected_token"))
		(e-tag @1-71-1-81 (raw "PascalCase"))
		(e-malformed @1-81-1-84 (reason "expr_unexpected_token"))
		(e-string @1-83-1-91
			(e-string-part @1-84-1-90 (raw "pascal")))
		(e-malformed @1-91-1-98 (reason "expr_unexpected_token"))
		(e-binop @1-93-1-104 (op "-")
			(e-ident @1-93-1-98 (qaul "") (raw "kebab"))
			(e-ident @1-99-1-103 (qaul "") (raw "case")))
		(e-malformed @1-103-1-106 (reason "expr_unexpected_token"))
		(e-string @1-105-1-112
			(e-string-part @1-106-1-111 (raw "kebab")))
		(e-malformed @1-112-1-119 (reason "expr_unexpected_token"))
		(e-ident @1-114-1-119 (qaul "") (raw "field"))
		(e-malformed @1-119-1-127 (reason "expr_unexpected_token"))
		(s-type-anno @1-120-1-136 (name "special")
			(ty-malformed @1-129-1-136 (tag "ty_anno_unexpected_token")))
		(e-malformed @1-130-1-137 (reason "expr_unexpected_token"))
		(e-malformed @1-136-1-138 (reason "expr_unexpected_token"))
		(e-malformed @1-137-1-144 (reason "expr_unexpected_token"))
		(e-ident @1-139-1-144 (qaul "") (raw "field"))
		(e-malformed @1-144-1-152 (reason "expr_unexpected_token"))
		(e-malformed @1-151-1-154 (reason "expr_unexpected_token"))
		(e-string @1-153-1-164
			(e-string-part @1-154-1-163 (raw "at symbol")))))
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
(expr (id 107) (type "*"))
~~~
