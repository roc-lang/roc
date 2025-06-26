# META
~~~ini
description=Record with reserved keyword fields (error case)
type=expr
~~~
# SOURCE
~~~roc
{
    if: "conditional",
    when: "pattern match",
    expect: "test assertion",
    import: "module load",
    and: Bool.true,
    or: Bool.false,
}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **: "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:5:1:8:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:20:1:26:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"pattern match** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:28:1:42:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **pattern match"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:29:1:43:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **",** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:42:1:44:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, expect** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:43:1:51:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:51:1:54:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, import** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:69:1:77:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**IMPORT MUST BE TOP LEVEL**
Import statements must appear at the top level of a module.
Move this import to the top of the file, after the module header but before any definitions.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:71:1:78:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:77:1:80:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, and** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:92:1:97:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **and:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:94:1:98:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: Bool** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:97:1:103:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, or** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:108:1:112:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **or:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:110:1:113:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: Bool** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:1:112:1:118:**
```roc
{ if: "conditional", when: "pattern match", expect: "test assertion", import: "module load", and: Bool.true, or: Bool.false }
```


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**NOT IMPLEMENTED**
This feature is not yet implemented: statement type in block

**NOT IMPLEMENTED**
This feature is not yet implemented: statement type in block

**UNDEFINED VARIABLE**
Nothing is named `true` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `false` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenCurly(1:1-1:2),KwIf(1:3-1:5),OpColon(1:5-1:6),StringStart(1:7-1:8),StringPart(1:8-1:19),StringEnd(1:19-1:20),Comma(1:20-1:21),LowerIdent(1:22-1:26),OpColon(1:26-1:27),StringStart(1:28-1:29),StringPart(1:29-1:42),StringEnd(1:42-1:43),Comma(1:43-1:44),KwExpect(1:45-1:51),OpColon(1:51-1:52),StringStart(1:53-1:54),StringPart(1:54-1:68),StringEnd(1:68-1:69),Comma(1:69-1:70),KwImport(1:71-1:77),OpColon(1:77-1:78),StringStart(1:79-1:80),StringPart(1:80-1:91),StringEnd(1:91-1:92),Comma(1:92-1:93),OpAnd(1:94-1:97),OpColon(1:97-1:98),UpperIdent(1:99-1:103),NoSpaceDotLowerIdent(1:103-1:108),Comma(1:108-1:109),OpOr(1:110-1:112),OpColon(1:112-1:113),UpperIdent(1:114-1:118),NoSpaceDotLowerIdent(1:118-1:124),CloseCurly(1:125-1:126),EndOfFile(1:126-1:126),
~~~
# PARSE
~~~clojure
(e-block @1-1-1-126
	(statements
		(e-malformed @1-20-1-26 (reason "no_else"))
		(s-type-anno @1-22-1-42 (name "when")
			(ty-malformed @1-28-1-42 (tag "ty_anno_unexpected_token")))
		(e-malformed @1-29-1-43 (reason "expr_unexpected_token"))
		(e-malformed @1-42-1-44 (reason "expr_unexpected_token"))
		(e-malformed @1-43-1-51 (reason "expr_unexpected_token"))
		(s-expect @1-45-1-54
			(e-malformed @1-51-1-54 (reason "expr_unexpected_token")))
		(e-string @1-53-1-69
			(e-string-part @1-54-1-68 (raw "test assertion")))
		(e-malformed @1-69-1-77 (reason "expr_unexpected_token"))
		(s-malformed @1-71-1-78 (tag "import_must_be_top_level"))
		(e-malformed @1-77-1-80 (reason "expr_unexpected_token"))
		(e-string @1-79-1-92
			(e-string-part @1-80-1-91 (raw "module load")))
		(e-malformed @1-92-1-97 (reason "expr_unexpected_token"))
		(e-malformed @1-94-1-98 (reason "expr_unexpected_token"))
		(e-malformed @1-97-1-103 (reason "expr_unexpected_token"))
		(e-ident @1-99-1-108 (qaul "Bool") (raw ".true"))
		(e-malformed @1-108-1-112 (reason "expr_unexpected_token"))
		(e-malformed @1-110-1-113 (reason "expr_unexpected_token"))
		(e-malformed @1-112-1-118 (reason "expr_unexpected_token"))
		(e-ident @1-114-1-124 (qaul "Bool") (raw ".false"))))
~~~
# FORMATTED
~~~roc
{

	when :



	expect
	"test assertion"



	"module load"



	Bool.true



	Bool.false
}
~~~
# TYPES
~~~clojure
(expr (id 91) (type "*"))
~~~
