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
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:2:7:2:8
IF WITHOUT ELSE - record_different_fields_reserved_error.md:2:5:2:7
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_different_fields_reserved_error.md:3:11:3:12
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:3:12:3:25
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:3:25:3:26
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:3:26:3:27
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:4:11:4:12
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:4:29:4:30
IMPORT MUST BE TOP LEVEL - record_different_fields_reserved_error.md:5:5:5:11
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:5:11:5:12
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:5:26:5:27
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:6:5:6:8
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:6:8:6:9
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:6:19:6:20
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:7:5:7:7
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:7:7:7:8
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:7:19:7:20
MALFORMED TYPE - record_different_fields_reserved_error.md:3:11:3:12
NOT IMPLEMENTED - :0:0:0:0
UNDEFINED VARIABLE - record_different_fields_reserved_error.md:6:10:6:19
UNDEFINED VARIABLE - record_different_fields_reserved_error.md:7:9:7:19
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:2:7:2:8:**
```roc
    if: "conditional",
```
      ^


**IF WITHOUT ELSE**
This `if` is being used as an expression, but it doesn't have an `else`.

When `if` is used as an expression (to produce a value), it must have an `else` branch to handle the case when the condition is false.

Here is the problematic code:
**record_different_fields_reserved_error.md:2:5:2:7:**
```roc
    if: "conditional",
```
    ^^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_different_fields_reserved_error.md:3:11:3:12:**
```roc
    when: "pattern match",
```
          ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **pattern match** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:3:12:3:25:**
```roc
    when: "pattern match",
```
           ^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:3:25:3:26:**
```roc
    when: "pattern match",
```
                        ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:3:26:3:27:**
```roc
    when: "pattern match",
```
                         ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:4:11:4:12:**
```roc
    expect: "test assertion",
```
          ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:4:29:4:30:**
```roc
    expect: "test assertion",
```
                            ^


**IMPORT MUST BE TOP LEVEL**
Import statements must appear at the top level of a module.
Move this import to the top of the file, after the module header but before any definitions.

Here is the problematic code:
**record_different_fields_reserved_error.md:5:5:5:11:**
```roc
    import: "module load",
```
    ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:5:11:5:12:**
```roc
    import: "module load",
```
          ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:5:26:5:27:**
```roc
    import: "module load",
```
                         ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **and** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:6:5:6:8:**
```roc
    and: Bool.true,
```
    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:6:8:6:9:**
```roc
    and: Bool.true,
```
       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:6:19:6:20:**
```roc
    and: Bool.true,
```
                  ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **or** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:7:5:7:7:**
```roc
    or: Bool.false,
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:7:7:7:8:**
```roc
    or: Bool.false,
```
      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:7:19:7:20:**
```roc
    or: Bool.false,
```
                  ^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**record_different_fields_reserved_error.md:3:11:3:12:**
```roc
    when: "pattern match",
```
          ^


**NOT IMPLEMENTED**
This feature is not yet implemented: statement type in block

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNDEFINED VARIABLE**
Nothing is named `true` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_different_fields_reserved_error.md:6:10:6:19:**
```roc
    and: Bool.true,
```
         ^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `false` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_different_fields_reserved_error.md:7:9:7:19:**
```roc
    or: Bool.false,
```
        ^^^^^^^^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),
KwIf(2:5-2:7),OpColon(2:7-2:8),StringStart(2:9-2:10),StringPart(2:10-2:21),StringEnd(2:21-2:22),Comma(2:22-2:23),
LowerIdent(3:5-3:9),OpColon(3:9-3:10),StringStart(3:11-3:12),StringPart(3:12-3:25),StringEnd(3:25-3:26),Comma(3:26-3:27),
KwExpect(4:5-4:11),OpColon(4:11-4:12),StringStart(4:13-4:14),StringPart(4:14-4:28),StringEnd(4:28-4:29),Comma(4:29-4:30),
KwImport(5:5-5:11),OpColon(5:11-5:12),StringStart(5:13-5:14),StringPart(5:14-5:25),StringEnd(5:25-5:26),Comma(5:26-5:27),
OpAnd(6:5-6:8),OpColon(6:8-6:9),UpperIdent(6:10-6:14),NoSpaceDotLowerIdent(6:14-6:19),Comma(6:19-6:20),
OpOr(7:5-7:7),OpColon(7:7-7:8),UpperIdent(7:9-7:13),NoSpaceDotLowerIdent(7:13-7:19),Comma(7:19-7:20),
CloseCurly(8:1-8:2),EndOfFile(8:2-8:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-8.2
	(statements
		(e-malformed @2.5-2.23 (reason "no_else"))
		(s-type-anno @3.5-3.12 (name "when")
			(ty-malformed @3.11-3.12 (tag "ty_anno_unexpected_token")))
		(e-malformed @3.12-3.25 (reason "expr_unexpected_token"))
		(e-malformed @3.25-3.26 (reason "expr_unexpected_token"))
		(e-malformed @3.26-3.27 (reason "expr_unexpected_token"))
		(s-expect @4.5-4.12
			(e-malformed @4.11-4.12 (reason "expr_unexpected_token")))
		(e-string @4.13-4.29
			(e-string-part @4.14-4.28 (raw "test assertion")))
		(e-malformed @4.29-4.30 (reason "expr_unexpected_token"))
		(s-malformed @5.5-5.11 (tag "import_must_be_top_level"))
		(e-malformed @5.11-5.12 (reason "expr_unexpected_token"))
		(e-string @5.13-5.26
			(e-string-part @5.14-5.25 (raw "module load")))
		(e-malformed @5.26-5.27 (reason "expr_unexpected_token"))
		(e-malformed @6.5-6.8 (reason "expr_unexpected_token"))
		(e-malformed @6.8-6.9 (reason "expr_unexpected_token"))
		(e-ident @6.10-6.19 (raw "Bool.true"))
		(e-malformed @6.19-6.20 (reason "expr_unexpected_token"))
		(e-malformed @7.5-7.7 (reason "expr_unexpected_token"))
		(e-malformed @7.7-7.8 (reason "expr_unexpected_token"))
		(e-ident @7.9-7.19 (raw "Bool.false"))
		(e-malformed @7.19-7.20 (reason "expr_unexpected_token"))))
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
# CANONICALIZE
~~~clojure
(e-block @1.1-8.2
	(s-type-anno @3.5-3.12 (name "when")
		(ty-malformed @3.11-3.12))
	(s-expr @4.13-4.29
		(e-string @4.13-4.29
			(e-literal @4.14-4.28 (string "test assertion"))))
	(s-expr @5.13-5.26
		(e-string @5.13-5.26
			(e-literal @5.14-5.25 (string "module load"))))
	(s-expr @6.10-6.19
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-expr @7.9-7.19
		(e-runtime-error (tag "ident_not_in_scope")))
	(e-empty_record @1.1-8.2))
~~~
# TYPES
~~~clojure
(expr @1.1-8.2 (type "{}"))
~~~
