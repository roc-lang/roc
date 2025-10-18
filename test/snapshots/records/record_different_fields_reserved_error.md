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
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:2:5:2:23
MALFORMED TYPE - record_different_fields_reserved_error.md:3:11:3:12
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:3:12:3:25
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:3:25:3:26
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:3:26:3:27
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:4:11:4:12
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:4:29:4:30
NOT IMPLEMENTED - :0:0:0:0
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:5:11:5:12
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:5:26:5:27
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:6:5:6:8
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:6:8:6:9
DOES NOT EXIST - record_different_fields_reserved_error.md:6:10:6:19
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:6:19:6:20
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:7:5:7:7
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:7:7:7:8
DOES NOT EXIST - record_different_fields_reserved_error.md:7:9:7:19
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:7:19:7:20
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:2:7:2:8:**
```roc
    if: "conditional",
```
      ^


**IF WITHOUT ELSE**
This `if` is being used as an expression, but it doesn't have an `else`.

When `if` is used as an expression (to evaluate to a value), it must have an `else` branch to specify what value to use when the condition is `False`.

**record_different_fields_reserved_error.md:2:5:2:7:**
```roc
    if: "conditional",
```
    ^^


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**record_different_fields_reserved_error.md:3:11:3:12:**
```roc
    when: "pattern match",
```
          ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **pattern match** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:3:12:3:25:**
```roc
    when: "pattern match",
```
           ^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:3:25:3:26:**
```roc
    when: "pattern match",
```
                        ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:3:26:3:27:**
```roc
    when: "pattern match",
```
                         ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:4:11:4:12:**
```roc
    expect: "test assertion",
```
          ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:4:29:4:30:**
```roc
    expect: "test assertion",
```
                            ^


**IMPORT MUST BE TOP LEVEL**
Import statements must appear at the top level of a module.
Move this import to the top of the file, after the module header but before any definitions.

**record_different_fields_reserved_error.md:5:5:5:11:**
```roc
    import: "module load",
```
    ^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:5:11:5:12:**
```roc
    import: "module load",
```
          ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:5:26:5:27:**
```roc
    import: "module load",
```
                         ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **and** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:6:5:6:8:**
```roc
    and: Bool.true,
```
    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:6:8:6:9:**
```roc
    and: Bool.true,
```
       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:6:19:6:20:**
```roc
    and: Bool.true,
```
                  ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **or** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:7:5:7:7:**
```roc
    or: Bool.false,
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:7:7:7:8:**
```roc
    or: Bool.false,
```
      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:7:19:7:20:**
```roc
    or: Bool.false,
```
                  ^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:2:5:2:23:**
```roc
    if: "conditional",
```
    ^^^^^^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**record_different_fields_reserved_error.md:3:11:3:12:**
```roc
    when: "pattern match",
```
          ^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:3:12:3:25:**
```roc
    when: "pattern match",
```
           ^^^^^^^^^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:3:25:3:26:**
```roc
    when: "pattern match",
```
                        ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:3:26:3:27:**
```roc
    when: "pattern match",
```
                         ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:4:11:4:12:**
```roc
    expect: "test assertion",
```
          ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:4:29:4:30:**
```roc
    expect: "test assertion",
```
                            ^

This might be a syntax error, an unsupported language feature, or a typo.

**NOT IMPLEMENTED**
This feature is not yet implemented: statement type in block

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:5:11:5:12:**
```roc
    import: "module load",
```
          ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:5:26:5:27:**
```roc
    import: "module load",
```
                         ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:6:5:6:8:**
```roc
    and: Bool.true,
```
    ^^^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:6:8:6:9:**
```roc
    and: Bool.true,
```
       ^

This might be a syntax error, an unsupported language feature, or a typo.

**DOES NOT EXIST**
`Bool.true` does not exist.

**record_different_fields_reserved_error.md:6:10:6:19:**
```roc
    and: Bool.true,
```
         ^^^^^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:6:19:6:20:**
```roc
    and: Bool.true,
```
                  ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:7:5:7:7:**
```roc
    or: Bool.false,
```
    ^^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:7:7:7:8:**
```roc
    or: Bool.false,
```
      ^

This might be a syntax error, an unsupported language feature, or a typo.

**DOES NOT EXIST**
`Bool.false` does not exist.

**record_different_fields_reserved_error.md:7:9:7:19:**
```roc
    or: Bool.false,
```
        ^^^^^^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:7:19:7:20:**
```roc
    or: Bool.false,
```
                  ^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
OpenCurly,
KwIf,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
KwExpect,OpColon,StringStart,StringPart,StringEnd,Comma,
KwImport,OpColon,StringStart,StringPart,StringEnd,Comma,
OpAnd,OpColon,UpperIdent,NoSpaceDotLowerIdent,Comma,
OpOr,OpColon,UpperIdent,NoSpaceDotLowerIdent,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(e-malformed (reason "no_else"))
		(s-type-anno (name "when")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(s-expect
			(e-malformed (reason "expr_unexpected_token")))
		(e-string
			(e-string-part (raw "test assertion")))
		(e-malformed (reason "expr_unexpected_token"))
		(s-malformed (tag "import_must_be_top_level"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-string
			(e-string-part (raw "module load")))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-ident (raw "Bool.true"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-ident (raw "Bool.false"))
		(e-malformed (reason "expr_unexpected_token"))))
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
(e-block
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expect
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-string
			(e-literal (string "test assertion"))))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-runtime-error (tag "not_implemented"))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-string
			(e-literal (string "module load"))))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "qualified_ident_does_not_exist")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "qualified_ident_does_not_exist")))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
