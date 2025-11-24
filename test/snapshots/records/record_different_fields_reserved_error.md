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
    and: True,
    or: False,
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:2:7:2:8
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:2:22:2:23
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
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:6:14:6:15
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:7:5:7:7
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:7:7:7:8
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:7:14:7:15
INVALID IF CONDITION - :0:0:0:0
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:2:22:2:23
MALFORMED TYPE - record_different_fields_reserved_error.md:3:11:3:12
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:3:12:3:25
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:3:25:3:26
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:3:26:3:27
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:4:11:4:12
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:4:29:4:30
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:5:11:5:12
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:5:26:5:27
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:6:5:6:8
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:6:8:6:9
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:6:14:6:15
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:7:5:7:7
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:7:7:7:8
UNRECOGNIZED SYNTAX - record_different_fields_reserved_error.md:7:14:7:15
UNUSED VARIABLE - record_different_fields_reserved_error.md:3:5:3:12
UNUSED VALUE - record_different_fields_reserved_error.md:4:13:4:29
UNUSED VALUE - record_different_fields_reserved_error.md:5:13:5:26
UNUSED VALUE - record_different_fields_reserved_error.md:6:10:6:14
UNUSED VALUE - record_different_fields_reserved_error.md:7:9:7:14
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:2:7:2:8:**
```roc
    if: "conditional",
```
      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:2:22:2:23:**
```roc
    if: "conditional",
```
                     ^


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
    and: True,
```
    ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:6:8:6:9:**
```roc
    and: True,
```
       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:6:14:6:15:**
```roc
    and: True,
```
             ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **or** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:7:5:7:7:**
```roc
    or: False,
```
    ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:7:7:7:8:**
```roc
    or: False,
```
      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_different_fields_reserved_error.md:7:14:7:15:**
```roc
    or: False,
```
             ^


**INVALID IF CONDITION**
The condition in this `if` expression could not be processed.

The condition must be a valid expression that evaluates to a `Bool` value (`True` or `False`).

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:2:22:2:23:**
```roc
    if: "conditional",
```
                     ^

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
    and: True,
```
    ^^^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:6:8:6:9:**
```roc
    and: True,
```
       ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:6:14:6:15:**
```roc
    and: True,
```
             ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:7:5:7:7:**
```roc
    or: False,
```
    ^^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:7:7:7:8:**
```roc
    or: False,
```
      ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**record_different_fields_reserved_error.md:7:14:7:15:**
```roc
    or: False,
```
             ^

This might be a syntax error, an unsupported language feature, or a typo.

**UNUSED VARIABLE**
Variable `when` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_when` to suppress this warning.
The unused variable is declared here:
**record_different_fields_reserved_error.md:3:5:3:12:**
```roc
    when: "pattern match",
```
    ^^^^^^^


**UNUSED VALUE**
This expression produces a value, but it's not being used:
**record_different_fields_reserved_error.md:4:13:4:29:**
```roc
    expect: "test assertion",
```
            ^^^^^^^^^^^^^^^^

It has the type:
    _Str_

**UNUSED VALUE**
This expression produces a value, but it's not being used:
**record_different_fields_reserved_error.md:5:13:5:26:**
```roc
    import: "module load",
```
            ^^^^^^^^^^^^^

It has the type:
    _Str_

**UNUSED VALUE**
This expression produces a value, but it's not being used:
**record_different_fields_reserved_error.md:6:10:6:14:**
```roc
    and: True,
```
         ^^^^

It has the type:
    _[True]_others_

**UNUSED VALUE**
This expression produces a value, but it's not being used:
**record_different_fields_reserved_error.md:7:9:7:14:**
```roc
    or: False,
```
        ^^^^^

It has the type:
    _[False]_others_

# TOKENS
~~~zig
OpenCurly,
KwIf,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
KwExpect,OpColon,StringStart,StringPart,StringEnd,Comma,
KwImport,OpColon,StringStart,StringPart,StringEnd,Comma,
OpAnd,OpColon,UpperIdent,Comma,
OpOr,OpColon,UpperIdent,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(e-if-without-else
			(e-malformed (reason "expr_unexpected_token"))
			(e-string
				(e-string-part (raw "conditional"))))
		(e-malformed (reason "expr_unexpected_token"))
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
		(e-tag (raw "True"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-tag (raw "False"))
		(e-malformed (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
{
	if  "conditional"
	
	when : 
			
	expect 
	"test assertion"
	
			"module load"
	
			True
	
			False
	
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-expr
		(e-runtime-error (tag "if_condition_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-let
		(p-assign (ident "when"))
		(e-anno-only))
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
		(e-tag (name "True")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-tag (name "False")))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
