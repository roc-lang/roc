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
**record_different_fields_reserved_error.md:2:7:2:10:**
```roc
    if: "conditional",
```


**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**record_different_fields_reserved_error.md:2:22:2:22:**
```roc
    if: "conditional",
```


**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **"pattern match** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**record_different_fields_reserved_error.md:3:11:3:25:**
```roc
    when: "pattern match",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **pattern match"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:3:12:3:26:**
```roc
    when: "pattern match",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **",** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:3:25:3:27:**
```roc
    when: "pattern match",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:3:26:3:26:**
```roc
    when: "pattern match",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:4:11:4:14:**
```roc
    expect: "test assertion",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:4:29:4:29:**
```roc
    expect: "test assertion",
```


**IMPORT MUST BE TOP LEVEL**
Import statements must appear at the top level of a module.
Move this import to the top of the file, after the module header but before any definitions.

Here is the problematic code:
**record_different_fields_reserved_error.md:5:5:5:12:**
```roc
    import: "module load",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:5:11:5:14:**
```roc
    import: "module load",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:5:26:5:26:**
```roc
    import: "module load",
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **and:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:6:5:6:9:**
```roc
    and: Bool.true,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: Bool** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:6:8:6:14:**
```roc
    and: Bool.true,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:6:19:6:19:**
```roc
    and: Bool.true,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **or:** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:7:5:7:8:**
```roc
    or: Bool.false,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **: Bool** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:7:7:7:13:**
```roc
    or: Bool.false,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**record_different_fields_reserved_error.md:7:19:7:19:**
```roc
    or: Bool.false,
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
OpenCurly(1:1-1:2),Newline(1:1-1:1),
KwIf(2:5-2:7),OpColon(2:7-2:8),StringStart(2:9-2:10),StringPart(2:10-2:21),StringEnd(2:21-2:22),Comma(2:22-2:23),Newline(1:1-1:1),
LowerIdent(3:5-3:9),OpColon(3:9-3:10),StringStart(3:11-3:12),StringPart(3:12-3:25),StringEnd(3:25-3:26),Comma(3:26-3:27),Newline(1:1-1:1),
KwExpect(4:5-4:11),OpColon(4:11-4:12),StringStart(4:13-4:14),StringPart(4:14-4:28),StringEnd(4:28-4:29),Comma(4:29-4:30),Newline(1:1-1:1),
KwImport(5:5-5:11),OpColon(5:11-5:12),StringStart(5:13-5:14),StringPart(5:14-5:25),StringEnd(5:25-5:26),Comma(5:26-5:27),Newline(1:1-1:1),
OpAnd(6:5-6:8),OpColon(6:8-6:9),UpperIdent(6:10-6:14),NoSpaceDotLowerIdent(6:14-6:19),Comma(6:19-6:20),Newline(1:1-1:1),
OpOr(7:5-7:7),OpColon(7:7-7:8),UpperIdent(7:9-7:13),NoSpaceDotLowerIdent(7:13-7:19),Comma(7:19-7:20),Newline(1:1-1:1),
CloseCurly(8:1-8:2),EndOfFile(8:2-8:2),
~~~
# PARSE
~~~clojure
(e-block @1-1-8-2
	(statements
		(e-malformed @1-1-1-1 (reason "no_else"))
		(s-type-anno @3-5-3-25 (name "when")
			(ty-malformed @3-11-3-25 (tag "ty_anno_unexpected_token")))
		(e-malformed @3-12-3-26 (reason "expr_unexpected_token"))
		(e-malformed @3-25-3-27 (reason "expr_unexpected_token"))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))
		(s-expect @4-5-4-14
			(e-malformed @4-11-4-14 (reason "expr_unexpected_token")))
		(e-string @4-13-4-29
			(e-string-part @4-14-4-28 (raw "test assertion")))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))
		(s-malformed @5-5-5-12 (tag "import_must_be_top_level"))
		(e-malformed @5-11-5-14 (reason "expr_unexpected_token"))
		(e-string @5-13-5-26
			(e-string-part @5-14-5-25 (raw "module load")))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))
		(e-malformed @6-5-6-9 (reason "expr_unexpected_token"))
		(e-malformed @6-8-6-14 (reason "expr_unexpected_token"))
		(e-ident @6-10-6-19 (qaul "Bool") (raw ".true"))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))
		(e-malformed @7-5-7-8 (reason "expr_unexpected_token"))
		(e-malformed @7-7-7-13 (reason "expr_unexpected_token"))
		(e-ident @7-9-7-19 (qaul "Bool") (raw ".false"))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))))
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
(expr (id 93) (type "*"))
~~~