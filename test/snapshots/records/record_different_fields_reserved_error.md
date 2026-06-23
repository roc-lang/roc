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
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:6:19:6:20
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:7:5:7:7
UNEXPECTED TOKEN IN EXPRESSION - record_different_fields_reserved_error.md:7:19:7:20
DECLARATION HAS NO VALUE - record_different_fields_reserved_error.md:3:5:3:12
MISSING METHOD - record_different_fields_reserved_error.md:4:13:4:29
MISSING METHOD - record_different_fields_reserved_error.md:5:13:5:26
# PROBLEMS

┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token : is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  if: "conditional",                                                        │
 │    ‾                                                                       │
 └───────────────────────────── record_different_fields_reserved_error.md:2:7 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  if: "conditional",                                                        │
 │                   ‾                                                        │
 └──────────────────────────── record_different_fields_reserved_error.md:2:22 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌─────────────────────────────────────┐
│ UNEXPECTED TOKEN IN TYPE ANNOTATION ├─ The token " is not expected in a ────┐
└┬────────────────────────────────────┘  type annotation.                     │
 │                                                                            │
 │  when: "pattern match",                                                    │
 │        ‾                                                                   │
 └──────────────────────────── record_different_fields_reserved_error.md:3:11 ┘

    Type annotations should contain types like Str, Num a, or List U64.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token pattern match is not expected ──┐
└┬───────────────────────────────┘  in an expression.                         │
 │                                                                            │
 │  when: "pattern match",                                                    │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                      │
 └──────────────────────────── record_different_fields_reserved_error.md:3:12 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token " is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  when: "pattern match",                                                    │
 │                      ‾                                                     │
 └──────────────────────────── record_different_fields_reserved_error.md:3:25 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  when: "pattern match",                                                    │
 │                       ‾                                                    │
 └──────────────────────────── record_different_fields_reserved_error.md:3:26 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token : is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  expect: "test assertion",                                                 │
 │        ‾                                                                   │
 └──────────────────────────── record_different_fields_reserved_error.md:4:11 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  expect: "test assertion",                                                 │
 │                          ‾                                                 │
 └──────────────────────────── record_different_fields_reserved_error.md:4:29 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌──────────────────────────┐
│ IMPORT MUST BE TOP LEVEL ├─ Import statements must appear at the top ───────┐
└┬─────────────────────────┘  level of a module.                              │
 │                                                                            │
 │  import: "module load",                                                    │
 │  ‾‾‾‾‾‾                                                                    │
 └───────────────────────────── record_different_fields_reserved_error.md:5:5 ┘

    Move this import to the top of the file, after the module header but before
    any definitions.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token : is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  import: "module load",                                                    │
 │        ‾                                                                   │
 └──────────────────────────── record_different_fields_reserved_error.md:5:11 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  import: "module load",                                                    │
 │                       ‾                                                    │
 └──────────────────────────── record_different_fields_reserved_error.md:5:26 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token and is not expected in an ──────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  and: Bool.true,                                                           │
 │  ‾‾‾                                                                       │
 └───────────────────────────── record_different_fields_reserved_error.md:6:5 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  and: Bool.true,                                                           │
 │                ‾                                                           │
 └──────────────────────────── record_different_fields_reserved_error.md:6:19 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token or is not expected in an ───────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  or: Bool.false,                                                           │
 │  ‾‾                                                                        │
 └───────────────────────────── record_different_fields_reserved_error.md:7:5 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌────────────────────────────────┐
│ UNEXPECTED TOKEN IN EXPRESSION ├─ The token , is not expected in an ────────┐
└┬───────────────────────────────┘  expression.                               │
 │                                                                            │
 │  or: Bool.false,                                                           │
 │                ‾                                                           │
 └──────────────────────────── record_different_fields_reserved_error.md:7:19 ┘

    Expressions can be identifiers, literals, function calls, or operators.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  when: "pattern match",                                                    │
 │  ‾‾‾‾‾‾‾                                                                   │
 └───────────────────────────── record_different_fields_reserved_error.md:3:5 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌────────────────┐
│ MISSING METHOD ├─ This `from_quote` method is being called on a value ──────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  expect: "test assertion",                                                 │
 │          ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                  │
 └──────────────────────────── record_different_fields_reserved_error.md:4:13 ┘

    The value's type, which does not have a method named `from_quote`, is:

        {}


┌────────────────┐
│ MISSING METHOD ├─ This `from_quote` method is being called on a value ──────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  import: "module load",                                                    │
 │          ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └──────────────────────────── record_different_fields_reserved_error.md:5:13 ┘

    The value's type, which does not have a method named `from_quote`, is:

        {}

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
		(e-ident (raw "Bool.true"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-malformed (reason "expr_unexpected_token"))
		(e-ident (raw "Bool.false"))
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
	
		Bool.true
	
		Bool.false
	
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
		(e-runtime-error (tag "nested_value_not_found")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-expr
		(e-runtime-error (tag "nested_value_not_found")))
	(e-runtime-error (tag "expr_not_canonicalized")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
