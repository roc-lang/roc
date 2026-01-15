# META
~~~ini
description=Test associated function with type-qualified name lookup
type=file:Counter.roc
~~~
# SOURCE
~~~roc
Counter := U64.{
    new : {} -> Counter
    new = |{}| @Counter(0)

    increment : Counter -> Counter
    increment = |@Counter(n)| @Counter(n + 1)
}

result = Counter.new({})
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - can_associated_type_qualified.md:3:16:3:24
UNEXPECTED TOKEN IN PATTERN - can_associated_type_qualified.md:6:18:6:26
PARSE ERROR - can_associated_type_qualified.md:6:26:6:27
UNEXPECTED TOKEN IN EXPRESSION - can_associated_type_qualified.md:6:28:6:29
UNEXPECTED TOKEN IN PATTERN - can_associated_type_qualified.md:6:31:6:39
PARSE ERROR - can_associated_type_qualified.md:6:39:6:40
UNEXPECTED TOKEN IN EXPRESSION - can_associated_type_qualified.md:6:45:6:46
EXPRESSION IN ASSOCIATED ITEMS - can_associated_type_qualified.md:6:45:6:46
INVALID LAMBDA - :0:0:0:0
UNRECOGNIZED SYNTAX - can_associated_type_qualified.md:6:26:6:27
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **@Counter** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_associated_type_qualified.md:3:16:3:24:**
```roc
    new = |{}| @Counter(0)
```
               ^^^^^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **@Counter** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**can_associated_type_qualified.md:6:18:6:26:**
```roc
    increment = |@Counter(n)| @Counter(n + 1)
```
                 ^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_bar`
This is an unexpected parsing error. Please check your syntax.

**can_associated_type_qualified.md:6:26:6:27:**
```roc
    increment = |@Counter(n)| @Counter(n + 1)
```
                         ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_associated_type_qualified.md:6:28:6:29:**
```roc
    increment = |@Counter(n)| @Counter(n + 1)
```
                           ^


**UNEXPECTED TOKEN IN PATTERN**
The token **@Counter** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**can_associated_type_qualified.md:6:31:6:39:**
```roc
    increment = |@Counter(n)| @Counter(n + 1)
```
                              ^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_bar`
This is an unexpected parsing error. Please check your syntax.

**can_associated_type_qualified.md:6:39:6:40:**
```roc
    increment = |@Counter(n)| @Counter(n + 1)
```
                                      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_associated_type_qualified.md:6:45:6:46:**
```roc
    increment = |@Counter(n)| @Counter(n + 1)
```
                                            ^


**EXPRESSION IN ASSOCIATED ITEMS**
Associated items (such as types or methods) can only have associated types and values, not plain expressions.

To fix this, remove the expression at the very end.

**can_associated_type_qualified.md:6:45:6:46:**
```roc
    increment = |@Counter(n)| @Counter(n + 1)
```
                                            ^


**INVALID LAMBDA**
The body of this lambda expression is not valid.

**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**can_associated_type_qualified.md:6:26:6:27:**
```roc
    increment = |@Counter(n)| @Counter(n + 1)
```
                         ^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,UpperIdent,Dot,OpenCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,OpenCurly,CloseCurly,OpBar,OpaqueName,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,OpaqueName,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,OpaqueName,NoSpaceOpenRound,LowerIdent,OpPlus,Int,CloseRound,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Counter")
				(args))
			(ty (name "U64"))
			(associated
				(s-type-anno (name "new")
					(ty-fn
						(ty-record)
						(ty (name "Counter"))))
				(s-decl
					(p-ident (raw "new"))
					(e-apply
						(e-lambda
							(args
								(p-record))
							(e-malformed (reason "expr_unexpected_token")))
						(e-int (raw "0"))))
				(s-type-anno (name "increment")
					(ty-fn
						(ty (name "Counter"))
						(ty (name "Counter"))))
				(s-decl
					(p-ident (raw "increment"))
					(e-malformed (reason "expected_expr_bar")))
				(e-ident (raw "n"))
				(e-malformed (reason "expr_unexpected_token"))
				(e-malformed (reason "expected_expr_bar"))
				(e-binop (op "+")
					(e-ident (raw "n"))
					(e-int (raw "1")))
				(e-malformed (reason "expr_unexpected_token"))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "Counter.new"))
				(e-record)))))
~~~
# FORMATTED
~~~roc
Counter := U64.{
	new : {} -> Counter
	new = |{}| (0)

	increment : Counter -> Counter
	increment = 
	n
			n + 1
	
}

result = Counter.new({})
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Counter.new"))
		(e-call
			(e-runtime-error (tag "lambda_body_not_canonicalized"))
			(e-num (value "0")))
		(annotation
			(ty-fn (effectful false)
				(ty-record)
				(ty-lookup (name "Counter") (local)))))
	(d-let
		(p-assign (ident "Counter.increment"))
		(e-runtime-error (tag "expr_not_canonicalized"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Counter") (local))
				(ty-lookup (name "Counter") (local)))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "Counter.new")))
			(e-empty_record)))
	(s-nominal-decl
		(ty-header (name "Counter"))
		(ty-lookup (name "U64") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{  } -> Counter"))
		(patt (type "Counter -> Counter"))
		(patt (type "Counter")))
	(type_decls
		(nominal (type "Counter")
			(ty-header (name "Counter"))))
	(expressions
		(expr (type "{  } -> Counter"))
		(expr (type "Counter -> Counter"))
		(expr (type "Counter"))))
~~~
