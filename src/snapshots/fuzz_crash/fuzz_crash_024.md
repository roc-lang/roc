# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module [module ] { pf: platform ".-/main._]where # A

#el
var t= ]

#el
var t= 0
~~~
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**PARSE ERROR**
A parsing error occurred: `exposed_item_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_024.md:1:9:1:17:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **platform "** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_024.md:1:24:1:34:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_024.md:1:33:1:53:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.-/main._]where # A** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_024.md:1:34:1:53:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_024.md:1:53:1:53:**
```roc
module [module ] { pf: platform ".-/main._]where # A
```


**PARSE ERROR**
A parsing error occurred: `var_only_allowed_in_a_body`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_024.md:4:1:4:6:**
```roc
var t= ]
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**fuzz_crash_024.md:4:8:4:8:**
```roc
var t= ]
```


**PARSE ERROR**
A parsing error occurred: `var_only_allowed_in_a_body`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_024.md:7:1:7:6:**
```roc
var t= 0
```


**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

**DUPLICATE DEFINITION**
The name `t` is being redeclared in this scope.

The redeclaration is here:
**fuzz_crash_024.md:7:5:7:6:**
```roc
var t= 0
```

But `t` was already defined here:
**fuzz_crash_024.md:4:5:4:6:**
```roc
var t= ]
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),KwModule(1:9-1:15),CloseSquare(1:16-1:17),OpenCurly(1:18-1:19),LowerIdent(1:20-1:22),OpColon(1:22-1:23),KwPlatform(1:24-1:32),StringStart(1:33-1:34),StringPart(1:34-1:53),StringEnd(1:53-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:4),
KwVar(4:1-4:4),LowerIdent(4:5-4:6),OpAssign(4:6-4:7),CloseCurly(4:8-4:9),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(6:2-6:4),
KwVar(7:1-7:4),LowerIdent(7:5-7:6),OpAssign(7:6-7:7),Int(7:8-7:9),EndOfFile(7:9-7:9),
~~~
# PARSE
~~~clojure
(file @1-1-7-9
	(module @1-1-1-17
		(exposes @1-8-1-17
			(exposed-malformed (reason "exposed_item_unexpected_token") @1-9-1-17)))
	(statements
		(e-malformed @1-33-1-53 (reason "expected_expr_close_curly_or_comma"))
		(e-malformed @1-34-1-53 (reason "expr_unexpected_token"))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))
		(s-malformed @4-1-4-6 (tag "var_only_allowed_in_a_body"))
		(s-decl @1-1-1-1
			(p-ident @4-5-4-6 (raw "t"))
			(e-malformed @1-1-1-1 (reason "expr_unexpected_token")))
		(s-malformed @7-1-7-6 (tag "var_only_allowed_in_a_body"))
		(s-decl @7-5-7-9
			(p-ident @7-5-7-6 (raw "t"))
			(e-int @7-8-7-9 (raw "0")))))
~~~
# FORMATTED
~~~roc
module []

# el
t = 

# el
t = 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 79)
		(p-assign @4-5-4-6 (ident "t") (id 76))
		(e-runtime-error (tag "expr_not_canonicalized") (id 78)))
	(d-let (id 83)
		(p-assign @7-5-7-6 (ident "t") (id 80))
		(e-int @7-8-7-9 (value "0") (id 82))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "t") (def_var 79) (type "Error"))
		(d_assign (name "t") (def_var 83) (type "Num(*)")))
	(expressions
		(expr @1-1-1-1 (type "Error"))
		(expr @7-8-7-9 (type "Num(*)"))))
~~~
