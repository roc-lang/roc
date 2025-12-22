# META
~~~ini
description=For loop with break
type=snippet
~~~
# SOURCE
~~~roc
result : Bool
result = {
	var allTrue_ = True
	for b in [True, True, False, True, True, True] {
		if b == False {
			allTrue_ = False
			break
		} else {
			{}
		}
	}
	allTrue_
}

expect result == False
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **break** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**for_loop_break.md:7:4:7:9:**
```roc
			break
```
			^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**for_loop_break.md:7:4:7:9:**
```roc
			break
```
			^^^^^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,UpperIdent,
KwFor,LowerIdent,KwIn,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,OpenCurly,
KwIf,LowerIdent,OpEquals,UpperIdent,OpenCurly,
LowerIdent,OpAssign,UpperIdent,
KwBreak,
CloseCurly,KwElse,OpenCurly,
OpenCurly,CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,
CloseCurly,
KwExpect,LowerIdent,OpEquals,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "result")
			(ty (name "Bool")))
		(s-decl
			(p-ident (raw "result"))
			(e-block
				(statements
					(s-var (name "allTrue_")
						(e-tag (raw "True")))
					(s-for
						(p-ident (raw "b"))
						(e-list
							(e-tag (raw "True"))
							(e-tag (raw "True"))
							(e-tag (raw "False"))
							(e-tag (raw "True"))
							(e-tag (raw "True"))
							(e-tag (raw "True")))
						(e-block
							(statements
								(e-if-then-else
									(e-binop (op "==")
										(e-ident (raw "b"))
										(e-tag (raw "False")))
									(e-block
										(statements
											(s-decl
												(p-ident (raw "allTrue_"))
												(e-tag (raw "False")))
											(e-malformed (reason "expr_unexpected_token"))))
									(e-block
										(statements
											(e-record)))))))
					(e-ident (raw "allTrue_")))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "result"))
				(e-tag (raw "False"))))))
~~~
# FORMATTED
~~~roc
result : Bool
result = {
	var allTrue_ = True
	for b in [True, True, False, True, True, True] {
		if b == False {
			allTrue_ = False
			
		} else {
			{}
		}
	}
	allTrue_
}

expect result == False
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "result"))
		(e-block
			(s-var
				(p-assign (ident "allTrue_"))
				(e-tag (name "True")))
			(s-for
				(p-assign (ident "b"))
				(e-list
					(elems
						(e-tag (name "True"))
						(e-tag (name "True"))
						(e-tag (name "False"))
						(e-tag (name "True"))
						(e-tag (name "True"))
						(e-tag (name "True"))))
				(e-block
					(e-if
						(if-branches
							(if-branch
								(e-binop (op "eq")
									(e-lookup-local
										(p-assign (ident "b")))
									(e-tag (name "False")))
								(e-block
									(s-reassign
										(p-assign (ident "allTrue_"))
										(e-tag (name "False")))
									(e-runtime-error (tag "expr_not_canonicalized")))))
						(if-else
							(e-block
								(e-empty_record))))))
			(e-lookup-local
				(p-assign (ident "allTrue_"))))
		(annotation
			(ty-lookup (name "Bool") (builtin))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "result")))
			(e-tag (name "False")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Bool")))
	(expressions
		(expr (type "Bool"))))
~~~
