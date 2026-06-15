# META
~~~ini
description=The single-question binary operator (lhs ? handler) is no longer part of the language; ? is postfix-only
type=snippet
~~~
# SOURCE
~~~roc
f : I64 -> Try(I64, [IsNegative])
f = |x| {
	if x < 0 { Err(IsNegative) } else { Ok(x) }
}

g = |x| {
	value = f(x) ? BadInput
	Ok(value)
}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - question_binop_removed.md:7:15:7:16
UNRECOGNIZED SYNTAX - question_binop_removed.md:7:15:7:16
TYPE MISMATCH - question_binop_removed.md:7:17:7:25
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**question_binop_removed.md:7:15:7:16:**
```roc
	value = f(x) ? BadInput
```
	             ^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**question_binop_removed.md:7:15:7:16:**
```roc
	value = f(x) ? BadInput
```
	             ^

This might be a syntax error, an unsupported language feature, or a typo.

**TYPE MISMATCH**
This expression produces a value, but it's not being used:
**question_binop_removed.md:7:17:7:25:**
```roc
	value = f(x) ? BadInput
```
	               ^^^^^^^^

It has the type:

    [BadInput, ..]

Since this expression is used as a statement, it must evaluate to `{}`.
If you don't need the value, you can ignore it with `_ =`.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwIf,LowerIdent,OpLessThan,Int,OpenCurly,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,KwElse,OpenCurly,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpQuestion,UpperIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "f")
			(ty-fn
				(ty (name "I64"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "I64"))
					(ty-tag-union
						(tags
							(ty (name "IsNegative")))))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-block
					(statements
						(e-if-then-else
							(e-binop (op "<")
								(e-ident (raw "x"))
								(e-int (raw "0")))
							(e-block
								(statements
									(e-apply
										(e-tag (raw "Err"))
										(e-tag (raw "IsNegative")))))
							(e-block
								(statements
									(e-apply
										(e-tag (raw "Ok"))
										(e-ident (raw "x"))))))))))
		(s-decl
			(p-ident (raw "g"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "value"))
							(e-apply
								(e-ident (raw "f"))
								(e-ident (raw "x"))))
						(e-malformed (reason "expr_unexpected_token"))
						(e-tag (raw "BadInput"))
						(e-apply
							(e-tag (raw "Ok"))
							(e-ident (raw "value")))))))))
~~~
# FORMATTED
~~~roc
f : I64 -> Try(I64, [IsNegative])
f = |x| {
	if x < 0 {
		Err(IsNegative)
	} else {
		Ok(x)
	}
}

g = |x| {
	value = f(x)
		BadInput
	Ok(value)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-block
				(e-if
					(if-branches
						(if-branch
							(e-dispatch-call (method "is_lt") (constraint-fn-var 107)
								(receiver
									(e-lookup-local
										(p-assign (ident "x"))))
								(args
									(e-num (value "0"))))
							(e-block
								(e-tag (name "Err")
									(args
										(e-tag (name "IsNegative")))))))
					(if-else
						(e-block
							(e-tag (name "Ok")
								(args
									(e-lookup-local
										(p-assign (ident "x"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "I64") (builtin))
					(ty-tag-union
						(ty-tag-name (name "IsNegative")))))))
	(d-let
		(p-assign (ident "g"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-block
				(s-let
					(p-assign (ident "value"))
					(e-call (constraint-fn-var 204)
						(e-lookup-local
							(p-assign (ident "f")))
						(e-lookup-local
							(p-assign (ident "x")))))
				(s-expr
					(e-runtime-error (tag "expr_not_canonicalized")))
				(s-expr
					(e-tag (name "BadInput")))
				(e-tag (name "Ok")
					(args
						(e-lookup-local
							(p-assign (ident "value")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "I64 -> Try(I64, [IsNegative])"))
		(patt (type "I64 -> [Ok(Try(I64, [IsNegative])), ..]")))
	(expressions
		(expr (type "I64 -> Try(I64, [IsNegative])"))
		(expr (type "I64 -> [Ok(Try(I64, [IsNegative])), ..]"))))
~~~
