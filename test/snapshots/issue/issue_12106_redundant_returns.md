# META
~~~ini
description=Warn on redundant final returns while preserving early returns and remove them during formatting
type=snippet
~~~
# SOURCE
~~~roc
sign_label : I64 -> Str
sign_label = |n| {
    if n < 0 {
        return "negative"
    }
    return "non-negative"
}

fizz_buzz : I64 -> Str
fizz_buzz = |n| {
    if n % 15 == 0 {
        return "fizzbuzz"
    } else if n % 3 == 0 {
        return "fizz"
    } else if n % 5 == 0 {
        return "buzz"
    } else {
        return "other"
    }
}

identity : I64 -> I64
identity = |n| { { return n } }

expect identity(42) == 42
expect sign_label(-1) == "negative"
expect sign_label(1) == "non-negative"
expect fizz_buzz(15) == "fizzbuzz"
expect fizz_buzz(3) == "fizz"
expect fizz_buzz(5) == "buzz"
expect fizz_buzz(1) == "other"
~~~
# EXPECTED
REDUNDANT RETURN - issue_12106_redundant_returns.md:6:5:6:26
REDUNDANT RETURN - issue_12106_redundant_returns.md:12:9:12:26
REDUNDANT RETURN - issue_12106_redundant_returns.md:14:9:14:22
REDUNDANT RETURN - issue_12106_redundant_returns.md:16:9:16:22
REDUNDANT RETURN - issue_12106_redundant_returns.md:18:9:18:23
REDUNDANT RETURN - issue_12106_redundant_returns.md:23:20:23:28
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Redundant Return")
		(region (start 6 5) (end 6 26))
		(headline
			(reflow "This return is unnecessary because its value is already the function's final expression."))
		(document
			(source-region (file "issue_12106_redundant_returns.md") (start 6 5) (end 6 26) (annotation error) (line-text "    return \"non-negative\""))
			(reflow "Remove `return` or run `roc fmt` to remove it automatically.")))
	(report
		(severity warning)
		(title "Redundant Return")
		(region (start 12 9) (end 12 26))
		(headline
			(reflow "This return is unnecessary because its value is already the function's final expression."))
		(document
			(source-region (file "issue_12106_redundant_returns.md") (start 12 9) (end 12 26) (annotation error) (line-text "        return \"fizzbuzz\""))
			(reflow "Remove `return` or run `roc fmt` to remove it automatically.")))
	(report
		(severity warning)
		(title "Redundant Return")
		(region (start 14 9) (end 14 22))
		(headline
			(reflow "This return is unnecessary because its value is already the function's final expression."))
		(document
			(source-region (file "issue_12106_redundant_returns.md") (start 14 9) (end 14 22) (annotation error) (line-text "        return \"fizz\""))
			(reflow "Remove `return` or run `roc fmt` to remove it automatically.")))
	(report
		(severity warning)
		(title "Redundant Return")
		(region (start 16 9) (end 16 22))
		(headline
			(reflow "This return is unnecessary because its value is already the function's final expression."))
		(document
			(source-region (file "issue_12106_redundant_returns.md") (start 16 9) (end 16 22) (annotation error) (line-text "        return \"buzz\""))
			(reflow "Remove `return` or run `roc fmt` to remove it automatically.")))
	(report
		(severity warning)
		(title "Redundant Return")
		(region (start 18 9) (end 18 23))
		(headline
			(reflow "This return is unnecessary because its value is already the function's final expression."))
		(document
			(source-region (file "issue_12106_redundant_returns.md") (start 18 9) (end 18 23) (annotation error) (line-text "        return \"other\""))
			(reflow "Remove `return` or run `roc fmt` to remove it automatically.")))
	(report
		(severity warning)
		(title "Redundant Return")
		(region (start 23 20) (end 23 28))
		(headline
			(reflow "This return is unnecessary because its value is already the function's final expression."))
		(document
			(source-region (file "issue_12106_redundant_returns.md") (start 23 20) (end 23 28) (annotation error) (line-text "identity = |n| { { return n } }"))
			(reflow "Remove `return` or run `roc fmt` to remove it automatically."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwIf,LowerIdent,OpLessThan,Int,OpenCurly,
KwReturn,StringStart,StringPart,StringEnd,
CloseCurly,
KwReturn,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwIf,LowerIdent,OpPercent,Int,OpEquals,Int,OpenCurly,
KwReturn,StringStart,StringPart,StringEnd,
CloseCurly,KwElse,KwIf,LowerIdent,OpPercent,Int,OpEquals,Int,OpenCurly,
KwReturn,StringStart,StringPart,StringEnd,
CloseCurly,KwElse,KwIf,LowerIdent,OpPercent,Int,OpEquals,Int,OpenCurly,
KwReturn,StringStart,StringPart,StringEnd,
CloseCurly,KwElse,OpenCurly,
KwReturn,StringStart,StringPart,StringEnd,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,OpenCurly,KwReturn,LowerIdent,CloseCurly,CloseCurly,
KwExpect,LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpEquals,Int,
KwExpect,LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpEquals,StringStart,StringPart,StringEnd,
KwExpect,LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpEquals,StringStart,StringPart,StringEnd,
KwExpect,LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpEquals,StringStart,StringPart,StringEnd,
KwExpect,LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpEquals,StringStart,StringPart,StringEnd,
KwExpect,LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpEquals,StringStart,StringPart,StringEnd,
KwExpect,LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpEquals,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "sign_label")
			(ty-fn
				(ty (name "I64"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "sign_label"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-block
					(statements
						(e-if-without-else
							(e-binop (op "<")
								(e-ident (raw "n"))
								(e-int (raw "0")))
							(e-block
								(statements
									(s-return
										(e-string
											(e-string-part (raw "negative")))))))
						(s-return
							(e-string
								(e-string-part (raw "non-negative"))))))))
		(s-type-anno (name "fizz_buzz")
			(ty-fn
				(ty (name "I64"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "fizz_buzz"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-block
					(statements
						(e-if-then-else
							(e-binop (op "==")
								(e-binop (op "%")
									(e-ident (raw "n"))
									(e-int (raw "15")))
								(e-int (raw "0")))
							(e-block
								(statements
									(s-return
										(e-string
											(e-string-part (raw "fizzbuzz"))))))
							(e-if-then-else
								(e-binop (op "==")
									(e-binop (op "%")
										(e-ident (raw "n"))
										(e-int (raw "3")))
									(e-int (raw "0")))
								(e-block
									(statements
										(s-return
											(e-string
												(e-string-part (raw "fizz"))))))
								(e-if-then-else
									(e-binop (op "==")
										(e-binop (op "%")
											(e-ident (raw "n"))
											(e-int (raw "5")))
										(e-int (raw "0")))
									(e-block
										(statements
											(s-return
												(e-string
													(e-string-part (raw "buzz"))))))
									(e-block
										(statements
											(s-return
												(e-string
													(e-string-part (raw "other")))))))))))))
		(s-type-anno (name "identity")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "identity"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-block
					(statements
						(e-block
							(statements
								(s-return
									(e-ident (raw "n")))))))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "identity"))
					(e-int (raw "42")))
				(e-int (raw "42"))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "sign_label"))
					(e-int (raw "-1")))
				(e-string
					(e-string-part (raw "negative")))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "sign_label"))
					(e-int (raw "1")))
				(e-string
					(e-string-part (raw "non-negative")))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "fizz_buzz"))
					(e-int (raw "15")))
				(e-string
					(e-string-part (raw "fizzbuzz")))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "fizz_buzz"))
					(e-int (raw "3")))
				(e-string
					(e-string-part (raw "fizz")))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "fizz_buzz"))
					(e-int (raw "5")))
				(e-string
					(e-string-part (raw "buzz")))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "fizz_buzz"))
					(e-int (raw "1")))
				(e-string
					(e-string-part (raw "other")))))))
~~~
# FORMATTED
~~~roc
sign_label : I64 -> Str
sign_label = |n| {
	if n < 0 {
		return "negative"
	}
	"non-negative"
}

fizz_buzz : I64 -> Str
fizz_buzz = |n| {
	if n % 15 == 0 {
		"fizzbuzz"
	} else if n % 3 == 0 {
		"fizz"
	} else if n % 5 == 0 {
		"buzz"
	} else {
		"other"
	}
}

identity : I64 -> I64
identity = |n| {
	{
		(n)
	}
}

expect identity(42) == 42
expect sign_label(-1) == "negative"
expect sign_label(1) == "non-negative"
expect fizz_buzz(15) == "fizzbuzz"
expect fizz_buzz(3) == "fizz"
expect fizz_buzz(5) == "buzz"
expect fizz_buzz(1) == "other"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "sign_label"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-block
				(s-expr
					(e-if
						(if-branches
							(if-branch
								(e-dispatch-call (method "is_lt") (constraint-fn-var 361)
									(receiver
										(e-lookup-local
											(p-assign (ident "n"))))
									(args
										(e-num (value "0"))))
								(e-block
									(e-return
										(e-string
											(e-literal (string "negative")))))))
						(if-else
							(e-empty_record))))
				(e-return
					(e-string
						(e-literal (string "non-negative"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "fizz_buzz"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-block
				(e-if
					(if-branches
						(if-branch
							(e-method-eq (negated "false")
								(lhs
									(e-dispatch-call (method "rem_by") (constraint-fn-var 396)
										(receiver
											(e-lookup-local
												(p-assign (ident "n"))))
										(args
											(e-num (value "15")))))
								(rhs
									(e-num (value "0"))))
							(e-block
								(e-return
									(e-string
										(e-literal (string "fizzbuzz"))))))
						(if-branch
							(e-method-eq (negated "false")
								(lhs
									(e-dispatch-call (method "rem_by") (constraint-fn-var 434)
										(receiver
											(e-lookup-local
												(p-assign (ident "n"))))
										(args
											(e-num (value "3")))))
								(rhs
									(e-num (value "0"))))
							(e-block
								(e-return
									(e-string
										(e-literal (string "fizz"))))))
						(if-branch
							(e-method-eq (negated "false")
								(lhs
									(e-dispatch-call (method "rem_by") (constraint-fn-var 467)
										(receiver
											(e-lookup-local
												(p-assign (ident "n"))))
										(args
											(e-num (value "5")))))
								(rhs
									(e-num (value "0"))))
							(e-block
								(e-return
									(e-string
										(e-literal (string "buzz")))))))
					(if-else
						(e-block
							(e-return
								(e-string
									(e-literal (string "other")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "identity"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-block
				(e-block
					(e-return
						(e-lookup-local
							(p-assign (ident "n")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin)))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 507)
					(e-lookup-local
						(p-assign (ident "identity")))
					(e-num (value "42"))))
			(rhs
				(e-num (value "42")))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 522)
					(e-lookup-local
						(p-assign (ident "sign_label")))
					(e-num (value "-1"))))
			(rhs
				(e-string
					(e-literal (string "negative"))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 544)
					(e-lookup-local
						(p-assign (ident "sign_label")))
					(e-num (value "1"))))
			(rhs
				(e-string
					(e-literal (string "non-negative"))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 563)
					(e-lookup-local
						(p-assign (ident "fizz_buzz")))
					(e-num (value "15"))))
			(rhs
				(e-string
					(e-literal (string "fizzbuzz"))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 582)
					(e-lookup-local
						(p-assign (ident "fizz_buzz")))
					(e-num (value "3"))))
			(rhs
				(e-string
					(e-literal (string "fizz"))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 598)
					(e-lookup-local
						(p-assign (ident "fizz_buzz")))
					(e-num (value "5"))))
			(rhs
				(e-string
					(e-literal (string "buzz"))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 614)
					(e-lookup-local
						(p-assign (ident "fizz_buzz")))
					(e-num (value "1"))))
			(rhs
				(e-string
					(e-literal (string "other")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "I64 -> Str"))
		(patt (type "I64 -> Str"))
		(patt (type "I64 -> I64")))
	(expressions
		(expr (type "I64 -> Str"))
		(expr (type "I64 -> Str"))
		(expr (type "I64 -> I64"))))
~~~
