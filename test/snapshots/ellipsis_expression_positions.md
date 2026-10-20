# META
~~~ini
description=The ellipsis expression in every kind of expression position
type=snippet
~~~
# SOURCE
~~~roc
lambda_body = |_| ...
block_body = |_| { ... }
block_then_value = |_| {
    ...
    1
}
assigned = |_| {
    x = ...
    x
}
reassigned = |_| {
    var $x = ...
    $x = ...
    $x
}
call_args = |f| f(..., ...)
list = |_| [..., ...]
record = |_| { a: ..., b: ... }
record_update = |r| { ..r, a: ... }
tuple = |_| (..., ...)
parens = |_| (...)
tag_payload = |_| Ok(...)
interpolation = |_| "a ${...} b"
if_branches = |a| if a ... else ...
if_condition = |_| if ... 1 else 2
match_scrutinee = |_| match ... {
    _ => 1
}
match_branches = |a| match a {
    A => ...
    _ => ...
}
match_guard = |a| match a {
    x if ... => x
    _ => 1
}
binary_lhs = |a| ... + a
binary_rhs = |a| a + ...
both_sides = |_| ... == ...
no_space_binary = |a| a+...
default_rhs = |a| a ?? ...
and_rhs = |a| a and ...
not = |_| !...
question = |_| ...?
access = |_| ....foo
applied = |_| ...(1)
range_end = |_| 1..=...
dbg_value = |_| {
    dbg ...
    1
}
returned = |a| {
    if a {
        return ...
    }
    1
}
for_list = |_| {
    for x in ... {
        _ = x
    }
    1
}
for_body = |l| {
    for x in l {
        _ = x
        ...
    }
    1
}
while_body = |a| {
    while a {
        ...
    }
    1
}
nested_lambda = |_| |_| ...
lambda_arg = |f| f(|_| ...)
~~~
# EXPECTED
TRAILING `?` - ellipsis_expression_positions.md:44:19:44:20
UNCONDITIONAL CONDITION - ellipsis_expression_positions.md:25:23:25:26
UNCONDITIONAL CONDITION - ellipsis_expression_positions.md:26:29:26:32
UNCONDITIONAL CONDITION - ellipsis_expression_positions.md:34:10:34:13
MISSING METHOD - ellipsis_expression_positions.md:39:18:39:21
MISSING METHOD - ellipsis_expression_positions.md:59:14:59:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Trailing `?`")
		(region (start 44 19) (end 44 20))
		(headline
			(reflow "It's usually a mistake to use a postfix ")
			(annotated code "?")
			(reflow " on a value being returned implicitly at the end of a function."))
		(document
			(source-region (file "ellipsis_expression_positions.md") (start 44 19) (end 44 20) (annotation warning) (line-text "question = |_| ...?"))
			(line-break)
			(reflow "This is because ")
			(annotated code "?")
			(reflow " is syntax sugar for doing a ")
			(annotated code "match")
			(reflow " on a ")
			(annotated code "Try")
			(reflow " value like this:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(annotated keyword "match")
			(text " ")
			(annotated symbol-unqualified "value_before_question_mark")
			(text " {")
			(line-break)
			(indent 2)
			(annotated tag "Ok")
			(text "(")
			(annotated symbol-unqualified "ok_payload")
			(text ") ")
			(annotated operator "=>")
			(text " ")
			(annotated symbol-unqualified "ok_payload")
			(line-break)
			(indent 2)
			(annotated tag "Err")
			(text "(")
			(annotated symbol-unqualified "err_payload")
			(text ") ")
			(annotated operator "=>")
			(text " ")
			(annotated keyword "return")
			(text " ")
			(annotated tag "Err")
			(text "(")
			(annotated symbol-unqualified "err_payload")
			(text ")")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "When you use ")
			(annotated code "?")
			(reflow " on the value at the end of a function, it changes \"implicitly return this ")
			(annotated code "Try")
			(reflow " value\" to \"return this ")
			(annotated code "Try")
			(reflow " value if it's an ")
			(annotated code "Err")
			(reflow ", but if it's ")
			(annotated code "Ok")
			(reflow ", unwrap its ")
			(annotated code "Ok")
			(reflow " payload and return that instead\" - which can only possibly type-check when returning ")
			(annotated code "Try(Try(..., ...), ...)")
			(reflow ", which is so unusual that using ")
			(annotated code "?")
			(reflow " here is almost always a mistake in practice.")
			(line-break)
			(line-break)
			(reflow "Usually removing the ")
			(annotated code "?")
			(reflow " here is what makes the most sense, but if you really want this behavior, make it clear by using an explicit ")
			(annotated code "match")
			(reflow " instead of the ")
			(annotated code "?")
			(reflow " syntax sugar.")))
	(report
		(severity warning)
		(title "Unconditional Condition")
		(region (start 25 23) (end 25 26))
		(headline
			(reflow "This")
			(reflow " ")
			(reflow "if condition")
			(reflow " ")
			(reflow "is known at compile time, so")
			(reflow " ")
			(reflow "this conditional will always make the same choice."))
		(document
			(source-region (file "ellipsis_expression_positions.md") (start 25 23) (end 25 26) (annotation warning) (line-text "if_condition = |_| if ... 1 else 2"))))
	(report
		(severity warning)
		(title "Unconditional Condition")
		(region (start 26 29) (end 26 32))
		(headline
			(reflow "This")
			(reflow " ")
			(reflow "match value")
			(reflow " ")
			(reflow "is known at compile time, so")
			(reflow " ")
			(reflow "this match will always inspect the same value."))
		(document
			(source-region (file "ellipsis_expression_positions.md") (start 26 29) (end 26 32) (annotation warning) (line-text "match_scrutinee = |_| match ... {"))))
	(report
		(severity warning)
		(title "Unconditional Condition")
		(region (start 34 10) (end 34 13))
		(headline
			(reflow "This")
			(reflow " ")
			(reflow "if guard")
			(reflow " ")
			(reflow "is known at compile time, so")
			(reflow " ")
			(reflow "this conditional will always make the same choice."))
		(document
			(source-region (file "ellipsis_expression_positions.md") (start 34 10) (end 34 13) (annotation warning) (line-text "    x if ... => x"))))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 39 18) (end 39 21))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "==")
			(reflow " ")
			(reflow "compares")
			(reflow " ")
			(reflow "values whose type never resolved to a concrete type,")
			(reflow " ")
			(reflow "so they have no")
			(reflow " ")
			(annotated code "is_eq")
			(reflow " ")
			(reflow "method for")
			(reflow " ")
			(annotated code "==")
			(reflow " ")
			(reflow "to call."))
		(document
			(source-region (file "ellipsis_expression_positions.md") (start 39 18) (end 39 21) (annotation error) (line-text "both_sides = |_| ... == ..."))
			(line-break)
			(reflow "Using the values somewhere that requires a specific type, or annotating them, will give them one.")))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 59 14) (end 59 17))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "iter")
			(reflow " ")
			(reflow "call is on")
			(reflow " ")
			(reflow "a value whose type never resolved to a concrete type,")
			(reflow " ")
			(reflow "so it has no")
			(reflow " ")
			(annotated code "iter")
			(reflow " ")
			(reflow "method."))
		(document
			(source-region (file "ellipsis_expression_positions.md") (start 59 14) (end 59 17) (annotation error) (line-text "    for x in ... {"))
			(line-break)
			(reflow "Using the value somewhere that requires a specific type, or annotating it, will give it one."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,Underscore,OpBar,TripleDot,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,TripleDot,CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
TripleDot,
Int,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,TripleDot,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,TripleDot,
LowerIdent,OpAssign,TripleDot,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,TripleDot,Comma,TripleDot,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenSquare,TripleDot,Comma,TripleDot,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,LowerIdent,OpColon,TripleDot,Comma,LowerIdent,OpColon,TripleDot,CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,TripleDot,CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenRound,TripleDot,Comma,TripleDot,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenRound,TripleDot,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,TripleDot,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,StringStart,StringPart,OpenStringInterpolation,TripleDot,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,TripleDot,KwElse,TripleDot,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,KwIf,TripleDot,Int,KwElse,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,KwMatch,TripleDot,OpenCurly,
Underscore,OpFatArrow,Int,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,TripleDot,
Underscore,OpFatArrow,TripleDot,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
LowerIdent,KwIf,TripleDot,OpFatArrow,LowerIdent,
Underscore,OpFatArrow,Int,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,TripleDot,OpPlus,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,TripleDot,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,TripleDot,OpEquals,TripleDot,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,TripleDot,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpDoubleQuestion,TripleDot,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpAnd,TripleDot,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpBang,TripleDot,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,TripleDot,NoSpaceOpQuestion,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,TripleDot,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,TripleDot,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,Int,OpDoubleDotEquals,TripleDot,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
KwDbg,TripleDot,
Int,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwIf,LowerIdent,OpenCurly,
KwReturn,TripleDot,
CloseCurly,
Int,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
KwFor,LowerIdent,KwIn,TripleDot,OpenCurly,
Underscore,OpAssign,LowerIdent,
CloseCurly,
Int,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
Underscore,OpAssign,LowerIdent,
TripleDot,
CloseCurly,
Int,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwWhile,LowerIdent,OpenCurly,
TripleDot,
CloseCurly,
Int,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpBar,Underscore,OpBar,TripleDot,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,OpBar,Underscore,OpBar,TripleDot,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "lambda_body"))
			(e-lambda
				(args
					(p-underscore))
				(e-ellipsis)))
		(s-decl
			(p-ident (raw "block_body"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "block_then_value"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(e-ellipsis)
						(e-int (raw "1"))))))
		(s-decl
			(p-ident (raw "assigned"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "x"))
							(e-ellipsis))
						(e-ident (raw "x"))))))
		(s-decl
			(p-ident (raw "reassigned"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-var (name "$x")
							(e-ellipsis))
						(s-decl
							(p-ident (raw "$x"))
							(e-ellipsis))
						(e-ident (raw "$x"))))))
		(s-decl
			(p-ident (raw "call_args"))
			(e-lambda
				(args
					(p-ident (raw "f")))
				(e-apply
					(e-ident (raw "f"))
					(e-ellipsis)
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "list"))
			(e-lambda
				(args
					(p-underscore))
				(e-list
					(e-ellipsis)
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "record"))
			(e-lambda
				(args
					(p-underscore))
				(e-record
					(field (field "a")
						(e-ellipsis))
					(field (field "b")
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "record_update"))
			(e-lambda
				(args
					(p-ident (raw "r")))
				(e-record
					(ext
						(e-ident (raw "r")))
					(field (field "a")
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "tuple"))
			(e-lambda
				(args
					(p-underscore))
				(e-tuple
					(e-ellipsis)
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "parens"))
			(e-lambda
				(args
					(p-underscore))
				(e-tuple
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "tag_payload"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-tag (raw "Ok"))
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "interpolation"))
			(e-lambda
				(args
					(p-underscore))
				(e-string
					(e-string-part (raw "a "))
					(e-ellipsis)
					(e-string-part (raw " b")))))
		(s-decl
			(p-ident (raw "if_branches"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-if-then-else
					(e-ident (raw "a"))
					(e-ellipsis)
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "if_condition"))
			(e-lambda
				(args
					(p-underscore))
				(e-if-then-else
					(e-ellipsis)
					(e-int (raw "1"))
					(e-int (raw "2")))))
		(s-decl
			(p-ident (raw "match_scrutinee"))
			(e-lambda
				(args
					(p-underscore))
				(e-match
					(e-ellipsis)
					(branches
						(branch
							(p-underscore)
							(e-int (raw "1")))))))
		(s-decl
			(p-ident (raw "match_branches"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-match
					(e-ident (raw "a"))
					(branches
						(branch
							(p-tag (raw "A"))
							(e-ellipsis))
						(branch
							(p-underscore)
							(e-ellipsis))))))
		(s-decl
			(p-ident (raw "match_guard"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-match
					(e-ident (raw "a"))
					(branches
						(branch
							(p-ident (raw "x"))
							(guard
								(e-ellipsis))
							(e-ident (raw "x")))
						(branch
							(p-underscore)
							(e-int (raw "1")))))))
		(s-decl
			(p-ident (raw "binary_lhs"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-binop (op "+")
					(e-ellipsis)
					(e-ident (raw "a")))))
		(s-decl
			(p-ident (raw "binary_rhs"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-binop (op "+")
					(e-ident (raw "a"))
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "both_sides"))
			(e-lambda
				(args
					(p-underscore))
				(e-binop (op "==")
					(e-ellipsis)
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "no_space_binary"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-binop (op "+")
					(e-ident (raw "a"))
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "default_rhs"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-binop (op "??")
					(e-ident (raw "a"))
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "and_rhs"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-binop (op "and")
					(e-ident (raw "a"))
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "not"))
			(e-lambda
				(args
					(p-underscore))
				(unary "!"
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "question"))
			(e-lambda
				(args
					(p-underscore))
				(e-question-suffix
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "access"))
			(e-lambda
				(args
					(p-underscore))
				(e-field-access
					(receiver
						(e-ellipsis))
					(segment (mode "required") (field "foo")))))
		(s-decl
			(p-ident (raw "applied"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-ellipsis)
					(e-int (raw "1")))))
		(s-decl
			(p-ident (raw "range_end"))
			(e-lambda
				(args
					(p-underscore))
				(e-binop (op "..=")
					(e-int (raw "1"))
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "dbg_value"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-dbg
							(e-ellipsis))
						(e-int (raw "1"))))))
		(s-decl
			(p-ident (raw "returned"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-block
					(statements
						(e-if-without-else
							(e-ident (raw "a"))
							(e-block
								(statements
									(s-return
										(e-ellipsis)))))
						(e-int (raw "1"))))))
		(s-decl
			(p-ident (raw "for_list"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-for
							(p-ident (raw "x"))
							(e-ellipsis)
							(e-block
								(statements
									(s-decl
										(p-underscore)
										(e-ident (raw "x"))))))
						(e-int (raw "1"))))))
		(s-decl
			(p-ident (raw "for_body"))
			(e-lambda
				(args
					(p-ident (raw "l")))
				(e-block
					(statements
						(s-for
							(p-ident (raw "x"))
							(e-ident (raw "l"))
							(e-block
								(statements
									(s-decl
										(p-underscore)
										(e-ident (raw "x")))
									(e-ellipsis))))
						(e-int (raw "1"))))))
		(s-decl
			(p-ident (raw "while_body"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-block
					(statements
						(s-while
							(e-ident (raw "a"))
							(e-block
								(statements
									(e-ellipsis))))
						(e-int (raw "1"))))))
		(s-decl
			(p-ident (raw "nested_lambda"))
			(e-lambda
				(args
					(p-underscore))
				(e-lambda
					(args
						(p-underscore))
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "lambda_arg"))
			(e-lambda
				(args
					(p-ident (raw "f")))
				(e-apply
					(e-ident (raw "f"))
					(e-lambda
						(args
							(p-underscore))
						(e-ellipsis)))))))
~~~
# FORMATTED
~~~roc
lambda_body = |_| ...

block_body = |_| {
	...
}

block_then_value = |_| {
	...
	1
}

assigned = |_| {
	x = ...
	x
}

reassigned = |_| {
	var $x = ...
	$x = ...
	$x
}

call_args = |f| f(..., ...)

list = |_| [..., ...]

record = |_| { a: ..., b: ... }

record_update = |r| { ..r, a: ... }

tuple = |_| (..., ...)

parens = |_| (...)

tag_payload = |_| Ok(...)

interpolation = |_| "a ${...} b"

if_branches = |a| if (a) ... else ...

if_condition = |_| if (...) 1 else 2

match_scrutinee = |_| match ... {
	_ => 1
}

match_branches = |a| match a {
	A => ...
	_ => ...
}

match_guard = |a| match a {
	x if ... => x
	_ => 1
}

binary_lhs = |a| ... + a

binary_rhs = |a| a + ...

both_sides = |_| ... == ...

no_space_binary = |a| a + ...

default_rhs = |a| a ?? ...

and_rhs = |a| a and ...

not = |_| !...

question = |_| ...?

access = |_| ....foo

applied = |_| ...(1)

range_end = |_| 1..=...

dbg_value = |_| {
	dbg ...
	1
}

returned = |a| {
	if a {
		return ...
	}
	1
}

for_list = |_| {
	for x in ... {
		_ = x
	}
	1
}

for_body = |l| {
	for x in l {
		_ = x
		...
	}
	1
}

while_body = |a| {
	while a {
		...
	}
	1
}

nested_lambda = |_| |_| ...

lambda_arg = |f| f(|_| ...)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "lambda_body"))
		(e-lambda
			(args
				(p-underscore))
			(e-not-implemented)))
	(d-let
		(p-assign (ident "block_body"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(e-not-implemented))))
	(d-let
		(p-assign (ident "block_then_value"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-expr
					(e-not-implemented))
				(e-num (value "1")))))
	(d-let
		(p-assign (ident "assigned"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "x"))
					(e-not-implemented))
				(e-lookup-local
					(p-assign (ident "x"))))))
	(d-let
		(p-assign (ident "reassigned"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-var
					(p-var-assign (ident "$x"))
					(e-not-implemented))
				(s-reassign
					(p-var-assign (ident "$x"))
					(e-not-implemented))
				(e-lookup-local
					(p-var-assign (ident "$x"))))))
	(d-let
		(p-assign (ident "call_args"))
		(e-lambda
			(args
				(p-assign (ident "f")))
			(e-call (constraint-fn-var 564)
				(e-lookup-local
					(p-assign (ident "f")))
				(e-not-implemented)
				(e-not-implemented))))
	(d-let
		(p-assign (ident "list"))
		(e-lambda
			(args
				(p-underscore))
			(e-list
				(elems
					(e-not-implemented)
					(e-not-implemented)))))
	(d-let
		(p-assign (ident "record"))
		(e-lambda
			(args
				(p-underscore))
			(e-record
				(fields
					(field (name "a")
						(e-not-implemented))
					(field (name "b")
						(e-not-implemented))))))
	(d-let
		(p-assign (ident "record_update"))
		(e-lambda
			(args
				(p-assign (ident "r")))
			(e-record
				(ext
					(e-lookup-local
						(p-assign (ident "r"))))
				(fields
					(field (name "a")
						(e-not-implemented))))))
	(d-let
		(p-assign (ident "tuple"))
		(e-lambda
			(args
				(p-underscore))
			(e-tuple
				(elems
					(e-not-implemented)
					(e-not-implemented)))))
	(d-let
		(p-assign (ident "parens"))
		(e-lambda
			(args
				(p-underscore))
			(e-not-implemented)))
	(d-let
		(p-assign (ident "tag_payload"))
		(e-lambda
			(args
				(p-underscore))
			(e-tag (name "Ok")
				(args
					(e-not-implemented)))))
	(d-let
		(p-assign (ident "interpolation"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "#interp_0"))
					(e-not-implemented))
				(e-interpolation (constraint-fn-var 585) (dispatcher-var 98)
					(first
						(e-literal (string "a ")))
					(parts
						(e-lookup-local
							(p-assign (ident "#interp_0")))
						(e-literal (string " b")))))))
	(d-let
		(p-assign (ident "if_branches"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-if
				(if-branches
					(if-branch
						(e-lookup-local
							(p-assign (ident "a")))
						(e-not-implemented)))
				(if-else
					(e-not-implemented)))))
	(d-let
		(p-assign (ident "if_condition"))
		(e-lambda
			(args
				(p-underscore))
			(e-if
				(if-branches
					(if-branch
						(e-not-implemented)
						(e-num (value "1"))))
				(if-else
					(e-num (value "2"))))))
	(d-let
		(p-assign (ident "match_scrutinee"))
		(e-lambda
			(args
				(p-underscore))
			(e-match
				(match
					(cond
						(e-not-implemented))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-num (value "1")))))))))
	(d-let
		(p-assign (ident "match_branches"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "a"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-not-implemented)))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-not-implemented))))))))
	(d-let
		(p-assign (ident "match_guard"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "a"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-assign (ident "x"))))
							(value
								(e-lookup-local
									(p-assign (ident "x"))))
							(guard
								(e-not-implemented)))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-num (value "1")))))))))
	(d-let
		(p-assign (ident "binary_lhs"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-dispatch-call (method "plus") (constraint-fn-var 619)
				(receiver
					(e-not-implemented))
				(args
					(e-lookup-local
						(p-assign (ident "a")))))))
	(d-let
		(p-assign (ident "binary_rhs"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-dispatch-call (method "plus") (constraint-fn-var 621)
				(receiver
					(e-lookup-local
						(p-assign (ident "a"))))
				(args
					(e-not-implemented)))))
	(d-let
		(p-assign (ident "both_sides"))
		(e-lambda
			(args
				(p-underscore))
			(e-method-eq (negated "false")
				(lhs
					(e-runtime-error (tag "erroneous_value_expr")))
				(rhs
					(e-not-implemented)))))
	(d-let
		(p-assign (ident "no_space_binary"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-dispatch-call (method "plus") (constraint-fn-var 626)
				(receiver
					(e-lookup-local
						(p-assign (ident "a"))))
				(args
					(e-not-implemented)))))
	(d-let
		(p-assign (ident "default_rhs"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "a"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal-external (builtin)
										(p-applied-tag))))
							(value
								(e-lookup-local
									(p-assign (ident "#ok")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal-external (builtin)
										(p-applied-tag))))
							(value
								(e-not-implemented))))))))
	(d-let
		(p-assign (ident "and_rhs"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-if
				(if-branches
					(if-branch
						(e-lookup-local
							(p-assign (ident "a")))
						(e-not-implemented)))
				(if-else
					(e-nominal-external
						(builtin)
						(e-tag (name "False")))))))
	(d-let
		(p-assign (ident "not"))
		(e-lambda
			(args
				(p-underscore))
			(e-call (constraint-fn-var 658)
				(e-lookup-associated-resolved (source "Bool.not") (builtin) (target-node "18250") (target-def "18250"))
				(e-not-implemented))))
	(d-let
		(p-assign (ident "question"))
		(e-lambda
			(args
				(p-underscore))
			(e-match
				(match
					(cond
						(e-not-implemented))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal-external (builtin)
										(p-applied-tag))))
							(value
								(e-lookup-local
									(p-assign (ident "#ok")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal-external (builtin)
										(p-applied-tag))))
							(value
								(e-return
									(e-nominal-external
										(builtin)
										(e-tag (name "Err")
											(args
												(e-lookup-local
													(p-assign (ident "#err"))))))))))))))
	(d-let
		(p-assign (ident "access"))
		(e-lambda
			(args
				(p-underscore))
			(e-field-access
				(receiver
					(e-not-implemented))
				(segments
					(segment (name "foo") (mode "required"))))))
	(d-let
		(p-assign (ident "applied"))
		(e-lambda
			(args
				(p-underscore))
			(e-call (constraint-fn-var 703)
				(e-not-implemented)
				(e-num (value "1")))))
	(d-let
		(p-assign (ident "range_end"))
		(e-lambda
			(args
				(p-underscore))
			(e-dispatch-call (method "range_inclusive_to") (constraint-fn-var 716)
				(receiver
					(e-num (value "1")))
				(args
					(e-not-implemented)))))
	(d-let
		(p-assign (ident "dbg_value"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-dbg
					(e-not-implemented))
				(e-num (value "1")))))
	(d-let
		(p-assign (ident "returned"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-block
				(s-expr
					(e-if
						(if-branches
							(if-branch
								(e-lookup-local
									(p-assign (ident "a")))
								(e-block
									(e-return
										(e-not-implemented)))))
						(if-else
							(e-empty_record))))
				(e-num (value "1")))))
	(d-let
		(p-assign (ident "for_list"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-for
					(p-assign (ident "x"))
					(e-runtime-error (tag "erroneous_value_expr"))
					(e-block
						(s-let
							(p-underscore)
							(e-lookup-local
								(p-assign (ident "x"))))
						(e-empty_record)))
				(e-num (value "1")))))
	(d-let
		(p-assign (ident "for_body"))
		(e-lambda
			(args
				(p-assign (ident "l")))
			(e-block
				(s-for
					(p-assign (ident "x"))
					(e-lookup-local
						(p-assign (ident "l")))
					(e-block
						(s-let
							(p-underscore)
							(e-lookup-local
								(p-assign (ident "x"))))
						(s-expr
							(e-not-implemented))
						(e-empty_record)))
				(e-num (value "1")))))
	(d-let
		(p-assign (ident "while_body"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-block
				(s-while
					(e-lookup-local
						(p-assign (ident "a")))
					(e-block
						(s-expr
							(e-not-implemented))
						(e-empty_record)))
				(e-num (value "1")))))
	(d-let
		(p-assign (ident "nested_lambda"))
		(e-lambda
			(args
				(p-underscore))
			(e-lambda
				(args
					(p-underscore))
				(e-not-implemented))))
	(d-let
		(p-assign (ident "lambda_arg"))
		(e-lambda
			(args
				(p-assign (ident "f")))
			(e-call (constraint-fn-var 825)
				(e-lookup-local
					(p-assign (ident "f")))
				(e-lambda
					(args
						(p-underscore))
					(e-not-implemented))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> _ret"))
		(patt (type "_arg -> _ret"))
		(patt (type "_arg -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "_arg -> _ret"))
		(patt (type "_arg -> _ret"))
		(patt (type "(_arg, _arg2 -> c) -> c"))
		(patt (type "_arg -> List(_c)"))
		(patt (type "_arg -> { a: _field, b: _field2 }"))
		(patt (type "{ a: c, ..d } -> { a: c, ..d }"))
		(patt (type "_arg -> (_field, _field2)"))
		(patt (type "_arg -> _ret"))
		(patt (type "_arg -> [Ok(_c)]"))
		(patt (type "_arg -> c where [c.from_interpolation : List(Str) -> Try(List(_d) -> c, [InvalidInterpolation(Str)])]"))
		(patt (type "Bool -> _ret"))
		(patt (type "_arg -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "_arg -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "[A, ..] -> _ret"))
		(patt (type "c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c -> d where [d.plus : d, c -> d]"))
		(patt (type "c -> c where [c.plus : c, d -> c]"))
		(patt (type "_arg -> Bool"))
		(patt (type "c -> c where [c.plus : c, d -> c]"))
		(patt (type "Try(ok, err) -> ok"))
		(patt (type "Bool -> Bool"))
		(patt (type "_arg -> Bool"))
		(patt (type "_arg -> Try(ok, err)"))
		(patt (type "_arg -> _ret"))
		(patt (type "_arg -> _ret"))
		(patt (type "_arg -> Range(num) where [num.from_numeral : Numeral -> Try(num, [InvalidNumeral(Str)]), num.range_inclusive_to : num, num -> Range(num)]"))
		(patt (type "_arg -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "Bool -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "_arg -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c -> d where [c.iter : c -> Iter(item), d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "Bool -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "_arg -> (_arg2 -> _ret)"))
		(patt (type "((_arg -> _ret) -> c) -> c")))
	(expressions
		(expr (type "_arg -> _ret"))
		(expr (type "_arg -> _ret"))
		(expr (type "_arg -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "_arg -> _ret"))
		(expr (type "_arg -> _ret"))
		(expr (type "(_arg, _arg2 -> c) -> c"))
		(expr (type "_arg -> List(_c)"))
		(expr (type "_arg -> { a: _field, b: _field2 }"))
		(expr (type "{ a: c, ..d } -> { a: c, ..d }"))
		(expr (type "_arg -> (_field, _field2)"))
		(expr (type "_arg -> _ret"))
		(expr (type "_arg -> [Ok(_c)]"))
		(expr (type "_arg -> c where [c.from_interpolation : List(Str) -> Try(List(_d) -> c, [InvalidInterpolation(Str)])]"))
		(expr (type "Bool -> _ret"))
		(expr (type "_arg -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "_arg -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "[A, ..] -> _ret"))
		(expr (type "c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "c -> d where [d.plus : d, c -> d]"))
		(expr (type "c -> c where [c.plus : c, d -> c]"))
		(expr (type "_arg -> Bool"))
		(expr (type "c -> c where [c.plus : c, d -> c]"))
		(expr (type "Try(ok, err) -> ok"))
		(expr (type "Bool -> Bool"))
		(expr (type "_arg -> Bool"))
		(expr (type "_arg -> Try(ok, err)"))
		(expr (type "_arg -> _ret"))
		(expr (type "_arg -> _ret"))
		(expr (type "_arg -> Range(num) where [num.from_numeral : Numeral -> Try(num, [InvalidNumeral(Str)]), num.range_inclusive_to : num, num -> Range(num)]"))
		(expr (type "_arg -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "Bool -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "_arg -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "c -> d where [c.iter : c -> Iter(item), d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "Bool -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "_arg -> (_arg2 -> _ret)"))
		(expr (type "((_arg -> _ret) -> c) -> c"))))
~~~
