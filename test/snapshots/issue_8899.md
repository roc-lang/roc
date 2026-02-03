# META
~~~ini
description=Issue 8899 - lambda with List.append and List.last
type=expr
~~~
# SOURCE
~~~roc
{
    sum_with_last = |l| {
        var $total = 0i64
        var $acc = [0i64]
        for e in l {
            $acc = List.append($acc, e)
            $total = match List.last($acc) { Ok(last) => $total + last, Err(_) => $total }
        }
        $total
    }
    sum_with_last([10i64, 20i64, 30i64])
}
~~~
# EXPECTED
~~~
60
~~~
# PROBLEMS
**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**issue_8899.md:3:22:3:26:**
```roc
        var $total = 0i64
```
                     ^^^^

The `i64` suffix is no longer supported. Use `0.I64` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**issue_8899.md:4:21:4:25:**
```roc
        var $acc = [0i64]
```
                    ^^^^

The `i64` suffix is no longer supported. Use `0.I64` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**issue_8899.md:11:20:11:25:**
```roc
    sum_with_last([10i64, 20i64, 30i64])
```
                   ^^^^^

The `i64` suffix is no longer supported. Use `10.I64` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**issue_8899.md:11:27:11:32:**
```roc
    sum_with_last([10i64, 20i64, 30i64])
```
                          ^^^^^

The `i64` suffix is no longer supported. Use `20.I64` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**issue_8899.md:11:34:11:39:**
```roc
    sum_with_last([10i64, 20i64, 30i64])
```
                                 ^^^^^

The `i64` suffix is no longer supported. Use `30.I64` instead.

# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwVar,LowerIdent,OpAssign,OpenSquare,Int,CloseSquare,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,KwMatch,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpenCurly,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpPlus,LowerIdent,Comma,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,LowerIdent,CloseCurly,
CloseCurly,
LowerIdent,
CloseCurly,
LowerIdent,NoSpaceOpenRound,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "sum_with_last"))
			(e-lambda
				(args
					(p-ident (raw "l")))
				(e-block
					(statements
						(s-var (name "$total")
							(e-int (raw "0i64")))
						(s-var (name "$acc")
							(e-list
								(e-int (raw "0i64"))))
						(s-for
							(p-ident (raw "e"))
							(e-ident (raw "l"))
							(e-block
								(statements
									(s-decl
										(p-ident (raw "$acc"))
										(e-apply
											(e-ident (raw "List.append"))
											(e-ident (raw "$acc"))
											(e-ident (raw "e"))))
									(s-decl
										(p-ident (raw "$total"))
										(e-match
											(e-apply
												(e-ident (raw "List.last"))
												(e-ident (raw "$acc")))
											(branches
												(branch
													(p-tag (raw "Ok")
														(p-ident (raw "last")))
													(e-binop (op "+")
														(e-ident (raw "$total"))
														(e-ident (raw "last"))))
												(branch
													(p-tag (raw "Err")
														(p-underscore))
													(e-ident (raw "$total")))))))))
						(e-ident (raw "$total"))))))
		(e-apply
			(e-ident (raw "sum_with_last"))
			(e-list
				(e-int (raw "10i64"))
				(e-int (raw "20i64"))
				(e-int (raw "30i64"))))))
~~~
# FORMATTED
~~~roc
{
	sum_with_last = |l| {
		var $total = 0i64
		var $acc = [0i64]
		for e in l {
			$acc = List.append($acc, e)
			$total = match List.last($acc) {
				Ok(last) => $total + last
				Err(_) => $total
			}
		}
		$total
	}
	sum_with_last([10i64, 20i64, 30i64])
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "sum_with_last"))
		(e-lambda
			(args
				(p-assign (ident "l")))
			(e-block
				(s-var
					(p-assign (ident "$total"))
					(e-num (value "0")))
				(s-var
					(p-assign (ident "$acc"))
					(e-list
						(elems
							(e-num (value "0")))))
				(s-for
					(p-assign (ident "e"))
					(e-lookup-local
						(p-assign (ident "l")))
					(e-block
						(s-reassign
							(p-assign (ident "$acc"))
							(e-call
								(e-lookup-external
									(builtin))
								(e-lookup-local
									(p-assign (ident "$acc")))
								(e-lookup-local
									(p-assign (ident "e")))))
						(s-reassign
							(p-assign (ident "$total"))
							(e-match
								(match
									(cond
										(e-call
											(e-lookup-external
												(builtin))
											(e-lookup-local
												(p-assign (ident "$acc")))))
									(branches
										(branch
											(patterns
												(pattern (degenerate false)
													(p-applied-tag)))
											(value
												(e-binop (op "add")
													(e-lookup-local
														(p-assign (ident "$total")))
													(e-lookup-local
														(p-assign (ident "last"))))))
										(branch
											(patterns
												(pattern (degenerate false)
													(p-applied-tag)))
											(value
												(e-lookup-local
													(p-assign (ident "$total")))))))))
						(e-empty_record)))
				(e-lookup-local
					(p-assign (ident "$total"))))))
	(e-call
		(e-lookup-local
			(p-assign (ident "sum_with_last")))
		(e-list
			(elems
				(e-num (value "10"))
				(e-num (value "20"))
				(e-num (value "30"))))))
~~~
# TYPES
~~~clojure
(expr (type "I64"))
~~~
