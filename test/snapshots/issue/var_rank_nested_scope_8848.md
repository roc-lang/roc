# META
~~~ini
description=Regression test for panic when mutable var reassigned in nested scope (issue 8848)
type=snippet
~~~
# SOURCE
~~~roc
# Regression test for https://github.com/roc-lang/roc/issues/8848
# The bug caused a panic "trying to add var at rank 2, but current rank is 1"
# during type generalization when mutable variables ($var) were reassigned
# in nested scopes like match branches.

ValueCombinationMethod := [
  Modulo,
]

Value := [
    UInt(U64),
    CombinedValue({
        combination_method: ValueCombinationMethod,
        value1: Value,
        value2: Value,
    }),
]

TokenContents : []

TokenizerResult : (
    Try(TokenContents, Str),
    U64, # Index of start of token/error
    U64, # New index in file
)
get_next_token : List(U8), U64 -> TokenizerResult
get_next_token = |file, index| {
    (Err("todo"), 0, 0)
}

parse_value : List(U8), TokenizerResult, List(Str) -> Try((Value, U64), Str)
parse_value = |file, result, possibilities| {
    (first_value, var $index) = parse_first_value(file, result, possibilities)?
    (token, token_pos, $index) = get_next_token(file, $index)
    (value1, token2, token2_pos) = match (first_value, token) {
      (VariableReference(name), Ok(OpenBracketToken)) => {
        # This reassignment of $index inside the match branch triggers the bug
        (t2, t2_pos, $index) = get_next_token(file, $index)
        (FunctionCall({name, args: []}), t2, t2_pos)
      }
      _ => (first_value, token, token_pos)
    }
    combination_method1 : ValueCombinationMethod
    combination_method1 = Modulo
    (value2, $index) = parse_value(file, get_next_token(file, $index), [])
    value : Value
    value = match value2 {
        CombinedValue({combination_method: combination_method2, value1: value2A, value2: value2B}) => {
            CombinedValue({
                combination_method: combination_method2,
                value1: CombinedValue({
                    combination_method: combination_method1,
                    value1: value1,
                    value2: value2A,
                }),
                value2: value2B,
            })
        }
        _ => CombinedValue({combination_method: combination_method1, value1, value2})
    }
    Ok((value, $index))
}
~~~
# EXPECTED
UNUSED VARIABLE - var_rank_nested_scope_8848.md:27:19:27:23
UNUSED VARIABLE - var_rank_nested_scope_8848.md:27:25:27:30
UNDEFINED VARIABLE - var_rank_nested_scope_8848.md:33:33:33:50
UNUSED VARIABLE - var_rank_nested_scope_8848.md:35:14:35:20
UNUSED VARIABLE - var_rank_nested_scope_8848.md:35:22:35:32
INCOMPATIBLE MATCH PATTERNS - var_rank_nested_scope_8848.md:35:36:35:36
TYPE MISMATCH - var_rank_nested_scope_8848.md:47:13:60:6
TYPE MISMATCH - var_rank_nested_scope_8848.md:32:15:62:2
# PROBLEMS
**UNUSED VARIABLE**
Variable `file` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_file` to suppress this warning.
The unused variable is declared here:
**var_rank_nested_scope_8848.md:27:19:27:23:**
```roc
get_next_token = |file, index| {
```
                  ^^^^


**UNUSED VARIABLE**
Variable `index` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_index` to suppress this warning.
The unused variable is declared here:
**var_rank_nested_scope_8848.md:27:25:27:30:**
```roc
get_next_token = |file, index| {
```
                        ^^^^^


**UNDEFINED VARIABLE**
Nothing is named `parse_first_value` in this scope.
Is there an `import` or `exposing` missing up-top?

**var_rank_nested_scope_8848.md:33:33:33:50:**
```roc
    (first_value, var $index) = parse_first_value(file, result, possibilities)?
```
                                ^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `token2` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_token2` to suppress this warning.
The unused variable is declared here:
**var_rank_nested_scope_8848.md:35:14:35:20:**
```roc
    (value1, token2, token2_pos) = match (first_value, token) {
```
             ^^^^^^


**UNUSED VARIABLE**
Variable `token2_pos` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_token2_pos` to suppress this warning.
The unused variable is declared here:
**var_rank_nested_scope_8848.md:35:22:35:32:**
```roc
    (value1, token2, token2_pos) = match (first_value, token) {
```
                     ^^^^^^^^^^


**INCOMPATIBLE MATCH PATTERNS**
The first pattern in this `match` is incompatible:
**var_rank_nested_scope_8848.md:35:36:**
```roc
    (value1, token2, token2_pos) = match (first_value, token) {
      (VariableReference(name), Ok(OpenBracketToken)) => {
        # This reassignment of $index inside the match branch triggers the bug
        (t2, t2_pos, $index) = get_next_token(file, $index)
        (FunctionCall({name, args: []}), t2, t2_pos)
      }
      _ => (first_value, token, token_pos)
    }
```
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The first pattern has the type:

    ([VariableReference(_a), .._others], [Ok([OpenBracketToken, .._others2]), .._others3])

But the expression between the `match` parenthesis has the type:

    (_field, Try(TokenContents, Str))

These two types can never match!

**TYPE MISMATCH**
This expression is used in an unexpected way:
**var_rank_nested_scope_8848.md:47:13:60:6:**
```roc
    value = match value2 {
        CombinedValue({combination_method: combination_method2, value1: value2A, value2: value2B}) => {
            CombinedValue({
                combination_method: combination_method2,
                value1: CombinedValue({
                    combination_method: combination_method1,
                    value1: value1,
                    value2: value2A,
                }),
                value2: value2B,
            })
        }
        _ => CombinedValue({combination_method: combination_method1, value1, value2})
    }
```

It has the type:

    [CombinedValue({ combination_method: ValueCombinationMethod, value1: [CombinedValue({ combination_method: ValueCombinationMethod, value1: ..., value2: a }), FunctionCall({ args: List(_b), name: _field }), .._others], value2: [CombinedValue({ combination_method: ValueCombinationMethod, value1: a, value2: ... }), .._others2] }), .._others3]

But the type annotation says it should have the type:

    Value

**TYPE MISMATCH**
This expression is used in an unexpected way:
**var_rank_nested_scope_8848.md:32:15:62:2:**
```roc
parse_value = |file, result, possibilities| {
    (first_value, var $index) = parse_first_value(file, result, possibilities)?
    (token, token_pos, $index) = get_next_token(file, $index)
    (value1, token2, token2_pos) = match (first_value, token) {
      (VariableReference(name), Ok(OpenBracketToken)) => {
        # This reassignment of $index inside the match branch triggers the bug
        (t2, t2_pos, $index) = get_next_token(file, $index)
        (FunctionCall({name, args: []}), t2, t2_pos)
      }
      _ => (first_value, token, token_pos)
    }
    combination_method1 : ValueCombinationMethod
    combination_method1 = Modulo
    (value2, $index) = parse_value(file, get_next_token(file, $index), [])
    value : Value
    value = match value2 {
        CombinedValue({combination_method: combination_method2, value1: value2A, value2: value2B}) => {
            CombinedValue({
                combination_method: combination_method2,
                value1: CombinedValue({
                    combination_method: combination_method1,
                    value1: value1,
                    value2: value2A,
                }),
                value2: value2B,
            })
        }
        _ => CombinedValue({combination_method: combination_method1, value1, value2})
    }
    Ok((value, $index))
}
```

It has the type:

    List(U8), TokenizerResult, List(Str) -> Try((Value, U64), Str)

But I expected it to be:

    List(U8), TokenizerResult, List(_a) -> ([CombinedValue({ combination_method: ValueCombinationMethod, value1: _field, value2: ... }), .._others], U64)

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,
UpperIdent,Comma,
CloseSquare,
UpperIdent,OpColonEqual,OpenSquare,
UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,
UpperIdent,NoSpaceOpenRound,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
LowerIdent,OpColon,UpperIdent,Comma,
LowerIdent,OpColon,UpperIdent,Comma,
CloseCurly,CloseRound,Comma,
CloseSquare,
UpperIdent,OpColon,OpenSquare,CloseSquare,
UpperIdent,OpColon,OpenRound,
UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,Comma,
UpperIdent,Comma,
UpperIdent,Comma,
CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
OpenRound,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,Comma,Int,Comma,Int,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
OpenRound,LowerIdent,Comma,KwVar,LowerIdent,CloseRound,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,NoSpaceOpQuestion,
OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,KwMatch,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpenCurly,
OpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,OpFatArrow,OpenCurly,
OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
OpenRound,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,Comma,LowerIdent,OpColon,OpenSquare,CloseSquare,CloseCurly,CloseRound,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
Underscore,OpFatArrow,OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,OpenSquare,CloseSquare,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,CloseRound,OpFatArrow,OpenCurly,
UpperIdent,NoSpaceOpenRound,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
CloseCurly,CloseRound,Comma,
LowerIdent,OpColon,LowerIdent,Comma,
CloseCurly,CloseRound,
CloseCurly,
Underscore,OpFatArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseCurly,CloseRound,
CloseCurly,
UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "ValueCombinationMethod")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Modulo")))))
		(s-type-decl
			(header (name "Value")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "UInt"))
						(ty (name "U64")))
					(ty-apply
						(ty (name "CombinedValue"))
						(ty-record
							(anno-record-field (name "combination_method")
								(ty (name "ValueCombinationMethod")))
							(anno-record-field (name "value1")
								(ty (name "Value")))
							(anno-record-field (name "value2")
								(ty (name "Value"))))))))
		(s-type-decl
			(header (name "TokenContents")
				(args))
			(ty-tag-union
				(tags)))
		(s-type-decl
			(header (name "TokenizerResult")
				(args))
			(ty-tuple
				(ty-apply
					(ty (name "Try"))
					(ty (name "TokenContents"))
					(ty (name "Str")))
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-type-anno (name "get_next_token")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty (name "U8")))
				(ty (name "U64"))
				(ty (name "TokenizerResult"))))
		(s-decl
			(p-ident (raw "get_next_token"))
			(e-lambda
				(args
					(p-ident (raw "file"))
					(p-ident (raw "index")))
				(e-block
					(statements
						(e-tuple
							(e-apply
								(e-tag (raw "Err"))
								(e-string
									(e-string-part (raw "todo"))))
							(e-int (raw "0"))
							(e-int (raw "0")))))))
		(s-type-anno (name "parse_value")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty (name "U8")))
				(ty (name "TokenizerResult"))
				(ty-apply
					(ty (name "List"))
					(ty (name "Str")))
				(ty-apply
					(ty (name "Try"))
					(ty-tuple
						(ty (name "Value"))
						(ty (name "U64")))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "parse_value"))
			(e-lambda
				(args
					(p-ident (raw "file"))
					(p-ident (raw "result"))
					(p-ident (raw "possibilities")))
				(e-block
					(statements
						(s-decl
							(p-tuple
								(p-ident (raw "first_value"))
								(p-var-ident (raw "$index")))
							(e-question-suffix
								(e-apply
									(e-ident (raw "parse_first_value"))
									(e-ident (raw "file"))
									(e-ident (raw "result"))
									(e-ident (raw "possibilities")))))
						(s-decl
							(p-tuple
								(p-ident (raw "token"))
								(p-ident (raw "token_pos"))
								(p-ident (raw "$index")))
							(e-apply
								(e-ident (raw "get_next_token"))
								(e-ident (raw "file"))
								(e-ident (raw "$index"))))
						(s-decl
							(p-tuple
								(p-ident (raw "value1"))
								(p-ident (raw "token2"))
								(p-ident (raw "token2_pos")))
							(e-match
								(e-tuple
									(e-ident (raw "first_value"))
									(e-ident (raw "token")))
								(branches
									(branch
										(p-tuple
											(p-tag (raw "VariableReference")
												(p-ident (raw "name")))
											(p-tag (raw "Ok")
												(p-tag (raw "OpenBracketToken"))))
										(e-block
											(statements
												(s-decl
													(p-tuple
														(p-ident (raw "t2"))
														(p-ident (raw "t2_pos"))
														(p-ident (raw "$index")))
													(e-apply
														(e-ident (raw "get_next_token"))
														(e-ident (raw "file"))
														(e-ident (raw "$index"))))
												(e-tuple
													(e-apply
														(e-tag (raw "FunctionCall"))
														(e-record
															(field (field "name"))
															(field (field "args")
																(e-list))))
													(e-ident (raw "t2"))
													(e-ident (raw "t2_pos"))))))
									(branch
										(p-underscore)
										(e-tuple
											(e-ident (raw "first_value"))
											(e-ident (raw "token"))
											(e-ident (raw "token_pos")))))))
						(s-type-anno (name "combination_method1")
							(ty (name "ValueCombinationMethod")))
						(s-decl
							(p-ident (raw "combination_method1"))
							(e-tag (raw "Modulo")))
						(s-decl
							(p-tuple
								(p-ident (raw "value2"))
								(p-ident (raw "$index")))
							(e-apply
								(e-ident (raw "parse_value"))
								(e-ident (raw "file"))
								(e-apply
									(e-ident (raw "get_next_token"))
									(e-ident (raw "file"))
									(e-ident (raw "$index")))
								(e-list)))
						(s-type-anno (name "value")
							(ty (name "Value")))
						(s-decl
							(p-ident (raw "value"))
							(e-match
								(e-ident (raw "value2"))
								(branches
									(branch
										(p-tag (raw "CombinedValue")
											(p-record
												(field (name "combination_method") (rest false)
													(p-ident (raw "combination_method2")))
												(field (name "value1") (rest false)
													(p-ident (raw "value2A")))
												(field (name "value2") (rest false)
													(p-ident (raw "value2B")))))
										(e-block
											(statements
												(e-apply
													(e-tag (raw "CombinedValue"))
													(e-record
														(field (field "combination_method")
															(e-ident (raw "combination_method2")))
														(field (field "value1")
															(e-apply
																(e-tag (raw "CombinedValue"))
																(e-record
																	(field (field "combination_method")
																		(e-ident (raw "combination_method1")))
																	(field (field "value1")
																		(e-ident (raw "value1")))
																	(field (field "value2")
																		(e-ident (raw "value2A"))))))
														(field (field "value2")
															(e-ident (raw "value2B"))))))))
									(branch
										(p-underscore)
										(e-apply
											(e-tag (raw "CombinedValue"))
											(e-record
												(field (field "combination_method")
													(e-ident (raw "combination_method1")))
												(field (field "value1"))
												(field (field "value2"))))))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-tuple
								(e-ident (raw "value"))
								(e-ident (raw "$index"))))))))))
~~~
# FORMATTED
~~~roc
# Regression test for https://github.com/roc-lang/roc/issues/8848
# The bug caused a panic "trying to add var at rank 2, but current rank is 1"
# during type generalization when mutable variables ($var) were reassigned
# in nested scopes like match branches.

ValueCombinationMethod := [
	Modulo,
]

Value := [
	UInt(U64),
	CombinedValue(
		{
			combination_method : ValueCombinationMethod,
			value1 : Value,
			value2 : Value,
		},
	),
]

TokenContents : []

TokenizerResult : (
	Try(TokenContents, Str),
	U64, # Index of start of token/error
	U64, # New index in file
)
get_next_token : List(U8), U64 -> TokenizerResult
get_next_token = |file, index| {
	(Err("todo"), 0, 0)
}

parse_value : List(U8), TokenizerResult, List(Str) -> Try((Value, U64), Str)
parse_value = |file, result, possibilities| {
	(first_value, var $index) = parse_first_value(file, result, possibilities)?
	(token, token_pos, $index) = get_next_token(file, $index)
	(value1, token2, token2_pos) = match (first_value, token) {
		(VariableReference(name), Ok(OpenBracketToken)) => {
			# This reassignment of $index inside the match branch triggers the bug
			(t2, t2_pos, $index) = get_next_token(file, $index)
			(FunctionCall({ name, args: [] }), t2, t2_pos)
		}
		_ => (first_value, token, token_pos)
	}
	combination_method1 : ValueCombinationMethod
	combination_method1 = Modulo
	(value2, $index) = parse_value(file, get_next_token(file, $index), [])
	value : Value
	value = match value2 {
		CombinedValue({ combination_method: combination_method2, value1: value2A, value2: value2B }) => {
			CombinedValue(
				{
					combination_method: combination_method2,
					value1: CombinedValue(
						{
							combination_method: combination_method1,
							value1: value1,
							value2: value2A,
						},
					),
					value2: value2B,
				},
			)
		}
		_ => CombinedValue({ combination_method: combination_method1, value1, value2 })
	}
	Ok((value, $index))
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "get_next_token"))
		(e-lambda
			(args
				(p-assign (ident "file"))
				(p-assign (ident "index")))
			(e-block
				(e-tuple
					(elems
						(e-tag (name "Err")
							(args
								(e-string
									(e-literal (string "todo")))))
						(e-num (value "0"))
						(e-num (value "0"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "U8") (builtin)))
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "TokenizerResult") (local)))))
	(d-let
		(p-assign (ident "parse_value"))
		(e-closure
			(captures
				(capture (ident "get_next_token"))
				(capture (ident "parse_value")))
			(e-lambda
				(args
					(p-assign (ident "file"))
					(p-assign (ident "result"))
					(p-assign (ident "possibilities")))
				(e-block
					(s-let
						(p-tuple
							(patterns
								(p-assign (ident "first_value"))
								(p-assign (ident "$index"))))
						(e-match
							(match
								(cond
									(e-call
										(e-runtime-error (tag "ident_not_in_scope"))
										(e-lookup-local
											(p-assign (ident "file")))
										(e-lookup-local
											(p-assign (ident "result")))
										(e-lookup-local
											(p-assign (ident "possibilities")))))
								(branches
									(branch
										(patterns
											(pattern (degenerate false)
												(p-applied-tag)))
										(value
											(e-lookup-local
												(p-assign (ident "#ok")))))
									(branch
										(patterns
											(pattern (degenerate false)
												(p-applied-tag)))
										(value
											(e-return
												(e-tag (name "Err")
													(args
														(e-lookup-local
															(p-assign (ident "#err"))))))))))))
					(s-let
						(p-tuple
							(patterns
								(p-assign (ident "token"))
								(p-assign (ident "token_pos"))
								(p-assign (ident "$index"))))
						(e-call
							(e-lookup-local
								(p-assign (ident "get_next_token")))
							(e-lookup-local
								(p-assign (ident "file")))
							(e-lookup-local
								(p-assign (ident "$index")))))
					(s-let
						(p-tuple
							(patterns
								(p-assign (ident "value1"))
								(p-assign (ident "token2"))
								(p-assign (ident "token2_pos"))))
						(e-match
							(match
								(cond
									(e-tuple
										(elems
											(e-lookup-local
												(p-assign (ident "first_value")))
											(e-lookup-local
												(p-assign (ident "token"))))))
								(branches
									(branch
										(patterns
											(pattern (degenerate false)
												(p-tuple
													(patterns
														(p-applied-tag)
														(p-applied-tag)))))
										(value
											(e-block
												(s-let
													(p-tuple
														(patterns
															(p-assign (ident "t2"))
															(p-assign (ident "t2_pos"))
															(p-assign (ident "$index"))))
													(e-call
														(e-lookup-local
															(p-assign (ident "get_next_token")))
														(e-lookup-local
															(p-assign (ident "file")))
														(e-lookup-local
															(p-assign (ident "$index")))))
												(e-tuple
													(elems
														(e-tag (name "FunctionCall")
															(args
																(e-record
																	(fields
																		(field (name "name")
																			(e-lookup-local
																				(p-assign (ident "name"))))
																		(field (name "args")
																			(e-empty_list))))))
														(e-lookup-local
															(p-assign (ident "t2")))
														(e-lookup-local
															(p-assign (ident "t2_pos"))))))))
									(branch
										(patterns
											(pattern (degenerate false)
												(p-underscore)))
										(value
											(e-tuple
												(elems
													(e-lookup-local
														(p-assign (ident "first_value")))
													(e-lookup-local
														(p-assign (ident "token")))
													(e-lookup-local
														(p-assign (ident "token_pos")))))))))))
					(s-let
						(p-assign (ident "combination_method1"))
						(e-tag (name "Modulo")))
					(s-let
						(p-tuple
							(patterns
								(p-assign (ident "value2"))
								(p-assign (ident "$index"))))
						(e-call
							(e-lookup-local
								(p-assign (ident "parse_value")))
							(e-lookup-local
								(p-assign (ident "file")))
							(e-call
								(e-lookup-local
									(p-assign (ident "get_next_token")))
								(e-lookup-local
									(p-assign (ident "file")))
								(e-lookup-local
									(p-assign (ident "$index"))))
							(e-empty_list)))
					(s-let
						(p-assign (ident "value"))
						(e-match
							(match
								(cond
									(e-lookup-local
										(p-assign (ident "value2"))))
								(branches
									(branch
										(patterns
											(pattern (degenerate false)
												(p-applied-tag)))
										(value
											(e-block
												(e-tag (name "CombinedValue")
													(args
														(e-record
															(fields
																(field (name "combination_method")
																	(e-lookup-local
																		(p-assign (ident "combination_method2"))))
																(field (name "value1")
																	(e-tag (name "CombinedValue")
																		(args
																			(e-record
																				(fields
																					(field (name "combination_method")
																						(e-lookup-local
																							(p-assign (ident "combination_method1"))))
																					(field (name "value1")
																						(e-lookup-local
																							(p-assign (ident "value1"))))
																					(field (name "value2")
																						(e-lookup-local
																							(p-assign (ident "value2A")))))))))
																(field (name "value2")
																	(e-lookup-local
																		(p-assign (ident "value2B")))))))))))
									(branch
										(patterns
											(pattern (degenerate false)
												(p-underscore)))
										(value
											(e-tag (name "CombinedValue")
												(args
													(e-record
														(fields
															(field (name "combination_method")
																(e-lookup-local
																	(p-assign (ident "combination_method1"))))
															(field (name "value1")
																(e-lookup-local
																	(p-assign (ident "value1"))))
															(field (name "value2")
																(e-lookup-local
																	(p-assign (ident "value2"))))))))))))))
					(e-tag (name "Ok")
						(args
							(e-tuple
								(elems
									(e-lookup-local
										(p-assign (ident "value")))
									(e-lookup-local
										(p-assign (ident "$index"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "U8") (builtin)))
				(ty-lookup (name "TokenizerResult") (local))
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Str") (builtin)))
				(ty-apply (name "Try") (builtin)
					(ty-tuple
						(ty-lookup (name "Value") (local))
						(ty-lookup (name "U64") (builtin)))
					(ty-lookup (name "Str") (builtin))))))
	(s-nominal-decl
		(ty-header (name "ValueCombinationMethod"))
		(ty-tag-union
			(ty-tag-name (name "Modulo"))))
	(s-nominal-decl
		(ty-header (name "Value"))
		(ty-tag-union
			(ty-tag-name (name "UInt")
				(ty-lookup (name "U64") (builtin)))
			(ty-tag-name (name "CombinedValue")
				(ty-record
					(field (field "combination_method")
						(ty-lookup (name "ValueCombinationMethod") (local)))
					(field (field "value1")
						(ty-lookup (name "Value") (local)))
					(field (field "value2")
						(ty-lookup (name "Value") (local)))))))
	(s-alias-decl
		(ty-header (name "TokenContents"))
		(ty-tag-union))
	(s-alias-decl
		(ty-header (name "TokenizerResult"))
		(ty-tuple
			(ty-apply (name "Try") (builtin)
				(ty-lookup (name "TokenContents") (local))
				(ty-lookup (name "Str") (builtin)))
			(ty-lookup (name "U64") (builtin))
			(ty-lookup (name "U64") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(U8), U64 -> TokenizerResult"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "ValueCombinationMethod")
			(ty-header (name "ValueCombinationMethod")))
		(nominal (type "Value")
			(ty-header (name "Value")))
		(alias (type "TokenContents")
			(ty-header (name "TokenContents")))
		(alias (type "TokenizerResult")
			(ty-header (name "TokenizerResult"))))
	(expressions
		(expr (type "List(U8), U64 -> TokenizerResult"))
		(expr (type "Error"))))
~~~
