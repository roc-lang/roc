# META
~~~ini
description=canonicalize hang: non-regular recursive nominal inhabitedness
type=file
~~~
# SOURCE
~~~roc
R000ee(k):=[No0e(R000ee(o))].{e={||{match 0{R000ee.No0e(0)=>0}}()}}
~~~
# EXPECTED
UNDECLARED TYPE VARIABLE - fuzz_crash_110.md:1:25:1:26
MISSING METHOD - fuzz_crash_110.md:1:57:1:58
TYPE MISMATCH - fuzz_crash_110.md:1:57:1:58
TYPE MISMATCH - fuzz_crash_110.md:1:43:1:44
MISSING METHOD - fuzz_crash_110.md:1:61:1:62
NON EXHAUSTIVE MATCH - fuzz_crash_110.md:1:37:1:63
# PROBLEMS

┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `o` is not declared in this ──┐
└┬─────────────────────────┘  scope.                                          │
 │                                                                            │
 │  R000ee(k):=[No0e(R000ee(o))].{e={||{match 0{R000ee.No0e(0)=>0}}()}}       │
 │                          ‾                                                 │
 └──────────────────────────────────────────────────── fuzz_crash_110.md:1:25 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌────────────────┐
│ MISSING METHOD ├─ The value before this `==` operator has a type that ──────┐
└┬───────────────┘  doesn't have a `is_eq` method.                            │
 │                                                                            │
 │  R000ee(k):=[No0e(R000ee(o))].{e={||{match 0{R000ee.No0e(0)=>0}}()}}       │
 │                                                          ‾                 │
 └──────────────────────────────────────────────────── fuzz_crash_110.md:1:57 ┘

    The value's type, which does not have a method named `is_eq`, is:

        R000ee(Error)

    Hint: The `==` operator calls a method named `is_eq` on the value preceding
    it, passing the value after the operator as the one argument.


┌───────────────┐
│ TYPE MISMATCH ├─ This number is being used where a non-number type is ──────┐
└┬──────────────┘  needed.                                                    │
 │                                                                            │
 │  R000ee(k):=[No0e(R000ee(o))].{e={||{match 0{R000ee.No0e(0)=>0}}()}}       │
 │                                                          ‾                 │
 └──────────────────────────────────────────────────── fuzz_crash_110.md:1:57 ┘

    Other code expects this to have the type:

        R000ee(Error)


┌───────────────┐
│ TYPE MISMATCH ├─ This number is being used where a non-number type is ──────┐
└┬──────────────┘  needed.                                                    │
 │                                                                            │
 │  R000ee(k):=[No0e(R000ee(o))].{e={||{match 0{R000ee.No0e(0)=>0}}()}}       │
 │                                            ‾                               │
 └──────────────────────────────────────────────────── fuzz_crash_110.md:1:43 ┘

    The type was determined to be non-numeric here:
      ┌───────────────────────────────────────────────────────────────────────┐
    1 │  R000ee(k):=[No0e(R000ee(o))].{e={||{match 0{R000ee.No0e(0)=>0}}()}}  │
      │                                              ‾‾‾‾‾‾‾‾‾‾‾‾‾‾           │
      └─────────────────────────────────────────────── fuzz_crash_110.md:1:45 ┘
    Other code expects this to have the type:

        R000ee(k)


┌────────────────┐
│ MISSING METHOD ├─ This `from_numeral` method is being called on a value ────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  R000ee(k):=[No0e(R000ee(o))].{e={||{match 0{R000ee.No0e(0)=>0}}()}}       │
 │                                                              ‾             │
 └──────────────────────────────────────────────────── fuzz_crash_110.md:1:61 ┘

    The value's type, which does not have a method named `from_numeral`, is:

        ({}) -> _ret


┌──────────────────────┐
│ NON EXHAUSTIVE MATCH ├─ This match expression doesn't cover all possible ───┐
└┬─────────────────────┘  cases.                                              │
 │                                                                            │
 │  R000ee(k):=[No0e(R000ee(o))].{e={||{match 0{R000ee.No0e(0)=>0}}()}}       │
 │                                      ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾            │
 └──────────────────────────────────────────────────── fuzz_crash_110.md:1:37 ┘

    The value being matched on has type:
            R000ee(k)

    Missing patterns:
            No0e _

    Hint: Add branches to handle these cases, or use `_` to match anything.

# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,CloseSquare,Dot,OpenCurly,LowerIdent,OpAssign,OpenCurly,OpBar,OpBar,OpenCurly,KwMatch,Int,OpenCurly,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Int,CloseRound,OpFatArrow,Int,CloseCurly,CloseCurly,NoSpaceOpenRound,CloseRound,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "R000ee")
				(args
					(ty-var (raw "k"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "No0e"))
						(ty-apply
							(ty (name "R000ee"))
							(ty-var (raw "o"))))))
			(associated
				(s-decl
					(p-ident (raw "e"))
					(e-block
						(statements
							(e-lambda
								(args)
								(e-apply
									(e-block
										(statements
											(e-match
												(e-int (raw "0"))
												(branches
													(branch
														(p-tag (raw ".No0e")
															(p-int (raw "0")))
														(e-int (raw "0"))))))))))))))))
~~~
# FORMATTED
~~~roc
R000ee(k) := [No0e(R000ee(o))].{
	e = {
		|| {
			match 0 {
				R000ee.No0e(0) => 0
			}
		}()
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "fuzz_crash_110.R000ee.e"))
		(e-block
			(e-lambda
				(args)
				(e-call (constraint-fn-var 239)
					(e-block
						(e-match
							(match
								(cond
									(e-num (value "0")))
								(branches
									(branch
										(patterns
											(pattern (degenerate false)
												(p-nominal
													(p-applied-tag))))
										(value
											(e-num (value "0"))))))))))))
	(s-nominal-decl
		(ty-header (name "R000ee")
			(ty-args
				(ty-rigid-var (name "k"))))
		(ty-tag-union
			(ty-tag-name (name "No0e")
				(ty-apply (name "R000ee") (local)
					(ty-malformed))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> _ret")))
	(type_decls
		(nominal (type "R000ee(k)")
			(ty-header (name "R000ee")
				(ty-args
					(ty-rigid-var (name "k"))))))
	(expressions
		(expr (type "({}) -> _ret"))))
~~~
