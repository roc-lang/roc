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
MISSING METHOD - fuzz_crash_110.md:1:61:1:62
# PROBLEMS
── ✗ undeclared type variable ─────────────────────────── fuzz_crash_110.md:1:25

The type variable o is not declared in this scope.

R000ee(k):=[No0e(R000ee(o))].{e={||{match 0{R000ee.No0e(0)=>0}}()}}
                        ^

Type variables must be introduced in a type annotation before they can be used.

── ✗ missing method ───────────────────────────────────── fuzz_crash_110.md:1:61

This from_numeral method is being called on a value whose type doesn't have
that method.

R000ee(k):=[No0e(R000ee(o))].{e={||{match 0{R000ee.No0e(0)=>0}}()}}
                                                            ^

The value's type, which does not have a method named from_numeral, is:

    ({}) -> _ret

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
				(e-call (constraint-fn-var 252)
					(e-block
						(e-runtime-error (tag "erroneous_value_expr")))))))
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
		(nominal (type "Error")
			(ty-header (name "R000ee")
				(ty-args
					(ty-rigid-var (name "k"))))))
	(expressions
		(expr (type "({}) -> _ret"))))
~~~
