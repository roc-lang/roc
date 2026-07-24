# META
~~~ini
description=Deeply nested tag union in a type mismatch renders without crashing (regression: TypeWriter scratch-list realloc use-after-free)
type=snippet
~~~
# SOURCE
~~~roc
# Regression test for a TypeWriter use-after-free. The annotation below
# mismatches the body, so the reporter renders the tag union. The deeply
# nested tags exceed the TypeWriter scratch-list capacity mid-walk; before the
# fix, the realloc dangled a slice held across writeTagUnion's loop and the
# next tag name was read from freed memory ("Ident.Idx lookup in wrong store").
collide : [A6([A5([A4([A3([A2([A1([Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10]), B1_1, B1_2, B1_3, B1_4, B1_5]), B2_1, B2_2, B2_3, B2_4, B2_5]), B3_1, B3_2, B3_3, B3_4, B3_5]), B4_1, B4_2, B4_3, B4_4, B4_5]), B5_1, B5_2, B5_3, B5_4, B5_5]), B6_1, B6_2, B6_3, B6_4, B6_5] -> Str
collide = |x| x
~~~
# EXPECTED
TYPE MISMATCH - type_mismatch_deep_nested_tag_union.md:7:15:7:16
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This expression is used in an unexpected way. ─────────────┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  collide = |x| x                                                           │
 │                ‾                                                           │
 └─────────────────────────────── type_mismatch_deep_nested_tag_union.md:7:15 ┘

    It has the type:

        [A6([A5([A4([A3([A2([A1([Z1, Z10, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9]),
        B1_1, B1_2, B1_3, B1_4, B1_5]), B2_1, B2_2, B2_3, B2_4, B2_5]), B3_1,
        B3_2, B3_3, B3_4, B3_5]), B4_1, B4_2, B4_3, B4_4, B4_5]), B5_1, B5_2,
        B5_3, B5_4, B5_5]), B6_1, B6_2, B6_3, B6_4, B6_5]

    But the annotation says it should be:

        Str

# TOKENS
~~~zig
LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,CloseRound,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,CloseRound,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,CloseRound,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,CloseRound,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,CloseRound,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,CloseRound,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "collide")
			(ty-fn
				(ty-tag-union
					(tags
						(ty-apply
							(ty (name "A6"))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "A5"))
										(ty-tag-union
											(tags
												(ty-apply
													(ty (name "A4"))
													(ty-tag-union
														(tags
															(ty-apply
																(ty (name "A3"))
																(ty-tag-union
																	(tags
																		(ty-apply
																			(ty (name "A2"))
																			(ty-tag-union
																				(tags
																					(ty-apply
																						(ty (name "A1"))
																						(ty-tag-union
																							(tags
																								(ty (name "Z1"))
																								(ty (name "Z2"))
																								(ty (name "Z3"))
																								(ty (name "Z4"))
																								(ty (name "Z5"))
																								(ty (name "Z6"))
																								(ty (name "Z7"))
																								(ty (name "Z8"))
																								(ty (name "Z9"))
																								(ty (name "Z10")))))
																					(ty (name "B1_1"))
																					(ty (name "B1_2"))
																					(ty (name "B1_3"))
																					(ty (name "B1_4"))
																					(ty (name "B1_5")))))
																		(ty (name "B2_1"))
																		(ty (name "B2_2"))
																		(ty (name "B2_3"))
																		(ty (name "B2_4"))
																		(ty (name "B2_5")))))
															(ty (name "B3_1"))
															(ty (name "B3_2"))
															(ty (name "B3_3"))
															(ty (name "B3_4"))
															(ty (name "B3_5")))))
												(ty (name "B4_1"))
												(ty (name "B4_2"))
												(ty (name "B4_3"))
												(ty (name "B4_4"))
												(ty (name "B4_5")))))
									(ty (name "B5_1"))
									(ty (name "B5_2"))
									(ty (name "B5_3"))
									(ty (name "B5_4"))
									(ty (name "B5_5")))))
						(ty (name "B6_1"))
						(ty (name "B6_2"))
						(ty (name "B6_3"))
						(ty (name "B6_4"))
						(ty (name "B6_5"))))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "collide"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "collide"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-runtime-error (tag "erroneous_value_use")))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "A6")
						(ty-tag-union
							(ty-tag-name (name "A5")
								(ty-tag-union
									(ty-tag-name (name "A4")
										(ty-tag-union
											(ty-tag-name (name "A3")
												(ty-tag-union
													(ty-tag-name (name "A2")
														(ty-tag-union
															(ty-tag-name (name "A1")
																(ty-tag-union
																	(ty-tag-name (name "Z1"))
																	(ty-tag-name (name "Z2"))
																	(ty-tag-name (name "Z3"))
																	(ty-tag-name (name "Z4"))
																	(ty-tag-name (name "Z5"))
																	(ty-tag-name (name "Z6"))
																	(ty-tag-name (name "Z7"))
																	(ty-tag-name (name "Z8"))
																	(ty-tag-name (name "Z9"))
																	(ty-tag-name (name "Z10"))))
															(ty-tag-name (name "B1_1"))
															(ty-tag-name (name "B1_2"))
															(ty-tag-name (name "B1_3"))
															(ty-tag-name (name "B1_4"))
															(ty-tag-name (name "B1_5"))))
													(ty-tag-name (name "B2_1"))
													(ty-tag-name (name "B2_2"))
													(ty-tag-name (name "B2_3"))
													(ty-tag-name (name "B2_4"))
													(ty-tag-name (name "B2_5"))))
											(ty-tag-name (name "B3_1"))
											(ty-tag-name (name "B3_2"))
											(ty-tag-name (name "B3_3"))
											(ty-tag-name (name "B3_4"))
											(ty-tag-name (name "B3_5"))))
									(ty-tag-name (name "B4_1"))
									(ty-tag-name (name "B4_2"))
									(ty-tag-name (name "B4_3"))
									(ty-tag-name (name "B4_4"))
									(ty-tag-name (name "B4_5"))))
							(ty-tag-name (name "B5_1"))
							(ty-tag-name (name "B5_2"))
							(ty-tag-name (name "B5_3"))
							(ty-tag-name (name "B5_4"))
							(ty-tag-name (name "B5_5"))))
					(ty-tag-name (name "B6_1"))
					(ty-tag-name (name "B6_2"))
					(ty-tag-name (name "B6_3"))
					(ty-tag-name (name "B6_4"))
					(ty-tag-name (name "B6_5")))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[A6([A5([A4([A3([A2([A1([Z1, Z10, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9]), B1_1, B1_2, B1_3, B1_4, B1_5]), B2_1, B2_2, B2_3, B2_4, B2_5]), B3_1, B3_2, B3_3, B3_4, B3_5]), B4_1, B4_2, B4_3, B4_4, B4_5]), B5_1, B5_2, B5_3, B5_4, B5_5]), B6_1, B6_2, B6_3, B6_4, B6_5] -> Str")))
	(expressions
		(expr (type "[A6([A5([A4([A3([A2([A1([Z1, Z10, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9]), B1_1, B1_2, B1_3, B1_4, B1_5]), B2_1, B2_2, B2_3, B2_4, B2_5]), B3_1, B3_2, B3_3, B3_4, B3_5]), B4_1, B4_2, B4_3, B4_4, B4_5]), B5_1, B5_2, B5_3, B5_4, B5_5]), B6_1, B6_2, B6_3, B6_4, B6_5] -> Str"))))
~~~
