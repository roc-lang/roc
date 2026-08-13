# META
~~~ini
description=Statements that are not valid in an associated block are reported instead of being silently dropped (issue 10730)
type=file:AssocStmts.roc
~~~
# SOURCE
~~~roc
Counter := [].{
    dbg 5

    crash "boom"

    return 5

    for x in [1, 2] {
        dbg x
    }

    while 1 == 2 {
        dbg 3
    }

    break
}
~~~
# EXPECTED
NOT IMPLEMENTED - assoc_invalid_statements.md:2:5:2:10
NOT IMPLEMENTED - assoc_invalid_statements.md:4:5:4:17
NOT IMPLEMENTED - assoc_invalid_statements.md:6:5:6:13
NOT IMPLEMENTED - assoc_invalid_statements.md:8:5:10:6
NOT IMPLEMENTED - assoc_invalid_statements.md:12:5:14:6
NOT IMPLEMENTED - assoc_invalid_statements.md:16:5:16:10
# PROBLEMS

┌─────────────────┐
│ NOT IMPLEMENTED ├─ This feature is not yet implemented: dbg statements in ──┐
└┬────────────────┘  associated blocks.                                       │
 │                                                                            │
 │  dbg 5                                                                     │
 │  ‾‾‾‾‾                                                                     │
 └─────────────────────────────────────────── assoc_invalid_statements.md:2:5 ┘

    This error doesn't have a proper diagnostic report yet. Let us know if you
    want to help improve Roc's error messages!


┌─────────────────┐
│ NOT IMPLEMENTED ├─ This feature is not yet implemented: crash statements ───┐
└┬────────────────┘  in associated blocks.                                    │
 │                                                                            │
 │  crash "boom"                                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾                                                              │
 └─────────────────────────────────────────── assoc_invalid_statements.md:4:5 ┘

    This error doesn't have a proper diagnostic report yet. Let us know if you
    want to help improve Roc's error messages!


┌─────────────────┐
│ NOT IMPLEMENTED ├─ This feature is not yet implemented: return statements ──┐
└┬────────────────┘  in associated blocks.                                    │
 │                                                                            │
 │  return 5                                                                  │
 │  ‾‾‾‾‾‾‾‾                                                                  │
 └─────────────────────────────────────────── assoc_invalid_statements.md:6:5 ┘

    This error doesn't have a proper diagnostic report yet. Let us know if you
    want to help improve Roc's error messages!


┌─────────────────┐
│ NOT IMPLEMENTED ├─ This feature is not yet implemented: for statements in ──┐
└┬────────────────┘  associated blocks.                                       │
 │                                                                            │
 │  for x in [1, 2] {                                                         │
 │      dbg x                                                                 │
 │  }                                                                         │
 │                                                                            │
 └─────────────────────────────────────────── assoc_invalid_statements.md:8:5 ┘

    This error doesn't have a proper diagnostic report yet. Let us know if you
    want to help improve Roc's error messages!


┌─────────────────┐
│ NOT IMPLEMENTED ├─ This feature is not yet implemented: while statements ───┐
└┬────────────────┘  in associated blocks.                                    │
 │                                                                            │
 │  while 1 == 2 {                                                            │
 │      dbg 3                                                                 │
 │  }                                                                         │
 │                                                                            │
 └────────────────────────────────────────── assoc_invalid_statements.md:12:5 ┘

    This error doesn't have a proper diagnostic report yet. Let us know if you
    want to help improve Roc's error messages!


┌─────────────────┐
│ NOT IMPLEMENTED ├─ This feature is not yet implemented: break statements ───┐
└┬────────────────┘  in associated blocks.                                    │
 │                                                                            │
 │  break                                                                     │
 │  ‾‾‾‾‾                                                                     │
 └────────────────────────────────────────── assoc_invalid_statements.md:16:5 ┘

    This error doesn't have a proper diagnostic report yet. Let us know if you
    want to help improve Roc's error messages!

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
KwDbg,Int,
KwCrash,StringStart,StringPart,StringEnd,
KwReturn,Int,
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,CloseSquare,OpenCurly,
KwDbg,LowerIdent,
CloseCurly,
KwWhile,Int,OpEquals,Int,OpenCurly,
KwDbg,Int,
CloseCurly,
KwBreak,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Counter")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-dbg
					(e-int (raw "5")))
				(s-crash
					(e-string
						(e-string-part (raw "boom"))))
				(s-return
					(e-int (raw "5")))
				(s-for
					(p-ident (raw "x"))
					(e-list
						(e-int (raw "1"))
						(e-int (raw "2")))
					(e-block
						(statements
							(s-dbg
								(e-ident (raw "x"))))))
				(s-while
					(e-binop (op "==")
						(e-int (raw "1"))
						(e-int (raw "2")))
					(e-block
						(statements
							(s-dbg
								(e-int (raw "3"))))))
				(s-break)))))
~~~
# FORMATTED
~~~roc
Counter := [].{
	dbg 5

	crash "boom"

	return 5

	for x in [1, 2] {
		dbg x
	}

	while 1 == 2 {
		dbg 3
	}

	break
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "Counter"))
		(ty-tag-union)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "Counter")
			(ty-header (name "Counter"))))
	(expressions))
~~~
