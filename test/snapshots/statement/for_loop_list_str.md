# META
~~~ini
description=For loop iterating over List Str
type=snippet
~~~
# SOURCE
~~~roc
count : U64
count = {
	var counter_ = 0
	for _ in ["hello", "world", "test"] {
		counter_ = counter_ + 1
	}
	counter_
}

expect count == 3
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**for_loop_list_str.md:3:17:3:18:**
```roc
	var counter_ = 0
```
	               ^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**for_loop_list_str.md:3:17:3:18:**
```roc
	var counter_ = 0
```
	               ^

It has the type:
    _Try(U64, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(U64, [InvalidNumeral(Str)])_

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwFor,Underscore,KwIn,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,
CloseCurly,
KwExpect,LowerIdent,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "count")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "count"))
			(e-block
				(statements
					(s-var (name "counter_")
						(e-int (raw "0")))
					(s-for
						(p-underscore)
						(e-list
							(e-string
								(e-string-part (raw "hello")))
							(e-string
								(e-string-part (raw "world")))
							(e-string
								(e-string-part (raw "test"))))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "counter_"))
									(e-binop (op "+")
										(e-ident (raw "counter_"))
										(e-int (raw "1")))))))
					(e-ident (raw "counter_")))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "count"))
				(e-int (raw "3"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "count"))
		(e-block
			(s-var
				(p-assign (ident "counter_"))
				(e-num (value "0")))
			(s-for
				(p-underscore)
				(e-list
					(elems
						(e-string
							(e-literal (string "hello")))
						(e-string
							(e-literal (string "world")))
						(e-string
							(e-literal (string "test")))))
				(e-block
					(s-reassign
						(p-assign (ident "counter_"))
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "counter_")))
							(e-num (value "1"))))
					(e-empty_record)))
			(e-lookup-local
				(p-assign (ident "counter_"))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "count")))
			(e-num (value "3")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
