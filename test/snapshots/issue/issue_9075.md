# META
~~~ini
description=static dispatch with num_literal is checked as expected
type=snippet
~~~
# SOURCE
~~~roc
call : a, (a -> b) -> b
call = |thing, f| { f(thing) }

y = 5 -> call(|i| {i + 1})

main = "${y}"
~~~
# EXPECTED
TYPE MISMATCH - issue_9075.md:4:5:4:6
MISSING METHOD - issue_9075.md:4:20:4:25
+ - :0:0:0:0
# PROBLEMS
**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**issue_9075.md:4:5:4:6:**
```roc
y = 5 -> call(|i| {i + 1})
```
    ^

The type was determined to be non-numeric here:
**issue_9075.md:6:11:6:12:**
```roc
main = "${y}"
```
          ^

Other code expects this to have the type:

    Str

**MISSING METHOD**
The value before this **+** operator has a type that doesn't have a **plus** method:
**issue_9075.md:4:20:4:25:**
```roc
y = 5 -> call(|i| {i + 1})
```
                   ^^^^^

The value's type, which does not have a method named**plus**, is:

    Str

**Hint:** The **+** operator calls a method named **plus** on the value preceding it, passing the value after the operator as the one argument.

# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,Comma,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseCurly,
LowerIdent,OpAssign,Int,OpArrow,LowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,OpenCurly,LowerIdent,OpPlus,Int,CloseCurly,CloseRound,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "call")
			(ty-fn
				(ty-var (raw "a"))
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-var (raw "b"))))
		(s-decl
			(p-ident (raw "call"))
			(e-lambda
				(args
					(p-ident (raw "thing"))
					(p-ident (raw "f")))
				(e-block
					(statements
						(e-apply
							(e-ident (raw "f"))
							(e-ident (raw "thing")))))))
		(s-decl
			(p-ident (raw "y"))
			(e-local-dispatch
				(e-int (raw "5"))
				(e-apply
					(e-ident (raw "call"))
					(e-lambda
						(args
							(p-ident (raw "i")))
						(e-block
							(statements
								(e-binop (op "+")
									(e-ident (raw "i"))
									(e-int (raw "1")))))))))
		(s-decl
			(p-ident (raw "main"))
			(e-string
				(e-string-part (raw ""))
				(e-ident (raw "y"))
				(e-string-part (raw ""))))))
~~~
# FORMATTED
~~~roc
call : a, (a -> b) -> b
call = |thing, f| {
	f(thing)
}

y = 5->call(
	|i| {
		i + 1
	},
)

main = "${y}"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "call"))
		(e-lambda
			(args
				(p-assign (ident "thing"))
				(p-assign (ident "f")))
			(e-block
				(e-call
					(e-lookup-local
						(p-assign (ident "f")))
					(e-lookup-local
						(p-assign (ident "thing"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-rigid-var (name "b"))))
				(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
	(d-let
		(p-assign (ident "y"))
		(e-call
			(e-lookup-local
				(p-assign (ident "call")))
			(e-num (value "5"))
			(e-lambda
				(args
					(p-assign (ident "i")))
				(e-block
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "i")))
						(e-num (value "1")))))))
	(d-let
		(p-assign (ident "main"))
		(e-string
			(e-literal (string ""))
			(e-lookup-local
				(p-assign (ident "y")))
			(e-literal (string "")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a, (a -> b) -> b"))
		(patt (type "Dec"))
		(patt (type "Str")))
	(expressions
		(expr (type "a, (a -> b) -> b"))
		(expr (type "Dec"))
		(expr (type "Str"))))
~~~
