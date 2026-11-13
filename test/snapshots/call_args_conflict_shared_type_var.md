# META
~~~ini
description=Call arguments that each fit the function's declared type but disagree on a type variable they share are reported as conflicting with each other, whichever comes later; an argument that is wrong by itself, or passed to a function whose type is no scheme, keeps the ordinary report
type=file
~~~
# SOURCE
~~~roc
In(c) := [In(c)]

Val(ctx, a) := [Val(a)]

Handle(ctx, a) := [Handle(a)].{
	lift : Handle(c, a) -> Handle(In(c), a)
	lift = |Handle(a)| Handle(a)
}

count : Handle(ctx, a), Val(ctx, List(b)) -> U64
count = |_handle, Val(rows)| rows.len()

count_swapped : Val(ctx, List(b)), Handle(ctx, a) -> U64
count_swapped = |rows, handle| count(handle, rows)

total : Handle(c, U64), Val(In(c), List(Str)) -> U64
total = |handle, rows| count(handle.lift().lift(), rows)

total_swapped : Handle(c, U64), Val(In(c), List(Str)) -> U64
total_swapped = |handle, rows| count_swapped(rows, handle.lift().lift())

same : a, a -> Bool
same = |_x, _y| Bool.True

bare : U8, Str -> Bool
bare = |n, s| same(n, s)

third : a, b, a -> Bool
third = |_x, _y, _z| Bool.True

skips_middle : U8, Str -> Bool
skips_middle = |n, s| third(n, s, s)

fixed_first : U8, a, a -> Bool
fixed_first = |_x, _y, _z| Bool.True

skips_first : U8, Str -> Bool
skips_first = |n, s| fixed_first(n, n, [s])

needs_str : a, Str -> Bool
needs_str = |_x, _y| Bool.True

wrong_by_itself : U8 -> Bool
wrong_by_itself = |n| needs_str(n, n)

immediate : U8, Str -> Bool
immediate = |n, s| (|x, y| x == y)(n, s)
~~~
# EXPECTED
TYPE MISMATCH - call_args_conflict_shared_type_var.md:17:30:17:50
TYPE MISMATCH - call_args_conflict_shared_type_var.md:20:46:20:50
TYPE MISMATCH - call_args_conflict_shared_type_var.md:26:20:26:21
TYPE MISMATCH - call_args_conflict_shared_type_var.md:32:29:32:30
TYPE MISMATCH - call_args_conflict_shared_type_var.md:38:37:38:38
TYPE MISMATCH - call_args_conflict_shared_type_var.md:44:23:44:23
TYPE MISMATCH - call_args_conflict_shared_type_var.md:47:39:47:40
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 17 30) (end 17 50))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "and")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "arguments")
			(reflow " ")
			(reflow "of this call to")
			(reflow " ")
			(annotated code "count")
			(reflow " ")
			(reflow "have types that conflict with each other."))
		(document
			(source-region (file "call_args_conflict_shared_type_var.md") (start 17 30) (end 17 50) (annotation error) (line-text "total = |handle, rows| count(handle.lift().lift(), rows)"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Handle(In(In(c)), U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "call_args_conflict_shared_type_var.md") (start 17 52) (end 17 56) (annotation error) (line-text "total = |handle, rows| count(handle.lift().lift(), rows)"))
			(line-break)
			(reflow "But the")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Val(In(c), List(Str))")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Each fits the type of")
			(reflow " ")
			(annotated code "count")
			(reflow " ")
			(reflow "by itself, but that type needs them to agree with each other:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Handle(ctx, a), Val(ctx, List(b)) -> U64")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 20 46) (end 20 50))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "and")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "arguments")
			(reflow " ")
			(reflow "of this call to")
			(reflow " ")
			(annotated code "count_swapped")
			(reflow " ")
			(reflow "have types that conflict with each other."))
		(document
			(source-region (file "call_args_conflict_shared_type_var.md") (start 20 46) (end 20 50) (annotation error) (line-text "total_swapped = |handle, rows| count_swapped(rows, handle.lift().lift())"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Val(In(c), List(Str))")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "call_args_conflict_shared_type_var.md") (start 20 52) (end 20 72) (annotation error) (line-text "total_swapped = |handle, rows| count_swapped(rows, handle.lift().lift())"))
			(line-break)
			(reflow "But the")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Handle(In(In(c)), U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Each fits the type of")
			(reflow " ")
			(annotated code "count_swapped")
			(reflow " ")
			(reflow "by itself, but that type needs them to agree with each other:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Val(ctx, List(b)), Handle(ctx, a) -> U64")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 26 20) (end 26 21))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "and")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "arguments")
			(reflow " ")
			(reflow "of this call to")
			(reflow " ")
			(annotated code "same")
			(reflow " ")
			(reflow "have types that conflict with each other."))
		(document
			(source-region (file "call_args_conflict_shared_type_var.md") (start 26 20) (end 26 21) (annotation error) (line-text "bare = |n, s| same(n, s)"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U8")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "call_args_conflict_shared_type_var.md") (start 26 23) (end 26 24) (annotation error) (line-text "bare = |n, s| same(n, s)"))
			(line-break)
			(reflow "But the")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Each fits the type of")
			(reflow " ")
			(annotated code "same")
			(reflow " ")
			(reflow "by itself, but that type needs them to agree with each other:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a, a -> Bool")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 32 29) (end 32 30))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "and")
			(reflow " ")
			(reflow "third")
			(reflow " ")
			(reflow "arguments")
			(reflow " ")
			(reflow "of this call to")
			(reflow " ")
			(annotated code "third")
			(reflow " ")
			(reflow "have types that conflict with each other."))
		(document
			(source-region (file "call_args_conflict_shared_type_var.md") (start 32 29) (end 32 30) (annotation error) (line-text "skips_middle = |n, s| third(n, s, s)"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U8")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "call_args_conflict_shared_type_var.md") (start 32 35) (end 32 36) (annotation error) (line-text "skips_middle = |n, s| third(n, s, s)"))
			(line-break)
			(reflow "But the")
			(reflow " ")
			(reflow "third")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Each fits the type of")
			(reflow " ")
			(annotated code "third")
			(reflow " ")
			(reflow "by itself, but that type needs them to agree with each other:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a, b, a -> Bool")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 38 37) (end 38 38))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "and")
			(reflow " ")
			(reflow "third")
			(reflow " ")
			(reflow "arguments")
			(reflow " ")
			(reflow "of this call to")
			(reflow " ")
			(annotated code "fixed_first")
			(reflow " ")
			(reflow "have types that conflict with each other."))
		(document
			(source-region (file "call_args_conflict_shared_type_var.md") (start 38 37) (end 38 38) (annotation error) (line-text "skips_first = |n, s| fixed_first(n, n, [s])"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U8")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "call_args_conflict_shared_type_var.md") (start 38 40) (end 38 43) (annotation error) (line-text "skips_first = |n, s| fixed_first(n, n, [s])"))
			(line-break)
			(reflow "But the")
			(reflow " ")
			(reflow "third")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(Str)")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Each fits the type of")
			(reflow " ")
			(annotated code "fixed_first")
			(reflow " ")
			(reflow "by itself, but that type needs them to agree with each other:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U8, a, a -> Bool")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 44 36) (end 44 37))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "argument being passed to this function has the wrong type."))
		(document
			(source-underlines
				(display (file "call_args_conflict_shared_type_var.md") (start 44 23) (end 44 38) (annotation dim) (line-text "wrong_by_itself = |n| needs_str(n, n)"))
				(underline (start 44 36) (end 44 37) (annotation error)))
			(line-break)
			(reflow "This argument has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U8")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But")
			(reflow " ")
			(annotated code "needs_str")
			(reflow " ")
			(reflow "needs the")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "argument to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Convert it to a")
			(reflow " ")
			(annotated code "Str")
			(reflow " ")
			(reflow "with")
			(reflow " ")
			(annotated code ".to_str()")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 47 39) (end 47 40))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "argument being passed to this function has the wrong type."))
		(document
			(source-region (file "call_args_conflict_shared_type_var.md") (start 47 39) (end 47 40) (annotation error) (line-text "immediate = |n, s| (|x, y| x == y)(n, s)"))
			(line-break)
			(reflow "This argument has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the function needs the")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "argument to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U8")
			(annotation-end))))
~~~
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseRound,
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,Comma,NamedUnderscore,OpBar,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,Comma,NamedUnderscore,Comma,NamedUnderscore,OpBar,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,Comma,LowerIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,Comma,NamedUnderscore,Comma,NamedUnderscore,OpBar,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,Comma,OpenSquare,LowerIdent,CloseSquare,CloseRound,
LowerIdent,OpColon,LowerIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,Comma,NamedUnderscore,OpBar,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenRound,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpEquals,LowerIdent,CloseRound,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "In")
				(args
					(ty-var (raw "c"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "In"))
						(ty-var (raw "c"))))))
		(s-type-decl
			(header (name "Val")
				(args
					(ty-var (raw "ctx"))
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Val"))
						(ty-var (raw "a"))))))
		(s-type-decl
			(header (name "Handle")
				(args
					(ty-var (raw "ctx"))
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Handle"))
						(ty-var (raw "a")))))
			(associated
				(s-type-anno (name "lift")
					(ty-fn
						(ty-apply
							(ty (name "Handle"))
							(ty-var (raw "c"))
							(ty-var (raw "a")))
						(ty-apply
							(ty (name "Handle"))
							(ty-apply
								(ty (name "In"))
								(ty-var (raw "c")))
							(ty-var (raw "a")))))
				(s-decl
					(p-ident (raw "lift"))
					(e-lambda
						(args
							(p-tag (raw "Handle")
								(p-ident (raw "a"))))
						(e-apply
							(e-tag (raw "Handle"))
							(e-ident (raw "a")))))))
		(s-type-anno (name "count")
			(ty-fn
				(ty-apply
					(ty (name "Handle"))
					(ty-var (raw "ctx"))
					(ty-var (raw "a")))
				(ty-apply
					(ty (name "Val"))
					(ty-var (raw "ctx"))
					(ty-apply
						(ty (name "List"))
						(ty-var (raw "b"))))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "count"))
			(e-lambda
				(args
					(p-ident (raw "_handle"))
					(p-tag (raw "Val")
						(p-ident (raw "rows"))))
				(e-method-call (method ".len")
					(receiver
						(e-ident (raw "rows")))
					(args))))
		(s-type-anno (name "count_swapped")
			(ty-fn
				(ty-apply
					(ty (name "Val"))
					(ty-var (raw "ctx"))
					(ty-apply
						(ty (name "List"))
						(ty-var (raw "b"))))
				(ty-apply
					(ty (name "Handle"))
					(ty-var (raw "ctx"))
					(ty-var (raw "a")))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "count_swapped"))
			(e-lambda
				(args
					(p-ident (raw "rows"))
					(p-ident (raw "handle")))
				(e-apply
					(e-ident (raw "count"))
					(e-ident (raw "handle"))
					(e-ident (raw "rows")))))
		(s-type-anno (name "total")
			(ty-fn
				(ty-apply
					(ty (name "Handle"))
					(ty-var (raw "c"))
					(ty (name "U64")))
				(ty-apply
					(ty (name "Val"))
					(ty-apply
						(ty (name "In"))
						(ty-var (raw "c")))
					(ty-apply
						(ty (name "List"))
						(ty (name "Str"))))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "total"))
			(e-lambda
				(args
					(p-ident (raw "handle"))
					(p-ident (raw "rows")))
				(e-apply
					(e-ident (raw "count"))
					(e-method-call (method ".lift")
						(receiver
							(e-method-call (method ".lift")
								(receiver
									(e-ident (raw "handle")))
								(args)))
						(args))
					(e-ident (raw "rows")))))
		(s-type-anno (name "total_swapped")
			(ty-fn
				(ty-apply
					(ty (name "Handle"))
					(ty-var (raw "c"))
					(ty (name "U64")))
				(ty-apply
					(ty (name "Val"))
					(ty-apply
						(ty (name "In"))
						(ty-var (raw "c")))
					(ty-apply
						(ty (name "List"))
						(ty (name "Str"))))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "total_swapped"))
			(e-lambda
				(args
					(p-ident (raw "handle"))
					(p-ident (raw "rows")))
				(e-apply
					(e-ident (raw "count_swapped"))
					(e-ident (raw "rows"))
					(e-method-call (method ".lift")
						(receiver
							(e-method-call (method ".lift")
								(receiver
									(e-ident (raw "handle")))
								(args)))
						(args)))))
		(s-type-anno (name "same")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "a"))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "same"))
			(e-lambda
				(args
					(p-ident (raw "_x"))
					(p-ident (raw "_y")))
				(e-tag (raw "Bool.True"))))
		(s-type-anno (name "bare")
			(ty-fn
				(ty (name "U8"))
				(ty (name "Str"))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "bare"))
			(e-lambda
				(args
					(p-ident (raw "n"))
					(p-ident (raw "s")))
				(e-apply
					(e-ident (raw "same"))
					(e-ident (raw "n"))
					(e-ident (raw "s")))))
		(s-type-anno (name "third")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b"))
				(ty-var (raw "a"))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "third"))
			(e-lambda
				(args
					(p-ident (raw "_x"))
					(p-ident (raw "_y"))
					(p-ident (raw "_z")))
				(e-tag (raw "Bool.True"))))
		(s-type-anno (name "skips_middle")
			(ty-fn
				(ty (name "U8"))
				(ty (name "Str"))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "skips_middle"))
			(e-lambda
				(args
					(p-ident (raw "n"))
					(p-ident (raw "s")))
				(e-apply
					(e-ident (raw "third"))
					(e-ident (raw "n"))
					(e-ident (raw "s"))
					(e-ident (raw "s")))))
		(s-type-anno (name "fixed_first")
			(ty-fn
				(ty (name "U8"))
				(ty-var (raw "a"))
				(ty-var (raw "a"))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "fixed_first"))
			(e-lambda
				(args
					(p-ident (raw "_x"))
					(p-ident (raw "_y"))
					(p-ident (raw "_z")))
				(e-tag (raw "Bool.True"))))
		(s-type-anno (name "skips_first")
			(ty-fn
				(ty (name "U8"))
				(ty (name "Str"))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "skips_first"))
			(e-lambda
				(args
					(p-ident (raw "n"))
					(p-ident (raw "s")))
				(e-apply
					(e-ident (raw "fixed_first"))
					(e-ident (raw "n"))
					(e-ident (raw "n"))
					(e-list
						(e-ident (raw "s"))))))
		(s-type-anno (name "needs_str")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str"))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "needs_str"))
			(e-lambda
				(args
					(p-ident (raw "_x"))
					(p-ident (raw "_y")))
				(e-tag (raw "Bool.True"))))
		(s-type-anno (name "wrong_by_itself")
			(ty-fn
				(ty (name "U8"))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "wrong_by_itself"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-apply
					(e-ident (raw "needs_str"))
					(e-ident (raw "n"))
					(e-ident (raw "n")))))
		(s-type-anno (name "immediate")
			(ty-fn
				(ty (name "U8"))
				(ty (name "Str"))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "immediate"))
			(e-lambda
				(args
					(p-ident (raw "n"))
					(p-ident (raw "s")))
				(e-apply
					(e-tuple
						(e-lambda
							(args
								(p-ident (raw "x"))
								(p-ident (raw "y")))
							(e-binop (op "==")
								(e-ident (raw "x"))
								(e-ident (raw "y")))))
					(e-ident (raw "n"))
					(e-ident (raw "s")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "call_args_conflict_shared_type_var.Handle.lift"))
		(e-lambda
			(args
				(p-applied-tag))
			(e-tag (name "Handle")
				(args
					(e-lookup-local
						(p-assign (ident "a"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Handle") (local)
					(ty-rigid-var (name "c"))
					(ty-rigid-var (name "a")))
				(ty-apply (name "Handle") (local)
					(ty-apply (name "In") (local)
						(ty-rigid-var-lookup (ty-rigid-var (name "c"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "count"))
		(e-lambda
			(args
				(p-assign (ident "_handle"))
				(p-applied-tag))
			(e-dispatch-call (method "len") (constraint-fn-var 594)
				(receiver
					(e-lookup-local
						(p-assign (ident "rows"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Handle") (local)
					(ty-rigid-var (name "ctx"))
					(ty-rigid-var (name "a")))
				(ty-apply (name "Val") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "ctx")))
					(ty-apply (name "List") (builtin)
						(ty-rigid-var (name "b"))))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "count_swapped"))
		(e-lambda
			(args
				(p-assign (ident "rows"))
				(p-assign (ident "handle")))
			(e-call (constraint-fn-var 622)
				(e-lookup-local
					(p-assign (ident "count")))
				(e-lookup-local
					(p-assign (ident "handle")))
				(e-lookup-local
					(p-assign (ident "rows")))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Val") (local)
					(ty-rigid-var (name "ctx"))
					(ty-apply (name "List") (builtin)
						(ty-rigid-var (name "b"))))
				(ty-apply (name "Handle") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "ctx")))
					(ty-rigid-var (name "a")))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "total"))
		(e-lambda
			(args
				(p-assign (ident "handle"))
				(p-assign (ident "rows")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Handle") (local)
					(ty-rigid-var (name "c"))
					(ty-lookup (name "U64") (builtin)))
				(ty-apply (name "Val") (local)
					(ty-apply (name "In") (local)
						(ty-rigid-var-lookup (ty-rigid-var (name "c"))))
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "Str") (builtin))))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "total_swapped"))
		(e-lambda
			(args
				(p-assign (ident "handle"))
				(p-assign (ident "rows")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Handle") (local)
					(ty-rigid-var (name "c"))
					(ty-lookup (name "U64") (builtin)))
				(ty-apply (name "Val") (local)
					(ty-apply (name "In") (local)
						(ty-rigid-var-lookup (ty-rigid-var (name "c"))))
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "Str") (builtin))))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "same"))
		(e-lambda
			(args
				(p-assign (ident "_x"))
				(p-assign (ident "_y")))
			(e-nominal-external
				(builtin)
				(e-tag (name "True"))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))
				(ty-lookup (name "Bool") (builtin)))))
	(d-let
		(p-assign (ident "bare"))
		(e-lambda
			(args
				(p-assign (ident "n"))
				(p-assign (ident "s")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Bool") (builtin)))))
	(d-let
		(p-assign (ident "third"))
		(e-lambda
			(args
				(p-assign (ident "_x"))
				(p-assign (ident "_y"))
				(p-assign (ident "_z")))
			(e-nominal-external
				(builtin)
				(e-tag (name "True"))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))
				(ty-lookup (name "Bool") (builtin)))))
	(d-let
		(p-assign (ident "skips_middle"))
		(e-lambda
			(args
				(p-assign (ident "n"))
				(p-assign (ident "s")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Bool") (builtin)))))
	(d-let
		(p-assign (ident "fixed_first"))
		(e-lambda
			(args
				(p-assign (ident "_x"))
				(p-assign (ident "_y"))
				(p-assign (ident "_z")))
			(e-nominal-external
				(builtin)
				(e-tag (name "True"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-rigid-var (name "a"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))
				(ty-lookup (name "Bool") (builtin)))))
	(d-let
		(p-assign (ident "skips_first"))
		(e-lambda
			(args
				(p-assign (ident "n"))
				(p-assign (ident "s")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Bool") (builtin)))))
	(d-let
		(p-assign (ident "needs_str"))
		(e-lambda
			(args
				(p-assign (ident "_x"))
				(p-assign (ident "_y")))
			(e-nominal-external
				(builtin)
				(e-tag (name "True"))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Bool") (builtin)))))
	(d-let
		(p-assign (ident "wrong_by_itself"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "Bool") (builtin)))))
	(d-let
		(p-assign (ident "immediate"))
		(e-lambda
			(args
				(p-assign (ident "n"))
				(p-assign (ident "s")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Bool") (builtin)))))
	(s-nominal-decl
		(ty-header (name "In")
			(ty-args
				(ty-rigid-var (name "c"))))
		(ty-tag-union
			(ty-tag-name (name "In")
				(ty-rigid-var-lookup (ty-rigid-var (name "c"))))))
	(s-nominal-decl
		(ty-header (name "Val")
			(ty-args
				(ty-rigid-var (name "ctx"))
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Val")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
	(s-nominal-decl
		(ty-header (name "Handle")
			(ty-args
				(ty-rigid-var (name "ctx"))
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Handle")
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Handle(c, a) -> Handle(In(c), a)"))
		(patt (type "Handle(ctx, a), Val(ctx, List(b)) -> U64"))
		(patt (type "Val(ctx, List(b)), Handle(ctx, a) -> U64"))
		(patt (type "Handle(c, U64), Val(In(c), List(Str)) -> U64"))
		(patt (type "Handle(c, U64), Val(In(c), List(Str)) -> U64"))
		(patt (type "a, a -> Bool"))
		(patt (type "U8, Str -> Bool"))
		(patt (type "a, b, a -> Bool"))
		(patt (type "U8, Str -> Bool"))
		(patt (type "U8, a, a -> Bool"))
		(patt (type "U8, Str -> Bool"))
		(patt (type "a, Str -> Bool"))
		(patt (type "U8 -> Bool"))
		(patt (type "U8, Str -> Bool")))
	(type_decls
		(nominal (type "In(c)")
			(ty-header (name "In")
				(ty-args
					(ty-rigid-var (name "c")))))
		(nominal (type "Val(ctx, a)")
			(ty-header (name "Val")
				(ty-args
					(ty-rigid-var (name "ctx"))
					(ty-rigid-var (name "a")))))
		(nominal (type "Handle(ctx, a)")
			(ty-header (name "Handle")
				(ty-args
					(ty-rigid-var (name "ctx"))
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "Handle(c, a) -> Handle(In(c), a)"))
		(expr (type "Handle(ctx, a), Val(ctx, List(b)) -> U64"))
		(expr (type "Val(ctx, List(b)), Handle(ctx, a) -> U64"))
		(expr (type "Handle(c, U64), Val(In(c), List(Str)) -> U64"))
		(expr (type "Handle(c, U64), Val(In(c), List(Str)) -> U64"))
		(expr (type "a, a -> Bool"))
		(expr (type "U8, Str -> Bool"))
		(expr (type "a, b, a -> Bool"))
		(expr (type "U8, Str -> Bool"))
		(expr (type "U8, a, a -> Bool"))
		(expr (type "U8, Str -> Bool"))
		(expr (type "a, Str -> Bool"))
		(expr (type "U8 -> Bool"))
		(expr (type "U8, Str -> Bool"))))
~~~
