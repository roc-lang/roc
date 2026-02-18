# META
~~~ini
description=List.join_with with custom nominal type that implements join_with
type=snippet
~~~
# SOURCE
~~~roc
# Define a custom nominal type with a join_with method
Html := [Raw(Str)].{
    join_with : List(Html), Html -> Html
    join_with = |list_items, separator| {
        # Extract separator string
        sep_str = match separator {
            Html.Raw(s) => s
        }
        # Fold over items, joining them
        joined = List.fold(list_items, "", |acc, item| {
            item_str = match item {
                Html.Raw(s) => s
            }
            if acc == "" {
                item_str
            } else {
                Str.concat(Str.concat(acc, sep_str), item_str)
            }
        })
        Html.Raw(joined)
    }

    to_str : Html -> Str
    to_str = |h| match h {
        Html.Raw(s) => s
    }
}

# Test using List.join_with with the custom Html type
items : List(Html)
items = [Html.Raw("div"), Html.Raw("span"), Html.Raw("p")]

sep : Html
sep = Html.Raw(" | ")

result : Html
result = List.join_with(items, sep)

output : Str
output = result.to_str()

expect output == "div | span | p"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,StringStart,StringPart,StringEnd,Comma,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
KwIf,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,OpenCurly,
LowerIdent,
CloseCurly,KwElse,OpenCurly,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,LowerIdent,CloseRound,
CloseCurly,
CloseCurly,CloseRound,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenSquare,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,Comma,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,Comma,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseSquare,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
KwExpect,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Html")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Raw"))
						(ty (name "Str")))))
			(associated
				(s-type-anno (name "join_with")
					(ty-fn
						(ty-apply
							(ty (name "List"))
							(ty (name "Html")))
						(ty (name "Html"))
						(ty (name "Html"))))
				(s-decl
					(p-ident (raw "join_with"))
					(e-lambda
						(args
							(p-ident (raw "list_items"))
							(p-ident (raw "separator")))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "sep_str"))
									(e-match
										(e-ident (raw "separator"))
										(branches
											(branch
												(p-tag (raw ".Raw")
													(p-ident (raw "s")))
												(e-ident (raw "s"))))))
								(s-decl
									(p-ident (raw "joined"))
									(e-apply
										(e-ident (raw "List.fold"))
										(e-ident (raw "list_items"))
										(e-string
											(e-string-part (raw "")))
										(e-lambda
											(args
												(p-ident (raw "acc"))
												(p-ident (raw "item")))
											(e-block
												(statements
													(s-decl
														(p-ident (raw "item_str"))
														(e-match
															(e-ident (raw "item"))
															(branches
																(branch
																	(p-tag (raw ".Raw")
																		(p-ident (raw "s")))
																	(e-ident (raw "s"))))))
													(e-if-then-else
														(e-binop (op "==")
															(e-ident (raw "acc"))
															(e-string
																(e-string-part (raw ""))))
														(e-block
															(statements
																(e-ident (raw "item_str"))))
														(e-block
															(statements
																(e-apply
																	(e-ident (raw "Str.concat"))
																	(e-apply
																		(e-ident (raw "Str.concat"))
																		(e-ident (raw "acc"))
																		(e-ident (raw "sep_str")))
																	(e-ident (raw "item_str")))))))))))
								(e-apply
									(e-tag (raw "Html.Raw"))
									(e-ident (raw "joined")))))))
				(s-type-anno (name "to_str")
					(ty-fn
						(ty (name "Html"))
						(ty (name "Str"))))
				(s-decl
					(p-ident (raw "to_str"))
					(e-lambda
						(args
							(p-ident (raw "h")))
						(e-match
							(e-ident (raw "h"))
							(branches
								(branch
									(p-tag (raw ".Raw")
										(p-ident (raw "s")))
									(e-ident (raw "s")))))))))
		(s-type-anno (name "items")
			(ty-apply
				(ty (name "List"))
				(ty (name "Html"))))
		(s-decl
			(p-ident (raw "items"))
			(e-list
				(e-apply
					(e-tag (raw "Html.Raw"))
					(e-string
						(e-string-part (raw "div"))))
				(e-apply
					(e-tag (raw "Html.Raw"))
					(e-string
						(e-string-part (raw "span"))))
				(e-apply
					(e-tag (raw "Html.Raw"))
					(e-string
						(e-string-part (raw "p"))))))
		(s-type-anno (name "sep")
			(ty (name "Html")))
		(s-decl
			(p-ident (raw "sep"))
			(e-apply
				(e-tag (raw "Html.Raw"))
				(e-string
					(e-string-part (raw " | ")))))
		(s-type-anno (name "result")
			(ty (name "Html")))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "List.join_with"))
				(e-ident (raw "items"))
				(e-ident (raw "sep"))))
		(s-type-anno (name "output")
			(ty (name "Str")))
		(s-decl
			(p-ident (raw "output"))
			(e-field-access
				(e-ident (raw "result"))
				(e-apply
					(e-ident (raw "to_str")))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "output"))
				(e-string
					(e-string-part (raw "div | span | p")))))))
~~~
# FORMATTED
~~~roc
# Define a custom nominal type with a join_with method
Html := [Raw(Str)].{
	join_with : List(Html), Html -> Html
	join_with = |list_items, separator| {
		# Extract separator string
		sep_str = match separator {
			Html.Raw(s) => s
		}
		# Fold over items, joining them
		joined = List.fold(
			list_items,
			"",
			|acc, item| {
				item_str = match item {
					Html.Raw(s) => s
				}
				if acc == "" {
					item_str
				} else {
					Str.concat(Str.concat(acc, sep_str), item_str)
				}
			},
		)
		Html.Raw(joined)
	}

	to_str : Html -> Str
	to_str = |h| match h {
		Html.Raw(s) => s
	}
}

# Test using List.join_with with the custom Html type
items : List(Html)
items = [Html.Raw("div"), Html.Raw("span"), Html.Raw("p")]

sep : Html
sep = Html.Raw(" | ")

result : Html
result = List.join_with(items, sep)

output : Str
output = result.to_str()

expect output == "div | span | p"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "list_join_with_custom.Html.join_with"))
		(e-lambda
			(args
				(p-assign (ident "list_items"))
				(p-assign (ident "separator")))
			(e-block
				(s-let
					(p-assign (ident "sep_str"))
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "separator"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal
												(p-applied-tag))))
									(value
										(e-lookup-local
											(p-assign (ident "s")))))))))
				(s-let
					(p-assign (ident "joined"))
					(e-call
						(e-lookup-external
							(builtin))
						(e-lookup-local
							(p-assign (ident "list_items")))
						(e-string
							(e-literal (string "")))
						(e-closure
							(captures
								(capture (ident "sep_str")))
							(e-lambda
								(args
									(p-assign (ident "acc"))
									(p-assign (ident "item")))
								(e-block
									(s-let
										(p-assign (ident "item_str"))
										(e-match
											(match
												(cond
													(e-lookup-local
														(p-assign (ident "item"))))
												(branches
													(branch
														(patterns
															(pattern (degenerate false)
																(p-nominal
																	(p-applied-tag))))
														(value
															(e-lookup-local
																(p-assign (ident "s")))))))))
									(e-if
										(if-branches
											(if-branch
												(e-binop (op "eq")
													(e-lookup-local
														(p-assign (ident "acc")))
													(e-string
														(e-literal (string ""))))
												(e-block
													(e-lookup-local
														(p-assign (ident "item_str"))))))
										(if-else
											(e-block
												(e-call
													(e-lookup-external
														(builtin))
													(e-call
														(e-lookup-external
															(builtin))
														(e-lookup-local
															(p-assign (ident "acc")))
														(e-lookup-local
															(p-assign (ident "sep_str"))))
													(e-lookup-local
														(p-assign (ident "item_str"))))))))))))
				(e-nominal (nominal "Html")
					(e-tag (name "Raw")
						(args
							(e-lookup-local
								(p-assign (ident "joined"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Html") (local)))
				(ty-lookup (name "Html") (local))
				(ty-lookup (name "Html") (local)))))
	(d-let
		(p-assign (ident "list_join_with_custom.Html.to_str"))
		(e-lambda
			(args
				(p-assign (ident "h")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "h"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-applied-tag))))
							(value
								(e-lookup-local
									(p-assign (ident "s")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Html") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "items"))
		(e-list
			(elems
				(e-nominal (nominal "Html")
					(e-tag (name "Raw")
						(args
							(e-string
								(e-literal (string "div"))))))
				(e-nominal (nominal "Html")
					(e-tag (name "Raw")
						(args
							(e-string
								(e-literal (string "span"))))))
				(e-nominal (nominal "Html")
					(e-tag (name "Raw")
						(args
							(e-string
								(e-literal (string "p"))))))))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "Html") (local)))))
	(d-let
		(p-assign (ident "sep"))
		(e-nominal (nominal "Html")
			(e-tag (name "Raw")
				(args
					(e-string
						(e-literal (string " | "))))))
		(annotation
			(ty-lookup (name "Html") (local))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-external
				(builtin))
			(e-lookup-local
				(p-assign (ident "items")))
			(e-lookup-local
				(p-assign (ident "sep"))))
		(annotation
			(ty-lookup (name "Html") (local))))
	(d-let
		(p-assign (ident "output"))
		(e-dot-access (field "to_str")
			(receiver
				(e-lookup-local
					(p-assign (ident "result"))))
			(args))
		(annotation
			(ty-lookup (name "Str") (builtin))))
	(s-nominal-decl
		(ty-header (name "Html"))
		(ty-tag-union
			(ty-tag-name (name "Raw")
				(ty-lookup (name "Str") (builtin)))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "output")))
			(e-string
				(e-literal (string "div | span | p"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Html), Html -> Html"))
		(patt (type "Html -> Str"))
		(patt (type "List(Html)"))
		(patt (type "Html"))
		(patt (type "Html"))
		(patt (type "Str")))
	(type_decls
		(nominal (type "Html")
			(ty-header (name "Html"))))
	(expressions
		(expr (type "List(Html), Html -> Html"))
		(expr (type "Html -> Str"))
		(expr (type "List(Html)"))
		(expr (type "Html"))
		(expr (type "Html"))
		(expr (type "Str"))))
~~~
