app "generator"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br",
    }
    imports [
        pf.Stdout,
        pf.File,
        pf.Path,
        pf.Task.{ Task },
    ]

    provides [main] to pf

RenderTree : [
    Text Str,
    Items (List RenderTree),
    Line (List RenderTree),
    Indent (List RenderTree),
    Import {modu: List Str, name: Str},
]

renderFile : RenderTree -> Str
renderFile = \tree ->
    render (Items [
        formatImports (findImports tree),
        tree
    ])

findImports : RenderTree -> Set { modu: (List Str), name: Str }
findImports = \tree ->
    when tree is
        Text _ -> Set.empty
        Items list | Indent list | Line list ->
            List.walk list Set.empty \acc, item -> Set.union acc (findImports item)
        Import import -> Set.single import

formatImports : Set { modu: (List Str), name: Str } -> RenderTree
formatImports = \set ->
    if hasDupImports set then
        crash "Duplicate imports!"
    else
        Items (
            set
                |> Set.toList
                # TODO: Sort by module name
                |> List.map \{ modu, name } ->
                    Line [
                        Text "use ",
                        Text (Str.joinWith (List.map modu \m -> Str.concat m "::") ""),
                        Text name,
                        Text ";",
                    ]
        )

hasDupImports : Set { modu: (List Str), name: Str } -> Bool
hasDupImports = \set ->
    nameSet =
        set
            |> Set.toList
            |> List.map \{ modu: _, name } -> name
            |> Set.fromList
    
    Set.len nameSet != Set.len nameSet

render : RenderTree -> Str
render = \tree ->
    Tuple text _ = renderInner tree 0 Bool.true
    text

renderGroup : List RenderTree, Nat, Bool -> [Tuple Str Bool]
renderGroup = \list, indent, newlineBefore ->
    List.walk list (Tuple "" newlineBefore) \(Tuple text nlb), item ->
        Tuple ntext nla = renderInner item indent nlb
        (Tuple
            (Str.concat text ntext)
            nla
        )


renderInner : RenderTree, Nat, Bool -> [Tuple Str Bool]
renderInner = \tree, indent, newlineBefore ->
    when tree is
        Text text ->
            result = if newlineBefore then
                    Str.concat (Str.repeat " " (4*indent)) text
                else
                    text
            Tuple result Bool.false
        Items list -> renderGroup list indent newlineBefore
        Line list ->
            Tuple ntext nla = renderGroup list indent Bool.true
            res = if newlineBefore then
                    # Already added the newline, no need!
                    ntext
                else
                    Str.concat "\n" ntext
            res2 = if nla then
                    res
                else
                    Str.concat res "\n"
            (Tuple res2 Bool.true)
        Indent list -> renderGroup list (indent + 1) newlineBefore
        Import {modu: _, name} ->
            Tuple name Bool.false

parserTrait = \t, e ->
    genCall (Import {modu: ["crate", "parser"], name: "Parser"}) [Text "'a", t, e]

parseFunction : Str, RenderTree, RenderTree, RenderTree -> RenderTree
parseFunction = \name, ty, err, body ->
    Items [
        Line [
            Text "pub fn \(name)<'a>() -> impl ",
            parserTrait ty err,
            Text " {",
        ],
        Indent [body, Line [Text ".trace(\"\(name)\")" ] ],
        Line [Text "}"],
        Line [Text ""],
    ]

Type : RenderTree
ErrTy : RenderTree

Parser : [
    Loc Parser,
    Specialize ErrTy Parser,
    Record Str (List { name: Str, parser: Parser }),
    Builtin RenderTree Type,
    # Named ParserName (World -> Parser),
]

errHeader : Str -> ErrTy
errHeader = \name ->
    Items [
        Import { modu: ["crate", "parser"], name: "EHeader" },
        Text "::",
        Text name,
    ]

fnCall : RenderTree, List RenderTree -> RenderTree
fnCall = \fnName, args ->
    Items [
        fnName,
        Text "(",
        Items (List.intersperse args (Text ",")),
        Text ")",
    ]

fn : RenderTree -> (List RenderTree -> RenderTree)
fn = \fnName -> \args -> fnCall fnName args

genCall : RenderTree, List RenderTree -> RenderTree
genCall = \genName, args ->
    Items [
        genName,
        Text "<",
        Items (List.intersperse args (Text ", ")),
        Text ">",
    ]

gen : RenderTree -> (List RenderTree -> RenderTree)
gen = \genName -> \args -> genCall genName args

ref : RenderTree -> RenderTree
ref = \name -> Items [Text "&'a ", name]

slice : RenderTree -> RenderTree
slice = \name -> Items [Text "[", name, Text "]"]

refSlice : RenderTree -> RenderTree
refSlice = \name -> ref (slice name)

commentOrNewline = genCall (Import {modu: ["crate", "ast"], name: "CommentOrNewline"}) [ Text "'a" ]

exposedName = genCall (Import {modu: ["crate", "header"], name: "ExposedName"}) [ Text "'a" ]
importsEntry = genCall (Import {modu: ["crate", "header"], name: "ImportsEntry"}) [ Text "'a" ]
uppercaseIdent = genCall (Import {modu: ["crate", "ident"], name: "UppercaseIdent"}) [ Text "'a" ]

moduleName = genCall (Import {modu: ["crate", "header"], name: "ModuleName"}) [ Text "'a" ]

space0E = fn (Import { modu: ["crate", "blankspace"], name: "space0_e" })

keyword = \keywordName ->
    Import { modu: ["crate", "header"], name: keywordName }

spaces = \errorName ->
    Builtin (space0E [errorName]) (refSlice commentOrNewline)

loc = gen (Import {modu: ["roc_region", "all"], name: "Loc"})

keywordItem = \kw, ty ->
    genCall (Import {modu: ["crate", "header"], name: "KeywordItem"}) [ Text "'a", kw, ty ]

collection = \ty ->
    genCall (Import {modu: ["crate", "ast"], name: "Collection"}) [ Text "'a", ty ]

spaced = \ty ->
    genCall (Import {modu: ["crate", "ast"], name: "Spaced"}) [ Text "'a", ty ]

moduleNameHelp = \err ->
    Builtin (fnCall (Import {modu: ["crate", "module"], name: "module_name_help" }) [ err ]) moduleName

exposesValues =
    Builtin (fnCall (Import {modu: ["crate", "module"], name: "exposes_values"}) []) (keywordItem (keyword "ExposesKeyword") (collection (loc [spaced exposedName])))

imports =
    Builtin (fnCall (Import {modu: ["crate", "module"], name: "imports"}) []) (keywordItem (keyword "ImportsKeyword") (collection (loc [spaced importsEntry])))

generates =
    Builtin (fnCall (Import {modu: ["crate", "module"], name: "generates"}) []) (keywordItem (keyword "GeneratesKeyword") uppercaseIdent)

generatesWith =
    Builtin (fnCall (Import {modu: ["crate", "module"], name: "generates_with"}) []) (keywordItem (keyword "WithKeyword") (collection (loc [spaced exposedName])))


interfaceHeader = Record "InterfaceHeader" [
    {name: "before_name", parser: spaces (errHeader "IndentStart")},
    {name: "name", parser: Loc (moduleNameHelp (errHeader "ModuleName"))},
    {name: "exposes", parser: Specialize (errHeader "Exposes") exposesValues },
    {name: "imports", parser: Specialize (errHeader "Imports") imports },
]

hostedHeader = Record "HostedHeader" [
    {name: "before_name", parser: spaces (errHeader "IndentStart")},
    {name: "name", parser: Loc (moduleNameHelp (errHeader "ModuleName"))},
    {name: "exposes", parser: Specialize (errHeader "Exposes") exposesValues},
    {name: "imports", parser: Specialize (errHeader "Imports") imports},
    {name: "generates", parser: Specialize (errHeader "Generates") generates},
    {name: "generates_with", parser: Specialize (errHeader "GeneratesWith") generatesWith},
]

printCombinatorParserFunction = \parser ->
    parseFunction (lowerName (resolveName parser)) (resolveType parser) (Text "EHeader<'a>") (printCombinatorParser parser)

resolveName : Parser -> Str
resolveName = \parser ->
    when parser is
        Loc _p -> crash "Unnamed parser!"
        Specialize _err _p -> crash "Unnamed parser!"
        Builtin _name _ty -> crash "Unnamed parser!"
        Record name _fields -> name

underscoreScalar = 95
aLowerScalar = 97
aUpperScalar = 65
zUpperScalar = 90

# map from a lower_case_name to a UpperCaseName
upperName : Str -> Str
upperName = \name ->
    result = Str.walkScalars name {text: "", needUpper: Bool.true} \{text, needUpper}, c ->
        if c == underscoreScalar then
            {text, needUpper: Bool.true}
        else
            newText =
                if needUpper then
                    Str.appendScalar text (c - aLowerScalar + aUpperScalar) |> orCrash
                else
                    Str.appendScalar text c |> orCrash
            {text: newText, needUpper: Bool.false}
    result.text

expect (upperName "hello_world") == "HelloWorld"

orCrash : Result a e -> a
orCrash = \result ->
    when result is
        Ok a -> a
        Err _e -> crash "orCrash"

lowerName : Str -> Str
lowerName = \name ->
    result = Str.walkScalars name {text: "", needUnder: Bool.false} \{text, needUnder}, c ->
        newText =
            if c >= aUpperScalar && c <= zUpperScalar then
                if needUnder then
                    text
                        |> Str.appendScalar underscoreScalar
                        |> orCrash
                        |> Str.appendScalar (c - aUpperScalar + aLowerScalar)
                        |> orCrash
                else
                    text
                        |> Str.appendScalar (c - aUpperScalar + aLowerScalar)
                        |> orCrash
            else
                Str.appendScalar text c |> orCrash
        
        {text: newText, needUnder: Bool.true}
    
    result.text

expect
    theResult = (lowerName "HelloWorld")
    theResult == "hello_world"

resolveType : Parser -> RenderTree
resolveType = \parser ->
    when parser is
        Loc p -> loc [resolveType p]
        Specialize _err p -> resolveType p
        Record name _fields -> Items [ Import {modu: ["crate", "generated_ast"], name}, Text "<'a>"]
        Builtin _name ty -> ty

printCombinatorParser : Parser -> RenderTree
printCombinatorParser = \parser ->
    when parser is
        Loc p ->
            printed = printCombinatorParser p
            value : RenderTree
            value = Items [ (Text "loc!("), printed, (Text ")") ]
            value
        Specialize err p ->
            printed = printCombinatorParser p
            Items [
                Import {modu: ["crate", "parser"], name: "specialize"}, 
                Text "(",
                err,
                (Text ", "),
                printed,
                (Text ")"),
            ]
        Record name fields ->
            Items [
                Text "record!(\(name) {",
                (Indent
                    (fields
                        |> List.map \f ->
                            Line [Text "\(f.name): ", printCombinatorParser f.parser, Text ","]
                    )
                ),
                Text "})"
            ]
        Builtin name _ty -> name

printAst : Parser -> RenderTree
printAst = \parser ->
    when parser is
        Record name fields ->
            Items [
                Line [ Text "#[derive(Clone, Debug, PartialEq)]" ],
                Line [ Text "pub struct \(name)<'a> {" ],
                (Indent (
                    fields
                        |> List.map \f ->
                            Line [Text "pub \(f.name): ", resolveType f.parser, Text ","]
                )),
                Line [Text "}"],
                Line [Text ""],
            ]
        _ -> crash "Not implemented"

expect (render (Text "foo")) == "foo"
expect (render (Line [Text "foo"])) == "foo\n"
expect (render (Indent [Text "foo"])) == "    foo"
expect (render (Line [Indent [Text "foo"]])) == "    foo\n"

expect
    res = (render (Items [Text "{", Indent [Line [Text "foo"]], Text "}"]))
    res ==
        """
        {
            foo
        }
        """

allSyntaxItems = [interfaceHeader, hostedHeader]

printedAstItems = Items (allSyntaxItems |> List.map printAst)
printedParserItems = Items (allSyntaxItems |> List.map printCombinatorParserFunction)


# main : Task {} []*
main =
    task =
        _ <- File.writeUtf8 (Path.fromStr "generated_ast.rs") (renderFile printedAstItems) |> Task.await
        
        File.writeUtf8 (Path.fromStr "generated_parser.rs") (renderFile printedParserItems)

    Task.attempt task \result ->
        when result is
            Ok _ -> Stdout.line "Success!"
            Err _e -> Stdout.line "Failed to write file"
