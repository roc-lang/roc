module [
    appendRenderedStatic,
    initServerApp,
]

import Html.Internal.Shared exposing [Html, Attribute, App, translateStatic, text, element]
import Json

# -------------------------------
#   STATIC HTML
# -------------------------------
appendRenderedStatic : Str, Html [] -> Str
appendRenderedStatic = \buffer, node ->
    when node is
        Text content ->
            Str.concat buffer content

        Element name _ attrs children ->
            withTagName = "$(buffer)<$(name)"
            withAttrs =
                if List.isEmpty attrs then
                    withTagName
                else
                    init = { buffer: Str.concat withTagName " ", styles: "" }
                    { buffer: attrBuffer, styles } =
                        List.walk attrs init appendRenderedStaticAttr

                    if Str.isEmpty styles then
                        attrBuffer
                    else
                        "$(attrBuffer) style=\"$(styles)\""

            withTag = Str.concat withAttrs ">"
            withChildren = List.walk children withTag appendRenderedStatic

            "$(withChildren)</$(name)>"

        None -> buffer

appendRenderedStaticAttr : { buffer : Str, styles : Str }, Attribute [] -> { buffer : Str, styles : Str }
appendRenderedStaticAttr = \{ buffer, styles }, attr ->
    when attr is
        HtmlAttr key value ->
            newBuffer = "$(buffer) $(key)=\"$(value)\""

            { buffer: newBuffer, styles }

        Style key value ->
            newStyles = "$(styles) $(key): $(value);"

            { buffer, styles: newStyles }

        DomProp _ _ -> { buffer, styles }

# -------------------------------
#   INITIALISATION
# -------------------------------
initServerApp : App state initData, initData, Str -> Result (Html []) [InvalidDocument] where initData implements Encoding
initServerApp = \app, initData, hostJavaScript ->
    initData
    |> Ok
    |> app.init
    |> app.render
    |> translateStatic
    |> insertRocScript initData app.wasmUrl hostJavaScript

insertRocScript : Html [], initData, Str, Str -> Result (Html []) [InvalidDocument] where initData implements Encoding
insertRocScript = \document, initData, wasmUrl, hostJavaScript ->
    encode =
        \value ->
            value
            |> Encode.toBytes Json.json
            |> Str.fromUtf8
            |> Result.withDefault ""

    # Convert initData to JSON as a Roc Str, then convert the Roc Str to a JS string.
    # JSON won't have invalid UTF-8 in it, since it would be escaped as part of JSON encoding.
    jsInitData =
        initData |> encode |> encode

    jsWasmUrl =
        encode wasmUrl

    script : Html []
    script = (element "script") [] [
        text
            """
            $(hostJavaScript)
            (function(){
            const initData = $(jsInitData);
            const wasmUrl = $(jsWasmUrl);
            window.roc = roc_init(initData, wasmUrl);
            })();
            """,
    ]

    # append the <script> to the end of the <body>
    when document is
        Element "html" hSize hAttrs hChildren ->
            empty = List.withCapacity (List.len hChildren)
            walkResult =
                List.walk hChildren { newHtmlChildren: empty, foundBody: Bool.false } \{ newHtmlChildren, foundBody }, hChild ->
                    when hChild is
                        Element "body" bSize bAttrs bChildren ->
                            {
                                newHtmlChildren: List.append newHtmlChildren (Element "body" bSize bAttrs (List.append bChildren script)),
                                foundBody: Bool.true,
                            }

                        _ ->
                            {
                                newHtmlChildren: List.append newHtmlChildren hChild,
                                foundBody,
                            }

            if walkResult.foundBody then
                Ok (Element "html" hSize hAttrs walkResult.newHtmlChildren)
            else
                Err InvalidDocument

        _ -> Err InvalidDocument
