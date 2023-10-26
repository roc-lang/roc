interface InteractiveExample
    exposes [view]
    imports [pf.Html.{ pre, samp }, pf.Html.Attributes.{ class }]

Section : [Desc (List Token) Str, Indent, Outdent, Newline]
Token : [Kw Str, Ident Str, Str Str, Num Str, Comment Str, ParensAround (List Token)]

view : Html.Node
view =
    output =
        # # Select anything here to see an explanation.
        # main =
        #     cacheUserInfo (Path.fromStr "url.txt")
        #     |> Task.onErr handleErr
        #
        # cacheUserInfo = \filename ->
        #     url <- File.readUtf8 filename |> Task.await
        #     { username, email } <- Http.get url Json.codec |> Task.await
        #
        #     File.writeUtf8 (Path.fromStr "\(username).txt") email
        #
        # handleUrl = \err ->
        #     when err is
        #         HttpErr url _ -> Stderr.line "Error fetching URL \(url)"
        #         FileReadErr path _ -> Stderr.line "Error reading \(Path.display path)"
        #         FileWriteErr path _ -> Stderr.line "Error writing \(Path.display path)"
        sectionsToStr [
            Desc [Comment "<span class='desktop'>Click anything here to see an explanation.</span><span class='mobile'>Tap anything here to\n# see an explanation.</span>"] "<p>Comments in Roc begin with a <code>#</code> and go to the end of the line.</p>",
            Newline,
            Desc [Ident "main", Kw "="] "<p>This begins the definition of <code class=\"ident\">main</code>, which is the code our program will run when it starts up.</p><p>In Roc, assignments are always constant, which means writing <code class=\"ident\">main =</code> again in the same scope would give an error.</p><p><a href=\"https://www.roc-lang.org/tutorial#naming-things\">Learn more about naming things</a></p>",
            Indent,
            Desc [Ident "cacheUserInfo", Str "\"url.txt\""] "<p>This calls the <code class=\"ident\">cacheUserInfo</code> function, passing the string <code class=\"str\">\"url.txt\"</code> as an argument.</p><p>In Roc, function arguments are separated with spaces and/or newlines. Parentheses are only used in nested function calls.</p>",
            Newline,
            Desc [Kw "|>", Ident "Task.onErr", Ident "handleErr"] "<p>TODO</p>",
            Outdent,
            Newline,
            # Desc [Ident "cacheUserInfo", Kw "=", Lambda [Ident "filename"]] "<p>TODO</p>",
            # Indent,
            # Desc [Ident "url", Kw "<-", Ident "File.readUtf8", Ident "filename"] "<p>TODO backpassing</p>",
            # Desc [Kw "|>", Ident "Task.await"] "<p>TODO Task.await</p>",
            # Newline,
            # Desc [Literal "{", Ident "username", Literal ",", Ident "email", Literal "}", Kw "<-"] "<p>TODO record destructuring and backpassing</p>",
        ]

    pre [] [
        samp [class "interactive-example"] [
            Html.text output,
        ],
    ]

tokensToStr : List Token -> Str
tokensToStr = \tokens ->
    List.walk tokens "" tokenToStr

tokenToStr : Str, Token -> Str
tokenToStr = \buf, token ->
    bufWithSpace =
        if Str.isEmpty buf then
            buf
        else
            Str.concat buf " "

    when token is
        ParensAround wrapped ->
            # Don't put spaces after opening parens or before closing parens
            bufWithSpace
            |> Str.concat "<span class=\"kw\">(</span>"
            |> Str.concat (tokensToStr wrapped)
            |> Str.concat "<span class=\"kw\">)</span>"

        Kw str ->
            Str.concat bufWithSpace "<span class=\"kw\">\(str)</span>"

        Num str ->
            Str.concat bufWithSpace "<span class=\"literal\">\(str)</span>"

        Str str ->
            Str.concat bufWithSpace "<span class=\"literal\">\(str)</span>"

        Comment str ->
            Str.concat bufWithSpace "<span class=\"comment\"># \(str)</span>"

        Ident str ->
            html =
                List.walk (Str.split str ".") "" \accum, ident ->
                    identHtml = "<span class=\"ident\">\(ident)</span>"

                    if Str.isEmpty accum then
                        identHtml
                    else
                        "\(accum)<span class=\"kw\">.</span>\(identHtml)"

            Str.concat bufWithSpace html

sectionsToStr : List Section -> Str
sectionsToStr = \sections ->
    answer = List.walk sections { buf: "", count: 0, indent: 0 } \{ buf, count, indent }, section ->
        bufWithSpace =
            if Str.isEmpty buf then
                buf
            else if buf |> Str.endsWith "\n" then
                Str.concat buf (Str.repeat " " indent)
            else
                Str.concat buf " "

        (afterSpaces, nextCount) =
            when section is
                Newline | Indent | Outdent ->
                    # Indent and outdent changes happen on the next iteration,
                    # so we only need a newline for them here.
                    (Str.concat buf "\n", count)

                Desc tokens str ->
                    html = radio count (tokensToStr tokens) str

                    (Str.concat bufWithSpace html, count + 1)

        nextIndent =
            when section is
                Indent -> indent + 4
                Outdent -> indent - 4
                Newline | Desc _ _ -> indent

        {
            buf: afterSpaces,
            count: nextCount,
            indent: nextIndent,
        }

    answer.buf

radio : U16, Str, Str -> Str
radio = \index, labelHtml, descHtml ->
    # The first radio button should always be checked, and none of the others should be.
    checkedHtml = if index == 0 then " checked" else ""

    """
    <input class="interactive-radio" type="radio" name="r" id="r\(Num.toStr index)" autocomplete=\"off\"\(checkedHtml)><label for="r\(Num.toStr index)" title="Tap to learn about this syntax">\(labelHtml)</label><div class="interactive-desc">\(descHtml)</div>
    """
