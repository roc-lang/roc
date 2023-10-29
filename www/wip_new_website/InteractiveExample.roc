interface InteractiveExample
    exposes [view]
    imports [pf.Html.{ pre, samp }, pf.Html.Attributes.{ class }]

Section : [Desc (List Token) Str, Indent, Outdent, Newline]
Token : [Kw Str, Ident Str, Str Str, Num Str, Comment Str, Literal Str, ParensAround (List Token), Lambda (List Str)]

view : Html.Node
view =
    output =
        # # Select anything here to see an explanation.
        # main =
        #     Path.fromStr "url.txt"
        #     |> storeEmail
        #     |> Task.onErr handleErr
        #
        # storeEmail = \filename ->
        #     url <- File.readUtf8 filename |> Task.await
        #     { name, email } <- Http.get url Json.codec |> Task.await
        #
        #     File.writeUtf8 (Path.fromStr "\(name).txt") email
        #
        # handleUrl = \err ->
        #     when err is
        #         HttpErr url _ -> Stderr.line "Error fetching URL \(url)"
        #         FileReadErr path _ -> Stderr.line "Error reading \(Path.display path)"
        #         FileWriteErr path _ -> Stderr.line "Error writing \(Path.display path)"
        sectionsToStr [
            Desc [Comment "<span class='desktop'>Click anything here to see an explanation.</span><span class='mobile'>Tap anything here to\n# see an explanation.</span>"] "<p><a href=\"/tutorial#comments\">Comments</a> in Roc begin with a <code>#</code> and go to the end of the line.</p>",
            Newline,
            Desc [Ident "main", Kw "="] "<p>This defines <code class=\"ident\">main</code>, which is where our program will begin.</p><p>In Roc, <a href=\"/tutorial#https://www.roc-lang.org/tutorial#naming-things\">all definitions are constant</a>, so writing <code class=\"ident\">main =</code> again in the same scope would give an error.</p>",
            Indent,
            Desc [Ident "Path.fromStr", Str "\"url.txt\""] "<p>This converts the string <code>\"url.txt\"</code> into a <code>Path</code> by passing it to <code>Path.fromStr</code>.</p><p>Function arguments are separated with whitespace. Parentheses are only needed in <a href=\"/tutorial#calling-functions\">nested function calls</a>.</p>",
            Newline,
            Desc [Kw "|>", Ident "storeEmail"] "<p>The <a href=\"/tutorial#the-pipe-operator\">pipe operator</a> (<code>|></code>) is syntax sugar for passing the previous value to the next function in the “pipeline.”</p><p>Here, we're taking the value returned by <code>Path.fromStr \"url.txt\"</code> and passing it to <code>storeEmail</code>.</p><p>The next <code>|></code> continues the pipeline.</p>",
            Newline,
            Desc [Kw "|>", Ident "Task.onErr", Ident "handleErr"] "<p>If the task returned by the previous step in the pipeline fails, pass its error to <code>handleErr</code>.</p><p>The pipeline essentially does this:</p><pre><code>val1 = Path.fromStr \"url.txt\"\nval2 = storeEmail val1\n\nTask.onErr val2 handleErr</code></pre><p>It creates a <code>Path</code> from a string, stores an email based on that path, and then does error handling.</p>",
            Outdent,
            Newline,
            Desc [Ident "storeEmail", Kw "=", Lambda ["filename"]] "<p>This <a href=\"/tutorial#defining-functions\">defines a function</a> named <code>storeEmail</code>.</p><p>In Roc, functions are ordinary values, so we assign names to them using <code>=</code> like with any other value.</p><p>The <code>\\arg1, arg2 -&gt;</code> syntax begins a function, and the part after <code>-&gt;</code> is the function's body.</p>",
            Indent,
            Desc [Ident "url", Kw "<-", Ident "File.readUtf8", Ident "filename", Kw "|>", Ident "Task.await"] "<p>This reads the contents of the file (as a <a href=\"https://en.wikipedia.org/wiki/UTF-8\">UTF-8</a> string) into <code>url</code>.</p><p>The <code>&lt;-</code> does <a href=\"/tutorial#backpassing\">backpassing</a>, which is syntax sugar for defining a function. This whole line desugars to:</p><pre><code>Task.await\n    (File.readUtf8 filename)\n    \\url -&gt;</code></pre><p>The lines after this one form the body of the <code>\\url -&gt;</code> <a href=\"https://en.wikipedia.org/wiki/Callback_(computer_programming)\">callback</a>, which runs if the file read succeeds.</p>",
            Newline,
            Desc [Literal "{", Ident "name", Literal ",", Ident "email", Literal "}"] "<p>This is <a href=\"/tutorial#record-destructuring\">record destructuring</a> syntax.</p><p>It takes ",
            Desc [Kw "<-", Ident "Http.get", Ident "url", Ident "Json.codec"] "<p>TODO Json.codec, type inference, early error</p>",
            Desc [Kw "|>", Ident "Task.await"] "<p>TODO Task.await</p>",
            Newline,
            Desc [Ident "dest", Kw "=", Str "\"\\(name).txt\""] "<p>The <code>\\(name)</code> in this string literal will be replaced with the value in <code>name</code>. This is <a href=\"/tutorial#string-interpolation\">string interpolation</a>.</p><p>Note that this line doesn't end with <code>|> Task.await</code>. Earlier lines needed that because they were I/O <a href=\"/tutorial#tasks\">tasks</a>, but this is a plain old <a href=\"/tutorial#defs\">definition</a>, so there's no task to await.</p>",
            Newline,
            Desc [Literal "_"] "<p>In Roc, if you don’t want to bother naming something, you can always choose the name <code>_</code>.</p><p>You can name as many things as you like <code>_</code>, even in the same scope, but you can’t reference anything named <code>_</code>.</p><p>So it’s only useful for when you don’t want to choose a name.</p>",
            Desc [Kw "<-", Ident "File.writeUtf8", Ident "(Path.fromStr dest)", Ident "email", Kw "|>", Ident "Task.await"] "<p>This writes the <code>email</code> string to the file encoded as <a href=\"https://en.wikipedia.org/wiki/UTF-8\">UTF-8</a>.</p><p>The parentheses here show where the nested call to <code>Path.fromStr</code> begins and ends.</p>",
            Newline,
            Desc [Ident "Stdout.line", Str "\"Wrote email to \\(dest)\""] "<p>This prints what we did to <a href=\"https://en.wikipedia.org/wiki/Standard_streams#Standard_output_(stdout)\">stdout</a>.</p><p>Note that this line doesn't end with <code>|> Task.await</code>. That’s because, although <code>Stdout.line</code> returns a <a href=\"/tutorial#tasks\">task</a>, we don’t need to await it because nothing happens after it.</p>",
            Outdent,
            Newline,
            Desc [Ident "handleErr", Kw "=", Lambda ["err"]] "<p>Like <code>storeEmail</code>, <code>handleErr</code> is also a function.</p><p>Although type annotations are optional everywhere in Roc—because the language has 100% type inference—you could add type annotations to <code>main</code>, <code>storeEmail</code>, and <code>handleErr</code> if you wanted to.</p>",
            Indent,
            Desc [Kw "when", Ident "err", Kw "is"] "<p>TODO when</p>",
            Indent,
            Desc [Literal "HttpErr", Ident "path", Kw "_", Kw "->"] "<p>TODO</p>",
            Desc [Ident "Stderr.line", Str "\"Error fetching URL \\(url)\""] "<p>TODO</p>",
            Newline,
            Desc [Literal "FileReadErr", Ident "path", Kw "_", Kw "->"] "<p>TODO</p>",
            Desc [Ident "Stderr.line", Str "\"Error reading from \\(Path.display path)\""] "<p>TODO</p>",
            Newline,
            Desc [Literal "FileWriteErr", Ident "path", Kw "_", Kw "->"] "<p>TODO</p>",
            Desc [Ident "Stderr.line", Str "\"Error writing to \\(Path.display path)\""] "<p>TODO</p>",
        ]

    pre [] [
        samp [class "interactive-example"] [
            Html.text output,
        ],
    ]

tokensToStr : List Token -> Str
tokensToStr = \tokens ->
    List.walk tokens "" \buf, token ->
        bufWithSpace =
            if Str.isEmpty buf || token == Literal "," then
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

            Lambda args ->
                # Don't put spaces after opening parens or before closing parens
                argsWithCommas =
                    args
                    |> List.map \ident -> "<span class=\"ident\">\(ident)</span>"
                    |> Str.joinWith "<span class=\"literal\">,</span> "

                bufWithSpace
                |> Str.concat "<span class=\"kw\">\\</span>"
                |> Str.concat argsWithCommas
                |> Str.concat "<span class=\"kw\"> -></span>"

            Kw str ->
                Str.concat bufWithSpace "<span class=\"kw\">\(str)</span>"

            Num str | Str str | Literal str -> # We may render these differently in the future
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
