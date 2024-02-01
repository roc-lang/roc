interface InteractiveExample
    exposes [view]
    imports [pf.Html.{ pre, samp, div, text, a, class, p }, pf.HtmlAttributes.{ class, role, href, id }]

Section : [Desc (List Token) Str, Indent, Outdent, Newline]
Token : [
    Kw Str,
    Ident Str,
    Str Str,
    Num Str,
    Comment Str,
    Literal Str,
    ParensAround (List Token),
    Lambda (List Str),
    StrInterpolation Str Str Str,
]

view : Html.Node
view =
    output =
        sectionsToStr [
            Desc [Comment "<span class='desktop'>Click anything here to see an explanation.</span><span class='mobile'>Tap anything here to\n# see an explanation.</span>"] "<p><a href=\"/tutorial#comments\">Comments</a> in Roc begin with a <code>#</code> and go to the end of the line.</p>",
            Newline,
            Desc [Ident "main", Kw "="] "<p>This defines <code class=\"ident\">main</code>, which is where our program will begin.</p><p>In Roc, <a href=\"/tutorial#https://www.roc-lang.org/tutorial#naming-things\">all definitions are constant</a>, so writing <code class=\"ident\">main =</code> again in the same scope would give an error.</p>",
            Indent,
            Desc [Ident "Path.fromStr", Str "\"url.txt\""] "<p>This converts the string <code>\"url.txt\"</code> into a <code>Path</code> by passing it to <code>Path.fromStr</code>.</p><p>Function arguments are separated with whitespace. Parentheses are only needed in <a href=\"/tutorial#calling-functions\">nested function calls</a>.</p>",
            Newline,
            Desc [Kw "|>", Ident "storeEmail"] "<p>The <a href=\"/tutorial#the-pipe-operator\">pipe operator</a> (<code>|></code>) is syntax sugar for passing the previous value to the next function in the \"pipeline.\"</p><p>This line takes the value that <code>Path.fromStr \"url.txt\"</code> returns and passes it to <code>storeEmail</code>.</p><p>The next <code>|></code> continues the pipeline.</p>",
            Newline,
            Desc [Kw "|>", Ident "Task.onErr", Ident "handleErr"] "<p>If the task returned by the previous step in the pipeline fails, pass its error to <code>handleErr</code>.</p><p>The pipeline essentially does this:</p><pre><code>a = Path.fromStr \"url.txt\"\nb = storeEmail a\n\nTask.onErr b handleErr</code></pre><p>It creates a <code>Path</code> from a string, stores an email based on that path, and then does error handling.</p>",
            Outdent,
            Newline,
            Desc [Ident "storeEmail", Kw "=", Lambda ["path"]] "<p>This <a href=\"/tutorial#defining-functions\">defines a function</a> named <code>storeEmail</code>. It takes one argument, named <code>path</code>.</p><p>In Roc, functions are ordinary values, so we assign names to them using <code>=</code> like with any other value.</p><p>The <code>\\arg1, arg2 -&gt;</code> syntax begins a function, and the part after <code>-&gt;</code> is the function's body.</p>",
            Indent,
            Desc [Ident "url", Kw "&lt;-", Ident "File.readUtf8", Ident "path", Kw "|>", Ident "Task.await"] "<p>This reads the contents of the file (as a <a href=\"https://en.wikipedia.org/wiki/UTF-8\">UTF-8</a> string) into <code>url</code>.</p><p>The <code>&lt;-</code> does <a href=\"/tutorial#backpassing\">backpassing</a>, which is syntax sugar for defining a function. This line desugars to:</p><pre><code>Task.await\n    (File.readUtf8 path)\n    \\url -&gt;</code></pre><p>The lines after this one form the body of the <code>\\url -&gt;</code> <a href=\"https://en.wikipedia.org/wiki/Callback_(computer_programming)\">callback</a>, which runs if the file read succeeds.</p>",
            Newline,
            Desc [Ident "user", Kw "&lt;-", Ident "Http.get", Ident "url", Ident "Json.codec", Kw "|>", Ident "Task.await"] "<p>This fetches the contents of the URL and decodes them as <a href=\"https://www.json.org\">JSON</a>.</p><p>If the shape of the JSON isn't compatible with the type of <code>user</code> (based on type inference), this will give a decoding error immediately.</p><p>As with all the other lines ending in <code>|> Task.await</code>, if there's an error, nothing else in <code>storeEmail</code> will be run, and <code>handleErr</code> will end up handling the error.</p>",
            Newline,
            Desc [Ident "dest", Kw "=", Ident "Path.fromStr", StrInterpolation "\"" "user.name" ".txt\""] "<p>The <code>\\(user.name)</code> in this string literal will be replaced with the value in <code>name</code>. This is <a href=\"/tutorial#string-interpolation\">string interpolation</a>.</p><p>Note that this line doesn't end with <code>|> Task.await</code>. Earlier lines needed that because they were I/O <a href=\"/tutorial#tasks\">tasks</a>, but this is a plain old <a href=\"/tutorial#defs\">definition</a>, so there's no task to await.</p>",
            Newline,
            Desc [Literal "_", Kw "&lt;-", Ident "File.writeUtf8", Ident "dest", Ident "user.email", Kw "|>", Ident "Task.await"] "<p>This writes <code>user.email</code> to the file, encoded as <a href=\"https://en.wikipedia.org/wiki/UTF-8\">UTF-8</a>.</p><p>We won't be using the output of <code>writeUtf8</code>, so we name it <code>_</code>. The special name <code>_</code> is for when you don't want to use something.</p><p>You can name as many things as you like <code>_</code>, but you can never reference anything named <code>_</code>. So it's only useful for when you don't want to choose a name.</p>",
            Newline,
            Desc [Ident "Stdout.line", StrInterpolation "\"Wrote email to " "Path.display dest" "\""] "<p>This prints what we did to <a href=\"https://en.wikipedia.org/wiki/Standard_streams#Standard_output_(stdout)\">stdout</a>.</p><p>Note that this line doesn't end with <code>|> Task.await</code>. That's because, although <code>Stdout.line</code> returns a <a href=\"/tutorial#tasks\">task</a>, we don't need to await it because nothing happens after it.</p>",
            Outdent,
            Newline,
            Desc [Ident "handleErr", Kw "=", Lambda ["err"]] "<p>Like <code>storeEmail</code>, <code>handleErr</code> is also a function.</p><p>Although type annotations are optional everywhere in Roc—because the language has 100% type inference—you could add type annotations to <code>main</code>, <code>storeEmail</code>, and <code>handleErr</code> if you wanted to.</p>",
            Indent,
            Desc [Kw "when", Ident "err", Kw "is"] "<p>This will run one of the following lines depending on what value the <code>err</code> argument has.</p><p>Each line does a <a href=\"/tutorial#tags-with-payloads\">pattern match</a> on the shape of the error to decide whether to run, or to move on and try the next line's pattern.</p><p>Roc will do compile-time <a href=\"/tutorial#exhaustiveness\">exhaustiveness checking</a> and tell you if you forgot to handle any error cases here that could have occurred, based on the tasks that were run in <code>storeEmail</code>.</p>",
            Indent,
            Desc [Literal "HttpErr", Ident "url", Kw "_", Kw "->", Ident "Stderr.line", StrInterpolation "\"Error fetching URL " "url" "\""] "<p>This line will run if the <code>Http.get</code> request from earlier encountered an HTTP error.</p><p>It handles the error by printing an error message to <a href=\"https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)\">stderr</a>.</p><p>The <code>_</code> is where more information about the error is stored in the <code>HttpErr</code>. If we wanted to print more detail about what the error was, we'd name that something other than <code>_</code> and actually use it.</p>",
            Newline,
            Desc [Literal "FileReadErr", Ident "path", Kw "_", Kw "->", Ident "Stderr.line", StrInterpolation "\"Error reading from " "Path.display path" "\""] "<p>This line will run if the <code>File.readUtf8</code> from earlier encountered a file I/O error.</p><p>It handles the error by printing an error message to <a href=\"https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)\">stderr</a>.</p><p>The <code>_</code> is where more information about the error is stored in the <code>FileReadErr</code>. If we wanted to print more detail about what the error was, we'd name that something other than <code>_</code> and actually use it.</p>",
            Newline,
            Desc [Literal "FileWriteErr", Ident "path", Kw "_", Kw "->", Ident "Stderr.line", StrInterpolation "\"Error writing to " "Path.display path" "\""] "<p>This line will run if the <code>File.writeUtf8</code> from earlier encountered a file I/O error.</p><p>It handles the error by printing an error message to <a href=\"https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)\">stderr</a>.</p><p>The <code>_</code> is where more information about the error is stored in the <code>FileWriteErr</code>. If we wanted to print more detail about what the error was, we'd name that something other than <code>_</code> and actually use it.</p>",
        ]

    div [role "presentation"] [
        pre [class "interactive-example"] [
            samp [] [text output],
        ],
        p [] [
            text "To get started with the language, try the ",
            a [href "/tutorial"] [text "tutorial"],
            text "!",
        ],
        p [id "final-tutorial-link"] [
            a [class "btn-small", href "/tutorial"] [text "Start Tutorial"]
        ]
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
                Str.concat bufWithSpace (identToHtml str)

            StrInterpolation before interp after ->
                bufWithSpace
                |> Str.concat (if Str.isEmpty before then "" else "<span class=\"literal\">\(before)</span>")
                |> Str.concat "<span class=\"kw\">\\(</span>\(identToHtml interp)<span class=\"kw\">)</span>"
                |> Str.concat (if Str.isEmpty after then "" else "<span class=\"literal\">\(after)</span>")

identToHtml : Str -> Str
identToHtml = \str ->
    List.walk (Str.split str ".") "" \accum, ident ->
        identHtml = "<span class=\"ident\">\(ident)</span>"

        if Str.isEmpty accum then
            identHtml
        else
            "\(accum)<span class=\"kw\">.</span>\(identHtml)"

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
    <input class="interactive-radio" type="radio" name="r" id="r\(Num.toStr index)" \(checkedHtml)><label for="r\(Num.toStr index)" title="Tap to learn about this syntax">\(labelHtml)</label><span class="interactive-desc" role="presentation"><button class="close-desc">X</button>\(descHtml)</span>
    """
