module [view]

import pf.Html exposing [pre, samp, div, text, a, p]
import pf.Html.Attributes exposing [class, role, href, id]

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
        sections_to_str([
            Desc([Comment("<span class='desktop'>Click anything here to see an explanation.</span><span class='mobile'>Tap anything here to\n# see an explanation.</span>")], "<p><a href=\"/tutorial#comments\">Comments</a> in Roc begin with <code>#</code> and go to the end of the line.</p>"),
            Newline,
            Desc([Ident("main"), Kw("=")], "<p>This defines <code class=\"ident\">main</code>, which is where our program will begin.</p><p>In Roc, <a href=\"/tutorial#https://www.roc-lang.org/tutorial#naming-things\">all definitions are constant</a>, so writing <code class=\"ident\">main =</code> again in the same scope would give an error.</p>"),
            Indent,
            Desc([Ident("Path.fromStr"), Str("\"url.txt\"")], "<p>This converts the string <code>\"url.txt\"</code> into a <code>Path</code> by passing it to <code>Path.fromStr</code>.</p><p>Function arguments are separated with whitespace. Parentheses are only needed in <a href=\"/tutorial#calling-functions\">nested function calls</a>.</p>"),
            Newline,
            Desc([Kw("|>"), Ident("storeEmail")], "<p>The <a href=\"/tutorial#the-pipe-operator\">pipe operator</a> (<code>|></code>) is syntax sugar for passing the previous value to the next function in the \"pipeline.\"</p><p>This line takes the value that <code>Path.fromStr \"url.txt\"</code> returns and passes it to <code>storeEmail</code>.</p><p>The next <code>|></code> continues the pipeline.</p>"),
            Newline,
            Desc([Kw("|>"), Ident("Task.onErr"), Ident("handleErr")], "<p>If the task returned by the previous step in the pipeline fails, pass its error to <code>handleErr</code>.</p><p>The pipeline essentially does this:</p><pre><code>a = Path.fromStr \"url.txt\"\nb = storeEmail a\n\nTask.onErr b handleErr</code></pre><p>It creates a <code>Path</code> from a string, passes it to <code>storeEmail</code>, and specifies how to handle errors if storing the email fails.</p>"),
            Outdent,
            Newline,
            Desc([Ident("storeEmail"), Kw("="), Lambda(["path"])], "<p>This <a href=\"/tutorial#defining-functions\">defines a function</a> named <code>storeEmail</code>. It takes one argument, named <code>path</code>.</p><p>In Roc, functions are ordinary values, so we assign names to them using <code>=</code> like with any other value.</p><p>The <code>\\arg1, arg2 -&gt;</code> syntax begins a function, and the part after <code>-&gt;</code> is the function's body.</p>"),
            Indent,
            Desc([Ident("url"), Kw("="), Ident("File.readUtf8!"), Ident("path")], "<p>This passes <code>path</code> to the <code>File.readUtf8</code> function, which reads the contents of the file (as a <a href=\"https://en.wikipedia.org/wiki/UTF-8\">UTF-8</a> string) into <code>url</code>.</p><p>The <code>!</code> operator is similar to <code>await</code> in other languages. It means “wait until the asynchronous <code>File.readUtf8</code> operation successfully completes.”</p><p>If the file read fails (maybe because <code>path</code> refers to a missing file), the rest of this function will be skipped, and the <code>handleErr</code> function will take over.</p>"),
            Newline,
            Desc([Ident("user"), Kw("="), Ident("Http.get!"), Ident("url"), Ident("Json.utf8")], "<p>This fetches the contents of the URL and decodes them as <a href=\"https://www.json.org\">JSON</a>.</p><p>If the shape of the JSON isn't compatible with the type of <code>user</code> (based on type inference), this will give a decoding error immediately.</p><p>As with all the other function calls involving the <code>!</code> operator, if there's an error, nothing else in <code>storeEmail</code> will be run, and <code>handleErr</code> will run.</p>"),
            Newline,
            Desc([Ident("dest"), Kw("="), Ident("Path.fromStr"), StrInterpolation("\"", "user.name", ".txt\"")], "<p>The <code>\$(user.name)</code> in this string literal will be replaced with the value stored in the <code>user</code> record's <code>name</code> field. This is <a href=\"/tutorial#string-interpolation\">string interpolation</a>.</p><p>Note that this function call doesn't involve the <code>!</code> operator. That's because <code>Path.fromStr</code> doesn't involve any Tasks, so there's no need to use <code>!</code> to wait for it to finish.</p>"),
            Newline,
            Desc([Ident("File.writeUtf8!"), Ident("dest"), Ident("user.email")], "<p>This writes <code>user.email</code> to the file, encoded as <a href=\"https://en.wikipedia.org/wiki/UTF-8\">UTF-8</a>.</p><p>Since <code>File.writeUtf8</code> doesn't produce any information on success, we don't bother using <code>=</code> like we did on the other lines.</p>"),
            Newline,
            Desc([Ident("Stdout.line!"), StrInterpolation("\"Wrote email to ", "Path.display dest", "\"")], "<p>This prints what we did to <a href=\"https://en.wikipedia.org/wiki/Standard_streams#Standard_output_(stdout)\">stdout</a>.</p><p>Notice that this does a function call inside the string interpolation. Any valid Roc expression is allowed inside string interpolation, as long as it doesn't contain any newlines.</p>"),
            Outdent,
            Newline,
            Desc([Ident("handleErr"), Kw("="), Lambda(["err"])], "<p>Like <code>storeEmail</code>, <code>handleErr</code> is also a function.</p><p>Although type annotations are optional everywhere in Roc—because the language has 100% type inference—you could add type annotations to <code>main</code>, <code>storeEmail</code>, and <code>handleErr</code> if you wanted to.</p>"),
            Indent,
            Desc([Kw("when"), Ident("err"), Kw("is")], "<p>This will run one of the following lines depending on what value the <code>err</code> argument has.</p><p>Each line does a <a href=\"/tutorial#tags-with-payloads\">pattern match</a> on the shape of the error to decide whether to run, or to move on and try the next line's pattern.</p><p>Roc will do compile-time <a href=\"/tutorial#exhaustiveness\">exhaustiveness checking</a> and tell you if you forgot to handle any error cases here that could have occurred, based on the tasks that were run in <code>storeEmail</code>.</p>"),
            Indent,
            Desc([Literal("HttpErr"), Ident("url"), Kw("_"), Kw("->"), Ident("Stderr.line!"), StrInterpolation("\"Error fetching URL ", "url", "\"")], "<p>This line will run if the <code>Http.get</code> request from earlier encountered an HTTP error.</p><p>It handles the error by printing an error message to <a href=\"https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)\">stderr</a>.</p><p>The <code>_</code> is where more information about the error is stored in the <code>HttpErr</code>. If we wanted to print more detail about what the error was, we'd name that something other than <code>_</code> and actually use it.</p>"),
            Newline,
            Desc([Literal("FileReadErr"), Ident("path"), Kw("_"), Kw("->"), Ident("Stderr.line!"), StrInterpolation("\"Error reading from ", "Path.display path", "\"")], "<p>This line will run if the <code>File.readUtf8</code> from earlier encountered a file I/O error.</p><p>It handles the error by printing an error message to <a href=\"https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)\">stderr</a>.</p><p>The <code>_</code> is where more information about the error is stored in the <code>FileReadErr</code>. If we wanted to print more detail about what the error was, we'd name that something other than <code>_</code> and actually use it.</p>"),
            Newline,
            Desc([Literal("FileWriteErr"), Ident("path"), Kw("_"), Kw("->"), Ident("Stderr.line!"), StrInterpolation("\"Error writing to ", "Path.display path", "\"")], "<p>This line will run if the <code>File.writeUtf8</code> from earlier encountered a file I/O error.</p><p>It handles the error by printing an error message to <a href=\"https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)\">stderr</a>.</p><p>The <code>_</code> is where more information about the error is stored in the <code>FileWriteErr</code>. If we wanted to print more detail about what the error was, we'd name that something other than <code>_</code> and actually use it.</p>"),
        ])

    div([role("presentation")], [
        pre([class("interactive-example")], [
            samp([], [text(output)]),
        ]),
        p([], [
            text("To get started with the language, try the "),
            a([href("/tutorial")], [text("tutorial")]),
            text("!"),
        ]),
        p([id("final-tutorial-link")], [
            a([class("btn-small"), href("/tutorial")], [text("Start Tutorial")]),
        ]),
    ])

tokens_to_str : List Token -> Str
tokens_to_str = \tokens ->
    List.walk(tokens, "", \buf, token ->
        buf_with_space =
            if Str.is_empty(buf) || token == Literal(",") then
                buf
            else
                Str.concat(buf, " ")

        when token is
            ParensAround(wrapped) ->
                # Don't put spaces after opening parens or before closing parens
                buf_with_space
                |> Str.concat("<span class=\"kw\">(</span>")
                |> Str.concat(tokens_to_str(wrapped))
                |> Str.concat("<span class=\"kw\">)</span>")

            Lambda(args) ->
                # Don't put spaces after opening parens or before closing parens
                args_with_commas =
                    args
                    |> List.map(\ident -> "<span class=\"ident\">$(ident)</span>")
                    |> Str.join_with("<span class=\"literal\">,</span> ")

                buf_with_space
                |> Str.concat("<span class=\"kw\">\\</span>")
                |> Str.concat(args_with_commas)
                |> Str.concat("<span class=\"kw\"> -></span>")

            Kw(str) ->
                Str.concat(buf_with_space, "<span class=\"kw\">$(str)</span>")

            Num(str) | Str(str) | Literal(str) -> # We may render these differently in the future
                Str.concat(buf_with_space, "<span class=\"literal\">$(str)</span>")

            Comment(str) ->
                Str.concat(buf_with_space, "<span class=\"comment\"># $(str)</span>")

            Ident(str) ->
                Str.concat(buf_with_space, ident_to_html(str))

            StrInterpolation(before, interp, after) ->
                buf_with_space
                |> Str.concat((if Str.is_empty(before) then "" else "<span class=\"literal\">$(before)</span>"))
                |> Str.concat("<span class=\"kw\">\$(</span>$(ident_to_html(interp))<span class=\"kw\">)</span>")
                |> Str.concat((if Str.is_empty(after) then "" else "<span class=\"literal\">$(after)</span>")))

ident_to_html : Str -> Str
ident_to_html = \str ->
    List.walk(Str.split_on(str, "."), "", \accum, ident ->
        len = Str.count_utf8_bytes(ident)
        without_suffix = ident |> Str.replace_last("!", "")

        ident_html = "<span class=\"ident\">$(without_suffix)</span>"
        html =
            # If removing a trailing "!" changed the length, then there must have been a trailing "!"
            if len > Str.count_utf8_bytes(without_suffix) then
                "$(ident_html)<span class=\"kw\">!</span>"
            else
                ident_html

        if Str.is_empty(accum) then
            html
        else
            "$(accum)<span class=\"kw\">.</span>$(html)")

sections_to_str : List Section -> Str
sections_to_str = \sections ->
    answer = List.walk(sections, { buf: "", count: 0, indent: 0 }, \{ buf, count, indent }, section ->
        buf_with_space =
            if Str.is_empty(buf) then
                buf
            else if buf |> Str.ends_with("\n") then
                Str.concat(buf, Str.repeat(" ", indent))
            else
                Str.concat(buf, " ")

        (after_spaces, next_count) =
            when section is
                Newline | Indent | Outdent ->
                    # Indent and outdent changes happen on the next iteration,
                    # so we only need a newline for them here.
                    (Str.concat(buf, "\n"), count)

                Desc(tokens, str) ->
                    html = radio(count, tokens_to_str(tokens), str)

                    (Str.concat(buf_with_space, html), count + 1)

        next_indent =
            when section is
                Indent -> indent + 4
                Outdent -> indent - 4
                Newline | Desc(_, _) -> indent

        {
            buf: after_spaces,
            count: next_count,
            indent: next_indent,
        })

    answer.buf

radio : U16, Str, Str -> Str
radio = \index, label_html, desc_html ->
    # The first radio button should always be checked, and none of the others should be.
    checked_html = if index == 0 then " checked" else ""

    """
    <input class="interactive-radio" type="radio" name="r" id="r$(Num.to_str(index))" $(checked_html)><label for="r$(Num.to_str(index))" title="Tap to learn about this syntax">$(label_html)</label><span class="interactive-desc" role="presentation"><button class="close-desc">X</button>$(desc_html)</span>
    """
