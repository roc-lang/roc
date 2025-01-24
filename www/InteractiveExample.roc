module [view]

import pf.Html exposing [pre, samp, div, text, a, p]
import pf.Html.Attributes exposing [class, role, href, id]

Token : [
    Kw Str,
    Ident Str,
    Str Str,
    Num Str,
    Comment Str,
    Literal Str,
    Suffix Str,
    ParensAround (List Token),
    Lambda (List Str),
    StrInterpolation Str Str Str,
    PncApply Str (List Token),
]
Section : [Desc (List Token) Str, Indent, Outdent, Newline]

view : Html.Node
view =
    output =
        sections_to_str(
            [
                Desc(
                    [Comment("<span class='desktop'>Click anything here to see an explanation.</span><span class='mobile'>Tap anything here to\n# see an explanation.</span>")],
                    "<p><a href=\"/tutorial#comments\">Comments</a> in Roc begin with <code>#</code> and go to the end of the line.</p>",
                ),
                Newline,
                Desc(
                    [Ident("main!"), Kw("=")],
                    """
                    <p>This defines <code class=\"ident\">main!</code>, which is where our program will begin.</p>
                    <p>In Roc, <a href=\"/tutorial#https://www.roc-lang.org/tutorial#naming-things\">all definitions are constant</a>,
                    so writing <code class=\"ident\">main! =</code> again in the same scope would give an error.</p>
                    """,
                ),
                Indent,
                Desc(
                    [PncApply("store_email!", [PncApply("Path.from_str", [Str("\"url.txt\"")])]), Kw("?"), Ident("handle_err!")],
                    """
                    <p>This converts the string <code>\"url.txt\"</code> into a <code>Path</code> by passing it to <code>Path.from_str</code>.</p>
                    <p>It then sends that result to <code>store_email!</code>. The <code>!</code> sigil at the end tells you that this function is
                    <em>effectual</em>, meaning it will either do I/O, talk to the network, or can otherwise return a different result each time
                    it's called.  Functions that do not end in <code>!</code> will always return the same result given the same arguments.</p>
                    <p>Function arguments are surrounded with parenthesis and separated by commas.</p>
                    <p>The <code>?</code> infix operator here will return the result if successful, otherwise it will call <code>handle_err!</code>
                    with the error if it fails.</p>
                    """,
                ),
                Outdent,
                Newline,
                Desc(
                    [Ident("store_email!"), Kw("="), Lambda(["path"])],
                    """
                    <p>This <a href=\"/tutorial#defining-functions\">defines a function</a> named <code>store_email!</code>.
                    It takes one argument, named <code>path</code>.</p>
                    <p>In Roc, functions are ordinary values, so we assign names to them using <code>=</code> like with any other value.</p>
                    <p>The <code>|arg1, arg2|</code> syntax begins a function, and the part after the final <code>|</code> is the function's body.</p>
                    """,
                ),
                Indent,
                Desc([Ident("url"), Kw("="), PncApply("File.read_utf8!", [Ident("path")]), Suffix("?")],
                    """
                    <p>This passes <code>path</code> to the <code>File.read_utf8</code> function, which reads the contents of the file
                    (as a <a href=\"https://en.wikipedia.org/wiki/UTF-8\">UTF-8</a> string) into <code>url</code>.</p>
                    <p>The <code>!</code> sigil at the end tells you that this function is <em>effectual</em>, meaning it will either
                    do I/O, talk to the network, or can otherwise return a different result each time it's called.  Functions that do
                    not end in <code>!</code> will always return the same result given the same arguments.</p>
                    <p>The <code>?</code> is an operator that means, if the file read fails (maybe because <code>path</code> refers to a missing file),
                    the rest of this function will be skipped, and the <code>handle_err</code> function will take over.</p>
                    """),
                Newline,
                Desc([Ident("user"), Kw("="), PncApply("Http.get!", [Ident("url"), Ident("Json.utf8")]), Suffix("?")],
                    """
                    <p>This fetches the contents of the URL and decodes them as <a href=\"https://www.json.org\">JSON</a>.</p>
                    <p>If the shape of the JSON isn't compatible with the type of <code>user</code> (based on type inference),
                    this will give a decoding error immediately.</p>
                    <p>As with all the other function calls that end with the <code>?</code> postfix operator, if there's an error,
                    nothing else in <code>store_email!</code> will be run, and <code>handle_err!</code> will run.</p>
                    """,
                ),
                Newline,
                Desc([Ident("dest"), Kw("="), PncApply("Path.from_str", [StrInterpolation("\"", "user.name", ".txt\"")])],
                    """
                    <p>The <code>\$(user.name)</code> in this string literal will be replaced with the value stored in the
                    <code>user</code> record's <code>name</code> field.
                    This is <a href=\"/tutorial#string-interpolation\">string interpolation</a>.</p>
                    <p>Note that this function call doesn't involve the <code>?</code> operator. That's because
                    <code>Path.from_str</code> isn't an effectual function and can't fail, so there's no need to use <code>?</code> to handle it's result.</p>
                    """,
                ),
                Newline,
                Desc([PncApply("File.write_utf8!", [Ident("dest"), Ident("user.email")]), Suffix("?")],
                    """
                    <p>This writes <code>user.email</code> to the file, encoded as <a href=\"https://en.wikipedia.org/wiki/UTF-8\">UTF-8</a>.</p>
                    <p>Since <code>File.write_utf8!</code> doesn't produce any information on success, we don't bother using <code>=</code> like we did on the other lines.</p>
                    """,
                ),
                Newline,
                Desc([PncApply("Stdout.line!", [StrInterpolation("\"Wrote email to ", "Path.display(dest)", "\"")])],
                    """
                    <p>This prints what we did to <a href=\"https://en.wikipedia.org/wiki/Standard_streams#Standard_output_(stdout)\">stdout</a>.</p>
                    <p>Notice that this does a function call inside the string interpolation.
                    Any valid Roc expression is allowed inside string interpolation, as long as it doesn't contain any newlines.</p>
                    """,
                ),
                Outdent,
                Newline,
                Desc([Ident("handle_err!"), Kw("="), Lambda(["err"])],
                    """
                    <p>Like <code>store_email!</code>, <code>handle_err!</code> is also a function.</p>
                    <p>Although type annotations are optional everywhere in Roc—because the language has 100% type inference—you
                    could add type annotations to <code>main!</code>, <code>store_email!</code>, and <code>handle_err!</code> if you wanted to.</p>
                    """,
                ),
                Indent,
                Desc([Kw("when"), Ident("err"), Kw("is")],
                    """
                    <p>This will run one of the following lines depending on what value the <code>err</code> argument has.</p>
                    <p>Each line does a <a href=\"/tutorial#tags-with-payloads\">pattern match</a> on the shape of the error
                    to decide whether to run, or to move on and try the next line's pattern.</p>
                    <p>Roc will do compile-time <a href=\"/tutorial#exhaustiveness\">exhaustiveness checking</a> and tell you if you forgot to handle any error cases here
                    that could have occurred, based on the tasks that were run in <code>store_email</code>.</p>
                    """,
                ),
                Indent,
                Desc([PncApply("HttpErr", [Ident("url"), Kw("_")]), Kw("->"), PncApply("Stderr.line!", [StrInterpolation("\"Error fetching URL ", "url", "\"")])],
                    """
                    <p>This line will run if the <code>Http.get</code> request from earlier encountered an HTTP error.</p>
                    <p>It handles the error by printing an error message to <a href=\"https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)\">stderr</a>.</p>
                    <p>The <code>_</code> is where more information about the error is stored in the <code>HttpErr</code>. If we wanted to print more detail about what the error was,
                    we'd name that something other than <code>_</code> and actually use it.</p>
                    """,
                ),
                Newline,
                Desc([PncApply("FileReadErr", [Ident("path"), Kw("_")]), Kw("->"), PncApply("Stderr.line!", [StrInterpolation("\"Error reading from ", "Path.display(path)", "\"")])],
                    """
                    <p>This line will run if the <code>File.read_utf8!</code> from earlier encountered a file I/O error.</p>
                    <p>It handles the error by printing an error message to <a href=\"https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)\">stderr</a>.</p>
                    <p>The <code>_</code> is where more information about the error is stored in the <code>FileReadErr</code>. If we wanted to print more detail about what
                    the error was, we'd name that something other than <code>_</code> and actually use it.</p>
                    """,
                ),
                Newline,
                Desc([PncApply("FileWriteErr", [Ident("path"), Kw("_")]), Kw("->"), PncApply("Stderr.line!", [StrInterpolation("\"Error writing to ", "Path.display(path)", "\"")])],
                    """
                    <p>This line will run if the <code>File.write_utf8!</code> from earlier encountered a file I/O error.</p>
                    <p>It handles the error by printing an error message to <a href=\"https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)\">stderr</a>.</p>
                    <p>The <code>_</code> is where more information about the error is stored in the <code>FileWriteErr</code>.
                    If we wanted to print more detail about what the error was, we'd name that something other than <code>_</code> and actually use it.</p>
                    """,
                ),
            ],
        )

    div(
        [role("presentation")],
        [
            pre(
                [class("interactive-example")],
                [
                    samp([], [text(output)]),
                ],
            ),
            p(
                [],
                [
                    text("To get started with the language, try the "),
                    a([href("/tutorial")], [text("tutorial")]),
                    text("!"),
                ],
            ),
            p(
                [id("final-tutorial-link")],
                [
                    a([class("btn-small"), href("/tutorial")], [text("Start Tutorial")]),
                ],
            ),
        ],
    )

open_paren_html = "<span class=\"kw\">(</span>"
close_paren_html = "<span class=\"kw\">)</span>"
comma_html = "<span class=\"literal\">,</span> "

tokens_to_str : List Token -> Str
tokens_to_str = |tokens|
    List.walk(
        tokens,
        "",
        |buf, token|
            buf_with_space =
                if Str.is_empty(buf) then
                    buf
                else
                    when token is
                        Literal(",") | Suffix(_) -> buf
                        _ -> Str.concat(buf, " ")

            when token is
                ParensAround(wrapped) ->
                    # Don't put spaces after opening parens or before closing parens
                    buf_with_space
                    |> Str.concat(open_paren_html)
                    |> Str.concat(tokens_to_str(wrapped))
                    |> Str.concat(close_paren_html)

                Lambda(args) ->
                    # Don't put spaces after opening parens or before closing parens
                    args_with_commas =
                        args
                        |> List.map(|ident| "<span class=\"ident\">${ident}</span>")
                        |> Str.join_with(comma_html)

                    buf_with_space
                    |> Str.concat("<span class=\"kw\">|</span>")
                    |> Str.concat(args_with_commas)
                    |> Str.concat("<span class=\"kw\">|</span>")

                Suffix(str) | Kw(str) ->
                    Str.concat(buf_with_space, "<span class=\"kw\">${str}</span>")

                Num(str) | Str(str) | Literal(str) -> # We may render these differently in the future
                    Str.concat(buf_with_space, "<span class=\"literal\">${str}</span>")

                Comment(str) ->
                    Str.concat(buf_with_space, "<span class=\"comment\"># ${str}</span>")

                Ident(str) ->
                    Str.concat(buf_with_space, ident_to_html(str))

                StrInterpolation(before, interp, after) ->
                    buf_with_space
                    |> Str.concat((if Str.is_empty(before) then "" else "<span class=\"literal\">${before}</span>"))
                    |> Str.concat("<span class=\"kw\">\${</span>${ident_to_html(interp)}<span class=\"kw\">}</span>")
                    |> Str.concat((if Str.is_empty(after) then "" else "<span class=\"literal\">${after}</span>"))

                PncApply(ident_str, args) ->
                    args_with_commas =
                        args
                        |> List.map(|t| tokens_to_str([t]))
                        |> Str.join_with(comma_html)

                    buf_with_space
                    |> Str.concat(ident_to_html(ident_str))
                    |> Str.concat(open_paren_html)
                    |> Str.concat(args_with_commas)
                    |> Str.concat(close_paren_html),
    )

ident_to_html : Str -> Str
ident_to_html = |str|
    List.walk(
        Str.split_on(str, "."),
        "",
        |accum, ident|
            html = "<span class=\"ident\">${ident}</span>"

            if Str.is_empty(accum) then
                html
            else
                "${accum}<span class=\"kw\">.</span>${html}",
    )

sections_to_str : List Section -> Str
sections_to_str = |sections|
    answer = List.walk(
        sections,
        { buf: "", count: 0, indent: 0 },
        |{ buf, count, indent }, section|
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
            },
    )

    answer.buf

radio : U16, Str, Str -> Str
radio = |index, label_html, desc_html|
    # The first radio button should always be checked, and none of the others should be.
    checked_html = if index == 0 then " checked" else ""

    """
    <input class="interactive-radio" type="radio" name="r" id="r${Num.to_str(index)}" ${checked_html}><label for="r${Num.to_str(index)}" title="Tap to learn about this syntax">${label_html}</label><span class="interactive-desc" role="presentation"><button class="close-desc">X</button>${desc_html}</span>
    """
