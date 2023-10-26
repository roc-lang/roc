app "helloWorld"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf

Section : [Desc (List Token) Str, Indent, Outdent, Newline]
Token : [Kw Str, Ident Str, Str Str, Num Str]

main =
    output =
        # subject = "Awesome Programmer"
        #
        # helloWorld =
        #     Str.withCapacity 45
        #     |> Str.concat greeting
        #     |> Str.concat ", "
        #     |> Str.concat subject
        #     |> Str.concat "!"
        sectionsToStr [
            Desc [Ident "subject", Kw "=", Str "Awesome Programmer"] "<p>This assigns the name <code class=\"ident\">subject</code> to the string \"Awesome programmer\".</p><p>In Roc, assignments are always constant, which means writing <code class=\"ident\">subject =</code> again in the same scope would give an error.</p><p><a href=\"https://www.roc-lang.org/tutorial#naming-things\">Learn more about naming things</a></p>",
            Newline,
            Newline,
            Desc [Ident "helloWorld", Kw "="] "<p>This assigns the name <code class=\"ident\">helloWorld</code> to the value returned by this chain of function calls.</p><p>In Roc, assignments are always constant, which means writing <code class=\"ident\">helloWorld =</code> again in the same scope would give an error.</p><p><a href=\"https://www.roc-lang.org/tutorial#naming-things\">Learn more about naming things</a></p>",
            Indent,
            Desc [Ident "Str.withCapacity", Num "45"] "<p>This calls the <a href=\"https://www.roc-lang.org/builtins/Str#withCapacity\"><code class=\"ident\">Str.withCapacity</code> function</a> passing <code class=\"ident\">45</code> as its argument.</p><p>This creates a new string with capacity for 45 bytes without needing to allocate more space.</p>",
            Newline,
            Desc [Kw "|>", Ident "Str.concat", Ident "greeting"] "<p>This calls the <a href=\"https://www.roc-lang.org/builtins/Str#concat\"><code class=\"ident\">Str.concat</code> function</a> passing <code class=\"ident\">greeting</code> as its second argument, and the output of the pipeline up to this point as its first argument.</p>",
        ]

    Stdout.line output

tokensToStr : List Token -> Str
tokensToStr = \tokens ->
    List.walk tokens "" \buf, token ->
        bufWithSpace =
            if Str.isEmpty buf then
                buf
            else
                Str.concat buf " "

        when token is
            Kw str ->
                Str.concat bufWithSpace "<span class=\"kw\">\(str)</span>"

            Num str ->
                Str.concat bufWithSpace "<span class=\"literal\">\(str)</span>"

            Str str ->
                Str.concat bufWithSpace "<span class=\"literal\">\"\(str)\"</span>"

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
            indent: nextIndent
        }

    answer.buf

radio : U16, Str, Str -> Str
radio = \index, labelHtml, descHtml ->
    # The first radio button should always be checked, and none of the others should be.
    checkedHtml = if index == 0 then " checked" else ""

    """
    <input class="interactive-radio" type="radio" name="r" id="r\(Num.toStr index)" autocomplete=\"off\"\(checkedHtml)><label for="r\(Num.toStr index)" title="Tap to learn about this syntax">\(labelHtml)</label><div class="interactive-desc">\(descHtml)</div>
    """
