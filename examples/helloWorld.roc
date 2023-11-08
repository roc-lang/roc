app "helloWorld"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [pf.Stdout, TotallyNotJson]
    provides [main] to pf

main =
    { a, b } =
        when Decode.fromBytes (Str.toUtf8 "{\"b\": \"foo\", \"a\": \"bar\", \"c\": 123}") (TotallyNotJson.jsonWithOptions { fieldNameMapping: PascalCase }) is
            Ok rec -> rec
            Err _ -> crash "ahh"
    Stdout.line "\(a) + \(b)"
