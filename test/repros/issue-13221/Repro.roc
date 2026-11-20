# https://github.com/roc-lang/roc/issues/13221
app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst",
}
import cli.OsStr
import cli.Stdout

id_of : Str -> Str
id_of = |path| path.split_on("/").last() ?? path

main! : List(OsStr) => Try({}, _)
main! = |args| {
    for arg in args {
        input = OsStr.display(arg)
        Stdout.line!(id_of(input))?
    }
    Ok({})
}
