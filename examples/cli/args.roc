app "args"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdout, pf.Arg, pf.Task.{ Task }, pf.Process]
    provides [main] to pf

main : Task {} []
main =
    args <- Arg.list |> Task.await
    parser =
        divCmd =
            Arg.succeed (\dividend -> \divisor -> Div (Num.toF64 dividend) (Num.toF64 divisor))
            |> Arg.withParser
                (
                    Arg.i64Option {
                        long: "dividend",
                        short: "n",
                        help: "the number to divide; corresponds to a numerator",
                    }
                )
            |> Arg.withParser
                (
                    Arg.i64Option {
                        long: "divisor",
                        short: "d",
                        help: "the number to divide by; corresponds to a denominator",
                    }
                )
            |> Arg.subCommand "div"

        logCmd =
            Arg.succeed (\base -> \num -> Log (Num.toF64 base) (Num.toF64 num))
            |> Arg.withParser
                (
                    Arg.i64Option {
                        long: "base",
                        short: "b",
                        help: "base of the logarithm",
                    }
                )
            |> Arg.withParser
                (
                    Arg.i64Option {
                        long: "num",
                        help: "the number to take the logarithm of",
                    }
                )
            |> Arg.subCommand "log"

        Arg.choice [divCmd, logCmd]
        |> Arg.program { name: "args-example", help: "A calculator example of the CLI platform argument parser" }

    when Arg.parseFormatted parser args is
        Ok cmd ->
            runCmd cmd
            |> Num.toStr
            |> Stdout.line

        Err helpMenu ->
            {} <- Stdout.line helpMenu |> Task.await
            Process.exit 1

runCmd = \cmd ->
    when cmd is
        Div n d -> n / d
        Log b n ->
            # log_b(n) = log_x(n) / log_x(b) for all x
            runCmd (Div (Num.log n) (Num.log b))
