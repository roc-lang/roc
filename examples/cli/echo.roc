app "echo"
    packages { pf: "cli-platform/main.roc" }
    imports [pf.Stdin, pf.Stdout, pf.Stderr, pf.Task.{ Task }, pf.Program.{ Program, ExitCode }, I2.{ Task2 }]
    provides [main] to pf

main : Program
main = Program.noArgs mainTask

mainTask : Task ExitCode [] [Read [Stdin], Write [Stdout]]
mainTask =
    _ <- Task.await (Stdout.line "Trying out a simulation!")

    task = I2.stdoutLine "Hello there!"
    sim = I2.simStdoutLine \str -> str |> Str.endsWith "!"

    report = when I2.simulate sim task is
        Ok {} -> Stdout.line "Simulation passed!"
        Err _ -> Stderr.line "Simulation failed!"

    report
    |> Program.exit 0
