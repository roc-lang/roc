app [main] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.13.0/nW9yMRtZuCYf1Oa9vbE5XoirMwzLbtoSgv7NGhUlqYA.tar.br" }

import cli.Stdout
import cli.Task
import cli.Arg
import cli.File
import cli.Utc
import cli.Task exposing [Task]

dataPath : U64 -> Str
dataPath = \day ->
    "data/day$(if day < 10 then "0" else "")$(Num.toStr day).txt"

loadData : U64 -> Task Str _
loadData = \day ->
    day
    |> dataPath
    |> File.readUtf8
    |> Task.mapErr UnableToReadFile

solutions : List (Str -> Str, Str -> Str)
solutions = [
    (\i -> i,\i -> i),
]

runSolution : (Str -> Str), U64, Str -> Task {} []_
runSolution = \solution, index, input ->
    Stdout.line! "Part $(Num.toStr index):"
    startTime = Utc.now!
    result = solution input
    endTime = Utc.now!
    delta = Utc.deltaAsMillis startTime endTime |> Num.toStr
    Stdout.line! "$(result) ($(delta)ms)"

checkDay : U64 -> Task {} [InvalidDay Str]_
checkDay = \day ->
    if day < 1 || day > 25 then Task.err (InvalidDay "Must be between 1 and 25") else Task.ok {}

runDay : Str -> Task {} []_
runDay = \dayArg ->
    day = Str.toU64 dayArg |> Task.fromResult!
    checkDay! day
    Stdout.line! "Day $(Num.toStr day)"
    input = loadData! day
    (part1, part2) = List.get solutions (day - 1) |> Task.fromResult!
    runSolution! part1 1 input
    runSolution! part2 2 input

main =
    args = Arg.list! {}
    daysStr = if List.len args < 2 then List.range { start: At 1, end: At 25 } |> List.map Num.toStr else args |> List.dropFirst 1
    _ = daysStr |> List.map runDay |> List.reverse |> Task.sequence!
    Task.ok {}
