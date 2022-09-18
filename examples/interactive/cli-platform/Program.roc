interface Program
    exposes [Program, noArgs, withArgs, withNonUnicodeArgs]
    imports [Task.{ Task }, InternalProgram.{ InternalProgram }]

Program : InternalProgram

noArgs : Task U8 [] * -> Program
noArgs = InternalProgram.noArgs

withArgs : (List Str -> Task U8 [] *) -> Program
withArgs = \fromStrArgs ->
    InternalProgram.withNonUnicodeArgs \bytes ->
        bytes
        |> List.map Str.display # TODO implement Str.display
        |> fromStrArgs
        |> InternalTask.toEffect

withNonUnicodeArgs : (List (List U8) -> Task U8 [] *) -> Program
withNonUnicodeArgs = \toTask ->
    InternalProgram.withNonUnicodeArgs \bytes ->
        InternalTask.toEffect (toTask bytes)