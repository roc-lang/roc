interface InternalProgram
    exposes [InternalProgram, noArgs, withNonUnicodeArgs, toEffect]
    imports [InternalTask.{ Task }, InternalProgram.{ InternalProgram }]

InternalProgram := List U8 -> Effect (Result {} [])

noArgs : Task {} [] * -> InternalProgram
noArgs = \task ->
    # TODO give the host a different variant, so it doesn't bother translating args to RocStr
    @InternalProgram \_ -> InternalTask.toEffect task

withNonUnicodeArgs : (List (List U8) -> Task {} [] *) -> InternalProgram
withNonUnicodeArgs = \toTask ->
    @InternalProgram \bytes -> InternalTask.toEffect (toTask bytes)

toEffect : InternalProgram, List U8 -> Effect U8
toEffect = \InternalProgram @fn, args ->
    when fn args is
        Ok exitCode -> exitCode
        Err _ -> 0 # TODO this can never happen!