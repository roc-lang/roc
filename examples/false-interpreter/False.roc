#!/usr/bin/env roc
app "false"
    packages { base: "platform" }
    imports [ base.Task.{ Task }, base.Stdout, base.Stdin, Context.{ Context }, Variable.{ Variable } ]
    provides [ main ] to base

# An interpreter for the False programming language: https://strlen.com/false-language/
# This is just a silly example to test this variety of program.
# In general think of this as a program that parses a number of files and prints some output.
# It has some extra contraints:
# 1) The input files are considered too large to just read in at once. Instead it is read via buffer or line.
# 2) The output is also considered too large to generate in memory. It must be printed as we go via buffer or line.

# I think one of the biggest issues with this implementation is that it doesn't return the the platform frequently enough.
# What I mean by that is we build a chain of all Tasks period and return that to the host.
# In something like the elm architecture you return a single step with one Task.
# The huge difference here is when it comes to things like stack overflows.
# In an imperative language, a few of these peices would be in while loops and it would basically never overflow.
# This implementation is easy to overflow, either make the input long enough or make a false while loop run long enough.
# I assume all of the Task.awaits are the cause of this, but I am not 100% sure.

InterpreterErrors : [ BadUtf8, DivByZero, EmptyStack, InvalidBooleanValue, InvalidChar Str, NoLambdaOnStack, NoNumberOnStack Context.Data, NoVariableOnStack, OutOfBounds, UnexpectedEndOfData ]

main : List Str -> Task {} []
main = \filenames ->
    filenames
        |> List.walk  (\filename, task  -> Task.await task (\_ -> (interpretFile filename))) (Task.succeed {})
        |> Task.await (\_ -> Stdout.line "Completed all tasks successfully")
        |> Task.onFail (\StringErr e -> Stdout.line "Ran into problem:\n\(e)\n")
    

interpretFile : Str -> Task {} [ StringErr Str ]
interpretFile = \filename ->
    {} <- Task.await (Stdout.line "\nInterpreting file: \(filename)\n")
    ctx <- Context.with filename
    result <- Task.attempt (interpretContext ctx)
    when result is
        Ok _ ->
            Stdout.line "\n\nDone\n"
        Err BadUtf8 ->
            Task.fail (StringErr "Failed to convert string from Utf8 bytes")
        Err DivByZero ->
            Task.fail (StringErr "Division by zero")
        Err EmptyStack ->
            Task.fail (StringErr "Tried to pop a value off of the stack when it was empty")
        Err InvalidBooleanValue ->
            Task.fail (StringErr "Ran into an invalid boolean that was neither false (0) or true (-1)")
        Err (InvalidChar char) ->
            Task.fail (StringErr "Ran into an invalid character with ascii code: \(char)")
        Err NoLambdaOnStack ->
            Task.fail (StringErr "Tried to run a lambda when no lambda was on the stack")
        Err (NoNumberOnStack alt) ->
            when alt is
                Lambda _ ->
                    Task.fail (StringErr "Tried to load a number when no number was on the stack instead got a lambda")
                Var _ ->
                    Task.fail (StringErr "Tried to load a number when no number was on the stack instead got a variable")
                Number _ ->
                    Task.fail (StringErr "Tried to load a number when no number was on the stack instead got a ?number?")
        Err NoVariableOnStack ->
            Task.fail (StringErr "Tried to load a variable when no variable was on the stack")
        Err OutOfBounds ->
            Task.fail (StringErr "Tried to load from an offset that was outside of the stack")
        Err UnexpectedEndOfData ->
            Task.fail (StringErr "Hit end of data while still parsing something")

isDigit : U8 -> Bool
isDigit = \char ->
    char >= 0x30 # `0`
    && char <= 0x39 # `0`

isWhitespace : U8 -> Bool
isWhitespace = \char ->
    char == 0xA # new line
    || char == 0xB # carriage return
    || char == 0x20 # space
    || char == 0x9 # tab

interpretContext : Context -> Task Context InterpreterErrors
interpretContext = \ctx ->
    # If I move this definition below interpretContext it breaks.
    runLambda = \baseCtx, bytes ->
        lambdaCtx = {baseCtx & data: None, index: 0, buf: bytes}
        afterLambdaCtx <- Task.await (interpretContext lambdaCtx)
        # Merge lambda changes back into ctx.
        Task.succeed {baseCtx & stack: afterLambdaCtx.stack, vars: afterLambdaCtx.vars}

    runWhile = \baseCtx, cond, body ->
        afterCondCtx <- Task.await (runLambda baseCtx cond)
        when popNumber afterCondCtx is
            Ok (T popCtx 0) ->
                Task.succeed popCtx
            Ok (T popCtx _) ->
                afterBodyCtx <- Task.await (runLambda popCtx body)
                runWhile afterBodyCtx cond body
            Err e -> Task.fail e

    result <- Task.attempt (Context.getChar ctx)
    #{} <- Task.await (Stdout.raw (Context.toStr ctx))
    when result is
        Ok (T val newCtx) ->
            when val is 
                0x21 -> # `!` execute lambda 
                    # I couldn't seem to move this into it's own function otherwise the compiler would get angry.
                    when popLambda (Context.consumeChar newCtx) is
                        Ok (T popCtx bytes) ->
                            afterLambdaCtx <- Task.await (runLambda popCtx bytes)
                            interpretContext afterLambdaCtx
                        Err e ->
                            Task.fail e
                0x3F -> # `?` if
                    result2 =
                        (T popCtx1 bytes) <- Result.after (popLambda (Context.consumeChar newCtx))
                        (T popCtx2 n1) <- Result.after (popNumber popCtx1)
                        Ok (T3 popCtx2 n1 bytes)
                    when result2 is
                        Ok (T3 popCtx2 0 _) -> interpretContext popCtx2
                        Ok (T3 popCtx2 _ bytes) ->
                            afterLambdaCtx <- Task.await (runLambda popCtx2 bytes)
                            interpretContext afterLambdaCtx
                        Err e -> Task.fail e
                0x23 -> # `#` while
                    result2 =
                        (T popCtx1 body) <- Result.after (popLambda (Context.consumeChar newCtx))
                        (T popCtx2 cond) <- Result.after (popLambda popCtx1)
                        Ok (T3 popCtx2 cond body)
                    when result2 is
                        Ok (T3 popCtx2 cond body) ->
                            afterWhileCtx <- Task.await (runWhile popCtx2 cond body)
                            interpretContext afterWhileCtx
                        Err e -> Task.fail e
                0x24 -> # `$` dup
                    when List.last newCtx.stack is
                        Ok dupItem ->
                            interpretContext (Context.pushStack (Context.consumeChar newCtx) dupItem)
                        _ ->
                            Task.fail EmptyStack
                0x25 -> # `%` drop
                    consumeCtx = (Context.consumeChar newCtx)
                    when Context.popStack consumeCtx is
                        # Dropping with an empyt stack, all results here are fine
                        Ok (T popCtx _) ->
                            interpretContext popCtx
                        Err _ ->
                            interpretContext consumeCtx
                0x5C -> # `\` swap
                    result2 =
                        (T popCtx1 n1) <- Result.after (Context.popStack (Context.consumeChar newCtx))
                        (T popCtx2 n2) <- Result.after (Context.popStack popCtx1)
                        Ok (Context.pushStack (Context.pushStack popCtx2 n1) n2)
                    when result2 is
                        Ok a -> interpretContext a
                        # Being explicit with error type is required to stop the need to propogate the error parameters to Context.popStack
                        Err EmptyStack -> Task.fail EmptyStack
                0x40 -> # `@` rot
                    result2 =
                        (T popCtx1 n1) <- Result.after (Context.popStack (Context.consumeChar newCtx))
                        (T popCtx2 n2) <- Result.after (Context.popStack popCtx1)
                        (T popCtx3 n3) <- Result.after (Context.popStack popCtx2)
                        Ok (Context.pushStack (Context.pushStack (Context.pushStack popCtx3 n2) n1) n3)
                    when result2 is
                        Ok a -> interpretContext a
                        # Being explicit with error type is required to stop the need to propogate the error parameters to Context.popStack
                        Err EmptyStack -> Task.fail EmptyStack
                0xC3 -> # `ø` pick or `ß` flush
                    # this is actually 2 bytes, 0xC3 0xB8
                    # or it is 0xC3 0x9F
                    result2 <- Task.attempt (Context.getChar (Context.consumeChar newCtx))
                    when result2 is
                        Ok (T 0xB8 newCtx2) ->
                            result3 =
                                (T popCtx index) <- Result.after (popNumber (Context.consumeChar newCtx2))
                                # I think Num.abs is too restrictive, it should be able to produce a natural number, but it seem to be restricted to signed numbers.
                                size = (List.len popCtx.stack) - 1
                                offset = (Num.intCast size) - index
                                if offset >= 0 then
                                    stackVal <- Result.after (List.get popCtx.stack (Num.intCast offset))
                                    Ok (Context.pushStack popCtx stackVal)
                                else
                                    Err OutOfBounds
                            when result3 is
                                Ok a -> interpretContext a
                                Err e -> Task.fail e
                        Ok (T 0x9F newCtx2) ->
                            # This is supposed to flush io buffers. We don't buffer, so it does nothing
                            interpretContext (Context.consumeChar newCtx2)
                        Ok (T x _) ->
                            data = Str.fromInt (Num.intCast x)
                            Task.fail (InvalidChar data)
                        Err EndOfData ->
                            Task.fail UnexpectedEndOfData
                0x4F -> # `O` also treat this as pick for easier script writing
                    result2 =
                        (T popCtx index) <- Result.after (popNumber (Context.consumeChar newCtx))
                        # I think Num.abs is too restrictive, it should be able to produce a natural number, but it seem to be restricted to signed numbers.
                        size = (List.len popCtx.stack) - 1
                        offset = (Num.intCast size) - index
                        if offset >= 0 then
                            stackVal <- Result.after (List.get popCtx.stack (Num.intCast offset))
                            Ok (Context.pushStack popCtx stackVal)
                        else
                            Err OutOfBounds
                    when result2 is
                        Ok a -> interpretContext a
                        Err e -> Task.fail e
                0x42 -> # `B` also treat this as flush for easier script writing
                    # This is supposed to flush io buffers. We don't buffer, so it does nothing
                    interpretContext (Context.consumeChar newCtx)
                0x27 -> # `'` load next char
                    result2 <- Task.attempt (Context.getChar (Context.consumeChar newCtx))
                    when result2 is
                        Ok (T x newCtx2) ->
                            interpretContext (Context.pushStack (Context.consumeChar newCtx2) (Number (Num.intCast x)))
                        Err EndOfData ->
                            Task.fail UnexpectedEndOfData
                0x2B -> # `+` add
                    afterOpCtx <- Task.await (binaryOp (Context.consumeChar newCtx) Num.addWrap)
                    interpretContext afterOpCtx
                0x2D -> # `-` sub
                    afterOpCtx <- Task.await (binaryOp (Context.consumeChar newCtx) Num.subWrap)
                    interpretContext afterOpCtx
                0x2A -> # `*` mul
                    afterOpCtx <- Task.await (binaryOp (Context.consumeChar newCtx) Num.mulWrap)
                    interpretContext afterOpCtx
                0x2F -> # `/` div
                    # Due to possible division by zero error, this must be handled specially.
                    divResult =
                        (T popCtx1 numR) <- Result.after (popNumber (Context.consumeChar newCtx))
                        (T popCtx2 numL) <- Result.after (popNumber popCtx1)
                        res <- Result.after (Num.divFloor numL numR)
                        Ok (Context.pushStack popCtx2 (Number res))
                    when divResult is
                        Ok resCtx ->
                            interpretContext resCtx
                        Err e ->
                            Task.fail e
                0x26 -> # `&` bitwise and
                    afterOpCtx <- Task.await (binaryOp (Context.consumeChar newCtx) Num.bitwiseAnd)
                    interpretContext afterOpCtx
                0x7C -> # `|` bitwise or
                    afterOpCtx <- Task.await (binaryOp (Context.consumeChar newCtx) Num.bitwiseOr)
                    interpretContext afterOpCtx
                0x3D -> # `=` equals
                    afterOpCtx <- Task.await (binaryOp (Context.consumeChar newCtx) (\a, b ->
                        if a == b then
                            -1
                        else
                            0
                        ))
                    interpretContext afterOpCtx
                0x3E -> # `>` greater than
                    afterOpCtx <- Task.await (binaryOp (Context.consumeChar newCtx) (\a, b ->
                        if a > b then
                            -1
                        else
                            0
                        ))
                    interpretContext afterOpCtx
                0x5F -> # `_` negate
                    afterOpCtx <- Task.await (unaryOp (Context.consumeChar newCtx) Num.neg)
                    interpretContext afterOpCtx
                0x7E -> # `~` bitwise not
                    afterOpCtx <- Task.await (unaryOp (Context.consumeChar newCtx) (\x -> Num.bitwiseXor x -1)) # xor with -1 should be bitwise not
                    interpretContext afterOpCtx
                0x2C -> # `,` write char
                    when popNumber (Context.consumeChar newCtx) is
                        Ok (T popCtx num) ->
                            when Str.fromUtf8 [Num.intCast num] is
                                Ok str ->
                                    {} <- Task.await (Stdout.raw str)
                                    interpretContext popCtx
                                Err _ ->
                                    Task.fail BadUtf8
                        Err e ->
                            Task.fail e
                0x2E -> # `.` write int
                    when popNumber (Context.consumeChar newCtx) is
                        Ok (T popCtx num) ->
                            {} <- Task.await (Stdout.raw (Str.fromInt num))
                            interpretContext popCtx
                        Err e ->
                            Task.fail e
                0x5E -> # `^` read char as int
                    char <- Task.await Stdin.char
                    interpretContext (Context.pushStack (Context.consumeChar newCtx) (Number (Num.intCast char)))
                0x3A -> # `:` store to variable
                    result2 =
                        (T popCtx1 var) <- Result.after (popVariable (Context.consumeChar newCtx))
                        (T popCtx2 n1) <- Result.after (Context.popStack popCtx1)
                        Ok {popCtx2 & vars: List.set popCtx2.vars (Variable.toIndex var) n1}
                    when result2 is
                        Ok a -> interpretContext a
                        Err e -> Task.fail e
                0x3B -> # `;` load from variable
                    result2 =
                        (T popCtx var) <- Result.after (popVariable (Context.consumeChar newCtx))
                        elem <- Result.after (List.get popCtx.vars (Variable.toIndex var))
                        Ok (Context.pushStack popCtx elem)
                    when result2 is
                        Ok a -> interpretContext a
                        Err e -> Task.fail e
                0x22 -> # `"` string start
                    afterStringCtx <- Task.await (printString (Context.consumeChar newCtx))
                    interpretContext afterStringCtx
                0x5B -> # `[` lambda start
                    afterLambdaCtx <- Task.await (pushLambda (Context.consumeChar newCtx))
                    interpretContext afterLambdaCtx
                0x7B -> # `{` comment start
                    afterCommentCtx <- Task.await (consumeComment (Context.consumeChar newCtx))
                    interpretContext afterCommentCtx
                x if isDigit x -> # number start
                    afterNumberCtx <- Task.await (pushNumber newCtx)
                    interpretContext afterNumberCtx
                x if isWhitespace x -> interpretContext (Context.consumeChar newCtx)
                x ->
                    when (Variable.fromUtf8 x) is # letters are variable names
                        Ok var ->
                            interpretContext (Context.pushStack (Context.consumeChar newCtx) (Var var))
                        Err _ ->
                            data = Str.fromInt (Num.intCast x)
                            Task.fail (InvalidChar data)
        Err EndOfData ->
            # Computation complete.
            Task.succeed ctx

unaryOp: Context, (I64 -> I64) -> Task Context InterpreterErrors
unaryOp = \ctx, op ->
    result =
        (T popCtx num) <- Result.after (popNumber ctx)
        Ok (Context.pushStack popCtx (Number (op num)))
    when result is
        Ok resCtx ->
            Task.succeed resCtx
        Err e ->
            Task.fail e

binaryOp: Context, (I64, I64 -> I64) -> Task Context InterpreterErrors
binaryOp = \ctx, op ->
    result =
        (T popCtx1 numR) <- Result.after (popNumber ctx)
        (T popCtx2 numL) <- Result.after (popNumber popCtx1)
        Ok (Context.pushStack popCtx2 (Number (op numL numR)))
    when result is
        Ok resCtx ->
            Task.succeed resCtx
        Err e ->
            Task.fail e


popNumber: Context -> Result [T Context I64] InterpreterErrors
popNumber = \ctx ->
    when Context.popStack ctx is
        Ok (T popCtx (Number num)) ->
            Ok (T popCtx num)
        Ok (T _ alt) ->
            Err (NoNumberOnStack alt)
        Err EmptyStack ->
            Err EmptyStack

popLambda: Context -> Result [T Context (List U8)] InterpreterErrors
popLambda = \ctx ->
    when Context.popStack ctx is
        Ok (T popCtx (Lambda bytes)) ->
            Ok (T popCtx bytes)
        Ok _ ->
            Err NoLambdaOnStack
        Err EmptyStack ->
            Err EmptyStack

popVariable: Context -> Result [T Context Variable] InterpreterErrors
popVariable = \ctx ->
    when Context.popStack ctx is
        Ok (T popCtx (Var var)) ->
            Ok (T popCtx var)
        Ok _ ->
            Err NoVariableOnStack
        Err EmptyStack ->
            Err EmptyStack

# This has to be a bit more complex than other options because they can be nested.
# Also, it puts the lambda on the stack
pushLambda: Context -> Task Context InterpreterErrors
pushLambda = \ctx ->
    (T newCtx bytes) <- Task.await (pushLambdaHelper ctx [] 0)
    Task.succeed (Context.pushStack newCtx (Lambda bytes))

pushLambdaHelper: Context, List U8, U64 -> Task [ T Context (List U8) ] InterpreterErrors
pushLambdaHelper = \ctx, base, depth ->
    result <- Task.attempt (Context.getChar ctx)
    when result is
        Ok (T val newCtx) ->
            if val == 0x5B then # start of a nested lambda `[`
                pushLambdaHelper (Context.consumeChar newCtx) (List.append base val) (depth + 1)
            else if val != 0x5D then # not lambda end `]`
                pushLambdaHelper (Context.consumeChar newCtx) (List.append base val) depth
            else if depth == 0 then
                Task.succeed (T (Context.consumeChar newCtx) base)
            else # Still need to finish nested lambda
                pushLambdaHelper (Context.consumeChar newCtx) (List.append base val) (depth - 1)
        Err EndOfData ->
            Task.fail UnexpectedEndOfData

pushNumber: Context -> Task Context InterpreterErrors
pushNumber = \ctx ->
    pushNumberHelper ctx 0

pushNumberHelper: Context, I64 -> Task Context InterpreterErrors
pushNumberHelper = \ctx, accum ->
    result <- Task.attempt (Context.getChar ctx)
    when result is
        Ok (T val newCtx) ->
            if isDigit val then
                pushNumberHelper (Context.consumeChar newCtx) (accum * 10 + (Num.intCast (val - 0x30)))
            else 
                # Push the number onto the stack.
                Task.succeed (Context.pushStack newCtx (Number accum))
        Err EndOfData ->
            # Unlike many other cases, this is valid.
            # Still push the number to the stack.
            Task.succeed (Context.pushStack ctx (Number accum))


# It seems that I need to specify all error types in every function for some reason.
# I feel like this should just need to say UnexpectedEndOfData.
printString : Context -> Task Context InterpreterErrors
printString = \ctx ->
    (T afterStringCtx bytes) <- Task.await (printStringHelper ctx [])
    when Str.fromUtf8 bytes is
        Ok str ->
            {} <- Task.await (Stdout.raw str)
            Task.succeed afterStringCtx
        Err _ ->
            Task.fail BadUtf8

printStringHelper : Context, List U8 -> Task [ T Context (List U8) ] InterpreterErrors
printStringHelper = \ctx, base ->
    result <- Task.attempt (Context.getChar ctx)
    when result is
        Ok (T val newCtx) ->
            if val != 0x22 then # not string end `"`
                printStringHelper (Context.consumeChar newCtx) (List.append base val)
            else
                Task.succeed (T (Context.consumeChar newCtx) base )
        Err EndOfData ->
            Task.fail UnexpectedEndOfData

consumeComment : Context -> Task Context InterpreterErrors
consumeComment = \ctx ->
    result <- Task.attempt (Context.getChar ctx)
    when result is
        Ok (T val newCtx) ->
            if val != 0x7D then # not comment end `}`
                consumeComment (Context.consumeChar newCtx)
            else
                Task.succeed (Context.consumeChar newCtx)
        Err EndOfData ->
            Task.fail UnexpectedEndOfData
