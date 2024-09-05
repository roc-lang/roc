app [main] { pf: platform "platform/main.roc" }

import pf.Stdout
import pf.Stdin
import Context exposing [Context]
import Variable exposing [Variable]

# An interpreter for the False programming language: https://strlen.com/false-language/
# This is just a silly example to test this variety of program.
# In general think of this as a program that parses a number of files and prints some output.
# It has some extra constraints:
# 1) The input files are considered too large to just read in at once. Instead it is read via buffer or line.
# 2) The output is also considered too large to generate in memory. It must be printed as we go via buffer or line.
# I think one of the biggest issues with this implementation is that it doesn't return to the platform frequently enough.
# What I mean by that is we build a chain of all Tasks period and return that to the host.
# In something like the elm architecture you return a single step with one Task.
# The huge difference here is when it comes to things like stack overflows.
# In an imperative language, a few of these pieces would be in while loops and it would basically never overflow.
# This implementation is easy to overflow, either make the input long enough or make a false while loop run long enough.
# I assume all of the Task.awaits are the cause of this, but I am not 100% sure.
InterpreterErrors : [BadUtf8, DivByZero, EmptyStack, InvalidBooleanValue, InvalidChar Str, MaxInputNumber, NoLambdaOnStack, NoNumberOnStack, NoVariableOnStack, NoScope, OutOfBounds, UnexpectedEndOfData]

main : Str -> Task {} []
main = \filename ->
    interpretFile filename
    |> Task.onErr \StringErr e -> Stdout.line "Ran into problem:\n$(e)\n"

interpretFile : Str -> Task {} [StringErr Str]
interpretFile = \filename ->
    Context.with filename \ctx ->
        result = interpretCtx ctx |> Task.result!
        when result is
            Ok _ ->
                Task.ok {}

            Err BadUtf8 ->
                Task.err (StringErr "Failed to convert string from Utf8 bytes")

            Err DivByZero ->
                Task.err (StringErr "Division by zero")

            Err EmptyStack ->
                Task.err (StringErr "Tried to pop a value off of the stack when it was empty")

            Err InvalidBooleanValue ->
                Task.err (StringErr "Ran into an invalid boolean that was neither false (0) or true (-1)")

            Err (InvalidChar char) ->
                Task.err (StringErr "Ran into an invalid character with ascii code: $(char)")

            Err MaxInputNumber ->
                Task.err (StringErr "Like the original false compiler, the max input number is 320,000")

            Err NoLambdaOnStack ->
                Task.err (StringErr "Tried to run a lambda when no lambda was on the stack")

            Err NoNumberOnStack ->
                Task.err (StringErr "Tried to run a number when no number was on the stack")

            Err NoVariableOnStack ->
                Task.err (StringErr "Tried to load a variable when no variable was on the stack")

            Err NoScope ->
                Task.err (StringErr "Tried to run code when not in any scope")

            Err OutOfBounds ->
                Task.err (StringErr "Tried to load from an offset that was outside of the stack")

            Err UnexpectedEndOfData ->
                Task.err (StringErr "Hit end of data while still parsing something")

isDigit : U8 -> Bool
isDigit = \char ->
    char
    >= 0x30 # `0`
    && char
    <= 0x39 # `0`
isWhitespace : U8 -> Bool
isWhitespace = \char ->
    char
    == 0xA # new line
    || char
    == 0xD # carriage return
    || char
    == 0x20 # space
    || char
    == 0x9 # tab
interpretCtx : Context -> Task Context InterpreterErrors
interpretCtx = \ctx ->
    Task.loop ctx interpretCtxLoop

interpretCtxLoop : Context -> Task [Step Context, Done Context] InterpreterErrors
interpretCtxLoop = \ctx ->
    when ctx.state is
        Executing if Context.inWhileScope ctx ->
            # Deal with the current while loop potentially looping.
            last = (List.len ctx.scopes - 1)

            when List.get ctx.scopes last is
                Ok scope ->
                    when scope.whileInfo is
                        Some { state: InCond, body, cond } ->
                            # Just ran condition. Check the top of stack to see if body should run.
                            when popNumber ctx is
                                Ok (T popCtx n) ->
                                    if n == 0 then
                                        newScope = { scope & whileInfo: None }

                                        Task.ok (Step { popCtx & scopes: List.set ctx.scopes last newScope })
                                    else
                                        newScope = { scope & whileInfo: Some { state: InBody, body, cond } }

                                        Task.ok (Step { popCtx & scopes: List.append (List.set ctx.scopes last newScope) { data: None, buf: body, index: 0, whileInfo: None } })

                                Err e ->
                                    Task.err e

                        Some { state: InBody, body, cond } ->
                            # Just rand the body. Run the condition again.
                            newScope = { scope & whileInfo: Some { state: InCond, body, cond } }

                            Task.ok (Step { ctx & scopes: List.append (List.set ctx.scopes last newScope) { data: None, buf: cond, index: 0, whileInfo: None } })

                        None ->
                            Task.err NoScope

                Err OutOfBounds ->
                    Task.err NoScope

        Executing ->
            # Stdout.line! (Context.toStr ctx)
            result = Context.getChar ctx |> Task.result!
            when result is
                Ok (T val newCtx) ->
                    execCtx = stepExecCtx! newCtx val
                    Task.ok (Step execCtx)

                Err NoScope ->
                    Task.err NoScope

                Err EndOfData ->
                    # Computation complete for this scope.
                    # Drop a scope.
                    dropCtx = { ctx & scopes: List.dropAt ctx.scopes (List.len ctx.scopes - 1) }

                    # If no scopes left, all execution complete.
                    if List.isEmpty dropCtx.scopes then
                        Task.ok (Done dropCtx)
                    else
                        Task.ok (Step dropCtx)

        InComment ->
            result = Context.getChar ctx |> Task.result!
            when result is
                Ok (T val newCtx) ->
                    if val == 0x7D then
                        # `}` end of comment
                        Task.ok (Step { newCtx & state: Executing })
                    else
                        Task.ok (Step { newCtx & state: InComment })

                Err NoScope ->
                    Task.err NoScope

                Err EndOfData ->
                    Task.err UnexpectedEndOfData

        InNumber accum ->
            result = Context.getChar ctx |> Task.result!
            when result is
                Ok (T val newCtx) ->
                    if isDigit val then
                        # still in the number
                        # i32 multiplication is kinda broken because it implicitly seems to want to upcast to i64.
                        # so like should be (i32, i32) -> i32, but seems to be (i32, i32) -> i64
                        # so this is make i64 mul by 10 then convert back to i32.
                        nextAccum = (10 * Num.intCast accum) + Num.intCast (val - 0x30)

                        Task.ok (Step { newCtx & state: InNumber (Num.intCast nextAccum) })
                    else
                        # outside of number now, this needs to be executed.
                        pushCtx = Context.pushStack newCtx (Number accum)

                        execCtx = stepExecCtx! { pushCtx & state: Executing } val
                        Task.ok (Step execCtx)

                Err NoScope ->
                    Task.err NoScope

                Err EndOfData ->
                    Task.err UnexpectedEndOfData

        InString bytes ->
            result = Context.getChar ctx |> Task.result!
            when result is
                Ok (T val newCtx) ->
                    if val == 0x22 then
                        # `"` end of string
                        when Str.fromUtf8 bytes is
                            Ok str ->
                                Stdout.raw! str
                                Task.ok (Step { newCtx & state: Executing })

                            Err _ ->
                                Task.err BadUtf8
                    else
                        Task.ok (Step { newCtx & state: InString (List.append bytes val) })

                Err NoScope ->
                    Task.err NoScope

                Err EndOfData ->
                    Task.err UnexpectedEndOfData

        InLambda depth bytes ->
            result = Context.getChar ctx |> Task.result!
            when result is
                Ok (T val newCtx) ->
                    if val == 0x5B then
                        # start of a nested lambda `[`
                        Task.ok (Step { newCtx & state: InLambda (depth + 1) (List.append bytes val) })
                    else if val == 0x5D then
                        # `]` end of current lambda
                        if depth == 0 then
                            # end of all lambdas
                            Task.ok (Step (Context.pushStack { newCtx & state: Executing } (Lambda bytes)))
                        else
                            # end of nested lambda
                            Task.ok (Step { newCtx & state: InLambda (depth - 1) (List.append bytes val) })
                    else
                        Task.ok (Step { newCtx & state: InLambda depth (List.append bytes val) })

                Err NoScope ->
                    Task.err NoScope

                Err EndOfData ->
                    Task.err UnexpectedEndOfData

        InSpecialChar ->
            result = Context.getChar { ctx & state: Executing } |> Task.result!
            when result is
                Ok (T 0xB8 newCtx) ->
                    result2 =
                        (T popCtx index) = popNumber? newCtx
                        # I think Num.abs is too restrictive, it should be able to produce a natural number, but it seem to be restricted to signed numbers.
                        size = List.len popCtx.stack - 1
                        offset = Num.intCast size - index

                        if offset >= 0 then
                            stackVal = List.get? popCtx.stack (Num.intCast offset)
                            Ok (Context.pushStack popCtx stackVal)
                        else
                            Err OutOfBounds

                    when result2 is
                        Ok a -> Task.ok (Step a)
                        Err e -> Task.err e

                Ok (T 0x9F newCtx) ->
                    # This is supposed to flush io buffers. We don't buffer, so it does nothing
                    Task.ok (Step newCtx)

                Ok (T x _) ->
                    data = Num.toStr (Num.intCast x)

                    Task.err (InvalidChar data)

                Err NoScope ->
                    Task.err NoScope

                Err EndOfData ->
                    Task.err UnexpectedEndOfData

        LoadChar ->
            result = Context.getChar { ctx & state: Executing } |> Task.result!
            when result is
                Ok (T x newCtx) ->
                    Task.ok (Step (Context.pushStack newCtx (Number (Num.intCast x))))

                Err NoScope ->
                    Task.err NoScope

                Err EndOfData ->
                    Task.err UnexpectedEndOfData

# If it weren't for reading stdin or writing to stdout, this could return a result.
stepExecCtx : Context, U8 -> Task Context InterpreterErrors
stepExecCtx = \ctx, char ->
    when char is
        0x21 ->
            # `!` execute lambda
            Task.fromResult
                (
                    (T popCtx bytes) = popLambda? ctx
                    Ok { popCtx & scopes: List.append popCtx.scopes { data: None, buf: bytes, index: 0, whileInfo: None } }
                )

        0x3F ->
            # `?` if
            Task.fromResult
                (
                    (T popCtx1 bytes) = popLambda? ctx
                    (T popCtx2 n1) = popNumber? popCtx1
                    if n1 == 0 then
                        Ok popCtx2
                    else
                        Ok { popCtx2 & scopes: List.append popCtx2.scopes { data: None, buf: bytes, index: 0, whileInfo: None } }
                )

        0x23 ->
            # `#` while
            Task.fromResult
                (
                    (T popCtx1 body) = popLambda? ctx
                    (T popCtx2 cond) = popLambda? popCtx1
                    last = (List.len popCtx2.scopes - 1)

                    when List.get popCtx2.scopes last is
                        Ok scope ->
                            # set the current scope to be in a while loop.
                            scopes = List.set popCtx2.scopes last { scope & whileInfo: Some { cond: cond, body: body, state: InCond } }

                            # push a scope to execute the condition.
                            Ok { popCtx2 & scopes: List.append scopes { data: None, buf: cond, index: 0, whileInfo: None } }

                        Err OutOfBounds ->
                            Err NoScope
                )

        0x24 ->
            # `$` dup
            # Switching this to List.last and changing the error to ListWasEmpty leads to a compiler bug.
            # Complains about the types eq not matching.
            when List.get ctx.stack (List.len ctx.stack - 1) is
                Ok dupItem -> Task.ok (Context.pushStack ctx dupItem)
                Err OutOfBounds -> Task.err EmptyStack

        0x25 ->
            # `%` drop
            when Context.popStack ctx is
                # Dropping with an empty stack, all results here are fine
                Ok (T popCtx _) -> Task.ok popCtx
                Err _ -> Task.ok ctx

        0x5C ->
            # `\` swap
            result2 =
                (T popCtx1 n1) = Context.popStack? ctx
                (T popCtx2 n2) = Context.popStack? popCtx1
                Ok (Context.pushStack (Context.pushStack popCtx2 n1) n2)

            when result2 is
                Ok a ->
                    Task.ok a

                # Being explicit with error type is required to stop the need to propogate the error parameters to Context.popStack
                Err EmptyStack ->
                    Task.err EmptyStack

        0x40 ->
            # `@` rot
            result2 =
                (T popCtx1 n1) = Context.popStack? ctx
                (T popCtx2 n2) = Context.popStack? popCtx1
                (T popCtx3 n3) = Context.popStack? popCtx2
                Ok (Context.pushStack (Context.pushStack (Context.pushStack popCtx3 n2) n1) n3)

            when result2 is
                Ok a ->
                    Task.ok a

                # Being explicit with error type is required to stop the need to propogate the error parameters to Context.popStack
                Err EmptyStack ->
                    Task.err EmptyStack

        0xC3 ->
            # `ø` pick or `ß` flush
            # these are actually 2 bytes, 0xC3 0xB8 or  0xC3 0x9F
            # requires special parsing
            Task.ok { ctx & state: InSpecialChar }

        0x4F ->
            # `O` also treat this as pick for easier script writing
            Task.fromResult
                (
                    (T popCtx index) = popNumber? ctx
                    # I think Num.abs is too restrictive, it should be able to produce a natural number, but it seem to be restricted to signed numbers.
                    size = List.len popCtx.stack - 1
                    offset = Num.intCast size - index

                    if offset >= 0 then
                        stackVal = List.get? popCtx.stack (Num.intCast offset)
                        Ok (Context.pushStack popCtx stackVal)
                    else
                        Err OutOfBounds
                )

        0x42 ->
            # `B` also treat this as flush for easier script writing
            # This is supposed to flush io buffers. We don't buffer, so it does nothing
            Task.ok ctx

        0x27 ->
            # `'` load next char
            Task.ok { ctx & state: LoadChar }

        0x2B ->
            # `+` add
            Task.fromResult (binaryOp ctx Num.addWrap)

        0x2D ->
            # `-` sub
            Task.fromResult (binaryOp ctx Num.subWrap)

        0x2A ->
            # `*` mul
            Task.fromResult (binaryOp ctx Num.mulWrap)

        0x2F ->
            # `/` div
            # Due to possible division by zero error, this must be handled specially.
            Task.fromResult
                (
                    (T popCtx1 numR) = popNumber? ctx
                    (T popCtx2 numL) = popNumber? popCtx1
                    res = Num.divTruncChecked? numL numR
                    Ok (Context.pushStack popCtx2 (Number res))
                )

        0x26 ->
            # `&` bitwise and
            Task.fromResult (binaryOp ctx Num.bitwiseAnd)

        0x7C ->
            # `|` bitwise or
            Task.fromResult (binaryOp ctx Num.bitwiseOr)

        0x3D ->
            # `=` equals
            Task.fromResult
                (
                    binaryOp ctx \a, b ->
                        if a == b then
                            -1
                        else
                            0
                )

        0x3E ->
            # `>` greater than
            Task.fromResult
                (
                    binaryOp ctx \a, b ->
                        if a > b then
                            -1
                        else
                            0
                )

        0x5F ->
            # `_` negate
            Task.fromResult (unaryOp ctx Num.neg)

        0x7E ->
            # `~` bitwise not
            Task.fromResult (unaryOp ctx (\x -> Num.bitwiseXor x -1)) # xor with -1 should be bitwise not

        0x2C ->
            # `,` write char
            when popNumber ctx is
                Ok (T popCtx num) ->
                    when Str.fromUtf8 [Num.intCast num] is
                        Ok str ->
                            Stdout.raw! str
                            Task.ok popCtx

                        Err _ ->
                            Task.err BadUtf8

                Err e ->
                    Task.err e

        0x2E ->
            # `.` write int
            when popNumber ctx is
                Ok (T popCtx num) ->
                    Stdout.raw! (Num.toStr (Num.intCast num))
                    Task.ok popCtx

                Err e ->
                    Task.err e

        0x5E ->
            # `^` read char as int
            in = Stdin.char! {}
            if in == 255 then
                # max char sent on EOF. Change to -1
                Task.ok (Context.pushStack ctx (Number -1))
            else
                Task.ok (Context.pushStack ctx (Number (Num.intCast in)))

        0x3A ->
            # `:` store to variable
            Task.fromResult
                (
                    (T popCtx1 var) = popVariable? ctx
                    # The Result.mapErr on the next line maps from EmptyStack in Context.roc to the full InterpreterErrors union here.
                    (T popCtx2 n1) = Result.mapErr? (Context.popStack popCtx1) (\EmptyStack -> EmptyStack)
                    Ok { popCtx2 & vars: List.set popCtx2.vars (Variable.toIndex var) n1 }
                )

        0x3B ->
            # `;` load from variable
            Task.fromResult
                (
                    (T popCtx var) = popVariable? ctx
                    elem = List.get? popCtx.vars (Variable.toIndex var)
                    Ok (Context.pushStack popCtx elem)
                )

        0x22 ->
            # `"` string start
            Task.ok { ctx & state: InString [] }

        0x5B ->
            # `"` string start
            Task.ok { ctx & state: InLambda 0 [] }

        0x7B ->
            # `{` comment start
            Task.ok { ctx & state: InComment }

        x if isDigit x ->
            # number start
            Task.ok { ctx & state: InNumber (Num.intCast (x - 0x30)) }

        x if isWhitespace x ->
            Task.ok ctx

        x ->
            when Variable.fromUtf8 x is
                # letters are variable names
                Ok var ->
                    Task.ok (Context.pushStack ctx (Var var))

                Err _ ->
                    data = Num.toStr (Num.intCast x)

                    Task.err (InvalidChar data)

unaryOp : Context, (I32 -> I32) -> Result Context InterpreterErrors
unaryOp = \ctx, op ->
    (T popCtx num) = popNumber? ctx
    Ok (Context.pushStack popCtx (Number (op num)))

binaryOp : Context, (I32, I32 -> I32) -> Result Context InterpreterErrors
binaryOp = \ctx, op ->
    (T popCtx1 numR) = popNumber? ctx
    (T popCtx2 numL) = popNumber? popCtx1
    Ok (Context.pushStack popCtx2 (Number (op numL numR)))

popNumber : Context -> Result [T Context I32] InterpreterErrors
popNumber = \ctx ->
    when Context.popStack ctx is
        Ok (T popCtx (Number num)) -> Ok (T popCtx num)
        Ok _ -> Err (NoNumberOnStack)
        Err EmptyStack -> Err EmptyStack

popLambda : Context -> Result [T Context (List U8)] InterpreterErrors
popLambda = \ctx ->
    when Context.popStack ctx is
        Ok (T popCtx (Lambda bytes)) -> Ok (T popCtx bytes)
        Ok _ -> Err NoLambdaOnStack
        Err EmptyStack -> Err EmptyStack

popVariable : Context -> Result [T Context Variable] InterpreterErrors
popVariable = \ctx ->
    when Context.popStack ctx is
        Ok (T popCtx (Var var)) -> Ok (T popCtx var)
        Ok _ -> Err NoVariableOnStack
        Err EmptyStack -> Err EmptyStack
