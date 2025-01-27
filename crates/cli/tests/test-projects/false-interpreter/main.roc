app [main!] { pf: platform "platform/main.roc" }

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

InterpreterErrors : [BadUtf8, DivByZero, EmptyStack, InvalidBooleanValue, InvalidChar Str, MaxInputNumber, NoLambdaOnStack, NoNumberOnStack, NoVariableOnStack, NoScope, OutOfBounds, UnexpectedEndOfData]

main! : Str => {}
main! = \filename ->
    when interpret_file!(filename) is
        Ok({}) ->
            {}

        Err(StringErr(e)) ->
            Stdout.line!("Ran into problem:\n${e}\n")

interpret_file! : Str => Result {} [StringErr Str]
interpret_file! = \filename ->
    Context.with!(filename, \ctx ->
        result = interpret_ctx!(ctx)
        when result is
            Ok(_) ->
                Ok({})

            Err(BadUtf8) ->
                Err(StringErr("Failed to convert string from Utf8 bytes"))

            Err(DivByZero) ->
                Err(StringErr("Division by zero"))

            Err(EmptyStack) ->
                Err(StringErr("Tried to pop a value off of the stack when it was empty"))

            Err(InvalidBooleanValue) ->
                Err(StringErr("Ran into an invalid boolean that was neither false (0) or true (-1)"))

            Err(InvalidChar(char)) ->
                Err(StringErr("Ran into an invalid character with ascii code: ${char}"))

            Err(MaxInputNumber) ->
                Err(StringErr("Like the original false compiler, the max input number is 320,000"))

            Err(NoLambdaOnStack) ->
                Err(StringErr("Tried to run a lambda when no lambda was on the stack"))

            Err(NoNumberOnStack) ->
                Err(StringErr("Tried to run a number when no number was on the stack"))

            Err(NoVariableOnStack) ->
                Err(StringErr("Tried to load a variable when no variable was on the stack"))

            Err(NoScope) ->
                Err(StringErr("Tried to run code when not in any scope"))

            Err(OutOfBounds) ->
                Err(StringErr("Tried to load from an offset that was outside of the stack"))

            Err(UnexpectedEndOfData) ->
                Err(StringErr("Hit end of data while still parsing something")))

interpret_ctx! : Context => Result Context InterpreterErrors
interpret_ctx! = \ctx ->
    when interpret_ctx_loop!(ctx) is
        Ok(Step(next)) ->
            interpret_ctx!(next)

        Ok(Done(next)) ->
            Ok(next)

        Err(e) ->
            Err(e)

interpret_ctx_loop! : Context => Result [Step Context, Done Context] InterpreterErrors
interpret_ctx_loop! = \ctx ->
    when ctx.state is
        Executing if Context.in_while_scope(ctx) ->
            # Deal with the current while loop potentially looping.
            last = (List.len(ctx.scopes) - 1)

            scope = List.get(ctx.scopes, last) |> Result.map_err?(\_ -> NoScope)
            when scope.while_info is
                Some({ state: InCond, body, cond }) ->
                    # Just ran condition. Check the top of stack to see if body should run.
                    (pop_ctx, n) = pop_number?(ctx)
                    if n == 0 then
                        new_scope = { scope & while_info: None }

                        Ok(Step({ pop_ctx & scopes: List.set(ctx.scopes, last, new_scope) }))
                    else
                        new_scope = { scope & while_info: Some({ state: InBody, body, cond }) }

                        Ok(Step({ pop_ctx & scopes: List.append(List.set(ctx.scopes, last, new_scope), { data: None, buf: body, index: 0, while_info: None }) }))

                Some({ state: InBody, body, cond }) ->
                    # Just rand the body. Run the condition again.
                    new_scope = { scope & while_info: Some({ state: InCond, body, cond }) }

                    Ok(Step({ ctx & scopes: List.append(List.set(ctx.scopes, last, new_scope), { data: None, buf: cond, index: 0, while_info: None }) }))

                None ->
                    Err(NoScope)

        Executing ->
            # Stdout.line! (Context.to_str ctx)
            result = Context.get_char!(ctx)
            when result is
                Ok((val, new_ctx)) ->
                    exec_ctx = step_exec_ctx!?(new_ctx, val)
                    Ok(Step(exec_ctx))

                Err(NoScope) ->
                    Err(NoScope)

                Err(EndOfData) ->
                    # Computation complete for this scope.
                    # Drop a scope.
                    drop_ctx = { ctx & scopes: List.drop_at(ctx.scopes, (List.len(ctx.scopes) - 1)) }

                    # If no scopes left, all execution complete.
                    if List.is_empty(drop_ctx.scopes) then
                        Ok(Done(drop_ctx))
                    else
                        Ok(Step(drop_ctx))

        InComment ->
            (val, new_ctx) = Context.get_char!(ctx) |> Result.map_err?(end_unexpected)
            if val == 0x7D then
                # `}` end of comment
                Ok(Step({ new_ctx & state: Executing }))
            else
                Ok(Step({ new_ctx & state: InComment }))

        InNumber(accum) ->
            (val, new_ctx) = Context.get_char!(ctx) |> Result.map_err?(end_unexpected)
            if is_digit(val) then
                # still in the number
                # i32 multiplication is kinda broken because it implicitly seems to want to upcast to i64.
                # so like should be (i32, i32) -> i32, but seems to be (i32, i32) -> i64
                # so this is make i64 mul by 10 then convert back to i32.
                next_accum = (10 * Num.int_cast(accum)) + Num.int_cast((val - 0x30))

                Ok(Step({ new_ctx & state: InNumber(Num.int_cast(next_accum)) }))
            else
                # outside of number now, this needs to be executed.
                push_ctx = Context.push_stack(new_ctx, Number(accum))

                exec_ctx = step_exec_ctx!?({ push_ctx & state: Executing }, val)
                Ok(Step(exec_ctx))

        InString(bytes) ->
            (val, new_ctx) = Context.get_char!(ctx) |> Result.map_err?(end_unexpected)
            if val == 0x22 then
                # `"` end of string
                when Str.from_utf8(bytes) is
                    Ok(str) ->
                        Stdout.raw!(str)
                        Ok(Step({ new_ctx & state: Executing }))

                    Err(_) ->
                        Err(BadUtf8)
            else
                Ok(Step({ new_ctx & state: InString(List.append(bytes, val)) }))

        InLambda(depth, bytes) ->
            (val, new_ctx) = Context.get_char!(ctx) |> Result.map_err?(end_unexpected)
            if val == 0x5B then
                # start of a nested lambda `[`
                Ok(Step({ new_ctx & state: InLambda((depth + 1), List.append(bytes, val)) }))
            else if val == 0x5D then
                # `]` end of current lambda
                if depth == 0 then
                    # end of all lambdas
                    Ok(Step(Context.push_stack({ new_ctx & state: Executing }, Lambda(bytes))))
                else
                    # end of nested lambda
                    Ok(Step({ new_ctx & state: InLambda((depth - 1), List.append(bytes, val)) }))
            else
                Ok(Step({ new_ctx & state: InLambda(depth, List.append(bytes, val)) }))

        InSpecialChar ->
            val = Context.get_char!({ ctx & state: Executing }) |> Result.map_err?(end_unexpected)
            when val is
                (0xB8, new_ctx) ->
                    (pop_ctx, index) = pop_number?(new_ctx)
                    # I think Num.abs is too restrictive, it should be able to produce a natural number, but it seem to be restricted to signed numbers.
                    size = List.len(pop_ctx.stack) - 1
                    offset = Num.int_cast(size) - index

                    if offset >= 0 then
                        stack_val = List.get?(pop_ctx.stack, Num.int_cast(offset))
                        Ok(Step(Context.push_stack(pop_ctx, stack_val)))
                    else
                        Err(OutOfBounds)

                (0x9F, new_ctx) ->
                    # This is supposed to flush io buffers. We don't buffer, so it does nothing
                    Ok(Step(new_ctx))

                (x, _) ->
                    data = Num.to_str(Num.int_cast(x))

                    Err(InvalidChar(data))

        LoadChar ->
            (x, new_ctx) = Context.get_char!({ ctx & state: Executing }) |> Result.map_err?(end_unexpected)
            Ok(Step(Context.push_stack(new_ctx, Number(Num.int_cast(x)))))

# If it weren't for reading stdin or writing to stdout, this could return a result.
step_exec_ctx! : Context, U8 => Result Context InterpreterErrors
step_exec_ctx! = \ctx, char ->
    when char is
        0x21 ->
            # `!` execute lambda
            (pop_ctx, bytes) = pop_lambda?(ctx)
            Ok({ pop_ctx & scopes: List.append(pop_ctx.scopes, { data: None, buf: bytes, index: 0, while_info: None }) })

        0x3F ->
            # `?` if
            (pop_ctx1, bytes) = pop_lambda?(ctx)
            (pop_ctx2, n1) = pop_number?(pop_ctx1)
            if n1 == 0 then
                Ok(pop_ctx2)
            else
                Ok({ pop_ctx2 & scopes: List.append(pop_ctx2.scopes, { data: None, buf: bytes, index: 0, while_info: None }) })

        0x23 ->
            # `#` while
            (pop_ctx1, body) = pop_lambda?(ctx)
            (pop_ctx2, cond) = pop_lambda?(pop_ctx1)
            last = (List.len(pop_ctx2.scopes) - 1)

            scope = List.get(pop_ctx2.scopes, last) |> Result.map_err?(\_ -> NoScope)
            # set the current scope to be in a while loop.
            scopes = List.set(pop_ctx2.scopes, last, { scope & while_info: Some({ cond: cond, body: body, state: InCond }) })

            # push a scope to execute the condition.
            Ok({ pop_ctx2 & scopes: List.append(scopes, { data: None, buf: cond, index: 0, while_info: None }) })

        0x24 ->
            # `$` dup
            # Switching this to List.last and changing the error to ListWasEmpty leads to a compiler bug.
            # Complains about the types eq not matching.
            when List.get(ctx.stack, (List.len(ctx.stack) - 1)) is
                Ok(dup_item) -> Ok(Context.push_stack(ctx, dup_item))
                Err(OutOfBounds) -> Err(EmptyStack)

        0x25 ->
            # `%` drop
            when Context.pop_stack(ctx) is
                # Dropping with an empty stack, all results here are fine
                Ok((pop_ctx, _)) -> Ok(pop_ctx)
                Err(_) -> Ok(ctx)

        0x5C ->
            # `\` swap
            (pop_ctx1, n1) = Context.pop_stack?(ctx)
            (pop_ctx2, n2) = Context.pop_stack?(pop_ctx1)
            Ok(Context.push_stack(Context.push_stack(pop_ctx2, n1), n2))

        0x40 ->
            # `@` rot
            result2 =
                (pop_ctx1, n1) = Context.pop_stack?(ctx)
                (pop_ctx2, n2) = Context.pop_stack?(pop_ctx1)
                (pop_ctx3, n3) = Context.pop_stack?(pop_ctx2)
                Ok(Context.push_stack(Context.push_stack(Context.push_stack(pop_ctx3, n2), n1), n3))

            when result2 is
                Ok(a) ->
                    Ok(a)

                # Being explicit with error type is required to stop the need to propogate the error parameters to Context.popStack
                Err(EmptyStack) ->
                    Err(EmptyStack)

        0xC3 ->
            # `ø` pick or `ß` flush
            # these are actually 2 bytes, 0xC3 0xB8 or  0xC3 0x9F
            # requires special parsing
            Ok({ ctx & state: InSpecialChar })

        0x4F ->
            # `O` also treat this as pick for easier script writing
            (pop_ctx, index) = pop_number?(ctx)
            # I think Num.abs is too restrictive, it should be able to produce a natural number, but it seem to be restricted to signed numbers.
            size = List.len(pop_ctx.stack) - 1
            offset = Num.int_cast(size) - index

            if offset >= 0 then
                stack_val = List.get?(pop_ctx.stack, Num.int_cast(offset))
                Ok(Context.push_stack(pop_ctx, stack_val))
            else
                Err(OutOfBounds)

        0x42 ->
            # `B` also treat this as flush for easier script writing
            # This is supposed to flush io buffers. We don't buffer, so it does nothing
            Ok(ctx)

        0x27 ->
            # `'` load next char
            Ok({ ctx & state: LoadChar })

        0x2B ->
            # `+` add
            binary_op(ctx, Num.add_wrap)

        0x2D ->
            # `-` sub
            binary_op(ctx, Num.sub_wrap)

        0x2A ->
            # `*` mul
            binary_op(ctx, Num.mul_wrap)

        0x2F ->
            # `/` div
            # Due to possible division by zero error, this must be handled specially.
            (pop_ctx1, num_r) = pop_number?(ctx)
            (pop_ctx2, num_l) = pop_number?(pop_ctx1)
            res = Num.div_trunc_checked?(num_l, num_r)
            Ok(Context.push_stack(pop_ctx2, Number(res)))

        0x26 ->
            # `&` bitwise and
            binary_op(ctx, Num.bitwise_and)

        0x7C ->
            # `|` bitwise or
            binary_op(ctx, Num.bitwise_or)

        0x3D ->
            # `=` equals
            binary_op(ctx, \a, b ->
                if a == b then
                    -1
                else
                    0)

        0x3E ->
            # `>` greater than
            binary_op(ctx, \a, b ->
                if a > b then
                    -1
                else
                    0)

        0x5F ->
            # `_` negate
            unary_op(ctx, Num.neg)

        0x7E ->
            # `~` bitwise not
            unary_op(ctx, \x -> Num.bitwise_xor(x, -1)) # xor with -1 should be bitwise not

        0x2C ->
            # `,` write char
            (pop_ctx, num) = pop_number?(ctx)
            str = Str.from_utf8([Num.int_cast(num)]) |> Result.map_err?(\_ -> BadUtf8)
            Stdout.raw!(str)
            Ok(pop_ctx)

        0x2E ->
            # `.` write int
            (pop_ctx, num) = pop_number?(ctx)
            Stdout.raw!(Num.to_str(Num.int_cast(num)))
            Ok(pop_ctx)

        0x5E ->
            # `^` read char as int
            in = Stdin.char!({})
            if in == 255 then
                # max char sent on EOF. Change to -1
                Ok(Context.push_stack(ctx, Number(-1)))
            else
                Ok(Context.push_stack(ctx, Number(Num.int_cast(in))))

        0x3A ->
            # `:` store to variable
            (pop_ctx1, var) = pop_variable?(ctx)
            (pop_ctx2, n1) = Context.pop_stack?(pop_ctx1)
            Ok({ pop_ctx2 & vars: List.set(pop_ctx2.vars, Variable.to_index(var), n1) })

        0x3B ->
            # `;` load from variable
            (pop_ctx, var) = pop_variable?(ctx)
            elem = List.get?(pop_ctx.vars, Variable.to_index(var))
            Ok(Context.push_stack(pop_ctx, elem))

        0x22 ->
            # `"` string start
            Ok({ ctx & state: InString([]) })

        0x5B ->
            # `"` string start
            Ok({ ctx & state: InLambda(0, []) })

        0x7B ->
            # `{` comment start
            Ok({ ctx & state: InComment })

        x if is_digit(x) ->
            # number start
            Ok({ ctx & state: InNumber(Num.int_cast((x - 0x30))) })

        x if is_whitespace(x) ->
            Ok(ctx)

        x ->
            when Variable.from_utf8(x) is
                # letters are variable names
                Ok(var) ->
                    Ok(Context.push_stack(ctx, Var(var)))

                Err(_) ->
                    data = Num.to_str(Num.int_cast(x))

                    Err(InvalidChar(data))

unary_op : Context, (I32 -> I32) -> Result Context InterpreterErrors
unary_op = \ctx, op ->
    (pop_ctx, num) = pop_number?(ctx)
    Ok(Context.push_stack(pop_ctx, Number(op(num))))

binary_op : Context, (I32, I32 -> I32) -> Result Context InterpreterErrors
binary_op = \ctx, op ->
    (pop_ctx1, num_r) = pop_number?(ctx)
    (pop_ctx2, num_l) = pop_number?(pop_ctx1)
    Ok(Context.push_stack(pop_ctx2, Number(op(num_l, num_r))))

pop_number : Context -> Result (Context, I32) InterpreterErrors
pop_number = \ctx ->
    when Context.pop_stack?(ctx) is
        (pop_ctx, Number(num)) -> Ok((pop_ctx, num))
        _ -> Err(NoNumberOnStack)

pop_lambda : Context -> Result (Context, List U8) InterpreterErrors
pop_lambda = \ctx ->
    when Context.pop_stack?(ctx) is
        (pop_ctx, Lambda(bytes)) -> Ok((pop_ctx, bytes))
        _ -> Err(NoLambdaOnStack)

pop_variable : Context -> Result (Context, Variable) InterpreterErrors
pop_variable = \ctx ->
    when Context.pop_stack?(ctx) is
        (pop_ctx, Var(var)) -> Ok((pop_ctx, var))
        _ -> Err(NoVariableOnStack)

is_digit : U8 -> Bool
is_digit = \char ->
    char >= 0x30 # `0`
    and char <= 0x39 # `0`

is_whitespace : U8 -> Bool
is_whitespace = \char ->
    char == 0xA # new line
    or char == 0xD # carriage return
    or char == 0x20 # space
    or char == 0x9 # tab

end_unexpected = \err ->
    when err is
        NoScope ->
            NoScope

        EndOfData ->
            UnexpectedEndOfData

