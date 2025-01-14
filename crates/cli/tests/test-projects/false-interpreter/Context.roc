module [Context, Data, with!, get_char!, Option, push_stack, pop_stack, to_str, in_while_scope]

import pf.File
import Variable exposing [Variable]

Option a : [Some a, None]

# The underlying context of the current location within the file
Data : [Lambda (List U8), Number I32, Var Variable]
# While loops are special and have their own Scope specific state.
WhileState : { cond : List U8, body : List U8, state : [InCond, InBody] }
Scope : { data : Option File.Handle, index : U64, buf : List U8, while_info : Option WhileState }
State : [Executing, InComment, InLambda U64 (List U8), InString (List U8), InNumber I32, InSpecialChar, LoadChar]
Context : { scopes : List Scope, stack : List Data, vars : List Data, state : State }

push_stack : Context, Data -> Context
push_stack = \ctx, data ->
    { ctx & stack: List.append(ctx.stack, data) }

# I think an open tag union should just work here.
# Instead at a call sites, I need to match on the error and then return the same error.
# Otherwise it hits unreachable code in ir.rs
pop_stack : Context -> Result (Context, Data) [EmptyStack]
pop_stack = \ctx ->
    when List.last(ctx.stack) is
        Ok(val) ->
            popped_ctx = { ctx & stack: List.drop_at(ctx.stack, (List.len(ctx.stack) - 1)) }

            Ok((popped_ctx, val))

        Err(ListWasEmpty) ->
            Err(EmptyStack)

to_str_data : Data -> Str
to_str_data = \data ->
    when data is
        Lambda(_) -> "[]"
        Number(n) -> Num.to_str(Num.int_cast(n))
        Var(v) -> Variable.to_str(v)

to_str_state : State -> Str
to_str_state = \state ->
    when state is
        Executing -> "Executing"
        InComment -> "InComment"
        InString(_) -> "InString"
        InNumber(_) -> "InNumber"
        InLambda(_, _) -> "InLambda"
        InSpecialChar -> "InSpecialChar"
        LoadChar -> "LoadChar"

to_str : Context -> Str
to_str = \{ scopes, stack, state, vars } ->
    depth = Num.to_str(List.len(scopes))
    state_str = to_str_state(state)
    stack_str = Str.join_with(List.map(stack, to_str_data), " ")
    vars_str = Str.join_with(List.map(vars, to_str_data), " ")

    "\n============\nDepth: ${depth}\nState: ${state_str}\nStack: [${stack_str}]\nVars: [${vars_str}]\n============\n"

with! : Str, (Context => a) => a
with! = \path, callback! ->
    File.with_open!(path, \handle ->
        # I cant define scope here and put it in the list in callback. It breaks alias anaysis.
        # Instead I have to inline this.
        # root_scope = { data: Some handle, index: 0, buf: [], whileInfo: None }
        callback!({ scopes: [{ data: Some(handle), index: 0, buf: [], while_info: None }], state: Executing, stack: [], vars: List.repeat(Number(0), Variable.total_count) }))

# I am pretty sure there is a syntax to destructure and keep a reference to the whole, but Im not sure what it is.
get_char! : Context => Result (U8, Context) [EndOfData, NoScope]
get_char! = \ctx ->
    when List.last(ctx.scopes) is
        Ok(scope) ->
            (val, new_scope) = get_char_scope!?(scope)
            Ok((val, { ctx & scopes: List.set(ctx.scopes, (List.len(ctx.scopes) - 1), new_scope) }))

        Err(ListWasEmpty) ->
            Err(NoScope)

get_char_scope! : Scope => Result (U8, Scope) [EndOfData, NoScope]
get_char_scope! = \scope ->
    when List.get(scope.buf, scope.index) is
        Ok(val) ->
            Ok((val, { scope & index: scope.index + 1 }))

        Err(OutOfBounds) ->
            when scope.data is
                Some(h) ->
                    bytes = File.chunk!(h)
                    when List.first(bytes) is
                        Ok(val) ->
                            # This starts at 1 because the first character is already being returned.
                            Ok((val, { scope & buf: bytes, index: 1 }))

                        Err(ListWasEmpty) ->
                            Err(EndOfData)

                None ->
                    Err(EndOfData)

in_while_scope : Context -> Bool
in_while_scope = \ctx ->
    when List.last(ctx.scopes) is
        Ok(scope) ->
            scope.while_info != None

        Err(ListWasEmpty) ->
            Bool.false
