module [Context, Data, with!, getChar!, Option, pushStack, popStack, toStr, inWhileScope]

import pf.File
import Variable exposing [Variable]

Option a : [Some a, None]

# The underlying context of the current location within the file
Data : [Lambda (List U8), Number I32, Var Variable]
# While loops are special and have their own Scope specific state.
WhileState : { cond : List U8, body : List U8, state : [InCond, InBody] }
Scope : { data : Option File.Handle, index : U64, buf : List U8, whileInfo : Option WhileState }
State : [Executing, InComment, InLambda U64 (List U8), InString (List U8), InNumber I32, InSpecialChar, LoadChar]
Context : { scopes : List Scope, stack : List Data, vars : List Data, state : State }

pushStack : Context, Data -> Context
pushStack = \ctx, data ->
    { ctx & stack: List.append ctx.stack data }

# I think an open tag union should just work here.
# Instead at a call sites, I need to match on the error and then return the same error.
# Otherwise it hits unreachable code in ir.rs
popStack : Context -> Result (Context, Data) [EmptyStack]
popStack = \ctx ->
    when List.last ctx.stack is
        Ok val ->
            poppedCtx = { ctx & stack: List.dropAt ctx.stack (List.len ctx.stack - 1) }

            Ok (poppedCtx, val)

        Err ListWasEmpty ->
            Err EmptyStack

toStrData : Data -> Str
toStrData = \data ->
    when data is
        Lambda _ -> "[]"
        Number n -> Num.toStr (Num.intCast n)
        Var v -> Variable.toStr v

toStrState : State -> Str
toStrState = \state ->
    when state is
        Executing -> "Executing"
        InComment -> "InComment"
        InString _ -> "InString"
        InNumber _ -> "InNumber"
        InLambda _ _ -> "InLambda"
        InSpecialChar -> "InSpecialChar"
        LoadChar -> "LoadChar"

toStr : Context -> Str
toStr = \{ scopes, stack, state, vars } ->
    depth = Num.toStr (List.len scopes)
    stateStr = toStrState state
    stackStr = Str.joinWith (List.map stack toStrData) " "
    varsStr = Str.joinWith (List.map vars toStrData) " "

    "\n============\nDepth: $(depth)\nState: $(stateStr)\nStack: [$(stackStr)]\nVars: [$(varsStr)]\n============\n"

with! : Str, (Context => a) => a
with! = \path, callback! ->
    File.withOpen! path \handle ->
        # I cant define scope here and put it in the list in callback. It breaks alias anaysis.
        # Instead I have to inline this.
        # root_scope = { data: Some handle, index: 0, buf: [], whileInfo: None }
        callback! { scopes: [{ data: Some handle, index: 0, buf: [], whileInfo: None }], state: Executing, stack: [], vars: List.repeat (Number 0) Variable.totalCount }

# I am pretty sure there is a syntax to destructure and keep a reference to the whole, but Im not sure what it is.
getChar! : Context => Result (U8, Context) [EndOfData, NoScope]
getChar! = \ctx ->
    when List.last ctx.scopes is
        Ok scope ->
            (val, newScope) = getCharScope!? scope
            Ok (val, { ctx & scopes: List.set ctx.scopes (List.len ctx.scopes - 1) newScope })

        Err ListWasEmpty ->
            Err NoScope

getCharScope! : Scope => Result (U8, Scope) [EndOfData, NoScope]
getCharScope! = \scope ->
    when List.get scope.buf scope.index is
        Ok val ->
            Ok (val, { scope & index: scope.index + 1 })

        Err OutOfBounds ->
            when scope.data is
                Some h ->
                    bytes = File.chunk! h
                    when List.first bytes is
                        Ok val ->
                            # This starts at 1 because the first character is already being returned.
                            Ok (val, { scope & buf: bytes, index: 1 })

                        Err ListWasEmpty ->
                            Err EndOfData

                None ->
                    Err EndOfData

inWhileScope : Context -> Bool
inWhileScope = \ctx ->
    when List.last ctx.scopes is
        Ok scope ->
            scope.whileInfo != None

        Err ListWasEmpty ->
            Bool.false
