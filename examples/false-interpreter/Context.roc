interface Context
    exposes [ Context, Data, with, getChar, Option, consumeChar, pushStack, popStack, toStr ]
    imports [ base.File, base.Task.{ Task }, Variable.{ Variable } ]

Option a : [ Some a, None ]

# The underlying context of the current location within the file

# I want to change Number to I32, but now that everything is built out, I run into errors when doing so.
Data : [ Lambda (List U8), Number I64, Var Variable ] 
Context : { data: Option File.Handle, index: Nat, buf: List U8, stack: List Data, vars: List Data }

pushStack: Context, Data -> Context
pushStack = \ctx, data ->
    {ctx & stack: List.append ctx.stack data}

# I think an open tag union should just work here.
# Instead at a call sites, I need to match on the error and then return the same error.
# Otherwise it hits unreachable code in ir.rs
popStack: Context -> Result [T Context Data] [ EmptyStack ]*
popStack = \ctx ->
    when List.last ctx.stack is
        Ok val ->
            # This is terrible, but is the simplest way to drop the last element that I can think of.
            # Long term it looks like there is a List.dropLast builtin.
            poppedCtx = { ctx & stack: List.reverse (List.drop (List.reverse ctx.stack) 1)}
            Ok (T poppedCtx val)
        Err ListWasEmpty ->
            Err EmptyStack

toStrData: Data -> Str
toStrData = \data ->
    when data is
        Lambda _ -> "[]"
        Number n -> Str.fromInt n
        Var v -> Variable.toStr v

toStr: Context -> Str
toStr = \{stack, vars} ->
    stackStr = Str.joinWith (List.map stack toStrData) " "
    varsStr = Str.joinWith (List.map vars toStrData) " "
    "\n============\nStack: [\(stackStr)]\nVars: [\(varsStr)]\n============\n"

with : Str, (Context -> Task {} a) -> Task {} a
with = \path, callback ->
    handle <- File.withOpen path
    callback { data: Some handle, index: 0, buf: [], stack: [], vars: (List.repeat Variable.totalCount (Number 0)) }

# I am pretty sure there is a syntax to destructure and keep a reference to the whole, but Im not sure what it is.
getChar: Context -> Task [T U8 Context] [ EndOfData ]*
getChar = \ctx ->
    when List.get ctx.buf ctx.index is
        Ok val -> Task.succeed (T val ctx)
        Err OutOfBounds -> 
            when ctx.data is
                Some h ->
                    chunk <- Task.await (File.chunk h)
                    bytes = Str.toUtf8 chunk 
                    when List.first bytes is
                        Ok val ->
                            Task.succeed (T val {ctx & buf: bytes, index: 0 })
                        Err ListWasEmpty -> 
                            Task.fail EndOfData
                None ->
                    Task.fail EndOfData

consumeChar: Context -> Context
consumeChar = \ctx ->
    { ctx & index: ctx.index + 1 }