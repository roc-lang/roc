succeed = (val) -> Success val


fail = (val) -> Failure val


echo = (str) -> Echo str, succeed, fail


read = Read succeed, fail


map = (convert, task) ->
    await task, (output) ->
    succeed (convert output)


mapErr = (convert, task) ->
    fallback task, (err) ->
    fail (convert err)


await = (task, cont) ->
    match task
        when Success val then cont val
        when Failure val then Failure val

        when Echo str, prevCont, onFailure then
            Echo str,
                (({}) -> await (prevCont {}), cont)
                ((ioErr) -> await (onFailure ioErr), cont)

        when Read prevCont, onFailure then
            Read
                ((str) -> await (prevCont str), cont)
                ((ioErr) -> await (onFailure ioErr), cont)



fallback = (task, onFailure) ->
    match task
        when Success val then Success val
        when Failure val then onFailure val

        when Echo str, cont, prevOnFailure then
            Echo str
                (({}) -> fallback (cont {}), onFailure)
                ((ioErr) -> fallback (prevOnFailure ioErr), onFailure)

        when Read cont, prevOnFailure then
            Read
                ((str) -> fallback (cont str), onFailure)
                ((ioErr) -> fallback (prevOnFailure ioErr), onFailure)


demo =
    await (echo "Enter first name"), ({}) ->
    await read, (firstName) ->
    await (echo "Enter last name"), ({}) ->
    await read, (lastName) ->
    echo "Your name is: \(firstName) \(lastName)"


demo
