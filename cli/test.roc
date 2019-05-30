succeed = (val) ->
  Success val


fail = (val) ->
  Failure val


echo = (str) ->
  Echo str, succeed, fail


readInput =
  Read succeed, fail


map = (convert, task) ->
  after task, (output) ->
    succeed (convert output)


mapErr = (convert, task) ->
  fallback task, (err) ->
    fail (convert err)


after = (task, cont) ->
  case task
    when Success val then cont val
    when Failure val then Failure val

    when Echo str, prevCont, onFailure then
      Echo str,
        ({} -> after (prevCont {}), cont),
        (ioErr -> after (onFailure ioErr), cont)

    when Read prevCont, onFailure then
      Read
        (str -> after (prevCont str), cont),
        (ioErr -> after (onFailure ioErr), cont)


fallback = (task, onFailure) ->
  case task
    when Success val then Success val
    when Failure val then onFailure val

    when Echo str, cont, prevOnFailure then
      Echo str
        ({} -> fallback (cont {}), onFailure),
        (ioErr -> fallback (prevOnFailure ioErr), onFailure)

    when Read cont, prevOnFailure then
      Read
        (str -> fallback (cont str), onFailure),
        (ioErr -> fallback (prevOnFailure ioErr), onFailure)


demo =
  after (echo "Enter first name"), ({}) ->
    after readInput, (firstName) ->
      after (echo "Enter last name"), ({}) ->
        after readInput, (lastName) ->
          fullName = "\(firstName) \(lastName)"

          echo "Your name is: \(fullName)"


demo
