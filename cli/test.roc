succeed = (val) ->
  Success val


fail = (val) ->
  Failure val


echo = (str) ->
  Echo fail, succeed, str


readInput =
  Read fail, succeed


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
    when Echo onFailure, prevCont, str then
      Echo
        (ioErr -> after (onFailure ioErr), cont),
        ({} -> after (prevCont {}), cont),
        str
    when Read onFailure, prevCont then
      Read
        (ioErr -> after (onFailure ioErr), cont),
        (str -> after (prevCont str), cont)


fallback = (task, onFailure) ->
  case task
    when Success val then Success val
    when Failure val then onFailure val
    when Echo prevOnFailure, cont, str then
      Echo
        (ioErr -> fallback (prevOnFailure ioErr), onFailure),
        ({} -> fallback (cont {}), onFailure),
        str
    when Read prevOnFailure, cont then
      Read
        (ioErr -> fallback (prevOnFailure ioErr), onFailure),
        (str -> fallback (cont str), onFailure)


demo =
  after (echo "Enter first name"), ({}) ->
    after readInput, (firstName) ->
      after (echo "Enter last name"), ({}) ->
        after readInput, (lastName) ->
          fullName = "\(firstName) \(lastName)"

          echo "Your name is: \(fullName)"


demo
