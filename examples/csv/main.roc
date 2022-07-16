app "main"
    packages { pf: "platform/main.roc" }
    imports [Parser.Core.{Parser, apply}, Parser.Str.{RawStr}, Parser.CSV.{CSV, record, field, string, nat}]
    provides [main] to pf

# Until issue https://github.com/rtfeldman/roc/issues/3438 is fixed,
# use the simple 'hello world' platform for testing
# with hard-coded input.

input = "John,Doe,100\r\nRichard,Feldman,42\r\nMarten,Wijnja,28\r\n"
main =
  when Parser.CSV.parseStr userCSVParser input is
    Ok result ->
        val = result |> List.map printUser |> Str.joinWith("\n")
        nResults = List.len result |> Num.toStr
        "Parse success! \(nResults) users were found:\n\n\(val)\n"
    Err problem ->
      when problem is
        ParsingFailure failure ->
          "Parsing failure: \(failure)\n"
        ParsingIncomplete leftover ->
          leftoverStr = leftover |> List.map Parser.Str.strFromRaw |> Str.joinWith ", "
          "Parsing incomplete. Following still left: \(leftoverStr)\n"
        SyntaxError error ->
          "Parsing failure. Syntax error in the CSV: \(error)"

User := {firstName: Str, lastName: Str, age: Nat}

userCSVParser =
  record (\firstName -> \lastName -> \age -> @User {firstName, lastName, age})
  |> apply (field string)
  |> apply (field string)
  |> apply (field nat)

printUser = \@User {firstName, lastName, age} ->
    ageStr = Num.toStr age
    "User {firstName: \(firstName), lastName: \(lastName), age: \(ageStr)}"
