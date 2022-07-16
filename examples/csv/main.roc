app "main"
    packages { pf: "platform/main.roc" }
    imports [Parser.Core.{Parser, map, apply}, Parser.Str.{RawStr}, Parser.CSV.{CSV, record, field, string, nat}]
    provides [main] to pf

input = "Airplane!,1980,\"Robert Hays,Julie Hagerty\"\r\nCaddyshack,1980,\"Chevy Chase,Rodney Dangerfield,Ted Knight,Michael O'Keefe,Bill Murray\"\r\n"
main =
  when Parser.CSV.parseStr movieInfoParser input is
    Ok movies ->
        moviesString =
          movies
          |> List.map movieInfoExplanation
          |> Str.joinWith("\n")
        nMovies = List.len movies |> Num.toStr
        "Parse success! \(nMovies) movies were found:\n\n\(moviesString)\n"
    Err problem ->
      when problem is
        ParsingFailure failure ->
          "Parsing failure: \(failure)\n"
        ParsingIncomplete leftover ->
          leftoverStr = leftover |> List.map Parser.Str.strFromRaw |> List.map (\val -> "\"\(val)\"") |> Str.joinWith ", "
          "Parsing incomplete. Following leftover fields while parsing a record: \(leftoverStr)\n"
        SyntaxError error ->
          "Parsing failure. Syntax error in the CSV: \(error)"

MovieInfo := {title: Str, releaseYear: Nat, actors: List Str}

movieInfoParser =
  record (\title -> \releaseYear -> \actors -> @MovieInfo {title, releaseYear, actors})
  |> apply (field string)
  |> apply (field nat)
  |> apply (field actorsParser)

actorsParser =
  string
  |> map (\val -> Str.split val ",")

movieInfoExplanation = \@MovieInfo {title, releaseYear, actors} ->
  enumeratedActors = enumerate actors
  releaseYearStr = Num.toStr releaseYear
  "The movie '\(title)' was released in \(releaseYearStr) and stars \(enumeratedActors)"

enumerate : List Str -> Str
enumerate = \elements ->
  {before: inits, others: last} = List.split elements (List.len elements - 1)
  last
  |> List.prepend (inits |> Str.joinWith ", ")
  |> Str.joinWith " and "

# movieInfoToStr = \@MovieInfo {title, releaseYear, actors} ->
#   releaseYearStr = Num.toStr releaseYear
#   actorsStr = actors |> Str.joinWith ", "
#   "MovieInfo {title: \(title), releaseYear: \(releaseYearStr)}, actors: [\(actorsStr))]}"
