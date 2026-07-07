# Regression fixture for https://github.com/roc-lang/roc/issues/9890 —
# roc-parser's examples/csv-movies.roc crashed in monotype lowering after
# PR #9873 ("checked direct call result type differed from its expected
# Monotype type"). See Numbers.roc for the vendoring provenance.
import Parser
import CSV
import String

main! = |_args| Ok({})

input : Str
input =
    \\Airplane!,1980,\"Robert Hays,Julie Hagerty\"
    \\Caddyshack,1980,\"Chevy Chase,Rodney Dangerfield,Ted Knight,Michael O'Keefe,Bill Murray\"

movie_info_parser =
    CSV.record(|title| |release_year| |actors| { { title, release_year, actors } })
    .keep(CSV.field(CSV.string))
    .keep(CSV.field(CSV.u64))
    .keep(CSV.field(actors_parser))

actors_parser = (CSV.string).map(|val| { val.split_on(",") })

movie_info_explanation = |{ title, release_year, actors }| {
    enumerated_actors = enumerate(actors)
    release_year_str = release_year.to_str()

    "The movie '${title}' was released in ${release_year_str} and stars ${enumerated_actors}"
}

enumerate : List(Str) -> Str
enumerate = |elements| {
    match elements {
        [] => ""
        [actor] => actor
        [.. as inits, last] =>
            [last]
            .prepend(inits->Str.join_with(", "))
            ->Str.join_with(" and ")
    }
}

# The original example's main! pipeline: parse the CSV and explain each movie.
expect {
    match CSV.parse_str(movie_info_parser, input) {
        Ok(movies) => {
            explanations = movies.map(movie_info_explanation)
            movies.len() == 2 and explanations.len() == 2
        }
        Err(_) => False
    }
}
