interface Csv
    exposes [example]
    imports [dep.CsvDep]

example : Str
example = "This text came from a CSV package: \(CsvDep.stuff)!"