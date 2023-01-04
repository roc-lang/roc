app "packages-test"
    packages { pf: "platform/main.roc", json: "json/main.roc", csv: "csv/main.roc" }
    imports [json.JsonParser, csv.Csv]
    provides [main] to pf

main = "Hello, World! \(JsonParser.example) \(Csv.example)"
