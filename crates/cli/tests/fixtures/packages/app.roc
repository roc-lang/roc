app "packages-test"
    packages { pf: "platform/main.roc", json: "json/main.roc", csv: "csv/main.roc" }
    provides [main] to pf

import json.JsonParser
import csv.Csv

main = "Hello, World! \(JsonParser.example) \(Csv.example)"
