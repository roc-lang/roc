app [main] {
    pf: platform "../../test-platform-simple-zig/main.roc",
    json: "json/main.roc",
    csv: "csv/main.roc",
}

import json.JsonParser
import csv.Csv

main = "Hello, World! ${JsonParser.example} ${Csv.example}"
