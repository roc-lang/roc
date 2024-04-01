app [main] { pf: platform "platform/main.roc", json: "json/main.roc", csv: "csv/main.roc" }

import json.JsonParser
import csv.Csv

main = "Hello, World! $(JsonParser.example) $(Csv.example)"
