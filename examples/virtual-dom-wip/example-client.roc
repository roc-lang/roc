app "example-client"
    packages { pf: "platform/client-side.roc" }
    provides [app] to pf

import ExampleApp exposing [exampleApp]

app = exampleApp
