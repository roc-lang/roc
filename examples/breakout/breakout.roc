app "breakout"
    packages { pf: "platform" }
    imports []# [ pf.Action.{ Action }, pf.Elem.{ button, text, row, col } ]
    provides [ program ] to pf

program = { render }

render = \state ->
    []
