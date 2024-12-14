platform "false-interpreter"
    requires {} { main! : Str => {} }
    exposes []
    packages {}
    imports []
    provides [mainForHost!]

mainForHost! : Str => {}
mainForHost! = \file -> main! file
