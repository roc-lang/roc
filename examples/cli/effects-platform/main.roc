platform "effects"
    requires {} { main! : {} => {} }
    exposes []
    packages {}
    imports []
    provides [mainForHost!]

mainForHost! : {} => {}
mainForHost! = \{} -> main! {}
