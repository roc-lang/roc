interface Package
    exposes []
    imports []


Type : [ Constructor UnkownType ]

insertHelper : UnkownType, Type -> Type
insertHelper = \h, m ->
    when m is
        Constructor h2 -> Constructor h 
