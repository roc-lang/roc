interface Package
    exposes []
    imports []


Type : [ Constructor UnknownType ]

insertHelper : UnknownType, Type -> Type
insertHelper = \h, m ->
    when m is
        Constructor h2 -> Constructor h 
