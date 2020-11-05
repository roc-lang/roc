platform folkertdev/foo    imports []
    provides [ mainForHost ]
    requires [ main ]
    effects Effect
        {
            putChar : Int -> Effect {}
        }
