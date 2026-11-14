# A constant whose type keeps its variables: a record of generic functions,
# each requiring a method of the type it is read at.
ImportedGenericRecordDep := [].{
    effects = { send: send, twice: twice }

    send = |x| x.concat("!")

    twice = |x| x.concat(x)
}
