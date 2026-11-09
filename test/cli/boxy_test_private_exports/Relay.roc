import Library

Relay := [].{
    run = |n| {
        callback = Library.make(0)
        Library.call(callback, Library.run(n))
    }
}
