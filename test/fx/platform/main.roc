platform ""
    requires {} {
        writeToStdout : Str => {},
        writeToStderr : Str => {}
    }
    exposes []
    packages {}
    provides { writeToStdout: "writeToStdout", writeToStderr: "writeToStderr" }

writeToStdout : Str => {}

writeToStderr : Str => {}
