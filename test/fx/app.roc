app [writeToStdout, writeToStderr] { pf: platform "./platform/main.roc" }

writeToStdout : Str => {}
writeToStdout = |_msg|
    # The host will actually handle the IO
    {}

writeToStderr : Str => {}
writeToStderr = |_msg|
    # The host will actually handle the IO
    {}
