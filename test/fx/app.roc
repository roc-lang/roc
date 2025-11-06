app [main!, putStdout!, putStderr!] { pf: platform "./platform/main.roc" }

main! : () => {}
main! = ||
    putStdout!("Hello from stdout!")
    putStderr!("Error from stderr!")
    putStdout!("Line 1 to stdout")
    putStderr!("Line 2 to stderr")
    putStdout!("Line 3 to stdout")
    {}

putStdout! : Str => {}
putStdout! = |_msg|
    # In a full implementation, this would call a host-provided function
    # For now, the host will intercept these calls and perform the actual I/O
    {}

putStderr! : Str => {}
putStderr! = |_msg|
    # In a full implementation, this would call a host-provided function
    # For now, the host will intercept these calls and perform the actual I/O
    {}
