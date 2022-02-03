app "hello-rust"
    packages { pf: "platform" }
    imports [ Rbt.{ Job, job } ]
    provides [ main ] to pf

main = "Hello, World!"

x =
    bundle

bundle : Job
bundle =
    job Command []
