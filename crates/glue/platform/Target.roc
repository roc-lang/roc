interface Target
    exposes [Target, Architecture, OperatingSystem]
    imports []

Target : {
    architecture : Architecture,
    operatingSystem : OperatingSystem,
}

Architecture : [
    Aarch32,
    Aarch64,
    Wasm32,
    X86x32,
    X86x64,
]

OperatingSystem : [
    Windows,
    Unix,
    Wasi,
]
