interface Target
    exposes [Architecture, OperatingSystem, Target]
    imports []

Target : {
    architecture : Architecture,
    operatingSystem : OperatingSystem,
}

OperatingSystem : [
    Windows,
    Unix,
    Wasi,
]

Architecture : [
    Aarch32,
    Aarch64,
    Wasm32,
    X86x32,
    X86x64,
]