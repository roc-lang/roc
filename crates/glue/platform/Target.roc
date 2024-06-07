module [Target, Architecture, OperatingSystem]

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
    Freestanding,
    Linux,
    Mac,
    Windows,
]
