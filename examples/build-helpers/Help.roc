interface Help
    exposes [
        RocTarget,
        Arch,
        Os,
        archFromStr,
        osFromStr,
        rocTarget,
        prebuiltBinaryName,
    ]
    imports []


RocTarget : [
    MacosArm64,
    MacosX64,
    LinuxArm64,
    LinuxX64,
    WindowsArm64,
    WindowsX64,
]

Arch : [
    Arm64,
    X64,
    UnsupportedArch Str,
]

Os : [
    Macos,
    Linux,
    UnsupportedOS Str,
]

archFromStr : List U8 -> Arch
archFromStr = \bytes -> 
    when Str.fromUtf8 bytes is 
        Ok str if str == "arm64\n" -> Arm64
        Ok str if str == "x86_64\n" -> X64
        Ok str -> UnsupportedArch str
        _ -> crash "invalid utf8 from uname -m"

osFromStr : List U8 -> Os
osFromStr = \bytes -> 
    when Str.fromUtf8 bytes is 
        Ok str if str == "Darwin\n" -> Macos
        Ok str if str == "Linux\n" -> Linux
        Ok str -> UnsupportedOS str
        _ -> crash "invalid utf8 from uname -s"

rocTarget : {os:Os, arch:Arch} -> Result RocTarget [UnsupportedTarget Os Arch] 
rocTarget = \{os, arch} ->  
    when (os, arch) is 
        (Macos, Arm64) -> Ok MacosArm64
        (Macos, X64) -> Ok MacosX64
        (Linux, Arm64) -> Ok LinuxArm64
        (Linux, X64) -> Ok LinuxX64
        _ -> Err (UnsupportedTarget os arch)

prebuiltBinaryName : RocTarget -> Str 
prebuiltBinaryName = \target ->
    when target is 
        MacosArm64 -> "macos-arm64.a"
        MacosX64 -> "macos-x64"
        LinuxArm64 -> "linux-arm64.a"
        LinuxX64 -> "linux-x64.a"
        WindowsArm64 -> "windows-arm64.a"
        WindowsX64 -> "windows-x64"