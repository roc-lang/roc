# META
~~~ini
description=Tag unions and pattern matching
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

Color : [Red, Green, Blue]

to_str : Color -> Str
to_str = |color|
    match color {
        Red => "red"
        Green => "green"
        Blue => "blue"
    }

main = to_str(Red)
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : Str }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
to_str = |color| match color {
	Red => "red"
	Green => "green"
	Blue => "blue"
}
main = to_str(Red)

~~~
# DEV OUTPUT
~~~ini
x64mac=455da978e404fdba27e67801f7ee5ba88e57b235963e25345cb814fd1592db6d
x64win=9173c7b66984e2f7b10b33bf5fd5009e59fed7546eaab98b7b3d415850748682
x64mingw=9173c7b66984e2f7b10b33bf5fd5009e59fed7546eaab98b7b3d415850748682
x64freebsd=61b2ebab1c9c0a007e537387432f8f1b4cf47056e51d140852a3d402b95ee740
x64openbsd=5aead990c3e8f1bf979fa3227798d7e83930482e1eda3ff79c7a00e6bdcf5d69
x64netbsd=18b5ab63b3f607c03300ed9b295f4e807b226c7081367560bf4c6a8aca3de086
x64musl=18b5ab63b3f607c03300ed9b295f4e807b226c7081367560bf4c6a8aca3de086
x64glibc=18b5ab63b3f607c03300ed9b295f4e807b226c7081367560bf4c6a8aca3de086
x64linux=18b5ab63b3f607c03300ed9b295f4e807b226c7081367560bf4c6a8aca3de086
x64elf=18b5ab63b3f607c03300ed9b295f4e807b226c7081367560bf4c6a8aca3de086
x64v1mac=455da978e404fdba27e67801f7ee5ba88e57b235963e25345cb814fd1592db6d
x64v1win=9173c7b66984e2f7b10b33bf5fd5009e59fed7546eaab98b7b3d415850748682
x64v1mingw=9173c7b66984e2f7b10b33bf5fd5009e59fed7546eaab98b7b3d415850748682
x64v1freebsd=61b2ebab1c9c0a007e537387432f8f1b4cf47056e51d140852a3d402b95ee740
x64v1openbsd=5aead990c3e8f1bf979fa3227798d7e83930482e1eda3ff79c7a00e6bdcf5d69
x64v1netbsd=18b5ab63b3f607c03300ed9b295f4e807b226c7081367560bf4c6a8aca3de086
x64v1musl=18b5ab63b3f607c03300ed9b295f4e807b226c7081367560bf4c6a8aca3de086
x64v1glibc=18b5ab63b3f607c03300ed9b295f4e807b226c7081367560bf4c6a8aca3de086
x64v1linux=18b5ab63b3f607c03300ed9b295f4e807b226c7081367560bf4c6a8aca3de086
x64v1elf=18b5ab63b3f607c03300ed9b295f4e807b226c7081367560bf4c6a8aca3de086
arm64mac=3c4529f832aac6807a4fed3549b85ba6ffd913a2605d8ca4bac370b8b974b3d2
arm64win=a9333f051dad9d7c463313cadda2a6c533ade5ff45bbabf072bc9240aba2f761
arm64mingw=a9333f051dad9d7c463313cadda2a6c533ade5ff45bbabf072bc9240aba2f761
arm64linux=43859ab83457d01802ec8e9bb823e6eeea4f5f0a87403615d4260b0234e72fbc
arm64musl=43859ab83457d01802ec8e9bb823e6eeea4f5f0a87403615d4260b0234e72fbc
arm64glibc=43859ab83457d01802ec8e9bb823e6eeea4f5f0a87403615d4260b0234e72fbc
arm64v1win=a9333f051dad9d7c463313cadda2a6c533ade5ff45bbabf072bc9240aba2f761
arm64v1mingw=a9333f051dad9d7c463313cadda2a6c533ade5ff45bbabf072bc9240aba2f761
arm64v1linux=43859ab83457d01802ec8e9bb823e6eeea4f5f0a87403615d4260b0234e72fbc
arm64v1musl=43859ab83457d01802ec8e9bb823e6eeea4f5f0a87403615d4260b0234e72fbc
arm64v1glibc=43859ab83457d01802ec8e9bb823e6eeea4f5f0a87403615d4260b0234e72fbc
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
