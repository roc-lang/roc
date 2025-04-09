app [main] {
    cli: platform "",
}

main =
"jq --version"
.(Cmd.new)()
.(Cmd.status)()
.(Result.mapErr?)(UnableToCheckJQVersion)
