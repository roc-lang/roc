HostedIO := [].{
    ## Print a string to stdout
    put_line! : Str => {}

    ## Read a line from stdin
    get_line! : {} => Str

    ## Write to a file
    write_file! : Str, Str => Try({}, [FileWriteError])
}
