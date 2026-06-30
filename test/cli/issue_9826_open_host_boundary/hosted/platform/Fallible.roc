Fallible := [].{
    IOErr : [NotFound, Other(Str)]
    line! : Str => Try({}, [LineErr(IOErr), ..])
}
