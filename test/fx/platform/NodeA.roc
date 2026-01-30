NodeA := [
    SourceA,
    MappedA({ source : NodeA }),
].{
    source : {} -> NodeA
    source = |{}| SourceA
}
