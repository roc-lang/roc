import beta.Beta

Alpha := [].{
    greet : Str -> Str
    greet = |name| "alpha[${Beta.tag(name)}]"
}
