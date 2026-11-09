Enc := [].{
    Form : { name : Str, email : Str, phone : Str }

    encode : Form -> Try(Str, [Unencodable])
    encode = |form| Ok(Json.to_str(form))
}
