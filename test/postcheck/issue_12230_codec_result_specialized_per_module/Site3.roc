import Enc

Site3 := [].{
    run : Enc.Form -> Str
    run = |form|
        match Enc.encode(form) {
            Ok(json) => json
            Err(_) => ""
        }
}
