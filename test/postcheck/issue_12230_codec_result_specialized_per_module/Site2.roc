import Enc

Site2 := [].{
    run : Enc.Form -> Str
    run = |form|
        match Enc.encode(form) {
            Ok(json) => json
            Err(_) => ""
        }
}
