import Enc

Site1 := [].{
    run : Enc.Form -> Str
    run = |form|
        match Enc.encode(form) {
            Ok(json) => json
            Err(_) => ""
        }
}
