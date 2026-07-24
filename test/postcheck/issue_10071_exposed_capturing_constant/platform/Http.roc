Http := [].{
    Task := { done : List(U8) -> Str }

    get_text_task : Str -> Task
    get_text_task = |_| { done: decode_text_response_payload }

    decode_text_response_payload : List(U8) -> Str
    decode_text_response_payload = |payload|
        match Str.from_utf8(payload) {
            Ok(text) => text
            Err(_) => Str.from_utf8_lossy(payload)
        }
}
