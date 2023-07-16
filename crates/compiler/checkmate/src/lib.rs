mod convert;
mod schema;

pub fn is_checkmate_enabled() -> bool {
    let flag = std::env::var("ROC_CHECKMATE");
    flag.as_deref() == Ok("1")
}
