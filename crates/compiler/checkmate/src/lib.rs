mod collector;
mod convert;
mod schema;

pub use collector::Collector;

pub fn is_checkmate_enabled() -> bool {
    let flag = std::env::var("ROC_CHECKMATE");
    flag.as_deref() == Ok("1")
}
