pub(super) use std::result as std_result;
///The types of errors in Roar
#[derive(Clone,Debug)]
pub enum Error {
    ///Another generic error
    Other(String),
    ///Not yet done
    Todo,
}
pub type Result<T> = std_result::Result<T,Error>;