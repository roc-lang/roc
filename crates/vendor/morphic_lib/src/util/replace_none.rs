#[derive(Clone, Debug, thiserror::Error)]
#[error("replace_none: expected 'None' option, found value {0:?}")]
pub struct ReplaceNoneError<T: std::fmt::Debug>(pub T);

pub fn replace_none<T: std::fmt::Debug>(
    opt: &mut Option<T>,
    val: T,
) -> Result<(), ReplaceNoneError<T>> {
    match std::mem::replace(opt, Some(val)) {
        None => Ok(()),
        Some(prev) => Err(ReplaceNoneError(prev)),
    }
}
