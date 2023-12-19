use crate::vec::Vec;

pub struct Report<'a> {
    errors: Vec<'a, Report<'a>>
}
