use crate::{Defs, Header, SpacesBefore};

#[derive(Debug)]
pub struct Module<'a> {
    pub header: SpacesBefore<'a, Header<'a>>,
    pub defs: Defs<'a>,
}
