use core::marker::PhantomData;

use bumpalo::Bump;

/// A string interned in a particular module's Interns
#[derive(Clone, Copy)]
pub struct InternId<'a> {
    id: u32,

    #[cfg(debug_assertions)]
    string: &'a str,

    #[cfg(debug_assertions)]
    module_name: &'a str,

    #[cfg(not(debug_assertions))]
    _phantom: PhantomData<'a>,
}

pub struct Interns<'a> {
    arena: &'a Bump,
}

impl<'a> Interns<'a> {
    #[cfg(debug_assertions)]
    pub fn get_or_set_id(&'a mut self, string: &'a str, module_name: &'a str) -> InternId<'a> {
        match self.get_id(string) {
            Some(id) => id,
            None => self.add_id(string)
        }
    }

    #[cfg(not(debug_assertions))]
    pub fn get_or_set_id(&'a mut self, string: &'a str) -> InternId<'a> {
        match self.get_id(string) {
            Some(id) => id,
            None => self.add_id(string)
        }
    }

    pub fn get_id(&'a self, string: &'a str) -> Option<InternId<'a>> {
        todo!()
    }

    fn add_id(&'a self, string: &'a str) -> InternId<'a> {
        todo!()
    }
}
