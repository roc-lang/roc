use bumpalo::Bump;

struct Interns<'a> {
    arena: &'a Bump,
}

pub struct Scope<'a> {
    arena: &'a Bump,
    interns: Interns<'a>,
    decls:
}

impl<'a> Scope<'a> {
    pub fn add_decl()
}
