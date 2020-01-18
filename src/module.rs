use crate::can::ident;
use crate::parse::ast::CommentOrNewline;
use crate::region::Loc;
use bumpalo::collections::Vec;
use inlinable_string::InlinableString;
use symbol_map::indexing::{HashIndexing, Indexing};
use symbol_map::Symbol;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct ModuleId(u32);

#[derive(Debug, Default)]
pub struct ModuleIdStore {
    store: HashIndexing<ident::ModuleName, u32>,
}

impl ModuleIdStore {
    pub fn get_id(&mut self, module_name: &ident::ModuleName) -> ModuleId {
        match self.store.get(module_name) {
            Some(symbol) => ModuleId(*symbol.id()),
            None => ModuleId(*self.store.get_or_insert(module_name.clone()).unwrap().id()),
        }
    }

    pub fn get_name(&self, id: ModuleId) -> Option<&ident::ModuleName> {
        self.store.get_symbol(&id.0).map(Symbol::data)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct ModuleName<'a>(&'a str);

impl<'a> Into<&'a str> for ModuleName<'a> {
    fn into(self) -> &'a str {
        self.0
    }
}

impl<'a> Into<InlinableString> for ModuleName<'a> {
    fn into(self) -> InlinableString {
        self.0.into()
    }
}

impl<'a> ModuleName<'a> {
    pub fn new(name: &'a str) -> Self {
        ModuleName(name)
    }

    pub fn as_str(&'a self) -> &'a str {
        self.0
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceHeader<'a> {
    pub name: Loc<ModuleName<'a>>,
    pub exposes: Vec<'a, Loc<Exposes<'a>>>,
    pub imports: Vec<'a, (ModuleName<'a>, Vec<'a, Loc<Imports<'a>>>)>,

    // Potential comments and newlines - these will typically all be empty.
    pub after_interface: &'a [CommentOrNewline<'a>],
    pub before_exposes: &'a [CommentOrNewline<'a>],
    pub after_exposes: &'a [CommentOrNewline<'a>],
    pub before_imports: &'a [CommentOrNewline<'a>],
    pub after_imports: &'a [CommentOrNewline<'a>],
}

#[derive(Clone, Debug, PartialEq)]
pub struct AppHeader<'a> {
    pub imports: Vec<'a, (ModuleName<'a>, Loc<Imports<'a>>)>,

    // Potential comments and newlines - these will typically all be empty.
    pub before_imports: &'a [CommentOrNewline<'a>],
    pub after_imports: &'a [CommentOrNewline<'a>],
}

#[derive(Clone, Debug, PartialEq)]
pub enum Exposes<'a> {
    /// e.g. `Task`
    Ident(&'a str),

    // Spaces
    SpaceBefore(&'a Exposes<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Exposes<'a>, &'a [CommentOrNewline<'a>]),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Imports<'a> {
    /// e.g. `Task` or `Task.{ Task, after }`
    Ident(&'a str, Vec<'a, &'a str>),

    // Spaces
    SpaceBefore(&'a Imports<'a>, &'a [CommentOrNewline<'a>]),
    SpaceAfter(&'a Imports<'a>, &'a [CommentOrNewline<'a>]),
}
