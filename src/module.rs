use crate::can::ident;
use crate::collections::MutMap;
use crate::parse::ast::CommentOrNewline;
use crate::region::Loc;
use bumpalo::collections::Vec;
use inlinable_string::InlinableString;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct ModuleId(u32);

#[derive(Clone, Debug)]
pub struct ModuleIdStore {
    next_module_id: u32,
    store: MutMap<ident::ModuleName, ModuleId>,
}

impl Default for ModuleIdStore {
    fn default() -> Self {
        ModuleIdStore {
            next_module_id: 1,
            store: MutMap::default(),
        }
    }
}

impl ModuleIdStore {
    pub fn get(&mut self, module_name: ident::ModuleName) -> ModuleId {
        match self.store.get(&module_name) {
            Some(id) => *id,
            None => {
                // We've never seen this module name before.
                // Assign it an ID and store the result, then return it.
                let module_id = ModuleId(self.next_module_id);

                self.store.insert(module_name, module_id);
                self.next_module_id = 1 + module_id.0;

                module_id
            }
        }
    }

    pub fn get_if_present(&self, module_name: &ident::ModuleName) -> Option<ModuleId> {
        match self.store.get(module_name) {
            Some(id) => Some(*id),
            None => None,
        }
    }

    /// Store all the given module names, but don't return any of them.
    /// This is useful when you have a Mutex<ModuleIdStore>, so you can
    /// lock it, store a bunch of module names, and then unlock it.
    pub fn store_all<'a, I>(&mut self, module_names: I)
    where
        I: Iterator<Item = &'a ident::ModuleName>,
    {
        let next = self.next_module_id;

        for module_name in module_names {
            if !self.store.contains_key(module_name) {
                self.store.insert(module_name.clone(), ModuleId(next));

                next += 1;
            }
        }

        self.next_module_id = next;
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
