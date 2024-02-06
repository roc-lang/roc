use arena::Vec32;

use crate::scope::Scope;

#[repr(align(4))]
pub struct Node([u8; 4]);

pub struct Nodes<'a> {
    inner: Vec32<'a, Node>,
    scope: Scope<'a>,
}

pub enum NodeType {
    FwdDeclare, // Forward-declare a name, so things later in scope can reference it. Only functions do this.
}

pub struct AstNodes<'a> {
    inner: Vec32<'a, Node>,
}

impl<'a> Nodes<'a> {
    pub fn with_capacity_in(node_capacity: usize, arena: &'a mut Arena<'a>) {
        let scope_capacity = node_capacity / 2;

        Self {
            inner: Vec32::with_capacity_in(node_capacity, arena),
            scope: Scope::with_capacity_in(scope_capacity, arena),
        }
    }

    pub fn add_ast(
        &mut self,
        arena: &'a mut Arena<'a>,
        ast_nodes: &'a AstNodes<'a>,
        interns: &'a mut Interns<'a>,
    ) {
        // TODO call self.scope.introduce etc
    }
}
