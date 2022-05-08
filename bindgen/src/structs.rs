use roc_collections::MutMap;
use roc_types::subs::Variable;

#[derive(Copy, Clone, Debug, Default)]
struct StructId(u64);

impl StructId {
    pub fn to_name(self) -> String {
        format!("R{}", self.0)
    }
}

/// Whenever we register a new Roc record type,
/// give it a unique and short name (e.g. R1, R2, R3...)
/// and then from then on, whenever we ask for that
/// same record type, return the same name.
#[derive(Default)]
pub struct Structs {
    by_variable: MutMap<Variable, StructId>,
    next_id: StructId,
}

impl Structs {
    pub fn get_name(&mut self, var: Variable) -> String {
        match self.by_variable.get(&var) {
            Some(struct_id) => struct_id.to_name(),
            None => self.next_id().to_name(),
        }
    }

    fn next_id(&mut self) -> StructId {
        self.next_id.0 += 1;

        self.next_id
    }
}
