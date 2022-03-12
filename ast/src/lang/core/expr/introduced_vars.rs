use roc_collections::all::MutMap;
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_types::subs::Variable;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct IntroducedVariables {
    // Rigids must be unique within a type annotation.
    // E.g. in `identity : a -> a`, there should only be one
    // variable (a rigid one, with name "a").
    // Hence `rigids : Map<Lowercase, Variable>`
    //
    // But then between annotations, the same name can occur multiple times,
    // but a variable can only have one name. Therefore
    // `ftv : Map<Variable, Lowercase>`.
    pub wildcards: Vec<Variable>,
    pub var_by_name: MutMap<Lowercase, Variable>,
    pub name_by_var: MutMap<Variable, Lowercase>,
    pub host_exposed_aliases: MutMap<Symbol, Variable>,
}

impl IntroducedVariables {
    pub fn insert_named(&mut self, name: Lowercase, var: Variable) {
        self.var_by_name.insert(name.clone(), var);
        self.name_by_var.insert(var, name);
    }

    pub fn insert_wildcard(&mut self, var: Variable) {
        self.wildcards.push(var);
    }

    pub fn insert_host_exposed_alias(&mut self, symbol: Symbol, var: Variable) {
        self.host_exposed_aliases.insert(symbol, var);
    }

    pub fn union(&mut self, other: &Self) {
        self.wildcards.extend(other.wildcards.iter().cloned());
        self.var_by_name.extend(other.var_by_name.clone());
        self.name_by_var.extend(other.name_by_var.clone());
        self.host_exposed_aliases
            .extend(other.host_exposed_aliases.clone());
    }

    pub fn var_by_name(&self, name: &Lowercase) -> Option<&Variable> {
        self.var_by_name.get(name)
    }

    pub fn name_by_var(&self, var: Variable) -> Option<&Lowercase> {
        self.name_by_var.get(&var)
    }
}
