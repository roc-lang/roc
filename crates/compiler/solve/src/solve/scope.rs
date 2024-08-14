use roc_module::symbol::Symbol;
use roc_types::subs::Variable;

/// The scope of the solver, as symbols are introduced.
#[derive(Clone, Debug)]
pub struct Scope {
    symbols: Vec<Symbol>,
    variables: Vec<Variable>,
}

impl Scope {
    pub fn new(params_pattern: Option<roc_can::pattern::Pattern>) -> Self {
        match params_pattern {
            Some(params_pattern) => match params_pattern {
                roc_can::pattern::Pattern::RecordDestructure {
                    whole_var: _,
                    ext_var: _,
                    destructs,
                } => {
                    let mut symbols = Vec::with_capacity(destructs.len());
                    let mut variables = Vec::with_capacity(destructs.len());

                    for destruct in destructs {
                        symbols.push(destruct.value.symbol);
                        variables.push(destruct.value.var);
                    }

                    Self { symbols, variables }
                }
                _ => unreachable!(
                    "other pattern types should have parsed: {:?}",
                    params_pattern
                ),
            },
            None => Self {
                symbols: Vec::default(),
                variables: Vec::default(),
            },
        }
    }

    pub fn vars_by_symbol(&self) -> impl Iterator<Item = (Symbol, Variable)> + '_ {
        let it1 = self.symbols.iter().copied();
        let it2 = self.variables.iter().copied();

        it1.zip(it2)
    }

    #[inline(always)]
    pub fn get_var_by_symbol(&self, symbol: &Symbol) -> Option<Variable> {
        self.symbols
            .iter()
            .position(|s| s == symbol)
            .map(|index| self.variables[index])
    }

    #[inline(always)]
    pub fn insert_symbol_var_if_vacant(&mut self, symbol: Symbol, var: Variable) {
        match self.symbols.iter().position(|s| *s == symbol) {
            None => {
                // symbol is not in vars_by_symbol yet; insert it
                self.symbols.push(symbol);
                self.variables.push(var);
            }
            Some(_) => {
                // do nothing
            }
        }
    }
}
