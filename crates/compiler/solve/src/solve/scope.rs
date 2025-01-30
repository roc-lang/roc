use roc_can::module::ModuleParams;
use roc_module::symbol::Symbol;
use roc_types::subs::Variable;

/// The scope of the solver, as symbols are introduced.
#[derive(Clone, Debug)]
pub struct Scope {
    symbols: Vec<Symbol>,
    variables: Vec<Variable>,
}

impl Scope {
    pub fn new(opt_module_params: Option<ModuleParams>) -> Self {
        match opt_module_params {
            Some(module_params) => {
                let mut symbols = Vec::with_capacity(module_params.destructs.len());
                let mut variables = Vec::with_capacity(module_params.destructs.len());

                for destruct in module_params.destructs {
                    symbols.push(destruct.value.symbol);
                    variables.push(destruct.value.var);
                }

                Self { symbols, variables }
            }
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
