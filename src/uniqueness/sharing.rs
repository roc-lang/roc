use crate::can::expr::Expr;
use crate::can::ident::Lowercase;
use crate::can::symbol::Symbol;
use crate::collections::ImMap;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum ReferenceCount {
    Seen,
    Unique,
    Access(FieldAccess),
    Update(FieldAccess),
    Shared,
}

impl ReferenceCount {
    pub fn add(a: &ReferenceCount, b: &ReferenceCount) -> Self {
        use ReferenceCount::*;
        match (a, b) {
            // Shared
            (Shared, _) => Shared,
            (_, Shared) => Shared,

            // Update
            (Update(_), Update(_)) => Shared,
            (Update(_), Access(_)) => Shared,
            (Access(fa1), Update(fa2)) => {
                let mut fa = fa1.clone();
                fa.sequential_merge(fa2);

                Update(fa)
            }
            (_, Update(fa)) => Update(fa.clone()),
            (Update(fa), _) => Update(fa.clone()),

            // Access
            (Access(fa1), Access(fa2)) => {
                let mut fa = fa1.clone();
                fa.sequential_merge(fa2);

                Access(fa)
            }
            (_, Access(fa)) => Access(fa.clone()),
            (Access(fa), _) => Access(fa.clone()),

            // Unique
            (Unique, Unique) => Shared,
            (_, Unique) => Unique,
            (Unique, _) => Unique,

            (Seen, Seen) => Seen,
        }
    }

    pub fn or(a: &ReferenceCount, b: &ReferenceCount) -> Self {
        match (a, b) {
            (Self::Seen, other) => other.clone(),
            (other, Self::Seen) => other.clone(),
            (Self::Unique, Self::Unique) => Self::Unique,
            _ => Self::Shared,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarUsage {
    usage: ImMap<Symbol, ReferenceCount>,
}

impl IntoIterator for VarUsage {
    type Item = (Symbol, ReferenceCount);
    type IntoIter = im_rc::hashmap::ConsumingIter<(Symbol, ReferenceCount)>;

    fn into_iter(self) -> Self::IntoIter {
        self.usage.into_iter()
    }
}

impl VarUsage {
    pub fn default() -> VarUsage {
        VarUsage {
            usage: (ImMap::default()),
        }
    }

    pub fn register_with(&mut self, symbol: &Symbol, rc: &ReferenceCount) {
        let value = match self.usage.get(symbol) {
            None => rc.clone(),
            Some(current) => ReferenceCount::add(current, rc),
        };

        self.usage.insert(symbol.clone(), value);
    }

    pub fn register(&mut self, symbol: &Symbol) {
        use self::ReferenceCount::*;
        self.register_with(symbol, &Unique);
    }

    pub fn unregister(&mut self, symbol: &Symbol) {
        self.usage.remove(symbol);
    }

    pub fn get_usage(&self, symbol: &Symbol) -> Option<&ReferenceCount> {
        self.usage.get(symbol)
    }

    pub fn add(&mut self, other: &Self) {
        for (symbol, v) in &other.usage {
            self.register_with(symbol, v);
        }
    }

    pub fn or(&mut self, other: &Self) {
        for (symbol, v) in &other.usage {
            let value = match self.usage.get(symbol) {
                None => v.clone(),
                Some(current) => ReferenceCount::or(current, v),
            };

            self.usage.insert(symbol.clone(), value);
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, PartialOrd)]
pub struct FieldAccess {
    pub fields: ImMap<String, (ReferenceCount, FieldAccess)>,
}

type FAMap = std::collections::HashMap<String, ReferenceCount>;

impl Into<FAMap> for FieldAccess {
    fn into(self) -> FAMap {
        let mut result = std::collections::HashMap::default();

        for (name, (rc, nested_access)) in self.fields {
            result.insert(name.clone(), rc);

            let nested_map: FAMap = nested_access.into();

            for (n_name, n_rc) in nested_map {
                result.insert(name.clone() + "." + &n_name, n_rc);
            }
        }

        result
    }
}

impl FieldAccess {
    pub fn from_chain(access_chain: Vec<Lowercase>) -> Self {
        use ReferenceCount::*;
        let strings: Vec<String> = access_chain
            .iter()
            .map(|v| v.as_str().to_string())
            .collect();

        let mut accum = FieldAccess::default();
        let mut is_final = true;
        for field in strings.into_iter().rev() {
            let mut next = FieldAccess::default();
            let uniq = if is_final {
                is_final = false;
                Unique
            } else {
                Seen
            };
            next.fields.insert(field.to_string(), (uniq, accum));
            accum = FieldAccess {
                fields: next.fields,
            };
        }

        accum
    }

    fn or_subtree(&mut self, constraint: &ReferenceCount) {
        for (rc, nested) in self.fields.iter_mut() {
            *rc = ReferenceCount::or(rc, constraint);
            nested.or_subtree(constraint);
        }
    }

    pub fn parallel_merge(&mut self, other: &Self) {
        for (field_name, (other_rc, other_nested)) in other.fields.clone() {
            if self.fields.contains_key(&field_name) {
                if let Some((self_rc, self_nested)) = self.fields.get_mut(&field_name) {
                    self_nested.parallel_merge(&other_nested);
                    *self_rc = ReferenceCount::or(self_rc, &other_rc);
                }
            } else {
                self.fields.insert(field_name, (other_rc, other_nested));
            }
        }
    }
    pub fn sequential_merge(&mut self, other: &Self) {
        for (field_name, (other_rc, mut other_nested)) in other.fields.clone() {
            if self.fields.contains_key(&field_name) {
                if let Some((self_rc, self_nested)) = self.fields.get_mut(&field_name) {
                    *self_rc = ReferenceCount::add(self_rc, &other_rc);
                    if &*self_rc > &ReferenceCount::Seen {
                        // e.g. we access `rec.foo` and `rec.foo.bar`.
                        // Since a reference to `rec.foo` exists, there are at least two references to `foo.bar`
                        // (`foo.bar` itself and `.bar rec.foo`)
                        // Therefore fields of the subtrees must be shared!
                        self_nested.or_subtree(&ReferenceCount::Shared);
                        other_nested.or_subtree(&ReferenceCount::Shared);
                    }

                    self_nested.sequential_merge(&other_nested);
                }
            } else {
                self.fields.insert(field_name, (other_rc, other_nested));
            }
        }
    }
    pub fn parallel(&mut self, access_chain: Vec<Lowercase>) {
        let other = Self::from_chain(access_chain);
        self.parallel_merge(&other);
    }

    pub fn sequential(&mut self, access_chain: Vec<Lowercase>) {
        let other = Self::from_chain(access_chain);
        self.sequential_merge(&other);
    }

    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }
}

pub fn annotate_usage(expr: &Expr, usage: &mut VarUsage) {
    use Expr::*;
    use ReferenceCount::*;

    match expr {
        RuntimeError(_)
        | Int(_, _)
        | Float(_, _)
        | Str(_)
        | BlockStr(_)
        | EmptyRecord
        | Accessor { .. } => {}

        Var {
            symbol_for_lookup, ..
        } => usage.register(symbol_for_lookup),

        If {
            loc_cond,
            loc_then,
            loc_else,
            ..
        } => {
            annotate_usage(&loc_cond.value, usage);

            let mut then_usage = VarUsage::default();
            let mut else_usage = VarUsage::default();

            annotate_usage(&loc_then.value, &mut then_usage);
            annotate_usage(&loc_else.value, &mut else_usage);

            then_usage.or(&else_usage);
            usage.add(&then_usage);
        }
        When {
            loc_cond, branches, ..
        } => {
            annotate_usage(&loc_cond.value, usage);

            let mut branches_usage = VarUsage::default();
            for (_, loc_branch) in branches {
                let mut current_usage = VarUsage::default();

                annotate_usage(&loc_branch.value, &mut current_usage);

                branches_usage.or(&current_usage);
            }

            usage.add(&branches_usage);
        }

        List { loc_elems, .. } => {
            for loc_elem in loc_elems {
                annotate_usage(&loc_elem.value, usage);
            }
        }
        LetNonRec(def, loc_expr, _) => {
            annotate_usage(&def.loc_expr.value, usage);
            annotate_usage(&loc_expr.value, usage);
        }
        LetRec(defs, loc_expr, _) => {
            if defs.len() == 1 {
                // just like a letrec, but mark defined symbol as Shared
                let def = &defs[0];
                for (symbol, _) in def.pattern_vars.clone() {
                    usage.register_with(&symbol, &Shared);
                }
                annotate_usage(&def.loc_expr.value, usage);
            } else {
                let mut rec_usage = VarUsage::default();
                // care is required. If f1 and f2 are mutually recursive, and f1 accesses a record
                // whilst f2 updates that record, the record must be marked as Shared, disallowing
                // a mutable update in f2
                for def in defs {
                    for (symbol, _) in def.pattern_vars.clone() {
                        usage.register_with(&symbol, &Shared);
                    }

                    let mut current_usage = VarUsage::default();
                    annotate_usage(&def.loc_expr.value, &mut current_usage);

                    let mut a = rec_usage.clone();
                    let b = rec_usage.clone();

                    a.add(&current_usage);
                    current_usage.add(&b);
                    current_usage.or(&a);

                    rec_usage.add(&current_usage);
                }

                usage.add(&rec_usage);
            }

            annotate_usage(&loc_expr.value, usage);
        }
        Call(fun, loc_args, _) => {
            annotate_usage(&fun.1.value, usage);

            for (_, arg) in loc_args {
                annotate_usage(&arg.value, usage);
            }
        }
        Closure(_, _, _, _, body) => {
            annotate_usage(&body.0.value, usage);
        }

        Tag { arguments, .. } => {
            for (_, loc_expr) in arguments {
                annotate_usage(&loc_expr.value, usage);
            }
        }
        Record(_, fields) => {
            for (_, field) in fields {
                annotate_usage(&field.loc_expr.value, usage);
            }
        }
        Expr::Update {
            symbol, updates, ..
        } => {
            for (_, field) in updates {
                annotate_usage(&field.loc_expr.value, usage);
            }

            usage.register_with(symbol, &ReferenceCount::Update(FieldAccess::default()));
        }
        Expr::Access {
            field, loc_expr, ..
        } => {
            let mut chain = Vec::new();
            if let Some(symbol) = get_access_chain(&loc_expr.value, &mut chain) {
                chain.push(field.clone());

                let fa = FieldAccess::from_chain(chain);

                usage.register_with(symbol, &ReferenceCount::Access(fa));
            } else {
                annotate_usage(&loc_expr.value, usage);
            }
        }
    }
}

fn get_access_chain<'a>(expr: &'a Expr, chain: &mut Vec<Lowercase>) -> Option<&'a Symbol> {
    use Expr::*;

    match expr {
        Expr::Access {
            field, loc_expr, ..
        } => {
            let symbol = get_access_chain(&loc_expr.value, chain)?;

            chain.push(field.clone());

            Some(symbol)
        }
        Var {
            symbol_for_lookup, ..
        } => Some(symbol_for_lookup),

        _ => None,
    }
}
