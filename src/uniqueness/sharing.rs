use crate::can::ident::Lowercase;
use crate::can::symbol::Symbol;
use crate::collections::ImMap;
use std::cmp::Ordering;

#[derive(Clone, Debug, PartialEq)]
pub enum ReferenceCount {
    Shared,
    Unique,
    Seen,
}

impl ReferenceCount {
    pub fn add(a: &ReferenceCount, b: &ReferenceCount) -> Self {
        match (a, b) {
            (Self::Seen, Self::Seen) => Self::Seen,
            (Self::Seen, Self::Unique) => Self::Unique,
            (Self::Unique, Self::Seen) => Self::Unique,
            _ => Self::Shared,
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

impl PartialOrd for ReferenceCount {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use ReferenceCount::*;

        match (self, other) {
            (Seen, Seen) => Some(Ordering::Equal),
            (Seen, _) => Some(Ordering::Less),
            (_, Seen) => Some(Ordering::Greater),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarUsage {
    usage: ImMap<Symbol, ReferenceCount>,
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

#[derive(Debug, Default, Clone, PartialEq)]
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

    fn parallel_merge(&mut self, other: &Self) {
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
    fn sequential_merge(&mut self, other: &Self) {
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
        dbg!(&self, &other);
        self.sequential_merge(&other);
    }

    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }
}
