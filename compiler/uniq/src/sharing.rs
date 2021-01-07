use roc_can::expr::Expr;
use roc_can::pattern::{DestructType, Pattern};
use roc_collections::all::{ImMap, ImSet};
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mark {
    Seen,
    Unique,
    Shared,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct FieldAccess {
    fields: ImMap<Lowercase, Usage>,
}

impl IntoIterator for FieldAccess {
    type Item = (Lowercase, Usage);
    type IntoIter = im_rc::hashmap::ConsumingIter<(Lowercase, Usage)>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.into_iter()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Usage {
    Simple(Mark),

    // Lists, Sets, ADTs
    ApplyAccess(Mark, Vec<Usage>),
    ApplyUpdate(ImSet<usize>, Vec<Usage>),

    // Records
    RecordAccess(Mark, FieldAccess),
    RecordUpdate(ImSet<Lowercase>, FieldAccess),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Container {
    Record,
    List,
}

pub trait Composable {
    fn sequential(&mut self, other: &Self);
    fn parallel(&mut self, other: &Self);
}

impl Composable for FieldAccess {
    fn sequential(&mut self, other: &Self) {
        for (field_name, other_usage) in other.fields.clone() {
            if self.fields.contains_key(&field_name) {
                if let Some(self_usage) = self.fields.get_mut(&field_name) {
                    self_usage.sequential(&other_usage);

                    if *self_usage != Usage::Simple(Mark::Seen) {
                        // e.g. we access `rec.foo` and `rec.foo.bar`.
                        // Since a reference to `rec.foo` exists, there are at least two references to `foo.bar`
                        // (`foo.bar` itself and `.bar rec.foo`)
                        // Therefore fields of the subtrees must be shared!

                        // TODO make this work? Seems to function well without it
                        // self_nested.or_subtree(&Usage::Shared);
                        // other_nested.or_subtree(&Usage::Shared);
                        //
                        // member function on FieldAccess
                        //    fn or_subtree(&mut self, constraint: &Usage) {
                        //        for field_usage in self.fields.iter_mut() {
                        //            field_usage.parallel(constraint);
                        //        }
                        //    }
                    }
                }
            } else {
                self.fields.insert(field_name, other_usage.clone());
            }
        }
    }

    fn parallel(&mut self, other: &Self) {
        for (field_name, other_usage) in other.fields.clone() {
            if self.fields.contains_key(&field_name) {
                if let Some(self_usage) = self.fields.get_mut(&field_name) {
                    self_usage.parallel(&other_usage);
                }
            } else {
                self.fields.insert(field_name, other_usage.clone());
            }
        }
    }
}

impl Composable for Vec<Usage> {
    fn sequential(&mut self, other: &Self) {
        // NOTE we don't know they have the same length

        let mut it_other = other.iter();
        {
            for (self_usage, other_usage) in self.iter_mut().zip(&mut it_other) {
                self_usage.sequential(&other_usage);

                if *self_usage != Usage::Simple(Mark::Seen) {
                    // e.g. we access `rec.foo` and `rec.foo.bar`.
                    // Since a reference to `rec.foo` exists, there are at least two references to `foo.bar`
                    // (`foo.bar` itself and `.bar rec.foo`)
                    // Therefore fields of the subtrees must be shared!

                    // TODO make this work? Seems to function well without it
                    // self_nested.or_subtree(&Usage::Shared);
                    // other_nested.or_subtree(&Usage::Shared);
                    //
                    // member function on FieldAccess
                    //    fn or_subtree(&mut self, constraint: &Usage) {
                    //        for field_usage in self.fields.iter_mut() {
                    //            field_usage.parallel(constraint);
                    //        }
                    //    }
                }
            }
        }

        // if there are remaining elements in other, push them onto self
        for other_usage in it_other {
            self.push(other_usage.clone());
        }
    }

    fn parallel(&mut self, other: &Self) {
        // NOTE we don't know they have the same length

        let mut it_other = other.iter();
        {
            for (self_usage, other_usage) in self.iter_mut().zip(&mut it_other) {
                self_usage.parallel(&other_usage);
            }
        }

        // if there are remaining elements in other, push them onto self
        for other_usage in it_other {
            self.push(other_usage.clone());
        }
    }
}

impl Composable for Usage {
    fn sequential(&mut self, other: &Self) {
        use Mark::*;
        use Usage::*;

        match (&self, other) {
            // Shared always wins
            (Simple(Shared), _) | (_, Simple(Shared)) => {
                *self = Simple(Shared);
            }

            // Record Update
            (RecordUpdate(_, _), RecordUpdate(_, _)) | (RecordUpdate(_, _), RecordAccess(_, _)) => {
                *self = Simple(Shared);
            }

            (RecordUpdate(_, _), Simple(Unique)) | (Simple(Unique), RecordUpdate(_, _)) => {
                *self = Simple(Shared);
            }

            (RecordAccess(m1, fields1), RecordUpdate(overwritten, fields2)) => {
                *self = correct_overwritten(*m1, fields1, Seen, fields2, overwritten);
            }

            (Simple(Seen), RecordUpdate(overwritten, fa)) => {
                *self = RecordUpdate(overwritten.clone(), fa.clone());
            }
            (RecordUpdate(overwritten, fa), Simple(Seen)) => {
                *self = RecordUpdate(overwritten.clone(), fa.clone());
            }

            // RecordAccess
            (RecordAccess(m1, fa1), RecordAccess(m2, fa2)) => {
                let mut fa = fa1.clone();
                fa.sequential(fa2);

                let mut m = *m1;
                m.sequential(m2);

                *self = RecordAccess(m, fa);
            }
            (RecordAccess(m, fa1), Simple(Unique)) => {
                let mut copy = RecordAccess(*m, fa1.clone());
                make_subtree_shared(&mut copy);

                // correct the mark of the top-level access
                *self = if let RecordAccess(_, fa) = copy {
                    let mut m = *m;
                    m.sequential(&Unique);

                    RecordAccess(m, fa)
                } else {
                    unreachable!()
                };
            }
            (Simple(Unique), RecordAccess(m, fa)) => {
                let mut copy = RecordAccess(*m, fa.clone());
                make_subtree_shared(&mut copy);

                // correct the mark of the top-level access
                *self = if let RecordAccess(_, fa) = copy {
                    let mut m = *m;
                    m.sequential(&Unique);

                    RecordAccess(m, fa)
                } else {
                    unreachable!()
                };
            }

            (Simple(m1 @ Seen), RecordAccess(m2, fa)) => {
                let mut m = *m1;
                m.sequential(m2);
                *self = RecordAccess(m, fa.clone())
            }

            (RecordAccess(m1, fa), Simple(m2 @ Seen)) => {
                let mut m = *m1;
                m.sequential(m2);
                *self = RecordAccess(m, fa.clone());
            }

            // Apply Update
            (ApplyUpdate(_, _), ApplyUpdate(_, _)) | (ApplyUpdate(_, _), ApplyAccess(_, _)) => {
                *self = Simple(Shared);
            }

            (ApplyUpdate(_, _), Simple(Unique)) | (Simple(Unique), ApplyUpdate(_, _)) => {
                *self = Simple(Shared);
            }

            (ApplyAccess(m1, fields1), ApplyUpdate(overwritten, fields2)) => {
                *self = correct_overwritten_apply(*m1, fields1, Seen, fields2, overwritten);
            }

            (Simple(Seen), ApplyUpdate(overwritten, fa)) => {
                *self = ApplyUpdate(overwritten.clone(), fa.clone());
            }
            (ApplyUpdate(overwritten, fa), Simple(Seen)) => {
                *self = ApplyUpdate(overwritten.clone(), fa.clone());
            }

            // RecordAccess
            (ApplyAccess(m1, fa1), ApplyAccess(m2, fa2)) => {
                let mut fa = fa1.clone();
                fa.sequential(fa2);

                let mut m = *m1;
                m.sequential(m2);

                *self = ApplyAccess(m, fa);
            }
            (ApplyAccess(m, fa1), Simple(Unique)) => {
                let mut copy = ApplyAccess(*m, fa1.clone());
                make_subtree_shared(&mut copy);

                // correct the mark of the top-level access
                *self = if let ApplyAccess(_, fa) = copy {
                    let mut m = *m;
                    m.sequential(&Unique);

                    ApplyAccess(m, fa)
                } else {
                    unreachable!()
                };
            }
            (Simple(Unique), ApplyAccess(m, fa)) => {
                let mut copy = ApplyAccess(*m, fa.clone());
                make_subtree_shared(&mut copy);

                // correct the mark of the top-level access
                *self = if let ApplyAccess(_, fa) = copy {
                    let mut m = *m;
                    m.sequential(&Unique);

                    ApplyAccess(m, fa)
                } else {
                    unreachable!()
                };
            }

            (Simple(m1 @ Seen), ApplyAccess(m2, fa)) => {
                let mut m = *m1;
                m.sequential(m2);
                *self = ApplyAccess(m, fa.clone())
            }

            (ApplyAccess(m1, fa), Simple(m2 @ Seen)) => {
                let mut m = *m1;
                m.sequential(m2);
                *self = ApplyAccess(m, fa.clone());
            }

            // Things cannot change type
            (ApplyAccess(_, _), RecordAccess(_, _))
            | (ApplyAccess(_, _), RecordUpdate(_, _))
            | (ApplyUpdate(_, _), RecordAccess(_, _))
            | (ApplyUpdate(_, _), RecordUpdate(_, _))
            | (RecordAccess(_, _), ApplyAccess(_, _))
            | (RecordUpdate(_, _), ApplyAccess(_, _))
            | (RecordAccess(_, _), ApplyUpdate(_, _))
            | (RecordUpdate(_, _), ApplyUpdate(_, _)) => {
                unreachable!("applies cannot turn into records or vice versa!")
            }

            // Simple
            (Simple(s1), Simple(s2)) => {
                let mut s = *s1;
                s.sequential(s2);
                *self = Simple(s);
            }
        }
    }

    fn parallel(&mut self, other: &Self) {
        use Mark::*;
        use Usage::*;

        match (&self, other) {
            (Simple(s1), Simple(s2)) => {
                let mut s = *s1;
                s.parallel(s2);
                *self = Simple(s);
            }

            // Shared always wins
            (Simple(Shared), _) | (_, Simple(Shared)) => {
                *self = Simple(Shared);
            }
            // Record update
            (RecordUpdate(w1, fa1), RecordUpdate(w2, fa2)) => {
                let mut fa = fa1.clone();
                fa.parallel(fa2);

                let w = w1.clone().intersection(w2.clone());

                *self = RecordUpdate(w, fa);
            }

            (RecordUpdate(_, _), Simple(Unique)) | (RecordUpdate(_, _), Simple(Seen)) => {
                //*self = RecordUpdate( w.clone(), fa.clone());
            }
            (Simple(Unique), RecordUpdate(w, fa)) | (Simple(Seen), RecordUpdate(w, fa)) => {
                *self = RecordUpdate(w.clone(), fa.clone());
            }

            (RecordUpdate(w, fa1), RecordAccess(_, fa2)) => {
                let mut fa = fa1.clone();
                fa.parallel(&fa2.clone());
                *self = RecordUpdate(w.clone(), fa);
            }
            (RecordAccess(_, fa1), RecordUpdate(w, fa2)) => {
                let mut fa = fa1.clone();
                fa.parallel(&fa2.clone());
                *self = RecordUpdate(w.clone(), fa);
            }

            // Record Access
            (RecordAccess(m1, fa1), RecordAccess(m2, fa2)) => {
                let mut m = *m1;
                m.parallel(m2);

                let mut fa = fa1.clone();
                fa.parallel(fa2);
                *self = RecordAccess(m, fa)
            }
            (RecordAccess(m, fa), Simple(Unique)) => {
                let mut m = *m;
                m.parallel(&Unique);
                *self = RecordAccess(m, fa.clone());
            }
            (RecordAccess(_, _), Simple(Seen)) => {
                // *self = RecordAccess( *m, fa.clone());
            }
            (Simple(m1 @ Unique), RecordAccess(m2, fa))
            | (Simple(m1 @ Seen), RecordAccess(m2, fa)) => {
                let mut m = *m1;
                m.sequential(m2);
                *self = RecordAccess(m, fa.clone());
            }

            // Apply Update
            (ApplyUpdate(w1, fa1), ApplyUpdate(w2, fa2)) => {
                let mut fa = fa1.clone();
                fa.parallel(fa2);

                let w = w1.clone().intersection(w2.clone());

                *self = ApplyUpdate(w, fa);
            }

            (ApplyUpdate(_, _), Simple(Unique)) | (ApplyUpdate(_, _), Simple(Seen)) => {
                //*self = ApplyUpdate( w.clone(), fa.clone());
            }
            (Simple(Unique), ApplyUpdate(w, fa)) | (Simple(Seen), ApplyUpdate(w, fa)) => {
                *self = ApplyUpdate(w.clone(), fa.clone());
            }

            (ApplyUpdate(w, fa1), ApplyAccess(_, fa2)) => {
                let mut fa = fa1.clone();
                fa.parallel(&fa2.clone());
                *self = ApplyUpdate(w.clone(), fa);
            }
            (ApplyAccess(_, fa1), ApplyUpdate(w, fa2)) => {
                let mut fa = fa1.clone();
                fa.parallel(&fa2.clone());
                *self = ApplyUpdate(w.clone(), fa);
            }

            // Apply Access
            (ApplyAccess(m1, fa1), ApplyAccess(m2, fa2)) => {
                let mut m = *m1;
                m.parallel(m2);

                let mut fa = fa1.clone();
                fa.parallel(fa2);
                *self = ApplyAccess(m, fa)
            }
            (ApplyAccess(m, fa), Simple(Unique)) => {
                let mut m = *m;
                m.parallel(&Unique);
                *self = ApplyAccess(m, fa.clone());
            }
            (ApplyAccess(_, _), Simple(Seen)) => {
                // *self = ApplyAccess( *m, fa.clone());
            }
            (Simple(m1 @ Unique), ApplyAccess(m2, fa))
            | (Simple(m1 @ Seen), ApplyAccess(m2, fa)) => {
                let mut m = *m1;
                m.sequential(m2);
                *self = ApplyAccess(m, fa.clone());
            }

            // Things cannot change type
            (ApplyAccess(_, _), RecordAccess(_, _))
            | (ApplyAccess(_, _), RecordUpdate(_, _))
            | (ApplyUpdate(_, _), RecordAccess(_, _))
            | (ApplyUpdate(_, _), RecordUpdate(_, _))
            | (RecordAccess(_, _), ApplyAccess(_, _))
            | (RecordUpdate(_, _), ApplyAccess(_, _))
            | (RecordAccess(_, _), ApplyUpdate(_, _))
            | (RecordUpdate(_, _), ApplyUpdate(_, _)) => {
                unreachable!("applies cannot turn into records or vice versa!")
            }
        }
    }
}

impl Composable for Mark {
    fn sequential(&mut self, other: &Self) {
        use Mark::*;

        match (&self, other) {
            (Shared, _) | (_, Shared) => {
                *self = Shared;
            }
            (Unique, Unique) => {
                *self = Shared;
            }
            (Unique, _) | (_, Unique) => {
                *self = Unique;
            }
            (Seen, Seen) => {}
        }
    }

    fn parallel(&mut self, other: &Self) {
        use Mark::*;

        match (&self, other) {
            (Shared, _) | (_, Shared) => {
                *self = Shared;
            }
            (Unique, Unique) => {
                *self = Unique;
            }
            (Unique, _) | (_, Unique) => {
                *self = Unique;
            }
            (Seen, Seen) => {}
        }
    }
}

fn correct_overwritten(
    mut mark1: Mark,
    fa1: &FieldAccess,
    mark2: Mark,
    fa2: &FieldAccess,
    overwritten: &ImSet<Lowercase>,
) -> Usage {
    use Usage::*;

    let mut fa1 = fa1.clone();

    mark1.sequential(&mark2);
    fa1.sequential(fa2);

    // fields that are accessed, but not overwritten in the update, must be shared!
    for (label, usage) in fa1.fields.clone().keys().zip(fa1.fields.iter_mut()) {
        if !overwritten.contains(&label.clone()) {
            make_subtree_shared(usage);
        }
    }

    RecordUpdate(overwritten.clone(), fa1)
}

fn correct_overwritten_apply(
    mut mark1: Mark,
    fa1: &[Usage],
    mark2: Mark,
    fa2: &[Usage],
    overwritten: &ImSet<usize>,
) -> Usage {
    use Usage::*;

    let mut fa1 = fa1.to_owned();
    // TODO fix this cloning
    // tricky because Composable is defined on Vec, not &[]
    let fa2 = fa2.to_owned();

    mark1.sequential(&mark2);
    fa1.sequential(&fa2);

    // fields that are accessed, but not overwritten in the update, must be shared!
    for (index, usage) in fa1.iter_mut().enumerate() {
        if !overwritten.contains(&index) {
            make_subtree_shared(usage);
        }
    }

    ApplyUpdate(overwritten.clone(), fa1)
}

fn make_subtree_shared(usage: &mut Usage) {
    use Mark::*;
    use Usage::*;

    // TODO should Seen not also become Shared?
    match usage {
        Simple(Seen) | Simple(Shared) => {}
        Simple(Unique) => {
            *usage = Simple(Shared);
        }

        RecordUpdate(_, fa) => {
            for nested in fa.fields.iter_mut() {
                make_subtree_shared(nested);
            }
        }
        RecordAccess(m, fa) => {
            for nested in fa.fields.iter_mut() {
                make_subtree_shared(nested);
            }
            *m = match &m {
                Seen => Seen,
                _ => Shared,
            };
        }
        ApplyUpdate(_, fa) => {
            for nested in fa.iter_mut() {
                make_subtree_shared(nested);
            }
        }

        ApplyAccess(m, fa) => {
            for nested in fa.iter_mut() {
                make_subtree_shared(nested);
            }
            *m = match &m {
                Seen => Seen,
                _ => Shared,
            };
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarUsage {
    pub usage: ImMap<Symbol, Usage>,
    pub closure_usage_signatures: ImMap<Symbol, Vec<Usage>>,
}

impl VarUsage {
    pub fn default() -> VarUsage {
        let mut closure_signatures = ImMap::default();

        closure_signatures.insert(
            Symbol::NUM_ADD,
            vec![Usage::Simple(Mark::Seen), Usage::Simple(Mark::Seen)],
        );
        closure_signatures.insert(
            Symbol::LIST_GET,
            vec![
                Usage::ApplyAccess(Mark::Seen, vec![Usage::Simple(Mark::Unique)]),
                Usage::Simple(Mark::Seen),
            ],
        );

        closure_signatures.insert(Symbol::LIST_IS_EMPTY, vec![Usage::Simple(Mark::Seen)]);
        closure_signatures.insert(Symbol::LIST_LEN, vec![Usage::Simple(Mark::Seen)]);

        closure_signatures.insert(
            Symbol::LIST_SET,
            vec![
                Usage::ApplyUpdate(ImSet::default(), vec![Usage::Simple(Mark::Seen)]),
                Usage::Simple(Mark::Seen),
                Usage::Simple(Mark::Unique),
            ],
        );

        VarUsage {
            usage: ImMap::default(),
            closure_usage_signatures: closure_signatures,
        }
    }

    pub fn register_with(&mut self, symbol: Symbol, rc: &Usage) {
        let value = match self.usage.get(&symbol) {
            None => rc.clone(),
            Some(current) => {
                let mut current = current.clone();
                current.sequential(rc);
                current
            }
        };

        self.usage.insert(symbol, value);
    }

    pub fn register_shared(&mut self, symbol: Symbol) {
        use self::Usage::*;
        self.register_with(symbol, &Simple(Mark::Shared));
    }

    pub fn register_unique(&mut self, symbol: Symbol) {
        use self::Usage::*;
        self.register_with(symbol, &Simple(Mark::Unique));
    }

    pub fn register_seen(&mut self, symbol: Symbol) {
        use self::Usage::*;
        self.register_with(symbol, &Simple(Mark::Seen));
    }

    pub fn unregister(&mut self, symbol: Symbol) {
        self.usage.remove(&symbol);
    }

    pub fn get_usage(&self, symbol: Symbol) -> Option<&Usage> {
        self.usage.get(&symbol)
    }
}

impl Composable for VarUsage {
    fn sequential(&mut self, other: &Self) {
        for (symbol, v) in &other.usage {
            self.register_with(*symbol, v);
        }
    }

    fn parallel(&mut self, other: &Self) {
        for (symbol, v) in &other.usage {
            let value = match self.usage.get(symbol) {
                None => v.clone(),
                Some(current) => {
                    let mut current = current.clone();
                    current.parallel(v);
                    current
                }
            };

            self.usage.insert(*symbol, value);
        }
    }
}

// type FAMap = std::collections::HashMap<String, Usage>;
//
// impl Into<FAMap> for FieldAccess {
//     fn into(self) -> FAMap {
//         let mut result = std::collections::HashMap::default();
//
//         for (name, (rc, nested_access)) in self.fields {
//             result.insert(name.clone(), rc);
//
//             let nested_map: FAMap = nested_access.into();
//
//             for (n_name, n_rc) in nested_map {
//                 result.insert(name.clone() + "." + &n_name, n_rc);
//             }
//         }
//
//         result
//     }
// }

impl Usage {
    pub fn from_chain(access_chain: Vec<Lowercase>) -> Self {
        use Mark::*;
        use Usage::*;
        let mut accum = Simple(Unique);

        for field in access_chain.into_iter().rev() {
            let mut fa = FieldAccess::default();
            fa.fields.insert(field, accum);
            accum = Usage::RecordAccess(Mark::Seen, fa);
        }

        accum
    }

    pub fn parallel_chain(&mut self, access_chain: Vec<Lowercase>) {
        let other = Self::from_chain(access_chain);
        self.parallel(&other);
    }

    pub fn sequential_chain(&mut self, access_chain: Vec<Lowercase>) {
        let other = Self::from_chain(access_chain);
        self.sequential(&other);
    }
}

impl FieldAccess {
    pub fn new(fields: ImMap<Lowercase, Usage>) -> Self {
        FieldAccess { fields }
    }

    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }

    pub fn len(&self) -> usize {
        self.fields.len()
    }

    pub fn get(&self, key: &Lowercase) -> Option<&Usage> {
        self.fields.get(key)
    }
}

fn annotate_usage_pattern(pattern: &Pattern, usage: &mut VarUsage) {
    use Pattern::*;

    match pattern {
        Identifier(_)
        | IntLiteral(_, _)
        | NumLiteral(_, _)
        | FloatLiteral(_, _)
        | StrLiteral(_)
        | Underscore
        | Shadowed(_, _)
        | UnsupportedPattern(_)
        | MalformedPattern(_, _) => {}

        AppliedTag { arguments, .. } => {
            for (_, loc_pattern) in arguments {
                annotate_usage_pattern(&loc_pattern.value, usage);
            }
        }
        RecordDestructure { destructs, .. } => {
            use DestructType::*;

            for loc_destruct in destructs {
                match &loc_destruct.value.typ {
                    Required | Guard(_, _) => {}
                    Optional(_, loc_expr) => annotate_usage(&loc_expr.value, usage),
                }
            }
        }
    }
}

pub fn annotate_usage(expr: &Expr, usage: &mut VarUsage) {
    use Expr::*;

    match expr {
        RuntimeError(_)
        | Num(_, _)
        | Int(_, _, _)
        | Float(_, _, _)
        | Str { .. }
        | EmptyRecord
        | Accessor { .. }
        | RunLowLevel { .. }
        | ForeignCall { .. } => {}

        Var(symbol) => usage.register_unique(*symbol),

        If {
            branches,
            final_else,
            ..
        } => {
            let mut branch_usage = VarUsage::default();
            for (loc_cond, loc_then) in branches {
                annotate_usage(&loc_cond.value, usage);

                let mut then_usage = VarUsage::default();

                annotate_usage(&loc_then.value, &mut then_usage);

                branch_usage.parallel(&then_usage);
            }

            let mut else_usage = VarUsage::default();
            annotate_usage(&final_else.value, &mut else_usage);

            branch_usage.parallel(&else_usage);
            usage.sequential(&branch_usage);
        }
        When {
            loc_cond, branches, ..
        } => {
            annotate_usage(&loc_cond.value, usage);

            let mut branches_usage = VarUsage::default();
            for branch in branches {
                let mut current_usage = VarUsage::default();
                // annotate defaults of optional record fields
                for loc_pattern in &branch.patterns {
                    annotate_usage_pattern(&loc_pattern.value, usage);
                }

                let loc_branch = &branch.value;
                annotate_usage(&loc_branch.value, &mut current_usage);

                branches_usage.parallel(&current_usage);
            }

            usage.sequential(&branches_usage);
        }

        List { loc_elems, .. } => {
            for loc_elem in loc_elems {
                annotate_usage(&loc_elem.value, usage);
            }
        }
        LetNonRec(def, loc_expr, _) => {
            annotate_usage(&def.loc_expr.value, usage);
            annotate_usage(&loc_expr.value, usage);

            // annotate defaults of optional record fields
            annotate_usage_pattern(&def.loc_pattern.value, usage)
        }
        LetRec(defs, loc_expr, _) => {
            // TODO test this with a practical example.
            if defs.len() == 1 {
                // just like a letrec, but mark defined symbol as Shared
                let def = &defs[0];
                for (symbol, _) in def.pattern_vars.clone() {
                    usage.register_shared(symbol);
                }
                // annotate defaults of optional record fields
                annotate_usage_pattern(&def.loc_pattern.value, usage);

                annotate_usage(&def.loc_expr.value, usage);
            } else {
                let mut rec_usage = VarUsage::default();
                // care is required. If f1 and f2 are mutually recursive, and f1 accesses a record
                // whilst f2 updates that record, the record must be marked as Shared, disallowing
                // a mutable update in f2
                for def in defs {
                    for (symbol, _) in def.pattern_vars.clone() {
                        usage.register_shared(symbol);
                    }

                    let mut current_usage = VarUsage::default();
                    annotate_usage(&def.loc_expr.value, &mut current_usage);

                    // annotate defaults of optional record fields
                    annotate_usage_pattern(&def.loc_pattern.value, usage);

                    let mut a = rec_usage.clone();
                    let b = rec_usage.clone();

                    a.sequential(&current_usage);
                    current_usage.sequential(&b);
                    current_usage.parallel(&a);

                    rec_usage.sequential(&current_usage);
                }

                usage.sequential(&rec_usage);
            }

            annotate_usage(&loc_expr.value, usage);
        }
        Call(fun, loc_args, _) => {
            annotate_usage(&fun.1.value, usage);
            if let Var(symbol) = fun.1.value {
                // call by name

                // fetch the signature
                let opt_signature = match usage.closure_usage_signatures.get(&symbol) {
                    Some(v) => Some(v.clone()),
                    None => None,
                };

                if let Some(signature) = opt_signature {
                    // we know the usage signature of this function
                    for ((_, arg), annotated) in loc_args.iter().zip(signature.iter()) {
                        if let Var(arg_symbol) = arg.value {
                            usage.register_with(arg_symbol, &annotated);
                        } else {
                            annotate_usage(&arg.value, usage);
                        }
                    }

                    return;
                }
            }

            // unknown call
            for (_, arg) in loc_args {
                annotate_usage(&arg.value, usage);
            }
        }
        Closure {
            arguments,
            loc_body,
            ..
        } => {
            // annotate defaults of optional record fields
            for (_, loc_pattern) in arguments {
                annotate_usage_pattern(&loc_pattern.value, usage);
            }

            annotate_usage(&loc_body.value, usage);
        }

        Tag { arguments, .. } => {
            for (_, loc_expr) in arguments {
                annotate_usage(&loc_expr.value, usage);
            }
        }
        Record { fields, .. } => {
            for (_, field) in fields {
                annotate_usage(&field.loc_expr.value, usage);
            }
        }
        Expr::Update {
            symbol, updates, ..
        } => {
            let mut labels = ImSet::default();

            for (label, field) in updates {
                annotate_usage(&field.loc_expr.value, usage);
                labels.insert(label.clone());
            }

            usage.register_with(
                *symbol,
                &Usage::RecordUpdate(labels, FieldAccess::default()),
            );
        }
        Expr::Access {
            field, loc_expr, ..
        } => {
            let mut chain = Vec::new();
            if let Some(symbol) = get_access_chain(&loc_expr.value, &mut chain) {
                chain.push(field.clone());

                usage.register_with(*symbol, &Usage::from_chain(chain))
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
        Var(symbol) => Some(symbol),

        _ => None,
    }
}
