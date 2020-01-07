use crate::collections::{ImMap, ImSet};
use crate::subs::Variable;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Bool {
    Zero,
    One,
    And(Box<Bool>, Box<Bool>),
    Or(Box<Bool>, Box<Bool>),
    Not(Box<Bool>),
    Variable(Variable),
}

use self::Bool::*;

// TODO add inline pragma
pub fn not(nested: Bool) -> Bool {
    Not(Box::new(nested))
}

pub fn and(left: Bool, right: Bool) -> Bool {
    And(Box::new(left), Box::new(right))
}

pub fn or(left: Bool, right: Bool) -> Bool {
    Or(Box::new(left), Box::new(right))
}

pub fn any<I>(mut it: I) -> Bool
where
    I: Iterator<Item = Bool>,
{
    if let Some(first) = it.next() {
        it.fold(first, |a, b| or(a, b))
    } else {
        Zero
    }
}

pub fn all(terms: Product<Bool>) -> Bool {
    let mut it = terms.into_iter();

    if let Some(first) = it.next() {
        it.fold(first, |a, b| and(a, b))
    } else {
        One
    }
}

type Substitution = ImMap<Variable, Bool>;

type Product<A> = ImSet<A>;
type Sum<A> = ImSet<A>;

impl Bool {
    pub fn variables(&self) -> ImSet<Variable> {
        let mut result = ImSet::default();

        self.variables_help(&mut result);
        result
    }

    fn variables_help(&self, vars: &mut ImSet<Variable>) {
        match self {
            Zero => (),
            One => (),
            And(left, right) => {
                left.variables_help(vars);
                right.variables_help(vars)
            }
            Or(left, right) => {
                left.variables_help(vars);
                right.variables_help(vars)
            }
            Not(nested) => nested.variables_help(vars),
            Variable(var) => {
                vars.insert(*var);
            }
        };
    }

    pub fn substitute(&self, substitutions: &Substitution) -> Self {
        match self {
            Zero => Zero,
            One => One,
            And(left, right) => and(
                left.substitute(substitutions),
                right.substitute(substitutions),
            ),
            Or(left, right) => or(
                left.substitute(substitutions),
                right.substitute(substitutions),
            ),
            Not(nested) => not(nested.substitute(substitutions)),
            Variable(current) => match substitutions.get(current) {
                Some(new) => new.clone(),
                None => Variable(*current),
            },
        }
    }

    pub fn is_var(&self) -> bool {
        match self {
            Variable(_) => true,
            _ => false,
        }
    }

    // TODO add inline pragma
    pub fn not(nested: Bool) -> Bool {
        not(nested)
    }

    pub fn and(left: Bool, right: Bool) -> Bool {
        and(left, right)
    }

    pub fn or(left: Bool, right: Bool) -> Bool {
        or(left, right)
    }
}

pub fn simplify(term: Bool) -> Bool {
    let normalized = normalize_term(term);
    sop_to_term(simplify_sop(bcf(normalize_sop(term_to_sop(normalized)))))
}

pub fn sop_to_term(sop: Sop) -> Bool {
    any(sop.into_iter().map(|v| all(v)))
}

fn simplify_sop(sop: Sop) -> Sop {
    // filter out anything that is included in the remaining elements
    let mut active = sop.clone();
    let mut result = ImSet::default();
    for t in sop {
        if included(all(t.clone()), sop_to_term(active.without(&t))) {
            active.remove(&t);
        } else {
            result.insert(t);
        }
    }

    result
}

/// Blake canonical form
fn bcf(sop: Sop) -> Sop {
    absorptive(syllogistic(sop))
}

fn syllogistic(terms: Sop) -> Sop {
    let mut cs_prime = ImSet::default();

    for c in cartesian_product(terms.clone())
        .iter()
        .filter_map(|(x, y)| consensus(x, y))
    {
        if !terms
            .clone()
            .into_iter()
            .any(|x| included_term(c.clone(), x))
        {
            cs_prime.insert(c);
        }
    }

    if cs_prime.is_empty() {
        terms
    } else {
        syllogistic(terms.union(cs_prime))
    }
}

/// Absorption (apply the identify p + pq = p)
fn absorptive(sop: Sop) -> Sop {
    // TODO this is extremely inefficient!
    let mut accum: Vec<Product<Bool>> = Vec::new();

    for product in sop {
        accum = accum
            .into_iter()
            .filter(|v| !absorbs(&product, v))
            .collect();
        accum.push(product);
    }

    accum.into()
}

/// Does p absorb q? (can we replace p + q by p?)
fn absorbs(p: &Product<Bool>, q: &Product<Bool>) -> bool {
    p.iter().all(|x| q.contains(x))
}

fn consensus(p: &Product<Bool>, q: &Product<Bool>) -> Option<Product<Bool>> {
    let mut it = oppositions(p, q).into_iter();

    if let Some(x) = it.next() {
        if let None = it.next() {
            let mut result = ImSet::default();

            let compx = not(x.clone());

            for elem in p
                .iter()
                .chain(q.iter())
                .filter(|y| **y != x && **y != compx)
            {
                result.insert(elem.clone());
            }

            return Some(result);
        }
    }

    None
}

fn oppositions(ps: &Product<Bool>, qs: &Product<Bool>) -> Product<Bool> {
    let it1 = ps
        .clone()
        .into_iter()
        .filter(|p| qs.contains(&not(p.clone())));
    let it2 = qs
        .clone()
        .into_iter()
        .filter(|q| ps.contains(&not(q.clone())));

    let mut result = ImSet::default();
    for x in it1.chain(it2) {
        result.insert(x);
    }

    result
}

pub fn try_unify(p: Bool, q: Bool) -> Option<Substitution> {
    let (sub, consistency) = unify(p, q);

    let mut substitution = ImMap::default();

    for (x, p) in sub.into_iter() {
        if p != Variable(x) {
            substitution.insert(x, p);
        }
    }

    if consistency == Zero {
        Some(substitution)
    } else {
        None
    }
}

fn unify(p: Bool, q: Bool) -> (Substitution, Bool) {
    let condition = q.is_var() && !p.is_var();
    let t = if condition {
        or(and(q.clone(), not(p.clone())), and(not(q), p))
    } else {
        or(and(p.clone(), not(q.clone())), and(not(p), q))
    };

    unify0(t.variables(), t)
}

fn unify0(names: ImSet<Variable>, mut term: Bool) -> (Substitution, Bool) {
    let mut substitution: Substitution = ImMap::default();

    for x in names {
        let mut sub_zero = ImMap::default();
        sub_zero.insert(x, Zero);

        let mut sub_one = ImMap::default();
        sub_one.insert(x, One);

        let subbed_zero = term.substitute(&sub_zero);
        let subbed_one = term.substitute(&sub_one);

        term = and(subbed_zero.clone(), subbed_one.clone());

        let complicated = or(
            subbed_zero.substitute(&substitution),
            and(Variable(x), not(subbed_one.substitute(&substitution))),
        );

        let replacement: Bool = simplify(complicated);

        substitution.insert(x, replacement);
    }

    (substitution, simplify(term))
}

// --- Simplification ---

/// Normalisation of terms. Applies (in bottom-up fashion) the identities
///
/// x * 1 = x
/// x * 0 = 0
/// x + 1 = 1
/// x + 0 = x
/// !1 = 0
/// !0 = 1
pub fn normalize_term(term: Bool) -> Bool {
    match term {
        Zero => Zero,
        One => One,
        And(left, right) => {
            let p = normalize_term(*left);
            let q = normalize_term(*right);

            match (p == One, p == Zero, q == One, q == Zero) {
                (true, _, _, _) => q,
                (_, true, _, _) => Zero,
                (_, _, true, _) => p,
                (_, _, _, true) => Zero,
                _ => and(p, q),
            }
        }
        Or(left, right) => {
            let p = normalize_term(*left);
            let q = normalize_term(*right);

            match (p == One, p == Zero, q == One, q == Zero) {
                (true, _, _, _) => One,
                (_, true, _, _) => q,
                (_, _, true, _) => One,
                (_, _, _, true) => p,
                _ => or(p, q),
            }
        }
        Not(nested) => {
            let p = normalize_term(*nested);

            match (p == One, p == Zero) {
                (true, _) => Zero,
                (_, true) => One,
                _ => not(p),
            }
        }
        Variable(v) => Variable(v),
    }
}

// --- Inclusion ---

fn included(g: Bool, h: Bool) -> bool {
    contradiction(and(g, not(h)))
}

fn included_term(g: Product<Bool>, h: Product<Bool>) -> bool {
    included(all(g), all(h))
}

// --- Tautology / Contradiction ---

fn tautology(term: Bool) -> bool {
    normalize_pos(term_to_pos(normalize_term(term))).is_empty()
}

fn contradiction(term: Bool) -> bool {
    tautology(not(term))
}

// --- Normalization of POS / SOP

type Pos = Product<Sum<Bool>>;
type Sop = Sum<Product<Bool>>;

fn term_to_pos(term: Bool) -> Pos {
    conj_to_list(cnf(term))
        .into_iter()
        .map(|v| disj_to_list(v))
        .collect()
}

pub fn term_to_sop(term: Bool) -> Sop {
    disj_to_list(dnf(term))
        .into_iter()
        .map(|v| conj_to_list(v))
        .collect()
}

fn conj_to_list(term: Bool) -> Product<Bool> {
    match term {
        And(left, right) => {
            let p = conj_to_list(*left);
            let q = conj_to_list(*right);

            p.union(q)
        }
        _ => unit(term),
    }
}

fn disj_to_list(term: Bool) -> Sum<Bool> {
    match term {
        Or(left, right) => {
            let p = disj_to_list(*left);
            let q = disj_to_list(*right);

            p.union(q)
        }
        _ => unit(term),
    }
}

fn normalize_pos(pos: Pos) -> Pos {
    let mut result = ImSet::default();

    let singleton_one = unit(One);
    for sum in pos {
        let normalized = normalize_disj(sum);
        if normalized != singleton_one {
            result.insert(normalized);
        }
    }

    result
}

pub fn normalize_sop(sop: Sop) -> Sop {
    let mut result = ImSet::default();

    let singleton_zero = unit(Zero);
    for product in sop {
        let normalized = normalize_conj(product);
        if normalized != singleton_zero {
            result.insert(normalized);
        }
    }

    result
}

fn cartesian_product<A>(set: ImSet<A>) -> ImSet<(A, A)>
where
    A: Eq + Clone + core::hash::Hash,
{
    let mut result = ImSet::default();

    for x in set.clone().into_iter() {
        for y in set.clone() {
            if x == y {
                break;
            }

            result.insert((x.clone(), y));
        }
    }

    result
}

fn unit<A>(a: A) -> ImSet<A>
where
    A: Clone + Eq + core::hash::Hash,
{
    let mut result = ImSet::default();
    result.insert(a);
    result
}

fn normalize_disj(mut sum: Sum<Bool>) -> Sum<Bool> {
    let is_always_false =
        sum.clone().into_iter().any(|x| sum.contains(&not(x))) || sum.contains(&One);

    if is_always_false {
        unit(One)
    } else {
        sum.remove(&Zero);
        sum
    }
}

fn normalize_conj(mut product: Product<Bool>) -> Product<Bool> {
    let is_always_false = product
        .clone()
        .into_iter()
        .any(|x| product.contains(&not(x)))
        || product.contains(&Zero);

    if is_always_false {
        unit(Zero)
    } else {
        product.remove(&One);
        product
    }
}

fn cnf(term: Bool) -> Bool {
    match term {
        And(p, q) => and(cnf(*p), cnf(*q)),
        Or(p, q) => distr_cnf(cnf(*p), cnf(*q)),
        _ => term,
    }
}

// TODO test this thoroughly
fn distr_cnf(p: Bool, q: Bool) -> Bool {
    match p {
        And(p1, p2) => and(distr_cnf(*p1, q.clone()), distr_cnf(*p2, q)),
        _ => distr_cnf_help(p, q),
    }
}

fn distr_cnf_help(p: Bool, q: Bool) -> Bool {
    match q {
        And(q1, q2) => and(distr_cnf(p.clone(), *q1), distr_cnf(p, *q2)),
        _ => or(p, q),
    }
}

fn dnf(term: Bool) -> Bool {
    match term {
        And(p, q) => distr_dnf(dnf(*p), dnf(*q)),
        Or(p, q) => or(dnf(*p), dnf(*q)),
        _ => term,
    }
}

// TODO test this thoroughly
fn distr_dnf(p: Bool, q: Bool) -> Bool {
    match p {
        Or(p1, p2) => or(distr_dnf(*p1, q.clone()), distr_dnf(*p2, q)),
        _ => distr_dnf_help(p, q),
    }
}

fn distr_dnf_help(p: Bool, q: Bool) -> Bool {
    match q {
        Or(q1, q2) => or(distr_dnf(p.clone(), *q1), distr_dnf(p, *q2)),
        _ => and(p, q),
    }
}

/*
fn nnf(term: &Bool) -> Bool {
    match term {
        Zero => Zero,
        One => One,
        And(p, q) => and(dnf(p), dnf(q)),
        Or(p, q) => distr_dnf(dnf(p), dnf(q)),
        Not(nested) => Not(nested.clone()),
        Variable(current) => Variable(*current),
    }
}
*/
