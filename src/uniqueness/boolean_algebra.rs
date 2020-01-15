// Based on work by Edsko de Vries for tfp 2007
//
// http://www.edsko.net/tcd/pub/tfp07-prototype-snapshot.tar.gz
//
// Thank you Edsko!
//
// quoting from that work:
//
// > Main reference for this module is "Boolean Reasoning: The Logic of
// > Boolean Equations" by Frank Markham Brown.
use crate::collections::{ImMap, ImSet};
use crate::subs::Variable;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Bool {
    Zero,
    One,
    And(Box<Bool>, Box<Bool>),
    Or(Box<Bool>, Box<Bool>),
    Not(Box<Bool>),
    Variable(Variable),
}

use self::Bool::*;

#[inline(always)]
pub fn not(nested: Bool) -> Bool {
    Not(Box::new(nested))
}

#[inline(always)]
pub fn and(left: Bool, right: Bool) -> Bool {
    And(Box::new(left), Box::new(right))
}

#[inline(always)]
pub fn or(left: Bool, right: Bool) -> Bool {
    Or(Box::new(left), Box::new(right))
}

pub fn any<I>(mut it: I) -> Bool
where
    I: Iterator<Item = Bool>,
{
    if let Some(first) = it.next() {
        it.fold(first, |x, y| or(y, x))
    } else {
        Zero
    }
}

pub fn all(terms: Product<Bool>) -> Bool {
    let mut it = terms.into_iter();

    if let Some(first) = it.next() {
        it.fold(first, and)
    } else {
        One
    }
}

pub fn all_iterator<I>(mut it: I) -> Bool
where
    I: Iterator<Item = Bool>,
{
    if let Some(first) = it.next() {
        it.fold(first, |a, b| and(a, b))
    } else {
        One
    }
}

type Substitution = ImMap<Variable, Bool>;

type Product<A> = ImSet<A>;
type Sum<A> = ImSet<A>;

#[allow(clippy::should_implement_trait)]
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

    pub fn map_variables<F>(&self, f: &mut F) -> Self
    where
        F: FnMut(Variable) -> Variable,
    {
        match self {
            Zero => Zero,
            One => One,
            And(left, right) => and(left.map_variables(f), right.map_variables(f)),
            Or(left, right) => or(left.map_variables(f), right.map_variables(f)),
            Not(nested) => not(nested.map_variables(f)),
            Variable(current) => Variable(f(*current)),
        }
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

    #[inline(always)]
    pub fn is_var(&self) -> bool {
        match self {
            Variable(_) => true,
            _ => false,
        }
    }

    #[inline(always)]
    pub fn not(nested: Bool) -> Bool {
        not(nested)
    }

    #[inline(always)]
    pub fn and(left: Bool, right: Bool) -> Bool {
        and(left, right)
    }

    #[inline(always)]
    pub fn or(left: Bool, right: Bool) -> Bool {
        or(left, right)
    }
}

pub fn simplify(term: Bool) -> Bool {
    let normalized = normalize_term(term);
    let a = term_to_sop(normalized);
    let b = normalize_sop(a);
    let after_bcf = bcf(b);
    sop_to_term_vector(simplify_sop_vector(after_bcf))
}

#[inline(always)]
pub fn sop_to_term(sop: Sop) -> Bool {
    let mut accum: Vec<Vec<Bool>> = Vec::new();

    for s in sop {
        accum.push(s.iter().cloned().collect());
    }

    accum.sort_by(|x, y| (y.len().cmp(&x.len())));

    println!("{:?}", &accum);

    any(accum.into_iter().map(|v| all_iterator(v.into_iter())))
}

#[inline(always)]
pub fn sop_to_term_vector(sop: Vec<Vec<Bool>>) -> Bool {
    any(sop.into_iter().rev().map(|v| all_iterator(v.into_iter())))
}

pub fn simplify_sop(sop: Sop) -> Sop {
    // sort by length longest to shortest (proxy for how many variables there are)
    let mut sorted: Vec<ImSet<Bool>> = sop.clone().into_iter().collect();

    sorted.sort_by(|x, y| y.len().cmp(&x.len()));

    // filter out anything that is included in the remaining elements
    let mut active = sop;
    let mut result = ImSet::default();
    for t in sorted {
        if !(active.remove(&t).is_some() && included(all(t.clone()), sop_to_term(active.clone()))) {
            result.insert(t);
        }
    }

    result
}

pub fn simplify_sop_vector(mut sorted: Vec<Vec<Bool>>) -> Vec<Vec<Bool>> {
    // sort by length longest to shortest (proxy for how many variables there are)
    // sorted.sort_by(|x, y| y.len().cmp(&x.len()));

    let sorted2 = sorted.clone();

    let mut p: Vec<Vec<Bool>> = Vec::new();

    for (i, t) in sorted.into_iter().enumerate() {
        let mut ts = (&sorted2[i + 1..]).clone();
        ts.to_vec().extend(p.clone());

        if included(
            all_iterator(t.clone().into_iter()).clone(),
            sop_to_term_vector(ts.to_vec()),
        ) {
            // do nothing
        } else {
            p.insert(0, t.into_iter().collect());
        }
    }

    p
}

/*
simplify_sop :: BooleanAlgebra a => Sum (Product a) -> Sum (Product a)
simplify_sop terms =
        let
            sorted = (sortBy (\x y -> compare (length y) (length x)) terms)
        in
            f sorted []
    where
        f [] p = p
        f (t:ts) p =
            if list_to_conj t `included` sop_to_term (ts ++ p)
            then f ts p
            else f ts (t:p)
*/

/// Blake canonical form
pub fn bcf(sop: Sop) -> Vec<Vec<Bool>> {
    absorptive(syllogistic(sop))
}

pub fn syllogistic(terms: Sop) -> Sop {
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

/*
absorptive :: BooleanAlgebra a => Sum (Product a) -> Sum (Product a)
absorptive p = f p p
    where
        f [] p = p
        f (t:ts) p = f ts (t : filter (not . absorbs t) p)
*/

/// Absorption (apply the identify p + pq = p)
pub fn absorptive(sop: Sop) -> Vec<Vec<Bool>> {
    let mut accum: Vec<Vec<Bool>> = Vec::new();

    for s in sop {
        accum.push(s.iter().cloned().collect());
    }

    accum.sort_by(|x, y| (x.len().cmp(&y.len())));

    absorptive_vector(accum)
}

pub fn absorptive_vector(mut accum: Vec<Vec<Bool>>) -> Vec<Vec<Bool>> {
    for product in accum.clone() {
        accum.retain(|v| !absorbs_vector(&product, v));
        accum.insert(0, product.clone());
    }

    // accum.reverse();

    accum
}

pub fn absorbs_vector(p: &Vec<Bool>, q: &Vec<Bool>) -> bool {
    let result = p.iter().all(|x| q.contains(x));
    result
}

/// Does p absorb q? (can we replace p + q by p?)
/// TODO investigate: either the comment or the implementation is wrong I think?
pub fn absorbs(p: &Product<Bool>, q: &Product<Bool>) -> bool {
    let result = p.iter().all(|x| q.contains(x));
    result
}

fn consensus(p: &Product<Bool>, q: &Product<Bool>) -> Option<Product<Bool>> {
    let mut it = oppositions(p, q).into_iter();

    // oppositions must have exactly one element
    if let Some(x) = it.next() {
        if it.next().is_none() {
            let compx = not(x.clone());

            return Some(
                p.clone()
                    .into_iter()
                    .chain(q.clone().into_iter())
                    .filter(|y| *y != x && *y != compx)
                    .collect(),
            );
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

    it1.chain(it2).collect()
}

pub fn try_unify(p: Bool, q: Bool) -> Option<Substitution> {
    let (sub, consistency) = unify(p, q);

    let substitution = sub
        .into_iter()
        .filter(|(x, p)| *p != Variable(*x))
        .collect();

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

    // unify0(t.variables(), t)
    let mut sorted_names: Vec<Variable> = t.variables().into_iter().collect();
    sorted_names.sort();
    sorted_names.reverse();
    unify0_rec(sorted_names, t)
}

fn unify0(names: ImSet<Variable>, mut term: Bool) -> (Substitution, Bool) {
    // NOTE sort is required for stable test order that is the same as the Haskell ref. impl.
    let mut substitution: Substitution = ImMap::default();

    let mut sorted_names: Vec<Variable> = names.into_iter().collect();
    sorted_names.sort();

    for x in sorted_names.into_iter() {
        let mut sub_zero = ImMap::default();
        sub_zero.insert(x, Zero);

        let mut sub_one = ImMap::default();
        sub_one.insert(x, One);

        let subbed_zero = term.substitute(&sub_zero);
        let subbed_one = term.substitute(&sub_one);

        term = and(subbed_zero.clone(), subbed_one.clone());

        println!("> {:?} {:?} {:?}", &subbed_zero, &subbed_one, &substitution);
        let raw = or(
            subbed_zero.substitute(&substitution),
            and(Variable(x), not(subbed_one.substitute(&substitution))),
        );

        let replacement = simplify(raw);

        substitution.insert(x, replacement);
    }

    (substitution, simplify(term))
}

fn unify0_rec(names: Vec<Variable>, mut term: Bool) -> (Substitution, Bool) {
    unify0_rec_help(names, term)
}

fn unify0_rec_help(mut names: Vec<Variable>, mut term: Bool) -> (Substitution, Bool) {
    if let Some(x) = names.pop() {
        let xs = names;

        let mut sub_zero = ImMap::default();
        sub_zero.insert(x, Zero);

        let mut sub_one = ImMap::default();
        sub_one.insert(x, One);

        let subbed_zero = term.substitute(&sub_zero);
        let subbed_one = term.substitute(&sub_one);

        let e = and(subbed_zero.clone(), subbed_one.clone());

        let (mut se, cc) = unify0_rec_help(xs, e);

        let input = or(
            subbed_zero.substitute(&se),
            and(Variable(x), (not(subbed_one)).substitute(&se)),
        );

        let replacement = simplify(input);

        se.insert(x, replacement);

        (se, cc)
    } else {
        (ImMap::default(), simplify(term))
    }
}

/*

unify0 :: BooleanAlgebra a => [Name] -> a -> (Subst a, a)
unify0 [] t = ([], simplify t)
unify0 (x:xs) t = (st, cc)
    where
        e = ( t @@ (x, zero)) `conj` (t @@ (x, one))

        -- !foo = Debug.traceShowId ("----->", input, replacement )
        (se,cc) = unify0 xs e

        input = (apply se (t @@ (x, zero))) `disj` (var x `conj` apply se (comp (t @@ (x, one))))

        !foo = Debug.traceShowId ("----->", t @@ (x, zero), t @@ (x, one) , se )


        replacement = simplify $ ( input)


        st = (x, replacement ):se
    */

// --- Simplification ---

/// Normalization of terms. Applies (in bottom-up fashion) the identities
///
/// x * 1 = x
/// x * 0 = 0
/// x + 1 = 1
/// x + 0 = x
/// !1 = 0
/// !0 = 1
pub fn normalize_term(term: Bool) -> Bool {
    match term {
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
        _ => term,
    }
}

// --- Inclusion ---

pub fn included(g: Bool, h: Bool) -> bool {
    contradiction(and(g, not(h)))
}

fn included_term(g: Product<Bool>, h: Product<Bool>) -> bool {
    included(all(g), all(h))
}

// --- Tautology / Contradiction ---

fn tautology(term: Bool) -> bool {
    normalize_pos(term_to_pos(normalize_term(term))).is_empty()
}

pub fn contradiction(term: Bool) -> bool {
    tautology(not(term))
}

// --- Normalization of POS / SOP

type Pos = Product<Sum<Bool>>;
type Sop = Sum<Product<Bool>>;

fn term_to_pos(term: Bool) -> Pos {
    conj_to_list(cnf(term))
        .into_iter()
        .map(disj_to_list)
        .collect()
}

pub fn term_to_sop(term: Bool) -> Sop {
    disj_to_list(dnf(term))
        .into_iter()
        .map(conj_to_list)
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
    let singleton_one = unit(One);

    pos.into_iter()
        .map(normalize_disj)
        .filter(|normalized| *normalized != singleton_one)
        .collect()
}

pub fn normalize_sop(sop: Sop) -> Sop {
    let singleton_zero = unit(Zero);

    sop.into_iter()
        .map(normalize_conj)
        .filter(|normalized| *normalized != singleton_zero)
        .collect()
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
    let is_always_true =
        sum.clone().into_iter().any(|x| sum.contains(&not(x))) || sum.contains(&One);

    if is_always_true {
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

/// Conjunction Normal Form
fn cnf(term: Bool) -> Bool {
    match nnf(term) {
        And(p, q) => and(cnf(*p), cnf(*q)),
        Or(p, q) => distr_cnf(cnf(*p), cnf(*q)),
        other => other,
    }
}

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

/// Disjunction Normal Form
pub fn dnf(term: Bool) -> Bool {
    match nnf(term) {
        And(p, q) => distr_dnf(dnf(*p), dnf(*q)),
        Or(p, q) => or(dnf(*p), dnf(*q)),
        other => other,
    }
}

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

/// Negation Normal Form
pub fn nnf(term: Bool) -> Bool {
    match term {
        Not(n) => nnf_help(*n),
        _ => term,
    }
}

pub fn nnf_help(term: Bool) -> Bool {
    match term {
        Zero => One,
        One => Zero,
        And(p, q) => or(nnf_help(*p), nnf_help(*q)),
        Or(p, q) => and(nnf_help(*p), nnf_help(*q)),
        // double negation
        Not(nested) => nnf(*nested),
        Variable(_) => not(term),
    }
}
