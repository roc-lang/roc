use crate::expr::Env;
use crate::expr::Expr;
use crate::expr::Pattern;
use crate::layout::Builtin;
use crate::layout::Layout;
use crate::pattern::{Ctor, RenderAs, TagId, Union};
use bumpalo::Bump;
use roc_collections::all::{MutMap, MutSet};
use roc_module::ident::TagName;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;

/// COMPILE CASES

type Label = u64;

/// Users of this module will mainly interact with this function. It takes
/// some normal branches and gives out a decision tree that has "labels" at all
/// the leafs and a dictionary that maps these "labels" to the code that should
/// run.
pub fn compile<'a>(raw_branches: Vec<(Guard<'a>, Pattern<'a>, u64)>) -> DecisionTree<'a> {
    let formatted = raw_branches
        .into_iter()
        .map(|(guard, pattern, index)| Branch {
            goal: index,
            patterns: vec![(Path::Empty, guard, pattern)],
        })
        .collect();

    to_decision_tree(formatted)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Guard<'a> {
    NoGuard,
    Guard {
        stores: &'a [(Symbol, Layout<'a>, Expr<'a>)],
        expr: Expr<'a>,
    },
}

impl<'a> Guard<'a> {
    fn is_none(&self) -> bool {
        self == &Guard::NoGuard
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DecisionTree<'a> {
    Match(Label),
    Decision {
        path: Path,
        edges: Vec<(Test<'a>, DecisionTree<'a>)>,
        default: Option<Box<DecisionTree<'a>>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Test<'a> {
    IsCtor {
        tag_id: u8,
        tag_name: TagName,
        union: crate::pattern::Union,
        arguments: Vec<(Pattern<'a>, Layout<'a>)>,
    },
    IsInt(i64),
    // float patterns are stored as u64 so they are comparable/hashable
    IsFloat(u64),
    IsStr(Box<str>),
    IsBit(bool),
    IsByte {
        tag_id: u8,
        num_alts: usize,
    },
    // A pattern that always succeeds (like `_`) can still have a guard
    Guarded {
        opt_test: Option<Box<Test<'a>>>,
        stores: &'a [(Symbol, Layout<'a>, Expr<'a>)],
        expr: Expr<'a>,
    },
}
use std::hash::{Hash, Hasher};
impl<'a> Hash for Test<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Test::*;

        match self {
            IsCtor { tag_id, .. } => {
                state.write_u8(0);
                tag_id.hash(state);
                // The point of this custom implementation is to not hash the tag arguments
            }
            IsInt(v) => {
                state.write_u8(1);
                v.hash(state);
            }
            IsFloat(v) => {
                state.write_u8(2);
                v.hash(state);
            }
            IsStr(v) => {
                state.write_u8(3);
                v.hash(state);
            }
            IsBit(v) => {
                state.write_u8(4);
                v.hash(state);
            }
            IsByte { tag_id, num_alts } => {
                state.write_u8(5);
                tag_id.hash(state);
                num_alts.hash(state);
            }
            Guarded { opt_test: None, .. } => {
                state.write_u8(6);
            }
            Guarded {
                opt_test: Some(nested),
                ..
            } => {
                state.write_u8(7);
                nested.hash(state);
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Path {
    Index {
        index: u64,
        tag_id: u8,
        path: Box<Path>,
    },
    Unbox(Box<Path>),
    Empty,
}

// ACTUALLY BUILD DECISION TREES

#[derive(Clone, Debug, PartialEq)]
struct Branch<'a> {
    goal: Label,
    patterns: Vec<(Path, Guard<'a>, Pattern<'a>)>,
}

fn to_decision_tree(raw_branches: Vec<Branch>) -> DecisionTree {
    let branches: Vec<_> = raw_branches.into_iter().map(flatten_patterns).collect();

    match check_for_match(&branches) {
        Some(goal) => DecisionTree::Match(goal),
        None => {
            // TODO remove clone
            let path = pick_path(branches.clone());

            let (edges, fallback) = gather_edges(branches, &path);

            let mut decision_edges: Vec<_> = edges
                .into_iter()
                .map(|(a, b)| (a, to_decision_tree(b)))
                .collect();

            match (decision_edges.split_last_mut(), fallback.split_last()) {
                (Some(((_tag, decision_tree), rest)), None) if rest.is_empty() => {
                    // TODO remove clone
                    decision_tree.clone()
                }
                (_, None) => DecisionTree::Decision {
                    path,
                    edges: decision_edges,
                    default: None,
                },
                (None, Some(_)) => to_decision_tree(fallback),
                _ => DecisionTree::Decision {
                    path,
                    edges: decision_edges,
                    default: Some(Box::new(to_decision_tree(fallback))),
                },
            }
        }
    }
}

fn is_complete(tests: &[Test]) -> bool {
    let length = tests.len();
    debug_assert!(length > 0);
    match tests.get(length - 1) {
        None => unreachable!("should never happen"),
        Some(v) => match v {
            Test::IsCtor { union, .. } => length == union.alternatives.len(),
            Test::IsByte { num_alts, .. } => length == *num_alts,
            Test::IsBit(_) => length == 2,
            Test::IsInt(_) => false,
            Test::IsFloat(_) => false,
            Test::IsStr(_) => false,
            Test::Guarded { .. } => false,
        },
    }
}

fn flatten_patterns(branch: Branch) -> Branch {
    let mut result = Vec::with_capacity(branch.patterns.len());

    for path_pattern in branch.patterns {
        flatten(path_pattern, &mut result);
    }
    Branch {
        goal: branch.goal,
        patterns: result,
    }
}

fn flatten<'a>(
    path_pattern: (Path, Guard<'a>, Pattern<'a>),
    path_patterns: &mut Vec<(Path, Guard<'a>, Pattern<'a>)>,
) {
    match &path_pattern.2 {
        Pattern::AppliedTag {
            union,
            arguments,
            tag_id,
            ..
        } => {
            // TODO do we need to check that guard.is_none() here?
            if union.alternatives.len() == 1 {
                let path = path_pattern.0;
                // Theory: unbox doesn't have any value for us, because one-element tag unions
                // don't store the tag anyway.
                if arguments.len() == 1 {
                    path_patterns.push((
                        Path::Unbox(Box::new(path)),
                        path_pattern.1.clone(),
                        path_pattern.2.clone(),
                    ));
                } else {
                    for (index, (arg_pattern, _)) in arguments.iter().enumerate() {
                        flatten(
                            (
                                Path::Index {
                                    index: index as u64,
                                    tag_id: *tag_id,
                                    path: Box::new(path.clone()),
                                },
                                // same guard here?
                                path_pattern.1.clone(),
                                arg_pattern.clone(),
                            ),
                            path_patterns,
                        );
                    }
                }
            } else {
                path_patterns.push(path_pattern);
            }
        }

        _ => {
            path_patterns.push(path_pattern);
        }
    }
}

/// SUCCESSFULLY MATCH

/// If the first branch has no more "decision points" we can finally take that
/// path. If that is the case we give the resulting label and a mapping from free
/// variables to "how to get their value". So a pattern like (Just (x,_)) will give
/// us something like ("x" => value.0.0)
fn check_for_match<'a>(branches: &Vec<Branch<'a>>) -> Option<Label> {
    match branches.get(0) {
        Some(Branch { goal, patterns })
            if patterns
                .iter()
                .all(|(_, guard, pattern)| guard.is_none() && !needs_tests(pattern)) =>
        {
            Some(*goal)
        }
        _ => None,
    }
}

/// GATHER OUTGOING EDGES

fn gather_edges<'a>(
    branches: Vec<Branch<'a>>,
    path: &Path,
) -> (Vec<(Test<'a>, Vec<Branch<'a>>)>, Vec<Branch<'a>>) {
    // TODO remove clone
    let relevant_tests = tests_at_path(path, branches.clone());

    let check = is_complete(&relevant_tests);

    // TODO remove clone
    let all_edges = relevant_tests
        .into_iter()
        .map(|t| edges_for(path, branches.clone(), t))
        .collect();

    let fallbacks = if check {
        vec![]
    } else {
        branches
            .into_iter()
            .filter(|b| is_irrelevant_to(path, b))
            .collect()
    };

    (all_edges, fallbacks)
}

/// FIND RELEVANT TESTS

fn tests_at_path<'a>(selected_path: &Path, branches: Vec<Branch<'a>>) -> Vec<Test<'a>> {
    // NOTE the ordering of the result is important!

    let mut all_tests = Vec::new();

    for branch in branches.into_iter() {
        test_at_path(selected_path, branch, &mut all_tests);
    }

    // The rust HashMap also uses equality, here we really want to use the custom hash function
    // defined on Test to determine whether a test is unique. So we have to do the hashing
    // explicitly

    use std::collections::hash_map::DefaultHasher;

    let mut visited = MutSet::default();
    let mut unique = Vec::new();

    for test in all_tests {
        let hash = {
            let mut hasher = DefaultHasher::new();
            test.hash(&mut hasher);
            hasher.finish()
        };

        if !visited.contains(&hash) {
            visited.insert(hash);
            unique.push(test);
        }
    }

    unique
}

fn test_at_path<'a>(selected_path: &Path, branch: Branch<'a>, all_tests: &mut Vec<Test<'a>>) {
    use Pattern::*;
    use Test::*;

    match branch
        .patterns
        .iter()
        .find(|(path, _, _)| path == selected_path)
    {
        None => {}
        Some((_, guard, pattern)) => {
            let guarded = |test| {
                if let Guard::Guard { stores, expr } = guard {
                    Guarded {
                        opt_test: Some(Box::new(test)),
                        stores,
                        expr: expr.clone(),
                    }
                } else {
                    test
                }
            };

            match pattern {
                // TODO use guard!
                Identifier(_) | Underscore | Shadowed(_, _) | UnsupportedPattern(_) => {
                    if let Guard::Guard { stores, expr } = guard {
                        all_tests.push(Guarded {
                            opt_test: None,
                            stores,
                            expr: expr.clone(),
                        });
                    }
                }

                RecordDestructure(destructs, _) => {
                    // not rendered, so pick the easiest
                    let union = Union {
                        render_as: RenderAs::Tag,
                        alternatives: vec![Ctor {
                            tag_id: TagId(0),
                            name: TagName::Global("#Record".into()),
                            arity: destructs.len(),
                        }],
                    };

                    let mut arguments = std::vec::Vec::new();

                    for destruct in destructs {
                        if let Some(guard) = &destruct.guard {
                            arguments.push((guard.clone(), destruct.layout.clone()));
                        } else {
                            arguments.push((Pattern::Underscore, destruct.layout.clone()));
                        }
                    }

                    all_tests.push(IsCtor {
                        tag_id: 0,
                        tag_name: TagName::Global("#Record".into()),
                        union,
                        arguments,
                    });
                }

                AppliedTag {
                    tag_name,
                    tag_id,
                    arguments,
                    union,
                    ..
                } => {
                    all_tests.push(IsCtor {
                        tag_id: *tag_id,
                        tag_name: tag_name.clone(),
                        union: union.clone(),
                        arguments: arguments.to_vec(),
                    });
                }
                BitLiteral { value, .. } => {
                    all_tests.push(IsBit(*value));
                }
                EnumLiteral { tag_id, union, .. } => {
                    all_tests.push(IsByte {
                        tag_id: *tag_id,
                        num_alts: union.alternatives.len(),
                    });
                }
                IntLiteral(v) => {
                    all_tests.push(guarded(IsInt(*v)));
                }
                FloatLiteral(v) => {
                    all_tests.push(IsFloat(*v));
                }
                StrLiteral(v) => {
                    all_tests.push(IsStr(v.clone()));
                }
            };
        }
    }
}

/// BUILD EDGES

fn edges_for<'a>(
    path: &Path,
    branches: Vec<Branch<'a>>,
    test: Test<'a>,
) -> (Test<'a>, Vec<Branch<'a>>) {
    let mut new_branches = Vec::new();

    for branch in branches.into_iter() {
        to_relevant_branch(&test, path, branch, &mut new_branches);
    }

    (test, new_branches)
}

fn to_relevant_branch<'a>(
    test: &Test<'a>,
    path: &Path,
    branch: Branch<'a>,
    new_branches: &mut Vec<Branch<'a>>,
) {
    // TODO remove clone
    match extract(path, branch.patterns.clone()) {
        Extract::NotFound => {
            new_branches.push(branch);
        }
        Extract::Found {
            start,
            found_pattern: (guard, pattern),
            end,
        } => {
            let actual_test = match test {
                Test::Guarded {
                    opt_test: Some(box_test),
                    ..
                } => box_test,
                _ => test,
            };

            if let Some(mut new_branch) =
                to_relevant_branch_help(actual_test, path, start, end, branch, guard, pattern)
            {
                // guards can/should only occur at the top level. When we recurse on these
                // branches, the guard is not relevant any more. Not setthing the guard to None
                // leads to infinite recursion.
                new_branch.patterns.iter_mut().for_each(|(_, guard, _)| {
                    *guard = Guard::NoGuard;
                });

                new_branches.push(new_branch);
            }
        }
    }
}

fn to_relevant_branch_help<'a>(
    test: &Test<'a>,
    path: &Path,
    mut start: Vec<(Path, Guard<'a>, Pattern<'a>)>,
    end: Vec<(Path, Guard<'a>, Pattern<'a>)>,
    branch: Branch<'a>,
    guard: Guard<'a>,
    pattern: Pattern<'a>,
) -> Option<Branch<'a>> {
    use Pattern::*;
    use Test::*;

    match pattern {
        Identifier(_) | Underscore | Shadowed(_, _) | UnsupportedPattern(_) => Some(branch),

        RecordDestructure(destructs, _) => match test {
            IsCtor {
                tag_name: test_name,
                tag_id,
                ..
            } => {
                debug_assert!(test_name == &TagName::Global("#Record".into()));
                let sub_positions = destructs.into_iter().enumerate().map(|(index, destruct)| {
                    let pattern = if let Some(guard) = destruct.guard {
                        guard.clone()
                    } else {
                        Pattern::Underscore
                    };

                    (
                        Path::Index {
                            index: index as u64,
                            tag_id: *tag_id,
                            path: Box::new(path.clone()),
                        },
                        Guard::NoGuard,
                        pattern,
                    )
                });
                start.extend(sub_positions);
                start.extend(end);

                Some(Branch {
                    goal: branch.goal,
                    patterns: start,
                })
            }
            _ => None,
        },

        AppliedTag {
            tag_name,
            arguments,
            union,
            ..
        } => {
            match test {
                IsCtor {
                    tag_name: test_name,
                    tag_id,
                    ..
                } if &tag_name == test_name => {
                    // Theory: Unbox doesn't have any value for us
                    if arguments.len() == 1 && union.alternatives.len() == 1 {
                        let arg = arguments[0].clone();
                        {
                            start.push((Path::Unbox(Box::new(path.clone())), guard, arg.0));
                            start.extend(end);
                        }
                    } else {
                        let sub_positions =
                            arguments
                                .into_iter()
                                .enumerate()
                                .map(|(index, (pattern, _))| {
                                    (
                                        Path::Index {
                                            index: index as u64,
                                            tag_id: *tag_id,
                                            path: Box::new(path.clone()),
                                        },
                                        Guard::NoGuard,
                                        pattern,
                                    )
                                });
                        start.extend(sub_positions);
                        start.extend(end);
                    }

                    Some(Branch {
                        goal: branch.goal,
                        patterns: start,
                    })
                }
                _ => None,
            }
        }
        StrLiteral(string) => match test {
            IsStr(test_str) if string == *test_str => {
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    patterns: start,
                })
            }
            _ => None,
        },

        IntLiteral(int) => match test {
            IsInt(is_int) if int == *is_int => {
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    patterns: start,
                })
            }
            _ => None,
        },

        FloatLiteral(float) => match test {
            IsFloat(test_float) if float == *test_float => {
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    patterns: start,
                })
            }
            _ => None,
        },

        BitLiteral { value: bit, .. } => match test {
            IsBit(test_bit) if bit == *test_bit => {
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    patterns: start,
                })
            }
            _ => None,
        },

        EnumLiteral { tag_id, .. } => match test {
            IsByte {
                tag_id: test_id, ..
            } if tag_id == *test_id => {
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    patterns: start,
                })
            }

            _ => None,
        },
    }
}

enum Extract<'a> {
    NotFound,
    Found {
        start: Vec<(Path, Guard<'a>, Pattern<'a>)>,
        found_pattern: (Guard<'a>, Pattern<'a>),
        end: Vec<(Path, Guard<'a>, Pattern<'a>)>,
    },
}

fn extract<'a>(
    selected_path: &Path,
    path_patterns: Vec<(Path, Guard<'a>, Pattern<'a>)>,
) -> Extract<'a> {
    let mut start = Vec::new();

    // TODO remove this clone
    let mut copy = path_patterns.clone();

    // TODO potential ordering problem
    for (index, current) in path_patterns.into_iter().enumerate() {
        if &current.0 == selected_path {
            return Extract::Found {
                start,
                found_pattern: (current.1, current.2),
                end: {
                    copy.drain(0..=index);
                    copy
                },
            };
        } else {
            start.push(current);
        }
    }

    Extract::NotFound
}

/// FIND IRRELEVANT BRANCHES

fn is_irrelevant_to<'a>(selected_path: &Path, branch: &Branch<'a>) -> bool {
    match branch
        .patterns
        .iter()
        .find(|(path, _, _)| path == selected_path)
    {
        None => true,
        Some((_, guard, pattern)) => guard.is_none() && !needs_tests(pattern),
    }
}

fn needs_tests<'a>(pattern: &Pattern<'a>) -> bool {
    use Pattern::*;

    match pattern {
        Identifier(_) | Underscore | Shadowed(_, _) | UnsupportedPattern(_) => false,

        RecordDestructure(_, _)
        | AppliedTag { .. }
        | BitLiteral { .. }
        | EnumLiteral { .. }
        | IntLiteral(_)
        | FloatLiteral(_)
        | StrLiteral(_) => true,
    }
}

/// PICK A PATH

fn pick_path(branches: Vec<Branch>) -> Path {
    // TODO remove this clone
    let all_paths = branches
        .clone()
        .into_iter()
        .map(|v| v.patterns)
        .flatten()
        .filter_map(is_choice_path);

    let mut by_small_defaults = bests_by_small_defaults(&branches, all_paths);

    if by_small_defaults.len() == 1 {
        by_small_defaults.remove(0)
    } else {
        debug_assert!(!by_small_defaults.is_empty());
        let mut result = bests_by_small_branching_factor(&branches, by_small_defaults.into_iter());

        match result.pop() {
            None => unreachable!("bests_by will always return at least one value in the vec"),
            Some(path) => path,
        }
    }
}

fn is_choice_path<'a>(path_and_pattern: (Path, Guard<'a>, Pattern<'a>)) -> Option<Path> {
    let (path, guard, pattern) = path_and_pattern;

    if !guard.is_none() || needs_tests(&pattern) {
        Some(path)
    } else {
        None
    }
}

fn bests_by_small_branching_factor<I>(branches: &Vec<Branch>, mut all_paths: I) -> Vec<Path>
where
    I: Iterator<Item = Path>,
{
    match all_paths.next() {
        None => panic!("Cannot choose the best of zero paths. This should never happen."),
        Some(first_path) => {
            let mut min_weight = small_branching_factor(branches, &first_path);
            let mut min_paths = vec![first_path];

            for path in all_paths {
                let weight = small_branching_factor(branches, &path);

                use std::cmp::Ordering;
                match weight.cmp(&min_weight) {
                    Ordering::Equal => {
                        min_paths.push(path.clone());
                    }
                    Ordering::Less => {
                        min_weight = weight;
                        min_paths.clear();
                        min_paths.push(path);
                    }
                    Ordering::Greater => {}
                }
            }

            min_paths
        }
    }
}

fn bests_by_small_defaults<I>(branches: &Vec<Branch>, mut all_paths: I) -> Vec<Path>
where
    I: Iterator<Item = Path>,
{
    match all_paths.next() {
        None => panic!("Cannot choose the best of zero paths. This should never happen."),
        Some(first_path) => {
            let mut min_weight = small_defaults(branches, &first_path);
            let mut min_paths = vec![first_path];

            for path in all_paths {
                let weight = small_defaults(branches, &path);

                use std::cmp::Ordering;
                match weight.cmp(&min_weight) {
                    Ordering::Equal => {
                        min_paths.push(path.clone());
                    }
                    Ordering::Less => {
                        min_weight = weight;
                        min_paths.clear();
                        min_paths.push(path);
                    }
                    Ordering::Greater => {}
                }
            }

            min_paths
        }
    }
}

/// PATH PICKING HEURISTICS

fn small_defaults(branches: &Vec<Branch>, path: &Path) -> usize {
    branches
        .iter()
        .filter(|b| is_irrelevant_to(path, b))
        .map(|_| 1)
        .sum()
}

fn small_branching_factor(branches: &Vec<Branch>, path: &Path) -> usize {
    // TODO remove clone
    let (edges, fallback) = gather_edges(branches.to_vec(), path);

    edges.len() + (if fallback.is_empty() { 0 } else { 1 })
}

#[derive(Clone, Debug, PartialEq)]
enum Decider<'a, T> {
    Leaf(T),
    Chain {
        test_chain: Vec<(Path, Test<'a>)>,
        success: Box<Decider<'a, T>>,
        failure: Box<Decider<'a, T>>,
    },
    FanOut {
        path: Path,
        tests: Vec<(Test<'a>, Decider<'a, T>)>,
        fallback: Box<Decider<'a, T>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
enum Choice<'a> {
    Inline(Stores<'a>, Expr<'a>),
    Jump(Label),
}

type Stores<'a> = &'a [(Symbol, Layout<'a>, Expr<'a>)];

pub fn optimize_when<'a>(
    env: &mut Env<'a, '_>,
    cond_symbol: Symbol,
    cond_layout: Layout<'a>,
    ret_layout: Layout<'a>,
    opt_branches: Vec<(Pattern<'a>, Guard<'a>, Stores<'a>, Expr<'a>)>,
) -> Expr<'a> {
    let (patterns, _indexed_branches) = opt_branches
        .into_iter()
        .enumerate()
        .map(|(index, (pattern, guard, stores, branch))| {
            (
                (guard, pattern, index as u64),
                (index as u64, stores, branch),
            )
        })
        .unzip();

    let indexed_branches: Vec<(u64, Stores<'a>, Expr<'a>)> = _indexed_branches;

    let decision_tree = compile(patterns);
    let decider = tree_to_decider(decision_tree);
    let target_counts = count_targets(&decider);

    let mut choices = MutMap::default();
    let mut jumps = Vec::new();

    for (index, stores, branch) in indexed_branches.into_iter() {
        let ((branch_index, choice), opt_jump) =
            create_choices(&target_counts, index, stores, branch);

        if let Some(jump) = opt_jump {
            jumps.push(jump);
        }

        choices.insert(branch_index, choice);
    }

    let choice_decider = insert_choices(&choices, decider);

    let (stores, expr) = decide_to_branching(
        env,
        cond_symbol,
        cond_layout,
        ret_layout,
        choice_decider,
        &jumps,
    );

    // increase the jump counter by the number of jumps in this branching structure
    *env.jump_counter += jumps.len() as u64;

    Expr::Store(stores, env.arena.alloc(expr))
}

fn path_to_expr<'a>(
    env: &mut Env<'a, '_>,
    symbol: Symbol,
    path: &Path,
    layout: &Layout<'a>,
) -> Expr<'a> {
    path_to_expr_help(env, symbol, path, layout.clone()).0
}

fn path_to_expr_help<'a>(
    env: &mut Env<'a, '_>,
    symbol: Symbol,
    path: &Path,
    layout: Layout<'a>,
) -> (Expr<'a>, Layout<'a>) {
    match path {
        Path::Unbox(unboxed) => path_to_expr_help(env, symbol, unboxed, layout),
        Path::Empty => (Expr::Load(symbol), layout),

        Path::Index {
            index,
            tag_id,
            path: nested,
        } => {
            let (outer_expr, outer_layout) = path_to_expr_help(env, symbol, nested, layout);

            let (is_unwrapped, field_layouts) = match outer_layout {
                Layout::Union(layouts) => (layouts.is_empty(), layouts[*tag_id as usize].to_vec()),
                Layout::Struct(layouts) => (true, layouts.to_vec()),
                other => (true, vec![other]),
            };

            debug_assert!(*index < field_layouts.len() as u64);

            let inner_layout = field_layouts[*index as usize].clone();

            let inner_expr = Expr::AccessAtIndex {
                index: *index,
                field_layouts: env.arena.alloc(field_layouts),
                expr: env.arena.alloc(outer_expr),
                is_unwrapped,
            };

            (inner_expr, inner_layout)
        }
    }
}

fn test_to_equality<'a>(
    env: &mut Env<'a, '_>,
    cond_symbol: Symbol,
    cond_layout: &Layout<'a>,
    path: &Path,
    test: Test<'a>,
    tests: &mut Vec<(Expr<'a>, Expr<'a>, Layout<'a>)>,
) {
    match test {
        Test::IsCtor {
            tag_id,
            union,
            arguments,
            ..
        } => {
            // the IsCtor check should never be generated for tag unions of size 1
            // (e.g. record pattern guard matches)
            debug_assert!(union.alternatives.len() > 1);

            let lhs = Expr::Int(tag_id as i64);

            let mut field_layouts =
                bumpalo::collections::Vec::with_capacity_in(arguments.len(), env.arena);

            // add the tag discriminant
            field_layouts.push(Layout::Builtin(Builtin::Int64));

            for (_, layout) in arguments {
                field_layouts.push(layout);
            }

            let rhs = Expr::AccessAtIndex {
                index: 0,
                field_layouts: field_layouts.into_bump_slice(),
                expr: env.arena.alloc(Expr::Load(cond_symbol)),
                is_unwrapped: union.alternatives.len() == 1,
            };

            tests.push((lhs, rhs, Layout::Builtin(Builtin::Int64)));
        }
        Test::IsInt(test_int) => {
            let lhs = Expr::Int(test_int);
            let rhs = path_to_expr(env, cond_symbol, &path, &cond_layout);

            tests.push((lhs, rhs, Layout::Builtin(Builtin::Int64)));
        }

        Test::IsFloat(test_int) => {
            // TODO maybe we can actually use i64 comparison here?
            let test_float = f64::from_bits(test_int as u64);
            let lhs = Expr::Float(test_float);
            let rhs = path_to_expr(env, cond_symbol, &path, &cond_layout);

            tests.push((lhs, rhs, Layout::Builtin(Builtin::Float64)));
        }

        Test::IsByte {
            tag_id: test_byte, ..
        } => {
            let lhs = Expr::Byte(test_byte);
            let rhs = path_to_expr(env, cond_symbol, &path, &cond_layout);

            tests.push((lhs, rhs, Layout::Builtin(Builtin::Int8)));
        }

        Test::IsBit(test_bit) => {
            let lhs = Expr::Bool(test_bit);
            let rhs = path_to_expr(env, cond_symbol, &path, &cond_layout);

            tests.push((lhs, rhs, Layout::Builtin(Builtin::Int8)));
        }

        Test::IsStr(test_str) => {
            let lhs = Expr::Str(env.arena.alloc(test_str));
            let rhs = path_to_expr(env, cond_symbol, &path, &cond_layout);

            tests.push((lhs, rhs, Layout::Builtin(Builtin::Str)));
        }

        Test::Guarded {
            opt_test,
            stores,
            expr,
        } => {
            if let Some(nested) = opt_test {
                test_to_equality(env, cond_symbol, cond_layout, path, *nested, tests);
            }

            let lhs = Expr::Bool(true);
            let rhs = Expr::Store(stores, env.arena.alloc(expr));

            tests.push((lhs, rhs, Layout::Builtin(Builtin::Int8)));
        }
    }
}

fn decide_to_branching<'a>(
    env: &mut Env<'a, '_>,
    cond_symbol: Symbol,
    cond_layout: Layout<'a>,
    ret_layout: Layout<'a>,
    decider: Decider<'a, Choice<'a>>,
    jumps: &Vec<(u64, Stores<'a>, Expr<'a>)>,
) -> (Stores<'a>, Expr<'a>) {
    use Choice::*;
    use Decider::*;

    match decider {
        Leaf(Jump(label)) => {
            // we currently inline the jumps: does fewer jumps but produces a larger artifact
            let (_, stores, expr) = jumps
                .iter()
                .find(|(l, _, _)| l == &label)
                .expect("jump not in list of jumps");
            (stores, expr.clone())
        }
        Leaf(Inline(stores, expr)) => (stores, expr),
        Chain {
            test_chain,
            success,
            failure,
        } => {
            // generate a switch based on the test chain

            let mut tests = Vec::with_capacity(test_chain.len());

            for (path, test) in test_chain {
                test_to_equality(env, cond_symbol, &cond_layout, &path, test, &mut tests);
            }

            let (pass_stores, pass_expr) = decide_to_branching(
                env,
                cond_symbol,
                cond_layout.clone(),
                ret_layout.clone(),
                *success,
                jumps,
            );

            let (fail_stores, fail_expr) = decide_to_branching(
                env,
                cond_symbol,
                cond_layout.clone(),
                ret_layout.clone(),
                *failure,
                jumps,
            );

            let fail = (fail_stores, &*env.arena.alloc(fail_expr));
            let pass = (pass_stores, &*env.arena.alloc(pass_expr));

            let condition = boolean_all(env.arena, tests);

            let branch_symbol = env.unique_symbol();
            let stores = [(branch_symbol, Layout::Builtin(Builtin::Int8), condition)];

            let cond_layout = Layout::Builtin(Builtin::Int8);

            (
                env.arena.alloc(stores),
                Expr::Store(
                    &[],
                    env.arena.alloc(Expr::Cond {
                        cond_symbol,
                        branch_symbol,
                        cond_layout,
                        pass,
                        fail,
                        ret_layout,
                    }),
                ),
            )
        }
        FanOut {
            path,
            tests,
            fallback,
        } => {
            // the cond_layout can change in the process. E.g. if the cond is a Tag, we actually
            // switch on the tag discriminant (currently an i64 value)
            let (cond, cond_layout) = path_to_expr_help(env, cond_symbol, &path, cond_layout);

            let (default_stores, default_expr) = decide_to_branching(
                env,
                cond_symbol,
                cond_layout.clone(),
                ret_layout.clone(),
                *fallback,
                jumps,
            );
            let default_branch = (default_stores, &*env.arena.alloc(default_expr));

            let mut branches = bumpalo::collections::Vec::with_capacity_in(tests.len(), env.arena);

            for (test, decider) in tests {
                let (stores, branch) = decide_to_branching(
                    env,
                    cond_symbol,
                    cond_layout.clone(),
                    ret_layout.clone(),
                    decider,
                    jumps,
                );

                let tag = match test {
                    Test::IsInt(v) => v as u64,
                    Test::IsFloat(v) => v as u64,
                    Test::IsBit(v) => v as u64,
                    Test::IsByte { tag_id, .. } => tag_id as u64,
                    Test::IsCtor { tag_id, .. } => tag_id as u64,
                    other => todo!("other {:?}", other),
                };

                branches.push((tag, stores, branch));
            }

            // make a jump table based on the tests
            (
                &[],
                Expr::Switch {
                    cond: env.arena.alloc(cond),
                    cond_layout,
                    branches: branches.into_bump_slice(),
                    default_branch,
                    ret_layout,
                },
            )
        }
    }
}

fn boolean_all<'a>(arena: &'a Bump, tests: Vec<(Expr<'a>, Expr<'a>, Layout<'a>)>) -> Expr<'a> {
    let mut expr = Expr::Bool(true);

    for (lhs, rhs, layout) in tests.into_iter().rev() {
        let test = Expr::RunLowLevel(
            LowLevel::Eq,
            bumpalo::vec![in arena; (lhs, layout.clone()), (rhs, layout.clone())].into_bump_slice(),
        );

        expr = Expr::CallByName {
            name: Symbol::BOOL_AND,
            layout,
            args: arena.alloc([
                (test, Layout::Builtin(Builtin::Int8)),
                (expr, Layout::Builtin(Builtin::Int8)),
            ]),
        };
    }

    expr
}

/// TREE TO DECIDER
///
/// Decision trees may have some redundancies, so we convert them to a Decider
/// which has special constructs to avoid code duplication when possible.

/// If a test always succeeds, we don't need to branch on it
/// this saves on work and jumps
fn test_always_succeeds(test: &Test) -> bool {
    match test {
        Test::IsCtor { union, .. } => union.alternatives.len() == 1,
        _ => false,
    }
}

fn tree_to_decider(tree: DecisionTree) -> Decider<u64> {
    use Decider::*;
    use DecisionTree::*;

    match tree {
        Match(target) => Leaf(target),

        Decision {
            path,
            mut edges,
            default,
        } => match default {
            None => match edges.len() {
                0 => panic!("compiler bug, somehow created an empty decision tree"),
                1 => {
                    let (_, sub_tree) = edges.remove(0);

                    tree_to_decider(sub_tree)
                }
                2 => {
                    let (_, failure_tree) = edges.remove(1);
                    let (test, success_tree) = edges.remove(0);

                    if test_always_succeeds(&test) {
                        tree_to_decider(success_tree)
                    } else {
                        to_chain(path, test, success_tree, failure_tree)
                    }
                }

                _ => {
                    let fallback = edges.remove(edges.len() - 1).1;

                    let necessary_tests = edges
                        .into_iter()
                        .map(|(test, decider)| (test, tree_to_decider(decider)))
                        .collect();

                    FanOut {
                        path,
                        tests: necessary_tests,
                        fallback: Box::new(tree_to_decider(fallback)),
                    }
                }
            },

            Some(last) => match edges.len() {
                0 => tree_to_decider(*last),
                1 => {
                    let failure_tree = *last;
                    let (test, success_tree) = edges.remove(0);

                    if test_always_succeeds(&test) {
                        tree_to_decider(success_tree)
                    } else {
                        to_chain(path, test, success_tree, failure_tree)
                    }
                }

                _ => {
                    let fallback = *last;

                    let necessary_tests = edges
                        .into_iter()
                        .map(|(test, decider)| (test, tree_to_decider(decider)))
                        .collect();

                    FanOut {
                        path,
                        tests: necessary_tests,
                        fallback: Box::new(tree_to_decider(fallback)),
                    }
                }
            },
        },
    }
}

fn to_chain<'a>(
    path: Path,
    test: Test<'a>,
    success_tree: DecisionTree<'a>,
    failure_tree: DecisionTree<'a>,
) -> Decider<'a, u64> {
    use Decider::*;

    let failure = tree_to_decider(failure_tree);

    match tree_to_decider(success_tree) {
        Chain {
            mut test_chain,
            success,
            failure: sub_failure,
        } if failure == *sub_failure => {
            test_chain.push((path, test));

            Chain {
                test_chain,
                success,
                failure: Box::new(failure),
            }
        }

        success => Chain {
            test_chain: vec![(path, test)],
            success: Box::new(success),
            failure: Box::new(failure),
        },
    }
}

/// INSERT CHOICES
///
/// If a target appears exactly once in a Decider, the corresponding expression
/// can be inlined. Whether things are inlined or jumps is called a "choice".

fn count_targets(decision_tree: &Decider<u64>) -> MutMap<u64, u64> {
    let mut result = MutMap::default();
    count_targets_help(decision_tree, &mut result);

    result
}

fn count_targets_help(decision_tree: &Decider<u64>, targets: &mut MutMap<u64, u64>) {
    use Decider::*;
    match decision_tree {
        Leaf(target) => match targets.get_mut(target) {
            None => {
                targets.insert(*target, 1);
            }
            Some(current) => {
                *current += 1;
            }
        },

        Chain {
            success, failure, ..
        } => {
            count_targets_help(success, targets);
            count_targets_help(failure, targets);
        }

        FanOut {
            tests, fallback, ..
        } => {
            count_targets_help(fallback, targets);

            for (_, decider) in tests {
                count_targets_help(decider, targets);
            }
        }
    }
}

#[allow(clippy::type_complexity)]
fn create_choices<'a>(
    target_counts: &MutMap<u64, u64>,
    target: u64,
    stores: Stores<'a>,
    branch: Expr<'a>,
) -> ((u64, Choice<'a>), Option<(u64, Stores<'a>, Expr<'a>)>) {
    match target_counts.get(&target) {
        None => unreachable!(
            "this should never happen: {:?} not in {:?}",
            target, target_counts
        ),
        Some(1) => ((target, Choice::Inline(stores, branch)), None),
        Some(_) => (
            (target, Choice::Jump(target)),
            Some((target, stores, branch)),
        ),
    }
}

fn insert_choices<'a>(
    choice_dict: &MutMap<u64, Choice<'a>>,
    decider: Decider<'a, u64>,
) -> Decider<'a, Choice<'a>> {
    use Decider::*;
    match decider {
        Leaf(target) => {
            // TODO remove clone
            // Only targes that appear once are Inline, so it's safe to remove them from the dict.
            Leaf(choice_dict[&target].clone())
        }

        Chain {
            test_chain,
            success,
            failure,
        } => Chain {
            test_chain,
            success: Box::new(insert_choices(choice_dict, *success)),
            failure: Box::new(insert_choices(choice_dict, *failure)),
        },

        FanOut {
            path,
            tests,
            fallback,
        } => FanOut {
            path,
            tests: tests
                .into_iter()
                .map(|(test, nested)| (test, insert_choices(choice_dict, nested)))
                .collect(),
            fallback: Box::new(insert_choices(choice_dict, *fallback)),
        },
    }
}

// Opt.FanOut path (map (second go) tests) (go fallback)
