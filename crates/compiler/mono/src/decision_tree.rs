use crate::ir::{
    BranchInfo, DestructType, Env, Expr, JoinPointId, Literal, Param, Pattern, Procs, Stmt,
};
use crate::layout::{Builtin, Layout, LayoutCache, TagIdIntType, UnionLayout};
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_collections::all::{MutMap, MutSet};
use roc_error_macros::internal_error;
use roc_exhaustive::{Ctor, CtorName, RenderAs, TagId, Union};
use roc_module::ident::TagName;
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;

/// COMPILE CASES

type Label = u64;
const RECORD_TAG_NAME: &str = "#Record";

/// Users of this module will mainly interact with this function. It takes
/// some normal branches and gives out a decision tree that has "labels" at all
/// the leafs and a dictionary that maps these "labels" to the code that should
/// run.
fn compile<'a>(raw_branches: Vec<(Guard<'a>, Pattern<'a>, u64)>) -> DecisionTree<'a> {
    let formatted = raw_branches
        .into_iter()
        .map(|(guard, pattern, index)| Branch {
            goal: index,
            guard,
            patterns: vec![(Vec::new(), pattern)],
        })
        .collect();

    to_decision_tree(formatted)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Guard<'a> {
    NoGuard,
    Guard {
        /// pattern
        pattern: Pattern<'a>,
        /// after assigning to symbol, the stmt jumps to this label
        id: JoinPointId,
        stmt: Stmt<'a>,
    },
}

impl<'a> Guard<'a> {
    fn is_none(&self) -> bool {
        self == &Guard::NoGuard
    }
}

#[derive(Clone, Debug, PartialEq)]
enum DecisionTree<'a> {
    Match(Label),
    Decision {
        path: Vec<PathInstruction>,
        edges: Vec<(GuardedTest<'a>, DecisionTree<'a>)>,
        default: Option<Box<DecisionTree<'a>>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
enum GuardedTest<'a> {
    // e.g. `_ if True -> ...`
    GuardedNoTest {
        /// pattern
        pattern: Pattern<'a>,
        /// after assigning to symbol, the stmt jumps to this label
        id: JoinPointId,
        /// body
        stmt: Stmt<'a>,
    },
    TestNotGuarded {
        test: Test<'a>,
    },
    Placeholder,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::enum_variant_names)]
enum Test<'a> {
    IsCtor {
        tag_id: TagIdIntType,
        ctor_name: CtorName,
        union: roc_exhaustive::Union,
        arguments: Vec<(Pattern<'a>, Layout<'a>)>,
    },
    IsInt([u8; 16], IntWidth),
    IsFloat(u64, FloatWidth),
    IsDecimal([u8; 16]),
    IsStr(Box<str>),
    IsBit(bool),
    IsByte {
        tag_id: TagIdIntType,
        num_alts: usize,
    },
}

impl<'a> Test<'a> {
    fn can_be_switch(&self) -> bool {
        match self {
            Test::IsCtor { .. } => true,
            Test::IsInt(_, int_width) => {
                // llvm does not like switching on 128-bit values
                !matches!(int_width, IntWidth::U128 | IntWidth::I128)
            }
            Test::IsFloat(_, _) => true,
            Test::IsDecimal(_) => false,
            Test::IsStr(_) => false,
            Test::IsBit(_) => true,
            Test::IsByte { .. } => true,
        }
    }
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
            IsInt(v, width) => {
                state.write_u8(1);
                v.hash(state);
                width.hash(state);
            }
            IsFloat(v, width) => {
                state.write_u8(2);
                v.hash(state);
                width.hash(state);
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
            IsDecimal(v) => {
                // TODO: Is this okay?
                state.write_u8(6);
                v.hash(state);
            }
        }
    }
}

impl<'a> Hash for GuardedTest<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            GuardedTest::GuardedNoTest { id, .. } => {
                state.write_u8(1);
                id.hash(state);
            }
            GuardedTest::TestNotGuarded { test } => {
                state.write_u8(0);
                test.hash(state);
            }
            GuardedTest::Placeholder => {
                state.write_u8(2);
            }
        }
    }
}

// ACTUALLY BUILD DECISION TREES

#[derive(Clone, Debug, PartialEq)]
struct Branch<'a> {
    goal: Label,
    guard: Guard<'a>,
    patterns: Vec<(Vec<PathInstruction>, Pattern<'a>)>,
}

fn to_decision_tree(raw_branches: Vec<Branch>) -> DecisionTree {
    let branches: Vec<_> = raw_branches.into_iter().map(flatten_patterns).collect();

    debug_assert!(!branches.is_empty());

    match check_for_match(&branches) {
        Match::Exact(goal) => DecisionTree::Match(goal),

        Match::GuardOnly => {
            // the first branch has no more tests to do, but it has an if-guard

            let mut branches = branches;
            let first = branches.remove(0);

            match first.guard {
                Guard::NoGuard => unreachable!(),

                Guard::Guard { id, stmt, pattern } => {
                    let guarded_test = GuardedTest::GuardedNoTest { id, stmt, pattern };

                    // the guard test does not have a path
                    let path = vec![];

                    // we expect none of the patterns need tests, those decisions should have been made already
                    debug_assert!(first
                        .patterns
                        .iter()
                        .all(|(_, pattern)| !needs_tests(pattern)));

                    let default = if branches.is_empty() {
                        None
                    } else {
                        Some(Box::new(to_decision_tree(branches)))
                    };

                    DecisionTree::Decision {
                        path,
                        edges: vec![(guarded_test, DecisionTree::Match(first.goal))],
                        default,
                    }
                }
            }
        }

        Match::None => {
            // must clone here to release the borrow on `branches`
            let path = pick_path(&branches).clone();

            let bs = branches.clone();
            let (edges, fallback) = gather_edges(branches, &path);

            let mut decision_edges: Vec<_> = edges
                .into_iter()
                .map(|(test, branches)| {
                    if bs == branches {
                        panic!();
                    } else {
                        (test, to_decision_tree(branches))
                    }
                })
                .collect();

            match (decision_edges.as_slice(), fallback.as_slice()) {
                ([(_test, _decision_tree)], []) => {
                    // only one test with no fallback: we will always enter this branch

                    // get the `_decision_tree` without cloning
                    decision_edges.pop().unwrap().1
                }
                (_, []) => break_out_guard(path, decision_edges, None),
                ([], _) => {
                    // should be guaranteed by the patterns
                    debug_assert!(!fallback.is_empty());
                    to_decision_tree(fallback)
                }
                (_, _) => break_out_guard(
                    path,
                    decision_edges,
                    Some(Box::new(to_decision_tree(fallback))),
                ),
            }
        }
    }
}

/// Give a guard it's own Decision
fn break_out_guard<'a>(
    path: Vec<PathInstruction>,
    mut edges: Vec<(GuardedTest<'a>, DecisionTree<'a>)>,
    default: Option<Box<DecisionTree<'a>>>,
) -> DecisionTree<'a> {
    match edges
        .iter()
        .position(|(t, _)| matches!(t, GuardedTest::Placeholder))
    {
        None => DecisionTree::Decision {
            path,
            edges,
            default,
        },
        Some(index) => {
            let (a, b) = edges.split_at_mut(index + 1);

            let new_default = break_out_guard(path.clone(), b.to_vec(), default);

            let mut left = a.to_vec();
            let guard = left.pop().unwrap();

            let help = DecisionTree::Decision {
                path: path.clone(),
                edges: vec![guard],
                default: Some(Box::new(new_default)),
            };

            DecisionTree::Decision {
                path,
                edges: left,
                default: Some(Box::new(help)),
            }
        }
    }
}

fn guarded_tests_are_complete(tests: &[GuardedTest]) -> bool {
    let length = tests.len();
    debug_assert!(length > 0);

    let no_guard = tests
        .iter()
        .all(|t| matches!(t, GuardedTest::TestNotGuarded { .. }));

    match tests.last().unwrap() {
        GuardedTest::Placeholder => false,
        GuardedTest::GuardedNoTest { .. } => false,
        GuardedTest::TestNotGuarded { test } => no_guard && tests_are_complete_help(test, length),
    }
}

fn tests_are_complete_help(last_test: &Test, number_of_tests: usize) -> bool {
    match last_test {
        Test::IsCtor { union, .. } => number_of_tests == union.alternatives.len(),
        Test::IsByte { num_alts, .. } => number_of_tests == *num_alts,
        Test::IsBit(_) => number_of_tests == 2,
        Test::IsInt(_, _) => false,
        Test::IsFloat(_, _) => false,
        Test::IsDecimal(_) => false,
        Test::IsStr(_) => false,
    }
}

fn flatten_patterns(branch: Branch) -> Branch {
    let mut result = Vec::with_capacity(branch.patterns.len());

    for path_pattern in branch.patterns {
        flatten(path_pattern, &mut result);
    }

    Branch {
        patterns: result,
        ..branch
    }
}

fn flatten<'a>(
    path_pattern: (Vec<PathInstruction>, Pattern<'a>),
    path_patterns: &mut Vec<(Vec<PathInstruction>, Pattern<'a>)>,
) {
    match path_pattern.1 {
        Pattern::AppliedTag {
            union,
            arguments,
            tag_id,
            tag_name,
            layout,
        } if union.alternatives.len() == 1 && !layout.is_nullable() => {
            // TODO ^ do we need to check that guard.is_none() here?

            let path = path_pattern.0;
            // Theory: unbox doesn't have any value for us, because one-element tag unions
            // don't store the tag anyway.
            if arguments.len() == 1 {
                // NOTE here elm will unbox, but we don't use that
                path_patterns.push((
                    path,
                    Pattern::AppliedTag {
                        union,
                        arguments,
                        tag_id,
                        tag_name,
                        layout,
                    },
                ));
            } else {
                for (index, (arg_pattern, _)) in arguments.iter().enumerate() {
                    let mut new_path = path.clone();
                    new_path.push(PathInstruction::TagIndex {
                        index: index as u64,
                        tag_id,
                    });

                    flatten((new_path, arg_pattern.clone()), path_patterns);
                }
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

enum Match {
    Exact(Label),
    GuardOnly,
    None,
}

fn check_for_match(branches: &[Branch]) -> Match {
    match branches.get(0) {
        Some(Branch {
            goal,
            patterns,
            guard,
        }) if patterns.iter().all(|(_, pattern)| !needs_tests(pattern)) => {
            if guard.is_none() {
                Match::Exact(*goal)
            } else {
                Match::GuardOnly
            }
        }
        _ => Match::None,
    }
}

/// GATHER OUTGOING EDGES

// my understanding: branches that we could jump to based on the pattern at the current path
fn gather_edges<'a>(
    branches: Vec<Branch<'a>>,
    path: &[PathInstruction],
) -> (Vec<(GuardedTest<'a>, Vec<Branch<'a>>)>, Vec<Branch<'a>>) {
    let relevant_tests = tests_at_path(path, &branches);

    let check = guarded_tests_are_complete(&relevant_tests);

    let all_edges = relevant_tests
        .into_iter()
        .map(|t| edges_for(path, &branches, t))
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

fn tests_at_path<'a>(
    selected_path: &[PathInstruction],
    branches: &[Branch<'a>],
) -> Vec<GuardedTest<'a>> {
    // NOTE the ordering of the result is important!

    let mut all_tests = Vec::new();

    for branch in branches {
        all_tests.extend(test_at_path(selected_path, branch));
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

fn test_at_path<'a>(
    selected_path: &[PathInstruction],
    branch: &Branch<'a>,
) -> Option<GuardedTest<'a>> {
    use Pattern::*;
    use Test::*;

    match branch
        .patterns
        .iter()
        .find(|(path, _)| path == selected_path)
    {
        None => None,
        Some((_, pattern)) => {
            let test = match pattern {
                Identifier(_) | Underscore => {
                    if let Guard::Guard { .. } = &branch.guard {
                        // no tests for this pattern remain, but we cannot discard it yet
                        // because it has a guard!
                        return Some(GuardedTest::Placeholder);
                    } else {
                        return None;
                    }
                }

                RecordDestructure(destructs, _) => {
                    // not rendered, so pick the easiest
                    let union = Union {
                        render_as: RenderAs::Tag,
                        alternatives: vec![Ctor {
                            tag_id: TagId(0),
                            name: CtorName::Tag(TagName(RECORD_TAG_NAME.into())),
                            arity: destructs.len(),
                        }],
                    };

                    let mut arguments = std::vec::Vec::new();

                    for destruct in destructs {
                        match &destruct.typ {
                            DestructType::Guard(guard) => {
                                arguments.push((guard.clone(), destruct.layout));
                            }
                            DestructType::Required(_) => {
                                arguments.push((Pattern::Underscore, destruct.layout));
                            }
                        }
                    }

                    IsCtor {
                        tag_id: 0,
                        ctor_name: CtorName::Tag(TagName(RECORD_TAG_NAME.into())),
                        union,
                        arguments,
                    }
                }

                NewtypeDestructure {
                    tag_name,
                    arguments,
                } => {
                    let tag_id = 0;
                    let union =
                        Union::newtype_wrapper(CtorName::Tag(tag_name.clone()), arguments.len());

                    IsCtor {
                        tag_id,
                        ctor_name: CtorName::Tag(tag_name.clone()),
                        union,
                        arguments: arguments.to_vec(),
                    }
                }

                AppliedTag {
                    tag_name,
                    tag_id,
                    arguments,
                    union,
                    ..
                } => IsCtor {
                    tag_id: *tag_id,
                    ctor_name: CtorName::Tag(tag_name.clone()),
                    union: union.clone(),
                    arguments: arguments.to_vec(),
                },

                Voided { .. } => internal_error!("unreachable"),

                OpaqueUnwrap { opaque, argument } => {
                    let union = Union {
                        render_as: RenderAs::Tag,
                        alternatives: vec![Ctor {
                            tag_id: TagId(0),
                            name: CtorName::Opaque(*opaque),
                            arity: 1,
                        }],
                    };

                    IsCtor {
                        tag_id: 0,
                        ctor_name: CtorName::Opaque(*opaque),
                        union,
                        arguments: vec![(**argument).clone()],
                    }
                }

                BitLiteral { value, .. } => IsBit(*value),
                EnumLiteral { tag_id, union, .. } => IsByte {
                    tag_id: *tag_id as _,
                    num_alts: union.alternatives.len(),
                },
                IntLiteral(v, precision) => IsInt(*v, *precision),
                FloatLiteral(v, precision) => IsFloat(*v, *precision),
                DecimalLiteral(v) => IsDecimal(*v),
                StrLiteral(v) => IsStr(v.clone()),
            };

            let guarded_test = GuardedTest::TestNotGuarded { test };

            Some(guarded_test)
        }
    }
}

/// BUILD EDGES

// understanding: if the test is successful, where could we go?
fn edges_for<'a>(
    path: &[PathInstruction],
    branches: &[Branch<'a>],
    test: GuardedTest<'a>,
) -> (GuardedTest<'a>, Vec<Branch<'a>>) {
    let mut new_branches = Vec::new();

    // if we test for a guard, skip all branches until one that has a guard

    let it = match test {
        GuardedTest::GuardedNoTest { .. } | GuardedTest::Placeholder => {
            let index = branches
                .iter()
                .position(|b| !b.guard.is_none())
                .expect("if testing for a guard, one branch must have a guard");

            branches[index..].iter()
        }
        GuardedTest::TestNotGuarded { .. } => branches.iter(),
    };

    for branch in it {
        new_branches.extend(to_relevant_branch(&test, path, branch));
    }

    (test, new_branches)
}

fn to_relevant_branch<'a>(
    guarded_test: &GuardedTest<'a>,
    path: &[PathInstruction],
    branch: &Branch<'a>,
) -> Option<Branch<'a>> {
    // TODO remove clone
    match extract(path, branch.patterns.clone()) {
        Extract::NotFound => Some(branch.clone()),
        Extract::Found {
            start,
            found_pattern: pattern,
            end,
        } => match guarded_test {
            GuardedTest::Placeholder | GuardedTest::GuardedNoTest { .. } => {
                // if there is no test, the pattern should not require any
                debug_assert!(
                    matches!(pattern, Pattern::Identifier(_) | Pattern::Underscore,),
                    "{:?}",
                    pattern,
                );

                Some(branch.clone())
            }
            GuardedTest::TestNotGuarded { test } => {
                to_relevant_branch_help(test, path, start, end, branch, pattern)
            }
        },
    }
}

fn to_relevant_branch_help<'a>(
    test: &Test<'a>,
    path: &[PathInstruction],
    mut start: Vec<(Vec<PathInstruction>, Pattern<'a>)>,
    end: Vec<(Vec<PathInstruction>, Pattern<'a>)>,
    branch: &Branch<'a>,
    pattern: Pattern<'a>,
) -> Option<Branch<'a>> {
    use Pattern::*;
    use Test::*;

    match pattern {
        Identifier(_) | Underscore => Some(branch.clone()),

        RecordDestructure(destructs, _) => match test {
            IsCtor {
                ctor_name: test_name,
                tag_id,
                ..
            } => {
                debug_assert!(test_name == &CtorName::Tag(TagName(RECORD_TAG_NAME.into())));
                let destructs_len = destructs.len();
                let sub_positions = destructs.into_iter().enumerate().map(|(index, destruct)| {
                    let pattern = match destruct.typ {
                        DestructType::Guard(guard) => guard.clone(),
                        DestructType::Required(_) => Pattern::Underscore,
                    };

                    let mut new_path = path.to_vec();
                    let next_instr = if destructs_len == 1 {
                        PathInstruction::NewType
                    } else {
                        PathInstruction::TagIndex {
                            index: index as u64,
                            tag_id: *tag_id,
                        }
                    };
                    new_path.push(next_instr);

                    (new_path, pattern)
                });
                start.extend(sub_positions);
                start.extend(end);

                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }
            _ => None,
        },

        OpaqueUnwrap { opaque, argument } => match test {
            IsCtor {
                ctor_name: test_opaque_tag_name,
                tag_id,
                ..
            } => {
                debug_assert_eq!(*tag_id, 0);
                debug_assert_eq!(test_opaque_tag_name, &CtorName::Opaque(opaque));

                let (argument, _) = *argument;

                let mut new_path = path.to_vec();
                new_path.push(PathInstruction::NewType);

                start.push((new_path, argument));
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }
            _ => None,
        },

        NewtypeDestructure {
            tag_name,
            arguments,
            ..
        } => match test {
            IsCtor {
                ctor_name: test_name,
                tag_id: test_id,
                ..
            } if test_name.is_tag(&tag_name) => {
                let tag_id = 0;
                debug_assert_eq!(tag_id, *test_id);

                let num_args = arguments.len();
                let sub_positions =
                    arguments
                        .into_iter()
                        .enumerate()
                        .map(|(index, (pattern, _))| {
                            let mut new_path = path.to_vec();
                            let next_instr = if num_args == 1 {
                                PathInstruction::NewType
                            } else {
                                PathInstruction::TagIndex {
                                    index: index as u64,
                                    tag_id,
                                }
                            };
                            new_path.push(next_instr);
                            (new_path, pattern)
                        });
                start.extend(sub_positions);
                start.extend(end);

                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }

            _ => None,
        },

        AppliedTag {
            tag_name,
            tag_id,
            arguments,
            layout,
            ..
        } => {
            match test {
                IsCtor {
                    ctor_name: test_name,
                    tag_id: test_id,
                    ..
                } if test_name.is_tag(&tag_name) => {
                    debug_assert_eq!(tag_id, *test_id);

                    // the test matches the constructor of this pattern
                    match layout {
                        UnionLayout::NonRecursive(
                            [[Layout::Struct {
                                field_layouts: [_], ..
                            }]],
                        ) => {
                            // a one-element record equivalent
                            // Theory: Unbox doesn't have any value for us
                            debug_assert_eq!(arguments.len(), 1);
                            let arg = arguments[0].clone();
                            {
                                // NOTE here elm unboxes, but we ignore that
                                // Path::Unbox(Box::new(path.clone()))
                                start.push((path.to_vec(), arg.0));
                                start.extend(end);
                            }
                        }
                        UnionLayout::NonRecursive([_]) | UnionLayout::NonNullableUnwrapped(_) => {
                            let sub_positions =
                                arguments
                                    .into_iter()
                                    .enumerate()
                                    .map(|(index, (pattern, _))| {
                                        let mut new_path = path.to_vec();
                                        new_path.push(PathInstruction::TagIndex {
                                            index: index as u64,
                                            tag_id,
                                        });
                                        (new_path, pattern)
                                    });
                            start.extend(sub_positions);
                            start.extend(end);
                        }
                        UnionLayout::NonRecursive(_)
                        | UnionLayout::Recursive(_)
                        | UnionLayout::NullableWrapped { .. }
                        | UnionLayout::NullableUnwrapped { .. } => {
                            let sub_positions =
                                arguments
                                    .into_iter()
                                    .enumerate()
                                    .map(|(index, (pattern, _))| {
                                        let mut new_path = path.to_vec();
                                        new_path.push(PathInstruction::TagIndex {
                                            index: index as u64,
                                            tag_id,
                                        });
                                        (new_path, pattern)
                                    });
                            start.extend(sub_positions);
                            start.extend(end);
                        }
                    }

                    Some(Branch {
                        goal: branch.goal,
                        guard: branch.guard.clone(),
                        patterns: start,
                    })
                }
                _ => None,
            }
        }
        Voided { .. } => internal_error!("unreachable"),
        StrLiteral(string) => match test {
            IsStr(test_str) if string == *test_str => {
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }
            _ => None,
        },

        IntLiteral(int, p1) => match test {
            IsInt(is_int, p2) if int == *is_int => {
                debug_assert_eq!(p1, *p2);
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }
            _ => None,
        },

        FloatLiteral(float, p1) => match test {
            IsFloat(test_float, p2) if float == *test_float => {
                debug_assert_eq!(p1, *p2);
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }
            _ => None,
        },

        DecimalLiteral(dec) => match test {
            IsDecimal(test_dec) if dec.eq(test_dec) => {
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
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
                    guard: branch.guard.clone(),
                    patterns: start,
                })
            }
            _ => None,
        },

        EnumLiteral { tag_id, .. } => match test {
            IsByte {
                tag_id: test_id, ..
            } if tag_id == *test_id as _ => {
                start.extend(end);
                Some(Branch {
                    goal: branch.goal,
                    guard: branch.guard.clone(),
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
        start: Vec<(Vec<PathInstruction>, Pattern<'a>)>,
        found_pattern: Pattern<'a>,
        end: Vec<(Vec<PathInstruction>, Pattern<'a>)>,
    },
}

fn extract<'a>(
    selected_path: &[PathInstruction],
    path_patterns: Vec<(Vec<PathInstruction>, Pattern<'a>)>,
) -> Extract<'a> {
    let mut start = Vec::new();

    // TODO potential ordering problem
    let mut it = path_patterns.into_iter();
    while let Some(current) = it.next() {
        if current.0 == selected_path {
            return Extract::Found {
                start,
                found_pattern: current.1,
                end: it.collect::<Vec<_>>(),
            };
        } else {
            start.push(current);
        }
    }

    Extract::NotFound
}

/// FIND IRRELEVANT BRANCHES

fn is_irrelevant_to<'a>(selected_path: &[PathInstruction], branch: &Branch<'a>) -> bool {
    match branch
        .patterns
        .iter()
        .find(|(path, _)| path == selected_path)
    {
        None => true,
        Some((_, pattern)) => branch.guard.is_none() && !needs_tests(pattern),
    }
}

/// Does this pattern need a branch test?
///
/// Keep up to date with [needs_path_instruction].
fn needs_tests(pattern: &Pattern) -> bool {
    use Pattern::*;

    match pattern {
        Identifier(_) | Underscore => false,

        NewtypeDestructure { .. }
        | RecordDestructure(..)
        | AppliedTag { .. }
        | OpaqueUnwrap { .. }
        | BitLiteral { .. }
        | EnumLiteral { .. }
        | IntLiteral(_, _)
        | FloatLiteral(_, _)
        | DecimalLiteral(_)
        | StrLiteral(_) => true,

        Voided { .. } => internal_error!("unreachable"),
    }
}

/// PICK A PATH

fn pick_path<'a>(branches: &'a [Branch]) -> &'a Vec<PathInstruction> {
    let mut all_paths = Vec::with_capacity(branches.len());

    // is choice path
    for branch in branches {
        for (path, pattern) in &branch.patterns {
            // NOTE we no longer check for the guard here
            // if !branch.guard.is_none() || needs_tests(&pattern) {
            if needs_tests(pattern) {
                all_paths.push(path);
            } else {
                // do nothing
            }
        }
    }

    let mut by_small_defaults = bests_by_small_defaults(branches, all_paths.into_iter());

    if by_small_defaults.len() == 1 {
        by_small_defaults.remove(0)
    } else {
        debug_assert!(!by_small_defaults.is_empty());
        let mut result = bests_by_small_branching_factor(branches, by_small_defaults.into_iter());

        match result.pop() {
            None => unreachable!("bests_by will always return at least one value in the vec"),
            Some(path) => path,
        }
    }
}

fn bests_by_small_branching_factor<'a, I>(
    branches: &[Branch],
    mut all_paths: I,
) -> Vec<&'a Vec<PathInstruction>>
where
    I: Iterator<Item = &'a Vec<PathInstruction>>,
{
    match all_paths.next() {
        None => panic!("Cannot choose the best of zero paths. This should never happen."),
        Some(first_path) => {
            let mut min_weight = small_branching_factor(branches, first_path);
            let mut min_paths = vec![first_path];

            for path in all_paths {
                let weight = small_branching_factor(branches, path);

                use std::cmp::Ordering;
                match weight.cmp(&min_weight) {
                    Ordering::Equal => {
                        min_paths.push(path);
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

fn bests_by_small_defaults<'a, I>(
    branches: &[Branch],
    mut all_paths: I,
) -> Vec<&'a Vec<PathInstruction>>
where
    I: Iterator<Item = &'a Vec<PathInstruction>>,
{
    match all_paths.next() {
        None => panic!("Cannot choose the best of zero paths. This should never happen."),
        Some(first_path) => {
            let mut min_weight = small_defaults(branches, first_path);
            let mut min_paths = vec![first_path];

            for path in all_paths {
                let weight = small_defaults(branches, path);

                use std::cmp::Ordering;
                match weight.cmp(&min_weight) {
                    Ordering::Equal => {
                        min_paths.push(path);
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

fn small_defaults(branches: &[Branch], path: &[PathInstruction]) -> usize {
    branches
        .iter()
        .filter(|b| is_irrelevant_to(path, b))
        .map(|_| 1)
        .sum()
}

fn small_branching_factor(branches: &[Branch], path: &[PathInstruction]) -> usize {
    // a specialized version of gather_edges that just counts the number of options

    let relevant_tests = tests_at_path(path, branches);

    let check = guarded_tests_are_complete(&relevant_tests);

    let fallbacks = if check {
        false
    } else {
        branches.iter().any(|b| is_irrelevant_to(path, b))
    };

    relevant_tests.len() + (if !fallbacks { 0 } else { 1 })
}

#[derive(Clone, Debug, PartialEq)]
enum Decider<'a, T> {
    Leaf(T),
    Guarded {
        /// after assigning to symbol, the stmt jumps to this label
        id: JoinPointId,
        stmt: Stmt<'a>,
        pattern: Pattern<'a>,

        success: Box<Decider<'a, T>>,
        failure: Box<Decider<'a, T>>,
    },
    Chain {
        test_chain: Vec<(Vec<PathInstruction>, Test<'a>)>,
        success: Box<Decider<'a, T>>,
        failure: Box<Decider<'a, T>>,
    },
    FanOut {
        path: Vec<PathInstruction>,
        tests: Vec<(Test<'a>, Decider<'a, T>)>,
        fallback: Box<Decider<'a, T>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
enum Choice<'a> {
    Inline(Stmt<'a>),
    Jump(Label),
}

type StoresVec<'a> = bumpalo::collections::Vec<'a, (Symbol, Layout<'a>, Expr<'a>)>;

pub fn optimize_when<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    cond_symbol: Symbol,
    cond_layout: Layout<'a>,
    ret_layout: Layout<'a>,
    opt_branches: bumpalo::collections::Vec<'a, (Pattern<'a>, Guard<'a>, Stmt<'a>)>,
) -> Stmt<'a> {
    let (patterns, _indexed_branches) = opt_branches
        .into_iter()
        .enumerate()
        .map(|(index, (pattern, guard, branch))| {
            let has_guard = !guard.is_none();
            (
                (guard, pattern.clone(), index as u64),
                (index as u64, branch, pattern, has_guard),
            )
        })
        .unzip();

    let indexed_branches: Vec<_> = _indexed_branches;

    let decision_tree = compile(patterns);
    let decider = tree_to_decider(decision_tree);

    // for each target (branch body), count in how many ways it can be reached
    let mut target_counts = bumpalo::vec![in env.arena; 0; indexed_branches.len()];
    count_targets(&mut target_counts, &decider);

    let mut choices = MutMap::default();
    let mut jumps = Vec::new();

    for (index, mut branch, pattern, has_guard) in indexed_branches.into_iter() {
        // bind the fields referenced in the pattern. For guards this happens separately, so
        // the pattern variables are defined when evaluating the guard.
        if !has_guard {
            branch =
                crate::ir::store_pattern(env, procs, layout_cache, &pattern, cond_symbol, branch);
        }

        let ((branch_index, choice), opt_jump) = create_choices(&target_counts, index, branch);

        if let Some((index, body)) = opt_jump {
            let id = JoinPointId(env.unique_symbol());
            jumps.push((index, id, body));
        }

        choices.insert(branch_index, choice);
    }

    let choice_decider = insert_choices(&choices, decider);

    let mut stmt = decide_to_branching(
        env,
        procs,
        layout_cache,
        cond_symbol,
        cond_layout,
        ret_layout,
        choice_decider,
        &jumps,
    );

    for (_, id, body) in jumps.into_iter() {
        stmt = Stmt::Join {
            id,
            parameters: &[],
            body: env.arena.alloc(body),
            remainder: env.arena.alloc(stmt),
        };
    }

    stmt
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PathInstruction {
    NewType,
    TagIndex { index: u64, tag_id: TagIdIntType },
}

fn path_to_expr_help<'a>(
    env: &mut Env<'a, '_>,
    mut symbol: Symbol,
    path: &[PathInstruction],
    mut layout: Layout<'a>,
) -> (Symbol, StoresVec<'a>, Layout<'a>) {
    let mut stores = bumpalo::collections::Vec::new_in(env.arena);

    // let instructions = reverse_path(path);
    let instructions = path;
    let mut it = instructions.iter().peekable();

    while let Some(path_instr) = it.next() {
        match path_instr {
            PathInstruction::NewType => {
                // pass through
            }

            PathInstruction::TagIndex { index, tag_id } => {
                let index = *index;

                match &layout {
                    Layout::Union(union_layout) => {
                        let inner_expr = Expr::UnionAtIndex {
                            tag_id: *tag_id,
                            structure: symbol,
                            index,
                            union_layout: *union_layout,
                        };

                        let inner_layout =
                            union_layout.layout_at(*tag_id as TagIdIntType, index as usize);

                        symbol = env.unique_symbol();
                        stores.push((symbol, inner_layout, inner_expr));

                        layout = inner_layout;
                    }

                    Layout::Struct { field_layouts, .. } => {
                        debug_assert!(field_layouts.len() > 1);

                        let inner_expr = Expr::StructAtIndex {
                            index,
                            field_layouts,
                            structure: symbol,
                        };

                        let inner_layout = field_layouts[index as usize];

                        symbol = env.unique_symbol();
                        stores.push((symbol, inner_layout, inner_expr));

                        layout = inner_layout;
                    }

                    _ => {
                        // this MUST be an index into a single-element (hence unwrapped) record

                        debug_assert_eq!(index, 0, "{:?}", &layout);
                        debug_assert_eq!(*tag_id, 0);
                        debug_assert!(it.peek().is_none());

                        break;
                    }
                }
            }
        }
    }

    (symbol, stores, layout)
}

fn test_to_equality<'a>(
    env: &mut Env<'a, '_>,
    cond_symbol: Symbol,
    cond_layout: &Layout<'a>,
    path: &[PathInstruction],
    test: Test<'a>,
) -> (StoresVec<'a>, Symbol, Symbol, Option<ConstructorKnown<'a>>) {
    let (rhs_symbol, mut stores, test_layout) =
        path_to_expr_help(env, cond_symbol, path, *cond_layout);

    match test {
        Test::IsCtor { tag_id, union, .. } => {
            let path_symbol = rhs_symbol;
            // the IsCtor check should never be generated for tag unions of size 1
            // (e.g. record pattern guard matches)
            debug_assert!(union.alternatives.len() > 1);

            match test_layout {
                Layout::Union(union_layout) => {
                    let lhs = Expr::Literal(Literal::Int((tag_id as i128).to_ne_bytes()));

                    let rhs = Expr::GetTagId {
                        structure: path_symbol,
                        union_layout,
                    };

                    let lhs_symbol = env.unique_symbol();
                    let rhs_symbol = env.unique_symbol();

                    let tag_id_layout = union_layout.tag_id_layout();

                    stores.push((lhs_symbol, tag_id_layout, lhs));
                    stores.push((rhs_symbol, tag_id_layout, rhs));

                    (
                        stores,
                        lhs_symbol,
                        rhs_symbol,
                        Some(ConstructorKnown::OnlyPass {
                            scrutinee: path_symbol,
                            layout: *cond_layout,
                            tag_id,
                        }),
                    )
                }
                _ => unreachable!("{:#?}", (cond_layout, union, test_layout, path)),
            }
        }

        Test::IsInt(test_int, precision) => {
            let lhs = Expr::Literal(Literal::Int(test_int));
            let lhs_symbol = env.unique_symbol();
            stores.push((lhs_symbol, Layout::int_width(precision), lhs));

            (stores, lhs_symbol, rhs_symbol, None)
        }

        Test::IsFloat(test_int, precision) => {
            // TODO maybe we can actually use i64 comparison here?
            let test_float = f64::from_bits(test_int as u64);
            let lhs = Expr::Literal(Literal::Float(test_float));
            let lhs_symbol = env.unique_symbol();
            stores.push((lhs_symbol, Layout::float_width(precision), lhs));

            (stores, lhs_symbol, rhs_symbol, None)
        }

        Test::IsDecimal(test_dec) => {
            let lhs = Expr::Literal(Literal::Decimal(test_dec));
            let lhs_symbol = env.unique_symbol();
            stores.push((lhs_symbol, *cond_layout, lhs));

            (stores, lhs_symbol, rhs_symbol, None)
        }

        Test::IsByte {
            tag_id: test_byte, ..
        } => {
            debug_assert!(test_byte <= (u8::MAX as u16));

            let lhs = Expr::Literal(Literal::Byte(test_byte as u8));
            let lhs_symbol = env.unique_symbol();
            stores.push((lhs_symbol, Layout::u8(), lhs));

            (stores, lhs_symbol, rhs_symbol, None)
        }

        Test::IsBit(test_bit) => {
            let lhs = Expr::Literal(Literal::Bool(test_bit));
            let lhs_symbol = env.unique_symbol();
            stores.push((lhs_symbol, Layout::Builtin(Builtin::Bool), lhs));

            (stores, lhs_symbol, rhs_symbol, None)
        }

        Test::IsStr(test_str) => {
            let lhs = Expr::Literal(Literal::Str(env.arena.alloc(test_str)));
            let lhs_symbol = env.unique_symbol();

            stores.push((lhs_symbol, Layout::Builtin(Builtin::Str), lhs));

            (stores, lhs_symbol, rhs_symbol, None)
        }
    }
}

type Tests<'a> = std::vec::Vec<(
    bumpalo::collections::Vec<'a, (Symbol, Layout<'a>, Expr<'a>)>,
    Symbol,
    Symbol,
    Option<ConstructorKnown<'a>>,
)>;

fn stores_and_condition<'a>(
    env: &mut Env<'a, '_>,
    cond_symbol: Symbol,
    cond_layout: &Layout<'a>,
    test_chain: Vec<(Vec<PathInstruction>, Test<'a>)>,
) -> Tests<'a> {
    let mut tests: Tests = Vec::with_capacity(test_chain.len());

    // Assumption: there is at most 1 guard, and it is the outer layer.
    for (path, test) in test_chain {
        tests.push(test_to_equality(env, cond_symbol, cond_layout, &path, test))
    }

    tests
}

fn compile_test<'a>(
    env: &mut Env<'a, '_>,
    ret_layout: Layout<'a>,
    stores: bumpalo::collections::Vec<'a, (Symbol, Layout<'a>, Expr<'a>)>,
    lhs: Symbol,
    rhs: Symbol,
    fail: &'a Stmt<'a>,
    cond: Stmt<'a>,
) -> Stmt<'a> {
    compile_test_help(
        env,
        ConstructorKnown::Neither,
        ret_layout,
        stores,
        lhs,
        rhs,
        fail,
        cond,
    )
}

#[allow(clippy::too_many_arguments)]
fn compile_test_help<'a>(
    env: &mut Env<'a, '_>,
    branch_info: ConstructorKnown<'a>,
    ret_layout: Layout<'a>,
    stores: bumpalo::collections::Vec<'a, (Symbol, Layout<'a>, Expr<'a>)>,
    lhs: Symbol,
    rhs: Symbol,
    fail: &'a Stmt<'a>,
    mut cond: Stmt<'a>,
) -> Stmt<'a> {
    // if test_symbol then cond else fail
    let test_symbol = env.unique_symbol();
    let arena = env.arena;

    let (pass_info, fail_info) = {
        use ConstructorKnown::*;
        match branch_info {
            Both {
                scrutinee,
                layout,
                pass,
                fail,
            } => {
                let pass_info = BranchInfo::Constructor {
                    scrutinee,
                    layout,
                    tag_id: pass,
                };
                let fail_info = BranchInfo::Constructor {
                    scrutinee,
                    layout,
                    tag_id: fail,
                };

                (pass_info, fail_info)
            }

            OnlyPass {
                scrutinee,
                layout,
                tag_id,
            } => {
                let pass_info = BranchInfo::Constructor {
                    scrutinee,
                    layout,
                    tag_id,
                };

                (pass_info, BranchInfo::None)
            }

            Neither => (BranchInfo::None, BranchInfo::None),
        }
    };

    let branches = env.arena.alloc([(1u64, pass_info, cond)]);
    let default_branch = (fail_info, &*env.arena.alloc(fail.clone()));

    cond = Stmt::Switch {
        cond_symbol: test_symbol,
        cond_layout: Layout::Builtin(Builtin::Bool),
        ret_layout,
        branches,
        default_branch,
    };

    let op = LowLevel::Eq;
    let test = Expr::Call(crate::ir::Call {
        call_type: crate::ir::CallType::LowLevel {
            op,
            update_mode: env.next_update_mode_id(),
        },
        arguments: arena.alloc([lhs, rhs]),
    });

    // write to the test symbol
    cond = Stmt::Let(
        test_symbol,
        test,
        Layout::Builtin(Builtin::Bool),
        arena.alloc(cond),
    );

    // stores are in top-to-bottom order, so we have to add them in reverse
    for (symbol, layout, expr) in stores.into_iter().rev() {
        cond = Stmt::Let(symbol, expr, layout, arena.alloc(cond));
    }

    cond
}

fn compile_tests<'a>(
    env: &mut Env<'a, '_>,
    ret_layout: Layout<'a>,
    tests: Tests<'a>,
    fail: &'a Stmt<'a>,
    mut cond: Stmt<'a>,
) -> Stmt<'a> {
    for (new_stores, lhs, rhs, opt_constructor_info) in tests.into_iter() {
        match opt_constructor_info {
            None => {
                cond = compile_test(env, ret_layout, new_stores, lhs, rhs, fail, cond);
            }
            Some(cinfo) => {
                cond = compile_test_help(env, cinfo, ret_layout, new_stores, lhs, rhs, fail, cond);
            }
        }
    }
    cond
}

#[derive(Debug)]
enum ConstructorKnown<'a> {
    Both {
        scrutinee: Symbol,
        layout: Layout<'a>,
        pass: TagIdIntType,
        fail: TagIdIntType,
    },
    OnlyPass {
        scrutinee: Symbol,
        layout: Layout<'a>,
        tag_id: TagIdIntType,
    },
    Neither,
}

impl<'a> ConstructorKnown<'a> {
    fn from_test_chain(
        cond_symbol: Symbol,
        cond_layout: &Layout<'a>,
        test_chain: &[(Vec<PathInstruction>, Test)],
    ) -> Self {
        match test_chain {
            [(path, test)] => match test {
                Test::IsCtor { tag_id, union, .. } if path.is_empty() => {
                    if union.alternatives.len() == 2 {
                        // excluded middle: we also know the tag_id in the fail branch
                        ConstructorKnown::Both {
                            layout: *cond_layout,
                            scrutinee: cond_symbol,
                            pass: *tag_id,
                            fail: (*tag_id == 0) as _,
                        }
                    } else {
                        ConstructorKnown::OnlyPass {
                            layout: *cond_layout,
                            scrutinee: cond_symbol,
                            tag_id: *tag_id,
                        }
                    }
                }
                _ => ConstructorKnown::Neither,
            },
            _ => ConstructorKnown::Neither,
        }
    }
}

// TODO procs and layout are currently unused, but potentially required
// for defining optional fields?
// if not, do remove
#[allow(clippy::too_many_arguments, clippy::needless_collect)]
fn decide_to_branching<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    cond_symbol: Symbol,
    cond_layout: Layout<'a>,
    ret_layout: Layout<'a>,
    decider: Decider<'a, Choice<'a>>,
    jumps: &[(u64, JoinPointId, Stmt<'a>)],
) -> Stmt<'a> {
    use Choice::*;
    use Decider::*;

    let arena = env.arena;

    match decider {
        Leaf(Jump(label)) => {
            let index = jumps
                .binary_search_by_key(&label, |r| r.0)
                .expect("jump not in list of jumps");

            Stmt::Jump(jumps[index].1, &[])
        }
        Leaf(Inline(expr)) => expr,
        Guarded {
            id,
            stmt,
            pattern,
            success,
            failure,
        } => {
            // the guard is the final thing that we check, so needs to be layered on first!
            let test_symbol = env.unique_symbol();
            let arena = env.arena;

            let pass_expr = decide_to_branching(
                env,
                procs,
                layout_cache,
                cond_symbol,
                cond_layout,
                ret_layout,
                *success,
                jumps,
            );

            let fail_expr = decide_to_branching(
                env,
                procs,
                layout_cache,
                cond_symbol,
                cond_layout,
                ret_layout,
                *failure,
                jumps,
            );

            let decide = crate::ir::cond(
                env,
                test_symbol,
                Layout::Builtin(Builtin::Bool),
                pass_expr,
                fail_expr,
                ret_layout,
            );

            // calculate the guard value
            let param = Param {
                symbol: test_symbol,
                layout: Layout::Builtin(Builtin::Bool),
                borrow: false,
            };

            let join = Stmt::Join {
                id,
                parameters: arena.alloc([param]),
                remainder: arena.alloc(stmt),
                body: arena.alloc(decide),
            };

            crate::ir::store_pattern(env, procs, layout_cache, &pattern, cond_symbol, join)
        }
        Chain {
            test_chain,
            success,
            failure,
        } => {
            // generate a (nested) if-then-else

            let pass_expr = decide_to_branching(
                env,
                procs,
                layout_cache,
                cond_symbol,
                cond_layout,
                ret_layout,
                *success,
                jumps,
            );

            let fail_expr = decide_to_branching(
                env,
                procs,
                layout_cache,
                cond_symbol,
                cond_layout,
                ret_layout,
                *failure,
                jumps,
            );

            let chain_branch_info =
                ConstructorKnown::from_test_chain(cond_symbol, &cond_layout, &test_chain);

            let tests = stores_and_condition(env, cond_symbol, &cond_layout, test_chain);

            let number_of_tests = tests.len() as i64;

            debug_assert!(number_of_tests > 0);

            let fail = env.arena.alloc(fail_expr);
            if number_of_tests == 1 {
                // if there is just one test, compile to a simple if-then-else

                let (new_stores, lhs, rhs, _cinfo) = tests.into_iter().next().unwrap();

                compile_test_help(
                    env,
                    chain_branch_info,
                    ret_layout,
                    new_stores,
                    lhs,
                    rhs,
                    fail,
                    pass_expr,
                )
            } else {
                // otherwise, we use a join point so the code for the `else` case
                // is only generated once.
                let fail_jp_id = JoinPointId(env.unique_symbol());
                let jump = arena.alloc(Stmt::Jump(fail_jp_id, &[]));

                let test_stmt = compile_tests(env, ret_layout, tests, jump, pass_expr);

                Stmt::Join {
                    id: fail_jp_id,
                    parameters: &[],
                    body: fail,
                    remainder: arena.alloc(test_stmt),
                }
            }
        }
        FanOut {
            path,
            tests,
            fallback,
        } => {
            // the cond_layout can change in the process. E.g. if the cond is a Tag, we actually
            // switch on the tag discriminant (currently an i64 value)
            // NOTE the tag discriminant is not actually loaded, `cond` can point to a tag
            let (inner_cond_symbol, cond_stores_vec, inner_cond_layout) =
                path_to_expr_help(env, cond_symbol, &path, cond_layout);

            let default_branch = decide_to_branching(
                env,
                procs,
                layout_cache,
                cond_symbol,
                cond_layout,
                ret_layout,
                *fallback,
                jumps,
            );

            let mut branches = bumpalo::collections::Vec::with_capacity_in(tests.len(), env.arena);

            let mut tag_id_sum: i64 = (0..tests.len() as i64 + 1).sum();
            let mut union_size: i64 = -1;

            for (test, decider) in tests {
                let branch = decide_to_branching(
                    env,
                    procs,
                    layout_cache,
                    cond_symbol,
                    cond_layout,
                    ret_layout,
                    decider,
                    jumps,
                );

                let tag = match test {
                    Test::IsInt(v, _) => i128::from_ne_bytes(v) as u64,
                    Test::IsFloat(v, _) => v as u64,
                    Test::IsBit(v) => v as u64,
                    Test::IsByte { tag_id, .. } => tag_id as u64,
                    Test::IsCtor { tag_id, .. } => tag_id as u64,
                    Test::IsDecimal(_) => unreachable!("decimals cannot be switched on"),
                    Test::IsStr(_) => unreachable!("strings cannot be switched on"),
                };

                // branch info is only useful for refcounted values
                let branch_info = if let Test::IsCtor { tag_id, union, .. } = test {
                    tag_id_sum -= tag_id as i64;
                    union_size = union.alternatives.len() as i64;

                    BranchInfo::Constructor {
                        scrutinee: inner_cond_symbol,
                        layout: inner_cond_layout,
                        tag_id,
                    }
                } else {
                    tag_id_sum = -1;
                    BranchInfo::None
                };

                branches.push((tag, branch_info, branch));
            }

            // determine if the switch is exhaustive
            let default_branch_info = if tag_id_sum > 0 && union_size > 0 {
                BranchInfo::Constructor {
                    scrutinee: inner_cond_symbol,
                    layout: inner_cond_layout,
                    tag_id: tag_id_sum as u8 as _,
                }
            } else {
                BranchInfo::None
            };

            // We have learned more about the exact layout of the cond (based on the path)
            // but tests are still relative to the original cond symbol
            let mut switch = if let Layout::Union(union_layout) = inner_cond_layout {
                let tag_id_symbol = env.unique_symbol();

                let temp = Stmt::Switch {
                    cond_layout: union_layout.tag_id_layout(),
                    cond_symbol: tag_id_symbol,
                    branches: branches.into_bump_slice(),
                    default_branch: (default_branch_info, env.arena.alloc(default_branch)),
                    ret_layout,
                };

                let expr = Expr::GetTagId {
                    structure: inner_cond_symbol,
                    union_layout,
                };

                Stmt::Let(
                    tag_id_symbol,
                    expr,
                    union_layout.tag_id_layout(),
                    env.arena.alloc(temp),
                )
            } else {
                Stmt::Switch {
                    cond_layout: inner_cond_layout,
                    cond_symbol: inner_cond_symbol,
                    branches: branches.into_bump_slice(),
                    default_branch: (default_branch_info, env.arena.alloc(default_branch)),
                    ret_layout,
                }
            };

            for (symbol, layout, expr) in cond_stores_vec.into_iter().rev() {
                switch = Stmt::Let(symbol, expr, layout, env.arena.alloc(switch));
            }

            // make a jump table based on the tests
            switch
        }
    }
}

/*
fn boolean_all<'a>(arena: &'a Bump, tests: Vec<(Expr<'a>, Expr<'a>, Layout<'a>)>) -> Expr<'a> {
    let mut expr = Expr::Bool(true);

    for (lhs, rhs, layout) in tests.into_iter().rev() {
        let test = Expr::RunLowLevel(
            LowLevel::Eq,
            bumpalo::vec![in arena; (lhs, layout.clone()), (rhs, layout.clone())].into_bump_slice(),
        );

        expr = Expr::RunLowLevel(
            LowLevel::And,
            arena.alloc([
                (test, Layout::Builtin(Builtin::Int1)),
                (expr, Layout::Builtin(Builtin::Int1)),
            ]),
        );
    }

    expr
}
*/

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
                    let (guarded_test, success_tree) = edges.remove(0);

                    chain_decider(path, guarded_test, failure_tree, success_tree)
                }

                _ => {
                    let fallback = edges.remove(edges.len() - 1).1;

                    fanout_decider(path, fallback, edges)
                }
            },

            Some(last) => match edges.len() {
                0 => tree_to_decider(*last),
                1 => {
                    let failure_tree = *last;
                    let (guarded_test, success_tree) = edges.remove(0);

                    chain_decider(path, guarded_test, failure_tree, success_tree)
                }

                _ => {
                    let fallback = *last;

                    fanout_decider(path, fallback, edges)
                }
            },
        },
    }
}

fn fanout_decider<'a>(
    path: Vec<PathInstruction>,
    fallback: DecisionTree<'a>,
    edges: Vec<(GuardedTest<'a>, DecisionTree<'a>)>,
) -> Decider<'a, u64> {
    let fallback_decider = tree_to_decider(fallback);
    let necessary_tests: Vec<_> = edges
        .into_iter()
        .map(|(test, tree)| fanout_decider_help(tree, test))
        .collect();

    if necessary_tests.iter().all(|(t, _)| t.can_be_switch()) {
        Decider::FanOut {
            path,
            tests: necessary_tests,
            fallback: Box::new(fallback_decider),
        }
    } else {
        // in llvm, we cannot switch on strings so must chain
        let mut decider = fallback_decider;

        for (test, branch_decider) in necessary_tests.into_iter().rev() {
            decider = Decider::Chain {
                test_chain: vec![(path.clone(), test)],
                success: Box::new(branch_decider),
                failure: Box::new(decider),
            };
        }

        decider
    }
}

fn fanout_decider_help<'a>(
    dectree: DecisionTree<'a>,
    guarded_test: GuardedTest<'a>,
) -> (Test<'a>, Decider<'a, u64>) {
    match guarded_test {
        GuardedTest::Placeholder | GuardedTest::GuardedNoTest { .. } => {
            unreachable!("this would not end up in a switch")
        }
        GuardedTest::TestNotGuarded { test } => {
            let decider = tree_to_decider(dectree);
            (test, decider)
        }
    }
}

fn chain_decider<'a>(
    path: Vec<PathInstruction>,
    guarded_test: GuardedTest<'a>,
    failure_tree: DecisionTree<'a>,
    success_tree: DecisionTree<'a>,
) -> Decider<'a, u64> {
    match guarded_test {
        GuardedTest::GuardedNoTest { id, stmt, pattern } => {
            let failure = Box::new(tree_to_decider(failure_tree));
            let success = Box::new(tree_to_decider(success_tree));

            Decider::Guarded {
                id,
                stmt,
                pattern,
                success,
                failure: failure.clone(),
            }
        }
        GuardedTest::TestNotGuarded { test } => {
            if test_always_succeeds(&test) {
                tree_to_decider(success_tree)
            } else {
                to_chain(path, test, success_tree, failure_tree)
            }
        }

        GuardedTest::Placeholder => {
            // ?
            tree_to_decider(success_tree)
        }
    }
}

fn to_chain<'a>(
    path: Vec<PathInstruction>,
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

fn count_targets(targets: &mut bumpalo::collections::Vec<u64>, initial: &Decider<u64>) {
    use Decider::*;

    let mut stack = vec![initial];

    while let Some(decision_tree) = stack.pop() {
        match decision_tree {
            Leaf(target) => {
                targets[*target as usize] += 1;
            }

            Guarded {
                success, failure, ..
            } => {
                stack.push(success);
                stack.push(failure);
            }

            Chain {
                success, failure, ..
            } => {
                stack.push(success);
                stack.push(failure);
            }

            FanOut {
                tests, fallback, ..
            } => {
                stack.push(fallback);

                for (_, decider) in tests {
                    stack.push(decider);
                }
            }
        }
    }
}

#[allow(clippy::type_complexity)]
fn create_choices<'a>(
    target_counts: &bumpalo::collections::Vec<'a, u64>,
    target: u64,
    branch: Stmt<'a>,
) -> ((u64, Choice<'a>), Option<(u64, Stmt<'a>)>) {
    match target_counts.get(target as usize) {
        None => unreachable!(
            "this should never happen: {:?} not in {:?}",
            target, target_counts
        ),
        Some(1) => ((target, Choice::Inline(branch)), None),
        Some(_) => ((target, Choice::Jump(target)), Some((target, branch))),
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

        Guarded {
            id,
            stmt,
            pattern,
            success,
            failure,
        } => Guarded {
            id,
            stmt,
            pattern,
            success: Box::new(insert_choices(choice_dict, *success)),
            failure: Box::new(insert_choices(choice_dict, *failure)),
        },

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
