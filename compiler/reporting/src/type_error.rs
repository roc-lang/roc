use crate::report::{plain_text, with_indent, Report, ReportText};
use roc_can::expected::{Expected, PExpected};
use roc_module::symbol::Symbol;
use roc_solve::solve;
use roc_types::subs::{Content, Variable};
use roc_types::types::{Category, ErrorType, PatternCategory, Reason};
use std::path::PathBuf;

pub fn type_problem(filename: PathBuf, problem: solve::TypeError) -> Report {
    use solve::TypeError::*;

    match problem {
        BadExpr(region, category, found, expected) => {
            to_expr_report(filename, region, category, found, expected)
        }
        BadPattern(region, category, found, expected) => {
            to_pattern_report(filename, region, category, found, expected)
        }
        CircularType(region, symbol, overall_type) => {
            to_circular_report(filename, region, symbol, overall_type)
        }
    }
}

fn type_in_focus(typ: ErrorType) -> ReportText {
    ReportText::ErrorType(typ)
}

fn int_to_ordinal(number: usize) -> String {
    // NOTE: one-based
    let remainder10 = number % 10;
    let remainder100 = number % 100;

    let ending = match remainder100 {
        11..=13 => "th",
        _ => match remainder10 {
            1 => "st",
            2 => "nd",
            3 => "rd",
            _ => "th",
        },
    };

    format!("{}{}", number, ending)
}

#[allow(clippy::too_many_arguments)]
fn report_mismatch(
    filename: PathBuf,
    category: &Category,
    found: ErrorType,
    expected_type: ErrorType,
    region: roc_region::all::Region,
    _opt_highlight: Option<roc_region::all::Region>,
    problem: &str,
    this_is: &str,
    instead_of: &str,
    further_details: ReportText,
) -> Report {
    use ReportText::*;
    let lines = vec![
        plain_text(problem),
        Region(region),
        type_comparison(
            found,
            expected_type,
            add_category(this_is, category),
            instead_of,
            further_details,
        ),
    ];

    Report {
        filename,
        text: Concat(lines),
    }
}

#[allow(clippy::too_many_arguments)]
fn report_bad_type(
    filename: PathBuf,
    category: &Category,
    found: ErrorType,
    expected_type: ErrorType,
    region: roc_region::all::Region,
    _opt_highlight: Option<roc_region::all::Region>,
    problem: &str,
    this_is: &str,
    further_details: ReportText,
) -> Report {
    use ReportText::*;
    let lines = vec![
        plain_text(problem),
        Region(region),
        lone_type(
            found,
            expected_type,
            add_category(this_is, &category),
            further_details,
        ),
    ];

    Report {
        filename,
        text: Concat(lines),
    }
}

fn to_expr_report(
    filename: PathBuf,
    expr_region: roc_region::all::Region,
    category: Category,
    found: ErrorType,
    expected: Expected<ErrorType>,
) -> Report {
    use ReportText::*;

    match expected {
        Expected::NoExpectation(_expected_type) => todo!(),
        Expected::FromAnnotation(_name, _arity, _sub_context, _expected_type) => todo!(),
        Expected::ForReason(reason, expected_type, region) => {
            match reason {
                Reason::IfCondition => report_bad_type(
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    "This `if` condition does not evaluate to a boolean value, True or False.",
                    "It is",
                    Concat(vec![
                        plain_text("But I need this `if` condition to be a "),
                        ReportText::Type(Content::Alias(Symbol::BOOL_BOOL, vec![], Variable::BOOL)),
                        plain_text(" value."),
                    ]),
                ),
                Reason::IfBranch { index } => {
                    let ith = int_to_ordinal(index);
                    report_mismatch(
                        filename,
                        &category,
                        found,
                        expected_type,
                        region,
                        Some(expr_region),
                        &format!(
                            "The {} branch of this `if` does not match all the previous branches:",
                            ith
                        ),
                        &format!("The {} branch is", ith),
                        "But all the previous branches result in",
                        Concat(vec![ /* TODO add hint */ ]),
                    )
                }
                _ => todo!(),
            }
        }
    }
}

pub enum Problem {}
pub struct Comparison {
    actual: ReportText,
    expected: ReportText,
    problems: Vec<Problem>,
}

fn problems_to_hint(_problems: Vec<Problem>) -> ReportText {
    // TODO
    ReportText::Concat(vec![])
}

fn to_comparison(actual: ErrorType, expected: ErrorType) -> Comparison {
    // TODO make this do actual comparison

    Comparison {
        actual: type_in_focus(actual),
        expected: type_in_focus(expected),
        problems: vec![],
    }
}

fn type_comparison(
    actual: ErrorType,
    expected: ErrorType,
    i_am_seeing: ReportText,
    instead_of: &str,
    context_hints: ReportText,
) -> ReportText {
    let comparison = to_comparison(actual, expected);

    ReportText::Stack(vec![
        i_am_seeing,
        with_indent(4, comparison.actual),
        plain_text(instead_of),
        with_indent(4, comparison.expected),
        context_hints,
        problems_to_hint(comparison.problems),
    ])
}

fn lone_type(
    actual: ErrorType,
    expected: ErrorType,
    i_am_seeing: ReportText,
    further_details: ReportText,
) -> ReportText {
    let comparison = to_comparison(actual, expected);

    ReportText::Stack(vec![
        i_am_seeing,
        with_indent(4, comparison.actual),
        further_details,
        problems_to_hint(comparison.problems),
    ])
}

fn add_category(this_is: &str, category: &Category) -> ReportText {
    use Category::*;

    let result = match category {
        Str => format!("{} a string of type:", this_is),
        _ => todo!(),
    };

    plain_text(&*result)
}

fn to_pattern_report(
    _filename: PathBuf,
    _expr_region: roc_region::all::Region,
    _category: PatternCategory,
    _found: ErrorType,
    _expected: PExpected<ErrorType>,
) -> Report {
    todo!()
}

fn to_circular_report(
    filename: PathBuf,
    region: roc_region::all::Region,
    symbol: Symbol,
    overall_type: ErrorType,
) -> Report {
    use ReportText::*;

    let lines = vec![
        plain_text("I'm inferring a weird self-referential type for "),
        Value(symbol),
        plain_text(":"),
        Region(region),
        Stack(vec![
            plain_text("Here is my best effort at writing down the type. You will see ∞ for parts of the type that repeat something already printed out infinitely."),
            with_indent(4, type_in_focus(overall_type)),
            /* TODO hint */
        ]),
    ];

    Report {
        filename,
        text: Concat(lines),
    }
}
