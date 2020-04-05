use crate::report::{
    global_tag_text, keyword_text, plain_text, private_tag_text, record_field_text, Report,
    ReportText,
};
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
    problem: ReportText,
    this_is: ReportText,
    instead_of: ReportText,
    further_details: ReportText,
) -> Report {
    use ReportText::*;
    let lines = vec![
        problem,
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
        title: "TYPE MISMATCH".to_string(),
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
    problem: ReportText,
    this_is: ReportText,
    further_details: ReportText,
) -> Report {
    use ReportText::*;
    let lines = vec![
        problem,
        Region(region),
        lone_type(
            found,
            expected_type,
            add_category(this_is, &category),
            further_details,
        ),
    ];

    Report {
        title: "TYPE MISMATCH".to_string(),
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
        Expected::NoExpectation(expected_type) => todo!("hit no expectation with type {:?}", expected_type),
        Expected::FromAnnotation(_name, _arity, _sub_context, _expected_type) => todo!("hit from annotation {:?} {:?}",_sub_context, _expected_type ),
        Expected::ForReason(reason, expected_type, region) => match reason {
            Reason::IfCondition => {
                let problem = Concat(vec![
                    plain_text("This "),
                    keyword_text("if"),
                    plain_text(" condition needs to be a "),
                    ReportText::Type(Content::Alias(Symbol::BOOL_BOOL, vec![], Variable::BOOL)),
                    plain_text(":"),
                ]);

                report_bad_type(
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    problem,
                    plain_text("Right now it’s"),
                    Concat(vec![
                        plain_text("But I need every "),
                        keyword_text("if"),
                        plain_text(" condition to evaluate to a "),
                        ReportText::Type(Content::Alias(Symbol::BOOL_BOOL, vec![], Variable::BOOL)),
                        plain_text("—either "),
                        global_tag_text("True"),
                        plain_text(" or "),
                        global_tag_text("False"),
                        plain_text("."),
                    ]),
                    // Note: Elm has a hint here about truthiness. I think that
                    // makes sense for Elm, since most Elm users will come from
                    // JS, where truthiness is a thing. I don't really know
                    // what the background of Roc programmers will be, and I'd
                    // rather not create a distraction by introducing a term
                    // they don't know. ("Wait, what's truthiness?")
                )
            }
            Reason::WhenGuard => {
                let problem = Concat(vec![
                    plain_text("This "),
                    keyword_text("if"),
                    plain_text(" guard condition needs to be a "),
                    ReportText::Type(Content::Alias(Symbol::BOOL_BOOL, vec![], Variable::BOOL)),
                    plain_text(":"),
                ]);
                report_bad_type(
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    problem,
                    plain_text("Right now it’s"),
                    Concat(vec![
                        plain_text("But I need every "),
                        keyword_text("if"),
                        plain_text(" guard condition to evaluate to a "),
                        ReportText::Type(Content::Alias(Symbol::BOOL_BOOL, vec![], Variable::BOOL)),
                        plain_text("—either "),
                        global_tag_text("True"),
                        plain_text(" or "),
                        global_tag_text("False"),
                        plain_text("."),
                    ]),
                )
            }
            Reason::IfBranch {
                index,
                total_branches,
            } => match total_branches {
                2 => report_mismatch(
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    Concat(vec![
                        plain_text("This "),
                        keyword_text("if"),
                        plain_text(" has an "),
                        keyword_text("else"),
                        plain_text(" branch with a different type from its "),
                        keyword_text("then"),
                        plain_text(" branch:"),
                    ]),
                    Concat(vec![
                        plain_text("The "),
                        keyword_text("else"),
                        plain_text(" branch is"),
                    ]),
                    Concat(vec![
                        plain_text("but the "),
                        keyword_text("then"),
                        plain_text(" branch has the type:"),
                    ]),
                    Concat(vec![
                        plain_text("I need all branches in an "),
                        keyword_text("if"),
                        plain_text(" to have the same type!"),
                    ]),
                ),
                _ => {
                    let ith = int_to_ordinal(index);

                    report_mismatch(
                        filename,
                        &category,
                        found,
                        expected_type,
                        region,
                        Some(expr_region),
                        plain_text(&format!(
                            "The {} branch of this `if` does not match all the previous branches:",
                            ith
                        )),
                        plain_text(&format!("The {} branch is", ith)),
                        plain_text("But all the previous branches have type:"),
                        Concat(vec![
                            plain_text("I need all branches in an "),
                            keyword_text("if"),
                            plain_text(" to have the same type!"),
                        ]),
                    )
                }
            },
            Reason::WhenBranch { index } => {
                // NOTE: is 0-based

                let ith = int_to_ordinal(index + 1);

                report_mismatch(
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    Concat(vec![
                        plain_text(&format!("The {} branch of this ", ith)),
                        keyword_text("when"),
                        plain_text(" does not match all the previous branches:"),
                    ]),
                    plain_text(&format!("The {} branch is", ith)),
                    plain_text("But all the previous branches have type:"),
                    Concat(vec![
                        plain_text("I need all branches of a "),
                        keyword_text("when"),
                        plain_text(" to have the same type!"),
                    ]),
                )
            }
            Reason::ElemInList { index } => {
                // NOTE: is 0-based

                let ith = int_to_ordinal(index + 1);

                report_mismatch(
                    filename,
                    &category,
                    found,
                    expected_type,
                    region,
                    Some(expr_region),
                    plain_text(&format!(
                        "The {} element of this list does not match all the previous elements:",
                        ith
                    )),
                    plain_text(&format!("The {} element is", ith)),
                    plain_text("But all the previous elements in the list have type:"),
                    plain_text("I need all elements of a list to have the same type!"),
                )
            }
            Reason::RecordUpdateValue(field) => report_mismatch(
                filename,
                &category,
                found,
                expected_type,
                region,
                Some(expr_region),
                Concat(vec![
                    plain_text("I cannot update the "),
                    record_field_text(field.as_str()),
                    plain_text(" field like this:"),
                ]),
                Concat(vec![
                    plain_text("You are trying to update "),
                    record_field_text(field.as_str()),
                    plain_text(" to be"),
                ]),
                plain_text("But it should be:"),
                plain_text("Record update syntax does not allow you to change the type of fields. You can achieve that with record literal syntax."),
            ),
            other => {
                //    AnonymousFnArg { arg_index: u8 },
                //    NamedFnArg(String /* function name */, u8 /* arg index */),
                //    AnonymousFnCall { arity: u8 },
                //    NamedFnCall(String /* function name */, u8 /* arity */),
                //    BinOpArg(BinOp, ArgSide),
                //    BinOpRet(BinOp),
                //    FloatLiteral,
                //    IntLiteral,
                //    NumLiteral,
                //    InterpolatedStringVar,
                //    RecordUpdateValue(Lowercase),
                //    RecordUpdateKeys(Symbol, SendMap<Lowercase, Type>),
                todo!("I don't have a message yet for reason {:?}", other)
            }
        },
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
    instead_of: ReportText,
    context_hints: ReportText,
) -> ReportText {
    let comparison = to_comparison(actual, expected);

    ReportText::Stack(vec![
        i_am_seeing,
        comparison.actual,
        instead_of,
        comparison.expected,
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
        comparison.actual,
        further_details,
        problems_to_hint(comparison.problems),
    ])
}

fn add_category(this_is: ReportText, category: &Category) -> ReportText {
    use roc_module::ident::TagName;
    use Category::*;
    use ReportText::*;

    match category {
        Lookup(name) => Concat(vec![
            plain_text("This "),
            Value(*name),
            plain_text(" value is a:"),
        ]),

        If => Concat(vec![
            plain_text("This "),
            keyword_text("if"),
            plain_text("expression produces:"),
        ]),
        When => Concat(vec![
            plain_text("This "),
            keyword_text("when"),
            plain_text("expression produces:"),
        ]),

        List => Concat(vec![this_is, plain_text(" a list of type:")]),
        Num => Concat(vec![this_is, plain_text(" a number of type:")]),
        Int => Concat(vec![this_is, plain_text(" an integer of type:")]),
        Float => Concat(vec![this_is, plain_text(" a float of type:")]),
        Str => Concat(vec![this_is, plain_text(" a string of type:")]),

        Lambda => Concat(vec![this_is, plain_text("an anonymous function of type:")]),

        TagApply(TagName::Global(name)) => Concat(vec![
            plain_text("This "),
            global_tag_text(name.as_str()),
            plain_text(" global tag application produces:"),
        ]),
        TagApply(TagName::Private(name)) => Concat(vec![
            plain_text("This "),
            private_tag_text(*name),
            plain_text(" private tag application produces:"),
        ]),

        Record => Concat(vec![this_is, plain_text(" a record of type:")]),

        Accessor(field) => Concat(vec![
            plain_text("This "),
            record_field_text(field.as_str()),
            plain_text(" value is a:"),
        ]),
        Access(field) => Concat(vec![
            plain_text("The value at "),
            record_field_text(field.as_str()),
            plain_text(" is a:"),
        ]),

        CallResult(Some(symbol)) => Concat(vec![
            plain_text("This "),
            Value(*symbol),
            plain_text(" call produces:"),
        ]),
        CallResult(None) => Concat(vec![this_is, plain_text(":")]),

        Uniqueness => Concat(vec![
            this_is,
            plain_text(" an uniqueness attribute of type:"),
        ]),
        Storage => Concat(vec![this_is, plain_text(" a value of type:")]),
    }
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
            type_in_focus(overall_type),
            /* TODO hint */
        ]),
    ];

    Report {
        title: "TYPE MISMATCH".to_string(),
        filename,
        text: Concat(lines),
    }
}
