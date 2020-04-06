use crate::report::{
    error_type_block, error_type_inline, global_tag_text, keyword_text, plain_text,
    private_tag_text, record_field_text, tag_name_text, Report, ReportText,
};
use roc_can::expected::{Expected, PExpected};
use roc_module::ident::Lowercase;
use roc_module::symbol::Symbol;
use roc_solve::solve;
use roc_types::pretty_print::Parens;
use roc_types::subs::{Content, Variable};
use roc_types::types::{Category, ErrorType, PatternCategory, Reason, TypeExt};
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
            error_type_block(to_doc(Parens::Unnecessary, &overall_type)),
            /* TODO hint */
        ]),
    ];

    Report {
        title: "TYPE MISMATCH".to_string(),
        filename,
        text: Concat(lines),
    }
}

#[derive(Clone)]
pub enum Problem {
    IntFloat,
    ArityMismatch(usize, usize),
    FieldTypo(Vec<Lowercase>),
}

fn problems_to_hint(_problems: Vec<Problem>) -> ReportText {
    // TODO
    ReportText::Concat(vec![])
}

pub struct Comparison {
    actual: ReportText,
    expected: ReportText,
    problems: Vec<Problem>,
}

fn to_comparison(actual: ErrorType, expected: ErrorType) -> Comparison {
    let diff = to_diff(Parens::Unnecessary, &actual, &expected);

    Comparison {
        actual: error_type_block(diff.left),
        expected: error_type_block(diff.right),
        problems: match diff.status {
            Status::Similar => vec![],
            Status::Different(problems) => problems,
        },
    }
}

pub enum Status {
    Similar,                 // the structure is the same or e.g. record fields are different
    Different(Vec<Problem>), // e.g. found Bool, expected Int
}

impl Status {
    pub fn merge(&mut self, other: Self) {
        use Status::*;
        match self {
            Similar => {
                *self = other;
            }
            Different(problems1) => match other {
                Similar => { /* nothing */ }
                Different(problems2) => {
                    // TODO pick a data structure that makes this merge cheaper
                    let mut problems = Vec::with_capacity(problems1.len() + problems2.len());
                    problems.extend(problems1.iter().cloned());
                    problems.extend(problems2);
                    *self = Different(problems);
                }
            },
        }
    }
}

pub struct Diff<T> {
    left: T,
    right: T,
    status: Status,
}

pub fn to_doc(parens: Parens, tipe: &ErrorType) -> ReportText {
    use ErrorType::*;

    match tipe {
        Function(args, ret) => report_text::function(
            parens,
            args.iter().map(|arg| to_doc(Parens::InFn, arg)).collect(),
            to_doc(Parens::InFn, ret),
        ),

        Infinite => plain_text("∞"),
        Error => plain_text("?"),

        FlexVar(lowercase) => plain_text(lowercase.as_str()),
        RigidVar(lowercase) => plain_text(lowercase.as_str()),

        Type(symbol, args) => report_text::apply(
            parens,
            ReportText::Value(*symbol),
            args.iter()
                .map(|arg| to_doc(Parens::InTypeParam, arg))
                .collect(),
        ),

        Alias(symbol, args, _) => report_text::apply(
            parens,
            ReportText::Value(*symbol),
            args.iter()
                .map(|(_, arg)| to_doc(Parens::InTypeParam, arg))
                .collect(),
        ),

        Record(fields_map, ext) => {
            let mut fields = fields_map.into_iter().collect::<Vec<_>>();
            fields.sort_by(|(a, _), (b, _)| a.cmp(&b));

            report_text::record(
                fields
                    .into_iter()
                    .map(|(k, v)| (plain_text(k.as_str()), to_doc(Parens::Unnecessary, v)))
                    .collect(),
                ext_to_doc(ext),
            )
        }

        TagUnion(tags_map, ext) => {
            let mut tags = tags_map
                .into_iter()
                .map(|(name, args)| {
                    (
                        name,
                        args.iter()
                            .map(|arg| to_doc(Parens::InTypeParam, arg))
                            .collect::<Vec<_>>(),
                    )
                })
                .collect::<Vec<_>>();
            tags.sort_by(|(a, _), (b, _)| a.cmp(&b));

            report_text::tag_union(
                tags.into_iter()
                    .map(|(k, v)| (tag_name_text(k.clone()), v))
                    .collect(),
                ext_to_doc(ext),
            )
        }

        RecursiveTagUnion(rec_var, tags_map, ext) => {
            let mut tags = tags_map
                .into_iter()
                .map(|(name, args)| {
                    (
                        name,
                        args.iter()
                            .map(|arg| to_doc(Parens::InTypeParam, arg))
                            .collect::<Vec<_>>(),
                    )
                })
                .collect::<Vec<_>>();
            tags.sort_by(|(a, _), (b, _)| a.cmp(&b));

            report_text::recursive_tag_union(
                to_doc(Parens::Unnecessary, rec_var),
                tags.into_iter()
                    .map(|(k, v)| (tag_name_text(k.clone()), v))
                    .collect(),
                ext_to_doc(ext),
            )
        }

        Boolean(b) => plain_text(&format!("{:?}", b)),
    }
}

fn ext_to_doc(ext: &TypeExt) -> Option<ReportText> {
    use TypeExt::*;

    match ext {
        Closed => None,
        FlexOpen(lowercase) | RigidOpen(lowercase) => Some(plain_text(lowercase.as_str())),
    }
}

fn same(parens: Parens, tipe: &ErrorType) -> Diff<ReportText> {
    let doc = to_doc(parens, tipe);

    Diff {
        left: doc.clone(),
        right: doc,
        status: Status::Similar,
    }
}

fn to_diff(parens: Parens, type1: &ErrorType, type2: &ErrorType) -> Diff<ReportText> {
    use ErrorType::*;

    match (type1, type2) {
        (Error, Error) | (Infinite, Infinite) => same(parens, type1),

        (FlexVar(x), FlexVar(y)) if x == y => same(parens, type1),
        (RigidVar(x), RigidVar(y)) if x == y => same(parens, type1),

        (Function(args1, ret1), Function(args2, ret2)) => {
            if args1.len() == args2.len() {
                let mut status = Status::Similar;
                let arg_diff = traverse(Parens::InFn, args1, args2);
                let ret_diff = to_diff(Parens::InFn, ret1, ret2);
                status.merge(arg_diff.status);
                status.merge(ret_diff.status);

                let left = report_text::function(parens, arg_diff.left, ret_diff.left);
                let right = report_text::function(parens, arg_diff.right, ret_diff.right);

                Diff {
                    left,
                    right,
                    status,
                }
            } else {
                let left = to_doc(Parens::InFn, type1);
                let right = to_doc(Parens::InFn, type2);

                Diff {
                    left,
                    right,
                    status: Status::Different(vec![Problem::ArityMismatch(
                        args1.len(),
                        args2.len(),
                    )]),
                }
            }
        }
        (Type(symbol1, args1), Type(symbol2, args2)) if symbol1 == symbol2 => {
            let args_diff = traverse(Parens::InTypeParam, args1, args2);
            let left = report_text::apply(parens, ReportText::Value(*symbol1), args_diff.left);
            let right = report_text::apply(parens, ReportText::Value(*symbol2), args_diff.right);

            Diff {
                left,
                right,
                status: args_diff.status,
            }
        }

        (Alias(symbol1, args1, _), Alias(symbol2, args2, _)) if symbol1 == symbol2 => {
            // TODO remove collects
            let a1 = args1.iter().map(|(_, v)| v).collect::<Vec<_>>();
            let a2 = args2.iter().map(|(_, v)| v).collect::<Vec<_>>();
            let args_diff = traverse(Parens::InTypeParam, a1, a2);
            let left = report_text::apply(parens, ReportText::Value(*symbol1), args_diff.left);
            let right = report_text::apply(parens, ReportText::Value(*symbol2), args_diff.right);

            Diff {
                left,
                right,
                status: args_diff.status,
            }
        }

        _ => {
            // TODO actually diff
            // (Record(fields1, ext1), Record(fields2, ext2)) => {

            let left = to_doc(Parens::Unnecessary, type1);
            let right = to_doc(Parens::Unnecessary, type2);

            Diff {
                left,
                right,
                status: Status::Similar,
            }
        }
    }
}

fn traverse<'a, I>(parens: Parens, args1: I, args2: I) -> Diff<Vec<ReportText>>
where
    I: IntoIterator<Item = &'a ErrorType>,
{
    let mut status = Status::Similar;

    // TODO use ExactSizeIterator to pre-allocate here
    let mut left = Vec::new();
    let mut right = Vec::new();

    for (arg1, arg2) in args1.into_iter().zip(args2.into_iter()) {
        let diff = to_diff(parens, arg1, arg2);

        left.push(diff.left);
        right.push(diff.right);
        status.merge(diff.status);
    }

    Diff {
        left,
        right,
        status,
    }
}

mod report_text {

    use super::ReportText;
    use crate::report::{concat, intersperse, plain_text, separate};
    use roc_types::pretty_print::Parens;

    fn with_parens(text: ReportText) -> ReportText {
        ReportText::Concat(vec![plain_text("("), text, plain_text(")")])
    }

    pub fn function(parens: Parens, args: Vec<ReportText>, ret: ReportText) -> ReportText {
        let function_text = concat(vec![
            intersperse(plain_text(", "), args),
            plain_text(" -> "),
            ret,
        ]);

        match parens {
            Parens::Unnecessary => function_text,
            _ => with_parens(function_text),
        }
    }

    pub fn apply(parens: Parens, name: ReportText, args: Vec<ReportText>) -> ReportText {
        if args.is_empty() {
            name
        } else {
            let apply_text = concat(vec![
                name,
                plain_text(" "),
                intersperse(plain_text(" "), args),
            ]);

            match parens {
                Parens::Unnecessary | Parens::InFn => apply_text,
                Parens::InTypeParam => with_parens(apply_text),
            }
        }
    }

    pub fn record(
        entries: Vec<(ReportText, ReportText)>,
        opt_ext: Option<ReportText>,
    ) -> ReportText {
        let ext_text = if let Some(t) = opt_ext {
            t
        } else {
            plain_text("")
        };

        if entries.is_empty() {
            concat(vec![plain_text("{}"), ext_text])
        } else {
            let entry_to_text = |(field_name, field_type)| {
                separate(vec![concat(vec![field_name, plain_text(" :")]), field_type])
            };

            let starts = std::iter::once(plain_text("{")).chain(std::iter::repeat(plain_text(",")));

            let mut lines: Vec<_> = entries
                .into_iter()
                .zip(starts)
                .map(|(entry, start)| concat(vec![start, entry_to_text(entry)]))
                .collect();

            lines.push(plain_text("}"));
            lines.push(ext_text);

            concat(lines)
        }
    }

    pub fn tag_union(
        entries: Vec<(ReportText, Vec<ReportText>)>,
        opt_ext: Option<ReportText>,
    ) -> ReportText {
        let ext_text = if let Some(t) = opt_ext {
            t
        } else {
            plain_text("")
        };

        if entries.is_empty() {
            concat(vec![plain_text("[]"), ext_text])
        } else {
            let entry_to_text = |(tag_name, arguments)| concat(vec![tag_name, separate(arguments)]);

            let starts = std::iter::once(plain_text("[")).chain(std::iter::repeat(plain_text(",")));

            let mut lines: Vec<_> = entries
                .into_iter()
                .zip(starts)
                .map(|(entry, start)| concat(vec![start, entry_to_text(entry)]))
                .collect();

            lines.push(plain_text("]"));
            lines.push(ext_text);

            concat(lines)
        }
    }

    pub fn recursive_tag_union(
        rec_var: ReportText,
        entries: Vec<(ReportText, Vec<ReportText>)>,
        opt_ext: Option<ReportText>,
    ) -> ReportText {
        let ext_text = if let Some(t) = opt_ext {
            t
        } else {
            plain_text("")
        };

        if entries.is_empty() {
            concat(vec![plain_text("[]"), ext_text])
        } else {
            let entry_to_text = |(tag_name, arguments)| concat(vec![tag_name, separate(arguments)]);

            let starts = std::iter::once(plain_text("[")).chain(std::iter::repeat(plain_text(",")));

            let mut lines: Vec<_> = entries
                .into_iter()
                .zip(starts)
                .map(|(entry, start)| concat(vec![start, entry_to_text(entry)]))
                .collect();

            lines.push(plain_text("]"));
            lines.push(ext_text);

            lines.push(plain_text(" as "));
            lines.push(rec_var);

            concat(lines)
        }
    }
}
