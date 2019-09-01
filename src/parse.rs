use operator::Operator;
use expr::{Expr, Pattern, Ident, VariantName};
use region::{Located, Region};

use std::char;
use parse_state::{IndentablePosition};

use combine::parser::char::{char, string, spaces, digit, hex_digit, HexDigit, alpha_num};
use combine::parser::repeat::{many, count_min_max, sep_by, sep_by1, skip_many, skip_many1, skip_until};
use combine::parser::item::{any, satisfy_map, value, position, satisfy};
use combine::parser::combinator::{look_ahead, not_followed_by};
use combine::{attempt, choice, eof, many1, parser, Parser, optional, between, unexpected_any, unexpected};
use combine::error::{Consumed, ParseError};
use combine::stream::{Stream, Positioned};
use combine::stream::state::{State};

pub const ERR_EMPTY_CHAR: &'static str = "EMPTY_CHAR";

pub fn parse_string(string: &str) -> Result<Located<Expr>, combine::easy::Errors<char, &str, IndentablePosition>> {
    let parse_state = State::with_positioner(string, IndentablePosition::default());

    located(expr()).skip(eof()).easy_parse(parse_state).map(|( expr, _ )| expr)
}

pub fn expr<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    spaces().with(expr_body(0)).skip(whitespace_or_eof())
}

fn located<I, O, P>(parser: P) -> impl Parser<Input = I, Output = Located<O>>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
    I: Positioned,
    P: Parser<Input = I, Output = O>
{
    attempt(position().and(parser))
        .and(position())
        .map(|( (start, val), end )| {
            Located::new(val, Region {
                start_line: start.line,
                start_col: start.column,

                end_line: end.line,
                end_col: end.column
            })
        })
}

fn indentation<I>() -> impl Parser<Input = I, Output = u32>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> ,
    I: Positioned
{
    position().map(|pos: IndentablePosition| pos.indent_col)
}

fn whitespace_or_eof<I>() -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    choice((
        whitespace1(),
        eof().with(value(()))
    ))
}

fn skipped_whitespace_char<I>() -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    choice((
        char(' ').with(value(())),
        char('\n').with(value(())),
        block_comment(),
        inline_comment()
    ))
}

fn whitespace<I>() -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    skip_many(skipped_whitespace_char())
}

fn whitespace1<I>() -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    skip_many1(skipped_whitespace_char())
}

fn block_comment<I>() -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    // This uses skip_until to make sure we don't bother saving anything
    // until we hit the closing ###, and then uses skip(string("###"))
    // to actually consume the closing ###.
    attempt(
        // 4+ consecutive '#' characters is *not* considered a
        // block comment. It's for "drawing horizontal lines" like so:
        // ###########################################################
        string("###")
            .skip(satisfy(|c| c != '#'))
    )
        .with(skip_until(attempt(string("###"))))
        .skip(string("###"))
}

fn inline_comment<I>() -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    char('#')
        .skip(skip_many(satisfy(|c| c != '\n')))
        .with(value(()))
}

fn indented_whitespaces<I>(min_indent: u32) -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    skip_many(skipped_indented_whitespace_char(min_indent))
}

fn indented_whitespaces1<I>(min_indent: u32) -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
    skip_many1(skipped_indented_whitespace_char(min_indent))
}

fn skipped_indented_whitespace_char<I>(min_indent: u32) -> impl Parser<Input = I, Output = ()>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position> {
        choice((
            char(' ').with(value(())),
            block_comment(),
            inline_comment(),
            // If we hit a newline, it must be followed by:
            //
            // - Any number of blank lines (which may contain only spaces)
            // - At least min_indent spaces, or else eof()
            char('\n')
                .skip(
                    skip_many(
                        char('\n')
                            .skip(
                                optional(
                                    attempt(
                                        skip_many(char(' ')).skip(look_ahead(char('\n')))
                                    )
                                )
                            )
                    )
                )
                .skip(
                    choice((
                        many::<Vec<_>, _>(char(' ')).then(move |chars| {
                            if chars.len() < min_indent as usize {
                                unexpected("outdent").left()
                            } else {
                                value(()).right()
                            }
                        }),
                        eof().with(value(()))
                    ))
                )
                .with(value(()))
        ))
}

/// This is separate from expr_body for the sake of function application,
/// so it can stop parsing when it reaches an operator (since they have
/// higher precedence.)
fn function_arg_expr<I>(min_indent: u32) -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    function_arg_expr_(min_indent)
}

parser! {
    #[inline(always)]
    fn function_arg_expr_[I](min_indent_ref: u32)(I) -> Expr
        where [ I: Stream<Item = char, Position = IndentablePosition> ]
    {
        // TODO figure out why min_indent_ref has the type &mut u32
        let min_indent = *min_indent_ref;

        // Rules for expressions that can go in function arguments:
        //
        // 1. Don't parse operators, because they have a higher
        //    precedence than function application.
        // 2. Don't parse assignments unless they're wrapped in parens.
        // 3. Don't parse variants; those will be handled separately by
        //    the function arg parser (it only accepts non-applied variants)
        // 4. Parse variables but not functions.
        choice((
            closure(min_indent),
            apply_with_parens(min_indent),
            list(min_indent),
            string("{}").with(value(Expr::EmptyRecord)),
            string_literal(),
            int_or_frac_literal(),
            negative_int_or_frac_literal(),
            char_literal(),
            if_expr(min_indent),
            case_expr(min_indent),
        ))
    }
}

fn expr_body<I>(min_indent: u32) -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    expr_body_(min_indent)
}

// This macro allows recursive parsers
parser! {
    #[inline(always)]
    fn expr_body_[I](min_indent_ref: u32)(I) -> Expr
        where [ I: Stream<Item = char, Position = IndentablePosition> ]
    {
        // TODO figure out why min_indent_ref has the type &mut u32
        let min_indent = *min_indent_ref;

        located(choice((
            function_arg_expr(min_indent),
            assignment(min_indent),
            apply_variant(min_indent),
            func_or_var(min_indent),
        )))
        .and(
            // Optionally follow the expression with an operator,
            //
            // e.g. In the expression (1 + 2), the subexpression 1 is
            // followed by the operator + and another subexpression, 2
            optional(
                attempt(
                    indented_whitespaces(min_indent)
                        .with(located(operator()))
                        .skip(indented_whitespaces(min_indent))
                        .and(located(expr_body(min_indent)))
                )
            )
        ).map(|(expr1, opt_op)| {
            match opt_op {
                None => expr1.value,
                Some((op, expr2)) => {
                    Expr::Operator(Box::new(expr1), op, Box::new(expr2))
                },
            }
        })
    }
}

pub fn if_expr<I>(min_indent: u32) -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    attempt(string("if").skip(indented_whitespaces1(min_indent)))
        .with(located(expr_body(min_indent))).skip(indented_whitespaces1(min_indent))
        .skip(string("then")).skip(indented_whitespaces1(min_indent))
        .and(located(expr_body(min_indent))).skip(indented_whitespaces1(min_indent))
        .skip(string("else")).skip(indented_whitespaces1(min_indent))
        .and(located(expr_body(min_indent)))
        .map(|((conditional, then_branch), else_branch)|
            Expr::If(
                Box::new(conditional),
                Box::new(then_branch),
                Box::new(else_branch)
            )
        )
}

pub fn case_expr<I>(min_indent: u32) -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    attempt(string("case").skip(indented_whitespaces1(min_indent)))
        .with(located(expr_body(min_indent)))
        .and(
            many::<Vec<_>, _>(
                attempt(
                    skip_many(indented_whitespaces1(min_indent))
                        .with(string("when").skip(indented_whitespaces1(min_indent)))
                )
                .with(located(pattern(min_indent))).skip(indented_whitespaces1(min_indent))
                .skip(string("then")).skip(indented_whitespaces1(min_indent))
                .and(located(expr_body(min_indent)))
            )
        )
        .map(|(conditional, branches)|
            if branches.is_empty() {
                // TODO handle this more gracefully
                panic!("encountered case-expression with no branches!")
            } else {
                Expr::Case(Box::new(conditional), branches)
            }
        )
}

pub fn list<I>(min_indent: u32) -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    between(char('['), char(']'),
        sep_by(
            indented_whitespaces(min_indent)
                .with(located(expr_body(min_indent)))
                .skip(indented_whitespaces(min_indent)),
            char(',')
        )
    ).map(|loc_elems: Vec<Located<Expr>>| {
        if loc_elems.is_empty() {
            Expr::EmptyList
        } else {
            Expr::List(loc_elems)
        }
    })
}


pub fn apply_with_parens<I>(min_indent: u32) -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    between(char('('), char(')'),
        indented_whitespaces(min_indent)
            .with(located(expr_body(min_indent)))
            .skip(indented_whitespaces(min_indent))
    ).and(
        // Parenthetical expressions can optionally be followed by
        // whitespace and one or more whitespace-separated expressions,
        // meaning this is function application!
        optional(
            attempt(apply_args(min_indent))
        )
    ).map(|(located_expr, opt_args): (Located<Expr>, Option<Vec<Located<Expr>>>)|
        match opt_args {
            // If there was nothing after the parens, that's okay; this is still parens, but not application.
            None => located_expr.value,
            Some(args) => Expr::Apply(Box::new(located_expr), args)
        }
    )
}

#[inline(always)]
fn function_arg<I>(min_indent: u32) -> impl Parser<Input = I, Output = Located<Expr>>
    where I: Stream<Item = char, Position = IndentablePosition>,
        I::Error: ParseError<I::Item, I::Range, I::Position>
{
    located(
        choice((
            // Don't use apply_with_parens here, because it will think anything following
            // this parenthetical expr is an argument *to be passed to the parenthetical expr*.
            between(char('('), char(')'),
                indented_whitespaces(min_indent)
                    .with(expr_body(min_indent))
                    .skip(indented_whitespaces(min_indent))),

            // Don't parse operators, because they have a higher
            // precedence than function application. If we see one,
            // we're done!
            function_arg_expr(min_indent),

            // Variants can't be applied in function args without parens;
            // (foo Bar baz) will pass 2 arguments to foo, rather than parsing like (foo (Bar baz))
            attempt(variant_name()).map(|name| Expr::ApplyVariant(VariantName::Unqualified(name), None)),

            // Functions can't be called by name in function args without parens;
            // (foo bar baz) will pass 2 arguments to foo, rather than parsing like (foo (bar baz))
            attempt(ident()).map(|name| Expr::Var(Ident::Unqualified(name))),
        ))
    )
}

pub fn apply_args<I>(min_indent: u32) -> impl Parser<Input = I, Output = Vec<Located<Expr>>>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    // Function application always begins with whitespace.
    attempt(
        indented_whitespaces1(min_indent)
            .skip(
                // If there's a reserved keyword next, this isn't function application after all!
                not_followed_by(choice((string("then"), string("else"), string("when"))))
            )
        .with(
            // Arguments are whitespace-separated.
            sep_by1(
                function_arg(min_indent),
                // Only consume these spaces if there's another argument after them.
                // Otherwise we consume too much and mess up indentation checking!
                attempt(
                    indented_whitespaces1(min_indent)
                    .skip(
                        // Any of these indicates we've hit the end of the argument list.
                        not_followed_by(
                            choice((
                                string(","),
                                string(")"),
                                string("]"),
                                string("}"),
                                operator().with(value("")),
                                string("then"),
                                string("else"),
                                string("when"),
                                eof().with(value(""))
                            ))
                        )
                    )
                )
            )
        )
    )
}

pub fn operator<I>() -> impl Parser<Input = I, Output = Operator>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    choice((
        string("==").map(|_| Operator::Equals),
        string("&&").map(|_| Operator::And),
        // either < or <=
        char('<').with(
            optional(char('='))
                .map(|opt_eq| {
                    if opt_eq.is_none() {
                        Operator::LessThan
                    } else {
                        Operator::LessThanOrEq
                    }
                })
        ),
        // either > or >=
        char('>').with(
            optional(char('='))
                .map(|opt_eq| {
                    if opt_eq.is_none() {
                        Operator::GreaterThan
                    } else {
                        Operator::GreaterThanOrEq
                    }
                })
        ),
        // either || or |>
        char('|').with(
            char('>').map(|_| Operator:: Pizza)
                .or(char('|').map(|_| Operator::Or))
        ),
        // either / or //
        char('/').with(
            optional(char('/'))
                .map(|opt_slash| {
                    if opt_slash.is_none() {
                        Operator::Slash
                    } else {
                        Operator::DoubleSlash
                    }
                })
        ),
        string("~/").map(|_| Operator::TildeSlash),
        char('+').map(|_| Operator::Plus),
        char('-').map(|_| Operator::Minus),
        char('*').map(|_| Operator::Star),
        char('/').map(|_| Operator::Slash),
        char('^').map(|_| Operator::Caret),
        char('%').map(|_| Operator::Percent),
        char('<').map(|_| Operator::LessThan),
        char('>').map(|_| Operator::GreaterThan),
    ))
}

pub fn nested_assignment<I>(min_indent: u32) -> impl Parser<Input = I, Output = (Located<Pattern>, Located<Expr>)>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    attempt(
        located(pattern(min_indent)).and(indentation())
            .skip(whitespace())
            .and(
                char('=').with(indentation())
                    // If the "=" after the identifier turns out to be
                    // either "==" or "=>" then this is not a declaration!
                    .skip(not_followed_by(choice((char('='), char('>')))))
            )
        .skip(whitespace())
        .then(move |((var_pattern, original_indent), equals_sign_indent)| {
            if original_indent < min_indent {
                unexpected_any("this assignment is outdented too far").left()
            } else if equals_sign_indent < original_indent /* `<` because '=' should be same indent or greater */ {
                unexpected_any("the = in this assignment seems outdented").left()
            } else {
                located(expr_body(original_indent + 1 /* declaration body must be indented relative to original decl */))
                    .skip(whitespace1())
                    .and(indentation())
                .then(move |(var_expr, in_expr_indent)| {
                    if in_expr_indent != original_indent {
                        unexpected_any("the return expression was indented differently from the original assignment").left()
                    } else {
                        value((var_pattern.to_owned(), var_expr)).right()
                    }
                }).right()
            }
        })
    )
}

/// A type, e.g. `String`
fn typ<I>(min_indent: u32) -> impl Parser<Input = I, Output = AnnType>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    typ_(min_indent)
}

// This macro allows recursive parsers
parser! {
    #[inline(always)]
    fn typ_[I](min_indent_ref: u32)(I) -> AnnType
        where [ I: Stream<Item = char, Position = IndentablePosition> ]
    {
        let min_indent = *min_indent_ref;

        sep_by1(
            choice((
                between(char('('), char(')'), typ(min_indent)),
                string("{}").map(|_| AnnType::EmptyRec),
                ident().map(|var| AnnType::Variable(var)),
                variant_name()
                    .and(many::<Vec<_>, _>(typ(min_indent))).map(|(name, args)| {
                        AnnType::ApplyUnqualified(name, args)
                    }),
            ))
            .skip(indented_whitespaces(min_indent)),
            char(',').skip(indented_whitespaces(min_indent))
        )
        .and(optional(
            string("->")
                .with(indented_whitespaces(min_indent).with(typ(min_indent)))
        ))
        .then(|(args, opt_ret): (Vec<AnnType>, Option<AnnType>)| {
            match opt_ret {
                None => {
                    // This is not a function!
                    if args.len() == 1 {
                        value(args.into_iter().next().unwrap()).right()
                    } else {
                        // If it didn't have a `->`, why did it have a comma?
                        unexpected_any("comma in non-function type").left()
                    }
                },
                Some(ret) => {
                    value(AnnType::Function(args, Box::new(ret))).right()
                }
            }
        })
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum AnnType {
    EmptyRec,
    Function(Vec<AnnType>, Box<AnnType>),
    /// Applying a type to some arguments (e.g. Map.Map String Int)
    ApplyQualified(String, String, Vec<AnnType>),
    ApplyUnqualified(String, Vec<AnnType>),
    Variable(String),
}

pub fn assignment<I>(min_indent: u32) -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    attempt(
        // TODO Allow standalone annotations with no attached imlementation.
        //      Do this by making both optional, and erroring if both are None.
        //      Might also need to make the newline optional, and use it to
        //      determine if they are attached or not.
        optional(
            located(ident()).and(indentation())
                .skip(whitespace())
                .and(char(':').with(indentation()))
                .skip(whitespace())
                .with(typ(min_indent))
                // There must be exactly 1 newline separating the annotation
                // and what it's annotating. No more, no fewer!
                .skip(char('\n'))
        )
        .and(
            located(pattern(min_indent)).and(indentation())
                .skip(whitespace())
                .and(
                    char('=').with(indentation())
                        // If the "=" after the identifier turns out to be
                        // either "==" or "=>" then this is not a declaration!
                        .skip(not_followed_by(choice((char('='), char('>')))))
                )
        )
    )
    .skip(whitespace())
    .then(move |(opt_annotation, ((first_assignment_pattern, original_indent), equals_sign_indent))| {
        if original_indent < min_indent {
            unexpected_any("this assignment is outdented too far").left()
        } else if equals_sign_indent < original_indent /* `<` because '=' should be same indent or greater */ {
            unexpected_any("the = in this assignment seems outdented").left()
        } else {
            located(expr_body(original_indent + 1 /* declaration body must be indented relative to original decl */))
                .skip(whitespace1())
                // Parse any additional assignments that appear right after this one
                .and(many::<Vec<_>, _>(nested_assignment(original_indent)))
                .and(located(expr_body(original_indent)).and(indentation()))
            .then(move |((first_assignment_expr, mut assignments), (in_expr, in_expr_indent))| {
                if in_expr_indent != original_indent {
                    unexpected_any("the return expression was indented differently from the original assignment").left()
                } else {
                    assignments.insert(0, (first_assignment_pattern.clone(), first_assignment_expr));

                    value(Expr::Assign(assignments, Box::new(in_expr))).right()
                }
            }).right()
        }
    })
}

pub fn func_or_var<I>(min_indent: u32) -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    located(ident()).and(optional(apply_args(min_indent)))
        .map(|(loc_name, opt_args): (Located<String>, Option<Vec<Located<Expr>>>)| {
            // Use optional(sep_by1()) over sep_by() to avoid
            // allocating a Vec in the common case where this is a var
            match opt_args {
                None => Expr::Var(Ident::Unqualified(loc_name.value)),
                Some(args) => Expr::Apply(Box::new(Located { region: loc_name.region, value: Expr::Var(Ident::Unqualified(loc_name.value))}), args)
            }
        })
}

/// e.g. \x y => expr
pub fn closure<I>(min_indent: u32) -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    char('\\')
        .skip(indented_whitespaces(min_indent))
        .with(
            sep_by1(
                located(pattern(min_indent)),
                attempt(many1::<Vec<_>, _>(skipped_indented_whitespace_char(min_indent).skip(not_followed_by(string("->")))))
            ))
        .skip(indented_whitespaces(min_indent))
        .skip(string("->"))
        .skip(indented_whitespaces1(min_indent))
    .and(located(expr_body(min_indent)))
    .map(|(patterns, closure_body)| {
        Expr::Closure(patterns, Box::new(closure_body))
    })
}

parser! {
    #[inline(always)]
    fn pattern[I](min_indent_ref: u32)(I) -> Pattern
        where [ I: Stream<Item = char, Position = IndentablePosition> ]
    {
        let min_indent = *min_indent_ref;

        choice((
            char('_').map(|_| Pattern::Underscore),
            string("{}").map(|_| Pattern::EmptyRecordLiteral),
            match_variant(min_indent),
            int_or_frac_pattern(), // This goes before ident() so number literals aren't mistaken for malformed idents.
            ident().map(Pattern::Identifier),
        ))
    }
}

pub fn apply_variant<I>(min_indent: u32) -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    attempt(variant_name())
        .and(optional(attempt(apply_args(min_indent))))
        .map(|(name, opt_args): (String, Option<Vec<Located<Expr>>>)|
            Expr::ApplyVariant(VariantName::Unqualified(name), opt_args)
        )
}

pub fn match_variant<I>(min_indent: u32) -> impl Parser<Input = I, Output = Pattern>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    attempt(located(variant_name()))
        .and(optional(attempt(
            indented_whitespaces(min_indent)
            .with(
                sep_by1(
                    located(pattern(min_indent)),
                    attempt(
                        indented_whitespaces1(min_indent)
                            .skip(not_followed_by(string("then")))
                    )
                )
        ))))
        .map(|(loc_name, opt_args): (Located<String>, Option<Vec<Located<Pattern>>>)|
            // Use optional(sep_by1()) over sep_by() to avoid
            // allocating a Vec in case the variant is empty
            Pattern::Variant(
                Located { region: loc_name.region, value: VariantName::Unqualified(loc_name.value)},
                opt_args
            )
        )
}

pub fn variant_name<I>() -> impl Parser<Input = I, Output = String>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    // Variants must begin with an uppercase letter, but can have any
    // combination of letters or numbers afterwards.
    // No underscores, dashes, or apostrophes.
    look_ahead(satisfy(|ch: char| ch.is_uppercase()))
        .with(many1::<Vec<_>, _>(alpha_num()))
        .map(|chars| chars.into_iter().collect())
}

pub fn ident<I>() -> impl Parser<Input = I, Output = String>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    // Identifiers must begin with a lowercase letter, but can have any
    // combination of letters or numbers afterwards.
    // No underscores, dashes, or apostrophes.
    many1::<Vec<_>, _>(alpha_num())
        .then(|chars: Vec<char>| {
            let valid_start_char = chars[0].is_lowercase();

            if valid_start_char {
                let ident_str:String = chars.into_iter().collect();

                match ident_str.as_str() {
                    "if" => unexpected_any("Reserved keyword `if`").left(),
                    "then" => unexpected_any("Reserved keyword `then`").left(),
                    "else" => unexpected_any("Reserved keyword `else`").left(),
                    "case" => unexpected_any("Reserved keyword `case`").left(),
                    "when" => unexpected_any("Reserved keyword `when`").left(),
                    _ => value(ident_str).right()
                }
            } else {
                unexpected_any("First character in an identifier that was not a lowercase letter").left()
            }
        })
}

pub fn string_literal<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    between(char('"'), char('"'),
        many::<Vec<(String, Located<String>)>, _>(
            choice((
                // Handle the edge cases where the interpolation happens
                // to be at the very beginning of the string literal,
                // or immediately following the previous interpolation.
                attempt(string("\\("))
                    .with(value("".to_string()))
                    .and(located(ident()).skip(char(')'))),

                // Parse a bunch of non-interpolated characters until we hit \(
                many1::<Vec<char>, _>(string_body())
                    .map(|chars: Vec<char>| chars.into_iter().collect::<String>())
                    .and(choice((
                        attempt(string("\\(").with(located(ident()).skip(char(')')))),
                        // If we never encountered \( then we hit the end of
                        // the string literal. Use empty Ident here because
                        // we're going to pop this Ident off the array anyhow.
                        located(value("".to_string()))
                    ))),
            ))
    )
    .map(|mut pairs| {
        match pairs.pop() {
            None => Expr::EmptyStr,
            Some(( trailing_str, located_name )) => {
                let mut ident_pairs = pairs.into_iter().map(|(string, located_name)| {
                    ( string, located_name.map(|name| Ident::Unqualified(name.clone())) )
                }).collect::<Vec<(String, Located<Ident>)>>();

                if located_name.value.is_empty() {
                    if ident_pairs.is_empty() {
                        // We didn't find any interpolation at all. This is a string literal!
                        Expr::Str(trailing_str.to_string())
                    } else {
                        Expr::InterpolatedStr(ident_pairs, trailing_str.to_string())
                    }
                } else {
                    // This is an interpolated string where the interpolation
                    // happened to occur at the very end of the literal.

                    // Put the tuple back.
                    ident_pairs.push((
                        trailing_str,
                        located_name.map(|name| Ident::Unqualified(name.clone()))
                    ));

                    Expr::InterpolatedStr(ident_pairs, "".to_string())
                }
            }
        }
    }))
}

pub fn char_literal<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    between(char('\''), char('\''), char_body().expected(ERR_EMPTY_CHAR))
        .map(|ch| Expr::Char(ch))
}


fn unicode_code_pt<I>() -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    // You can put up to 6 hex digits inside \u{...}
    // e.g. \u{00A0} or \u{101010}
    // They must be no more than 10FFFF
    let hex_code_pt =
        count_min_max::<Vec<char>, HexDigit<I>>(1, 6, hex_digit())
        .then(|hex_digits| {
            let hex_str:String = hex_digits.into_iter().collect();

            match u32::from_str_radix(&hex_str, 16) {
                Ok(code_pt) => {
                    if code_pt > 0x10FFFF {
                        unexpected_any("Invalid Unicode code point. It must be no more than \\u{10FFFF}.").right()
                    } else {
                        match char::from_u32(code_pt) {
                            Some(ch) => value(ch).left(),
                            None => unexpected_any("Invalid Unicode code point.").right()
                        }
                    }
                },
                Err(_) => {
                    unexpected_any("Invalid hex code - Unicode code points must be specified using hexadecimal characters (the numbers 0-9 and letters A-F)").right()
                }
            }
        });

    char('u').with(between(char('{'), char('}'), hex_code_pt))
}

fn string_body<I>() -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    parser(|input: &mut I| {
        let (parsed_char, consumed) = try!(any().parse_lazy(input).into());
        let mut escaped = satisfy_map(|escaped_char| {
            // NOTE! When modifying this, revisit char_body too!
            // Their implementations are similar but not the same.
            match escaped_char {
                '"' => Some('"'),
                '\\' => Some('\\'),
                't' => Some('\t'),
                'n' => Some('\n'),
                'r' => Some('\r'),
                _ => None,
            }
        });

        match parsed_char {
            '\\' => {
                if look_ahead(char('(')).parse_stream(input).is_ok() {
                    // If we hit a \( then we're doing string interpolation.
                    // Bail out after consuming the backslash!
                    Err(Consumed::Empty(I::Error::empty(input.position()).into()))
                } else {
                    consumed.combine(|_| {
                        // Try to parse basic backslash-escaped literals
                        // e.g. \t, \n, \r
                        escaped.parse_stream(input).or_else(|_|
                            // If we didn't find any of those, try \u{...}
                            unicode_code_pt().parse_stream(input)
                        )
                    })
                }
            },
            '"' => {
                // Never consume a double quote unless it was preceded by a
                // backslash. This means we're at the end of the string literal!
                Err(Consumed::Empty(I::Error::empty(input.position()).into()))
            },
            _ => Ok((parsed_char, consumed))
        }
    })
}

fn char_body<I>() -> impl Parser<Input = I, Output = char>
where
    I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    parser(|input: &mut I| {
        let (parsed_char, consumed) = try!(any().parse_lazy(input).into());
        let mut escaped = satisfy_map(|escaped_char| {
            // NOTE! When modifying this, revisit string_body too!
            // Their implementations are similar but not the same.
            match escaped_char {
                '\'' => Some('\''),
                '\\' => Some('\\'),
                't' => Some('\t'),
                'n' => Some('\n'),
                'r' => Some('\r'),
                _ => None,
            }
        });

        match parsed_char {
            '\\' => {
                consumed.combine(|_| {
                    // Try to parse basic backslash-escaped literals
                    // e.g. \t, \n, \r
                    escaped.parse_stream(input).or_else(|_|
                        // If we didn't find any of those, try \u{...}
                        unicode_code_pt().parse_stream(input)
                    )
                })
            },
            '\'' => {
                // We should never consume a single quote unless
                // it's preceded by a backslash
                Err(Consumed::Empty(I::Error::empty(input.position()).into()))
            },
            _ => Ok((parsed_char, consumed)),
        }
    })
}

pub fn digits_after_decimal<I>() -> impl Parser<Input = I, Output = Vec<char>>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    // We expect these to be digits, but read any alphanumeric characters
    // because it could turn out they're malformed identifiers which
    // happen to begin with a number. We'll check for that at the end.
    many1::<Vec<_>, _>(alpha_num())
}

pub fn digits_before_decimal<I>() -> impl Parser<Input = I, Output = Vec<char>>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    // Digits before the decimal point can be underscore-separated
    // e.g. one million can be written as 1_000_000
    many1::<Vec<_>, _>(
        alpha_num().skip(optional(
            attempt(
                char('_').skip(
                    // Don't mistake keywords like `then` and `else` for
                    // space-separated digits!
                    not_followed_by(choice((string("then"), string("else"), string("when"))))
                )
            )
        ))
    )
}

pub fn negative_int_or_frac_literal<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    // Do this lookahead to decide if we should parse this as a number.
    // This matters because once we commit to parsing it as a number,
    // we may discover non-digit chars, indicating this is actually an
    // invalid identifier. (e.g. "523foo" looks like a number, but turns
    // out to be an invalid identifier on closer inspection.)
    look_ahead(char('-').with(digit()))
        .skip(any()) // skip over the minus sign we already know is there
        .with(digits_before_decimal())
        .and(optional(char('.').with(digits_after_decimal())))
        .then(|(int_digits, decimals): (Vec<char>, Option<Vec<char>>)| {
            // TODO check length of digits and make sure not to overflow
            let int_str: String = int_digits.into_iter().collect();

            match ( int_str.parse::<i64>(), decimals ) {
                (Ok(int_val), None) => {
                    value(Expr::Int(-int_val as i64)).right()
                },
                (Ok(_), Some(nums)) => {
                    let decimal_str: String = nums.into_iter().collect();

                    match format!("{}.{}", int_str, decimal_str).parse::<f64>() {
                        Ok(float) => {
                            value(Expr::Float(-float)).right()
                        },
                        Err(_) => {
                            unexpected_any("looked like a negative Float literal but was actually malformed identifier").left()
                        }
                    }
                },
                (Err(_), _) =>
                    unexpected_any("looked like a negative number literal but was actually malformed identifier").left()
            }
        })
}

pub fn int_or_frac_literal<I>() -> impl Parser<Input = I, Output = Expr>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    // Confirm that it starts with a digit; otherwise, it's potentially an identifier!
    look_ahead(digit())
        .with(digits_before_decimal())
        .and(optional(char('.').with(digits_after_decimal())))
        .then(|(int_digits, decimals): (Vec<char>, Option<Vec<char>>)| {
            // TODO check length of digits and make sure not to overflow
            let int_str: String = int_digits.into_iter().collect();

            match ( int_str.parse::<i64>(), decimals ) {
                (Ok(int_val), None) => {
                    value(Expr::Int(int_val as i64)).right()
                },
                (Ok(int_val), Some(nums)) => {
                    let decimal_str: String = nums.into_iter().collect();

                    match format!("{}.{}", int_str, decimal_str).parse::<f64>() {
                        Ok(float) => {
                            value(Expr::Float(float)).right()
                        },
                        Err(_) => {
                            unexpected_any("non-digit characters after decimal point in a number literal").left()
                        }
                    }
                },
                (Err(_), _) =>
                    unexpected_any("looked like a number literal but was actually malformed identifier").left()
            }
        })
}

/// TODO find a way to remove the code duplication between this and int_or_frac_literal
/// without sacrificing performance. I attempted to do this in 0062e83d03d389f0f07e33e1e7929e77825d774f
/// but couldn't figure out how to address the resulting compiler error, which was:
/// "cannot move out of captured outer variable in an `FnMut` closure"
pub fn int_or_frac_pattern<I>() -> impl Parser<Input = I, Output = Pattern>
where I: Stream<Item = char, Position = IndentablePosition>,
    I::Error: ParseError<I::Item, I::Range, I::Position>
{
    // We expect these to be digits, but read any alphanumeric characters
    // because it could turn out they're malformed identifiers which
    // happen to begin with a number. We'll check for that at the end.
    let digits_after_decimal =  many1::<Vec<_>, _>(alpha_num());

    // Digits before the decimal point can be underscore-separated
    // e.g. one million can be written as 1_000_000
    let digits_before_decimal = many1::<Vec<_>, _>(
        alpha_num().skip(optional(
                attempt(
                    char('_').skip(
                        // Don't mistake keywords like `then` and `else` for
                        // space-separated digits!
                        not_followed_by(choice((string("then"), string("else"), string("when"))))
                    )
                )
        ))
    );

    optional(attempt(char('-')))
        // Do this lookahead to decide if we should parse this as a number.
        // This matters because once we commit to parsing it as a number,
        // we may discover non-digit chars, indicating this is actually an
        // invalid identifier. (e.g. "523foo" looks like a number, but turns
        // out to be an invalid identifier on closer inspection.)
        .and(look_ahead(digit()))
        .and(digits_before_decimal)
        .and(optional(char('.').with(digits_after_decimal)))
        .then(|(((opt_minus, _), int_digits), decimals): (((Option<char>, _), Vec<char>), Option<Vec<char>>)| {
            let is_positive = opt_minus.is_none();

            // TODO check length of digits and make sure not to overflow
            let int_str: String = int_digits.into_iter().collect();

            match ( int_str.parse::<i64>(), decimals ) {
                (Ok(int_val), None) => {
                    if is_positive {
                        value(Pattern::IntLiteral(int_val as i64)).right()
                    } else {
                        value(Pattern::IntLiteral(-int_val as i64)).right()
                    }
                },
                (Ok(int_val), Some(nums)) => {
                    let decimal_str: String = nums.into_iter().collect();

                    match format!("{}.{}", int_str, decimal_str).parse::<f64>() {
                        Ok(float) => {
                            value(Pattern::FloatLiteral(float)).right()
                        },
                        Err(_) => {
                            unexpected_any("non-digit characters after decimal point in a number literal").left()
                        }
                    }
                },
                (Err(_), _) =>
                    unexpected_any("looked like a number but was actually malformed identifier").left()
            }
        })
}
