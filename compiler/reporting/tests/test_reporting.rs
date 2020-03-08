#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
extern crate roc_reporting;

mod helpers;

#[cfg(test)]
mod test_report {
    use crate::helpers::test_home;
    use roc_module::symbol::{Interns, ModuleId};
    use roc_reporting::report::{Report, ReportText};
    use roc_types::pretty_print::name_all_type_vars;
    use roc_types::subs::Subs;
    use roc_types::types;
    use std::path::PathBuf;
    // use roc_region::all;
    use crate::helpers::{assert_correct_variable_usage, can_expr, infer_expr, CanExprOut};
    use roc_reporting::report::ReportText::{EmText, Plain, Region, Type, Url, Value};
    use roc_types::subs::Content::{FlexVar, RigidVar, Structure};
    use roc_types::subs::FlatType::EmptyRecord;

    // use roc_problem::can;
    fn to_simple_report(text: ReportText) -> Report {
        let mut filename = PathBuf::new();
        filename.push(r"\code\proj\Main.roc");
        Report {
            text: text,
            filename: filename,
        }
    }

    fn infer_expr_help(
        expr_src: &str,
    ) -> (
        Vec<types::Problem>,
        Vec<roc_problem::can::Problem>,
        Subs,
        ModuleId,
        Interns,
    ) {
        let CanExprOut {
            output,
            var_store,
            var,
            constraint,
            home,
            interns,
            problems: can_problems,
            ..
        } = can_expr(expr_src);
        let mut subs = Subs::new(var_store.into());

        assert_correct_variable_usage(&constraint);

        for (var, name) in output.introduced_variables.name_by_var {
            subs.rigid_var(var, name);
        }

        let mut unify_problems = Vec::new();
        let (_content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        name_all_type_vars(var, &mut subs);

        (unify_problems, can_problems, subs, home, interns)
    }

    fn report_renders_as_from_src(src: &str, report: Report, expected_rendering: &str) {
        let (_type_problems, _can_problems, mut subs, home, interns) = infer_expr_help(src);
        let mut buf = String::new();
        let src_lines: Vec<&str> = src.split('\n').collect();

        report
            .text
            .render_ci(&mut buf, &mut subs, home, &src_lines, &interns);

        assert_eq!(buf, expected_rendering);
    }

    fn report_renders_as(report: Report, expected_rendering: &str) {
        report_renders_as_from_src(
            indoc!(
                r#"
                    x = 1
                    y = 2

                    x
                "#
            ),
            report,
            expected_rendering,
        )
    }

    #[test]
    fn report_plain() {
        report_renders_as(to_simple_report(Plain(Box::from("y"))), "y");
    }

    #[test]
    fn report_emphasized_text() {
        report_renders_as(to_simple_report(EmText(Box::from("y"))), "*y*");
    }

    #[test]
    fn report_url() {
        report_renders_as(to_simple_report(Url(Box::from("y"))), "<y>");
    }

    #[test]
    fn report_symbol() {
        let src: &str = indoc!(
            r#"
                x = 1
                y = 2

                x
            "#
        );

        let (_type_problems, _can_problems, mut subs, home, interns) = infer_expr_help(src);

        let mut buf = String::new();
        let src_lines: Vec<&str> = src.split('\n').collect();

        to_simple_report(Value(interns.symbol(test_home(), "x".into())))
            .text
            .render_ci(&mut buf, &mut subs, home, &src_lines, &interns);

        assert_eq!(buf, "x");
    }

    #[test]
    fn report_wildcard() {
        report_renders_as(to_simple_report(Type(FlexVar(None))), "*");
    }

    #[test]
    fn report_flex_var() {
        report_renders_as(to_simple_report(Type(FlexVar(Some("msg".into())))), "msg");
    }

    #[test]
    fn report_rigid_var() {
        report_renders_as(to_simple_report(Type(RigidVar("a".into()))), "a");
    }

    #[test]
    fn report_empty_record() {
        report_renders_as(to_simple_report(Type(Structure(EmptyRecord))), "{}");
    }

    #[test]
    fn report_region() {
        report_renders_as_from_src(
            indoc!(
                r#"
                    x = 1
                    y = 2
                    f = \a ->  a + 4

                    f x
                "#
            ),
            to_simple_report(Region(roc_region::all::Region {
                start_line: 1,
                end_line: 4,
                start_col: 0,
                end_col: 0,
            })),
            indoc!(
                r#"
                    1 | y = 2
                    2 | f = \a ->  a + 4
                    3 |
                "#
            ),
        );
    }
}
