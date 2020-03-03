#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_report {
    use crate::helpers::{assert_correct_variable_usage, can_expr, CanExprOut};
    use roc::can;
    use roc::infer::infer_expr;
    use roc::module::symbol::{Interns, ModuleId};
    use roc::pretty_print_types::name_all_type_vars;
    use roc::reporting;
    use roc::reporting::ReportText::{Plain, Value};
    use roc::reporting::{Report, ReportText};
    use roc::subs::Subs;
    use roc::types;
    use std::path::PathBuf;

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
        Vec<can::problem::Problem>,
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

        for (var, name) in output.ftv {
            subs.rigid_var(var, name);
        }

        let mut unify_problems = Vec::new();
        let (content, solved) = infer_expr(subs, &mut unify_problems, &constraint, var);
        let mut subs = solved.into_inner();

        name_all_type_vars(var, &mut subs);

        (unify_problems, can_problems, subs, home, interns)
    }

    fn report_renders_as(src: &str, report: Report, expected_rendering: &str) {
        let (_type_problems, can_problems, mut subs, home, interns) = infer_expr_help(src);
        let mut buf = String::new();
        let src_lines: Vec<&str> = src.split('\n').collect();

        dbg!("canonicalization problems: {:?}", can_problems);

        report
            .text
            .render_ci(&mut buf, &mut subs, home, &src_lines, &interns);

        assert_eq!(buf, expected_rendering);
    }

    #[test]
    fn report_plain() {
        report_renders_as(
            indoc!(
                r#"
                    x = 1
                    y = 2

                    x
                "#
            ),
            to_simple_report(Plain(Box::from("hello"))),
            "good bye",
        );
    }
}
