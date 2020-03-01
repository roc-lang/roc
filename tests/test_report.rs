#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
#[macro_use]
extern crate roc;

#[cfg(test)]
mod test_report {
    use roc::module::symbol::ModuleId;
    use roc::reporting;
    use roc::reporting::ReportText::{Plain, Value};
    use roc::reporting::{Report, ReportText};
    use std::path::PathBuf;

    fn to_simple_report(text: ReportText) -> Report {
        let mut filename = PathBuf::new();
        filename.push(r"\code\proj\Main.roc");
        Report {
            text: text,
            filename: filename,
        }
    }

    fn report_renders_as(report: Report, expected_rendering: &str) {
        let mut buf = String::new();
        let src_lines: [&str; 0] = [];

        report.text.render_ci(
            &mut buf,
            panic!("Need a &mut Subs here"),
            panic!("Need a ModuleId"),
            &src_lines,
            panic!("Needs interns"),
        );

        assert_eq!(buf, expected_rendering);
    }

    #[test]
    fn report_plain() {
        report_renders_as(to_simple_report(Plain(Box::from("hello"))), "good bye");
    }
}
