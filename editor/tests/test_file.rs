#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

#[cfg(test)]
mod test_file {
    use bumpalo::Bump;
    use roc_editor::file::File;
    use std::path::Path;

    #[test]
    fn read_and_fmt_simple_roc_module() {
        let simple_module_path = Path::new("./tests/modules/Simple.roc");

        let arena = Bump::new();

        let file = File::read(simple_module_path, &arena)
            .expect("Could not read Simple.roc in test_file test");

        assert_eq!(
            file.fmt(),
            indoc!(
                r#"
                    interface Simple
                        exposes [ 
                        v, x
                         ]
                        imports []
    
                    v : Str
                    
                    v = "Value!"

                    x : Int
                    x = 4"#
            )
        );
    }
}
