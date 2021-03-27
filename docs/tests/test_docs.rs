use roc_docs::{documentation_to_template_data, files_to_documentations, ModuleEntry};
use std::path::PathBuf;

#[cfg(test)]
mod test_docs {
    use super::*;

    #[test]
    fn internal() {
        let files_docs = files_to_documentations(
            vec![PathBuf::from(r"tests/fixtures/Interface.roc")],
            roc_builtins::std::standard_stdlib(),
        );

        let package = roc_load::docs::Documentation {
            name: "roc/builtins".to_string(),
            version: "1.0.0".to_string(),
            docs: "Package introduction or README.".to_string(),
            modules: files_docs,
        };

        let expected_entries = vec![
            ModuleEntry {
                name: "singleline".to_string(),
                docs: "<p>Single line documentation.</p>\n".to_string(),
            },
            ModuleEntry {
                name: "multiline".to_string(),
                docs: "<p>Multiline documentation.\nWithout any complex syntax yet!</p>\n".to_string(),
            }, ModuleEntry {
                name: "multiparagraph".to_string(),
                docs: "<p>Multiparagraph documentation.</p>\n<p>Without any complex syntax yet!</p>\n".to_string(),
            }, ModuleEntry {
                name: "codeblock".to_string(),
                docs: "<p>Turns &gt;&gt;&gt; into code block for now.</p>\n<pre><code class=\"language-roc\">codeblock</code></pre>\n".to_string(),
            },
        ];

        for module in &package.modules {
            let template = documentation_to_template_data(&package, module);
            assert_eq!(template.module_name, "Test");
            template
                .module_entries
                .iter()
                .zip(expected_entries.iter())
                .for_each(|(x, y)| assert_eq!(x, y));
        }
    }
}
