fn report_missing_package_shorthand2<'a>(
    packages: &[Loc<PackageEntry>],
    imports: &[Loc<ImportsEntry>],
) -> Option<LoadingProblem<'a>> {
    imports.iter().find_map(|i| match i.value {
        ImportsEntry::Module(_, _) | ImportsEntry::IngestedFile(_, _) => None,
        ImportsEntry::Package(shorthand, name, _) => {
            let name=name.as_str();
            if packages
                .iter()
                .find(|p| p.value.shorthand == shorthand)
                .is_none()
            {
                Some(
                    LoadingProblem::FormattedReport(
                        format!("The package shorthand '{shorthand}' that you are importing the module '{name}' from in '{shorthand}.{name}', doesn't exist in this module.\nImport it in the \"packages\" section of the header.")))
            } else {
                None
            }
        }
    })
}
