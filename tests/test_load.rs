// #[macro_use]
// extern crate pretty_assertions;
// #[macro_use]
// extern crate indoc;

extern crate bumpalo;
extern crate inkwell;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_load {
    use crate::helpers::{fixtures_dir, builtins_dir};
    use roc::load::{load, Loaded, LoadedModule};
    use roc::can::module::Module;

    fn test_async<F: std::future::Future>(future: F) -> F::Output {
        use tokio::runtime::Runtime;

        // Create the runtime
        let mut rt = Runtime::new().expect("Error initializing Tokio runtime.");

        // Spawn the root task
        rt.block_on(future)
    }

    fn expect_module(loaded: Loaded) -> Module {
        match loaded.requested_module {
            LoadedModule::Valid(module) => module,
            LoadedModule::FileProblem{ filename, error } => panic!(
                "{:?} failed to load with FileProblem: {:?}",
                filename, error
            ),
            LoadedModule::ParsingFailed{ filename, fail } => panic!(
                "{:?} failed to load with ParsingFailed: {:?}",
                filename, fail
            ),
        }
    }

    async fn load_builtins(deps: &mut Vec<LoadedModule>) {
        let src_dir = builtins_dir();
        let filename = src_dir.join("Defaults.roc");

        load(src_dir, filename, deps).await;
    }

    #[test]
    fn interface_with_deps() {
        let mut deps = Vec::new();
        let src_dir = fixtures_dir().join("interface_with_deps");
        let filename = src_dir.join("Primary.roc");

        test_async(async {
            let module = expect_module(load(src_dir, filename, &mut deps).await);

            assert_eq!(module.name, Some("Primary".into()));
            assert_eq!(module.defs.len(), 6);

            let module_names: Vec<Option<Box<str>>> =
                deps.into_iter().map(|dep| dep.into_module().unwrap().name).collect();

            assert_eq!(module_names, vec![
                Some("Dep1".into()),
                Some("Dep3.Blah".into()),
                Some("Dep2".into())
            ]);
        });
    }

    #[test]
    fn builtins() {
        let mut deps = Vec::new();
        let src_dir = builtins_dir();
        let filename = src_dir.join("Defaults.roc");

        test_async(async {
            let module = expect_module(load(src_dir, filename, &mut deps).await);

            assert_eq!(module.name, Some("Defaults".into()));
            assert_eq!(module.defs.len(), 0);

            let module_names: Vec<Option<Box<str>>> =
                deps.into_iter().map(|dep| dep.into_module().unwrap().name).collect();

            assert_eq!(module_names, vec![
                Some("Map".into()),
                Some("Set".into()),
            ]);
        });
    }

    #[test]
    fn interface_with_builtins() {
        let mut deps = Vec::new();

        test_async(async {
            load_builtins(&mut deps).await;

            let src_dir = fixtures_dir().join("interface_with_deps");
            let filename = src_dir.join("Primary.roc");
            let module = expect_module(load(src_dir, filename, &mut deps).await);

            assert_eq!(module.name, Some("Primary".into()));
            assert_eq!(module.defs.len(), 6);

            let module_names: Vec<Option<Box<str>>> =
                deps.into_iter().map(|dep| dep.into_module().unwrap().name).collect();

            assert_eq!(module_names, vec![
                Some("Map".into()),
                Some("Set".into()),
                Some("Dep1".into()),
                Some("Dep3.Blah".into()),
                Some("Dep2".into())
            ]);
        });
    }
}
