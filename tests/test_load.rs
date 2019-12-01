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
    use crate::helpers::{fixtures_dir, send_set_from};
    use roc::load::{load, LoadedModule};

    fn test_async<F: std::future::Future>(future: F) -> F::Output {
        use tokio::runtime::Runtime;

        // Create the runtime
        let mut rt = Runtime::new().expect("Error initializing Tokio runtime.");

        // Spawn the root task
        rt.block_on(future)
    }

    #[test]
    fn interface_with_deps() {
        let src_dir = fixtures_dir().join("interface_with_deps");
        let filename = src_dir.join("Primary.roc");

        test_async(async {
            let loaded = load(src_dir, filename).await;

            let module = match loaded.requested_module {
                LoadedModule::Valid(module) => module,
                LoadedModule::FileProblem(err) => panic!(
                    "requested_module failed to load with FileProblem: {:?}",
                    err
                ),
                LoadedModule::ParsingFailed(fail) => panic!(
                    "requested_module failed to load with ParsingFailed: {:?}",
                    fail
                ),
            };

            assert_eq!(module.name, Some("Primary".into()));
            assert_eq!(module.defs.len(), 6);

            assert_eq!(
                loaded.deps,
                send_set_from(vec!["Dep1".into(), "Dep2".into(), "Dep3.Blah".into()])
            );
        });
    }
}
