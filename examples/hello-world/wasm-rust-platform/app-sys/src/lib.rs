use roc_std::RocStr;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed"]
    pub fn roc_main() -> RocStr;
}
