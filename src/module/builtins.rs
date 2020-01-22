use crate::can::ident::ModuleName;
use crate::module::module_id::{ModuleId, ModuleIds};

pub const FLOAT_STR: &str = "Float";
pub const BOOL_STR: &str = "Bool";
pub const INT_STR: &str = "Int";
pub const STR_STR: &str = "Str";
pub const LIST_STR: &str = "List";
pub const MAP_STR: &str = "Map";
pub const SET_STR: &str = "Set";
pub const NUM_STR: &str = "Num";
pub const DEFAULT_STR: &str = "Default";

lazy_static! {
    pub static ref BUILTIN_MODULES: ModuleIds = {
        let mut module_ids = ModuleIds::default();

        module_ids.get_or_insert_id(&ModuleName::from(FLOAT_STR));
        module_ids.get_or_insert_id(&ModuleName::from(INT_STR));
        module_ids.get_or_insert_id(&ModuleName::from(STR_STR));
        module_ids.get_or_insert_id(&ModuleName::from(LIST_STR));
        module_ids.get_or_insert_id(&ModuleName::from(MAP_STR));
        module_ids.get_or_insert_id(&ModuleName::from(SET_STR));
        module_ids.get_or_insert_id(&ModuleName::from(NUM_STR));
        module_ids.get_or_insert_id(&ModuleName::from(DEFAULT_STR));

        module_ids
    };
    pub static ref FLOAT_ID: ModuleId = BUILTIN_MODULES.get_id(&FLOAT_STR.into()).unwrap();
    pub static ref INT_ID: ModuleId = BUILTIN_MODULES.get_id(&INT_STR.into()).unwrap();
    pub static ref STR_ID: ModuleId = BUILTIN_MODULES.get_id(&STR_STR.into()).unwrap();
    pub static ref LIST_ID: ModuleId = BUILTIN_MODULES.get_id(&LIST_STR.into()).unwrap();
    pub static ref MAP_ID: ModuleId = BUILTIN_MODULES.get_id(&MAP_STR.into()).unwrap();
    pub static ref SET_ID: ModuleId = BUILTIN_MODULES.get_id(&SET_STR.into()).unwrap();
    pub static ref NUM_ID: ModuleId = BUILTIN_MODULES.get_id(&NUM_STR.into()).unwrap();
    pub static ref DEFAULT_ID: ModuleId = BUILTIN_MODULES.get_id(&DEFAULT_STR.into()).unwrap();
}
