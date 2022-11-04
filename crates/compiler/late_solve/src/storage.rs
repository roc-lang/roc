use roc_types::subs::StorageSubs;
use roc_types::subs::{storage_copy_var_to, Subs, Variable, VariableMapCache};
use std::iter::Iterator;

/// Storage for types to be sent to an external module, and written to only by one module's subs.
/// Maintains a cache so that independent writes can re-use types previously inserted into the
/// storage.
#[derive(Clone, Debug)]
pub struct ExternalModuleStorage {
    storage: StorageSubs,
    /// Variable they expose -> variable we record into storage
    variable_mapping_cache: VariableMapCache,
}

pub struct ExternalModuleStorageSnapshot {
    mapping_cache_len: usize,
}

impl ExternalModuleStorage {
    pub fn new(subs: Subs) -> Self {
        Self {
            storage: StorageSubs::new(subs),
            variable_mapping_cache: VariableMapCache::default(),
        }
    }

    pub fn extend_with_variable(&mut self, source: &Subs, variable: Variable) -> Variable {
        storage_copy_var_to(
            &mut self.variable_mapping_cache,
            source,
            self.storage.as_inner_mut(),
            variable,
        )
    }

    pub fn into_storage_subs(self) -> StorageSubs {
        self.storage
    }

    /// Invalidates the whole cache given a sequence of variables that should no longer be indexed
    /// from the cache.
    pub fn invalidate_cache(&mut self, changed_variables: &[Variable]) {
        for var in changed_variables {
            for cache in self.variable_mapping_cache.0.iter_mut().rev() {
                cache.remove(var);
            }
        }
    }

    /// Invalidates the whole cache.
    /// Should only be called if you need to invalidate the cache but don't have a snapshot.
    /// Generally you should prefer to create a snapshot and invalidate that snapshot, which avoids
    /// unnecessary cache invalidation.
    pub fn invalidate_whole_cache(&mut self) {
        debug_assert_eq!(self.variable_mapping_cache.0.len(), 1);
        self.variable_mapping_cache.0.last_mut().unwrap().clear();
    }

    /// Creates a snapshot of the cache, making it suitable for new ephemeral entries.
    /// The cache can be rolled back to the state it was in prior to the snapshot with [rollback_cache].
    pub fn snapshot_cache(&mut self) -> ExternalModuleStorageSnapshot {
        self.variable_mapping_cache.0.push(Default::default());
        ExternalModuleStorageSnapshot {
            mapping_cache_len: self.variable_mapping_cache.0.len(),
        }
    }

    pub fn rollback_cache(&mut self, snapshot: ExternalModuleStorageSnapshot) {
        debug_assert_eq!(
            self.variable_mapping_cache.0.len(),
            snapshot.mapping_cache_len
        );
        self.variable_mapping_cache.0.pop();
    }
}
