pub mod declaration;
mod resolved_import;
mod resolved_module;

use std::collections::hash_map::Entry;
use std::path::PathBuf;
use std::sync::Arc;

use fxhash::FxHashMap;
pub use resolved_import::{
    EntireModuleImport, ImportedSymbols, ResolvedImport, SpecificModuleImport, SymbolImport,
};
pub use resolved_module::ResolvedModule;

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct ModuleId(u32);

impl ModuleId {
    pub fn from_path(path: &PathBuf) -> Self {
        use std::hash::{Hash, Hasher};
        let mut hasher = fxhash::FxHasher::default();
        path.hash(&mut hasher);
        ModuleId(hasher.finish() as u32)
    }
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub id: ModuleId,
    pub path: PathBuf,
    pub absolute_path: PathBuf,
    pub source: Arc<String>,
}

#[derive(Debug, Default)]
pub struct ModuleMap {
    modules: FxHashMap<ModuleId, ModuleInfo>,
}

impl ModuleMap {
    pub fn add_file(&mut self, path: PathBuf) -> ModuleId {
        let id = ModuleId::from_path(&path);

        if let Entry::Vacant(e) = self.modules.entry(id) {
            let source = std::fs::read_to_string(&path).expect("failed to read file from disk");
            let source = Arc::new(source);
            let absolute_path = path.canonicalize().expect("failed to canonicalize path");
            let info = ModuleInfo {
                id,
                path,
                source,
                absolute_path,
            };
            e.insert(info);
        }

        id
    }

    pub fn get_module_info(&self, id: ModuleId) -> Option<&ModuleInfo> {
        self.modules.get(&id)
    }
}
