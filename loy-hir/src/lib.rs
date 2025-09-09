use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::hash::Hash;
use std::path::PathBuf;
use std::sync::Arc;

use fxhash::FxHashMap;
use loy_ast::ast::Ast;
use loy_ast::result::Result;
use loy_ast::token::TokenStream;
use parking_lot::RawRwLock;
use parking_lot::lock_api::MappedRwLockReadGuard;

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

#[derive(Debug)]
pub struct ModuleInfo {
    pub id: ModuleId,
    pub path: PathBuf,
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
            let info = ModuleInfo { id, path, source };
            e.insert(info);
        }

        id
    }

    pub fn get_module_info(&self, id: ModuleId) -> Option<&ModuleInfo> {
        self.modules.get(&id)
    }
}

#[derive(Debug)]
pub struct GlobalCtx<'ctx> {
    pub module_map: ModuleMap,
    pub query_engine: QueryEngine<'ctx>,
}

#[derive(Debug)]
pub struct QueryEngine<'ctx> {
    pub providers: QueryProviders,
    pub caches: QueryCaches,
    _marker: std::marker::PhantomData<&'ctx ()>,
}

#[derive(Debug, Copy, Clone)]
pub struct TyCtx<'ctx> {
    gcx: &'ctx GlobalCtx<'ctx>,
}

impl<'ctx> TyCtx<'ctx> {
    pub fn new(gcx: &'ctx GlobalCtx<'ctx>) -> Self {
        Self { gcx }
    }

    pub fn tokenize_module(self, module_id: ModuleId) -> Steal<TokenStream> {
        if let Some(cached) = self.gcx.query_engine.caches.tokenize_module.get(module_id) {
            return cached;
        }

        let result = (self.gcx.query_engine.providers.tokenize_module)(self, module_id);
        let result = Steal::new(result);

        self.gcx
            .query_engine
            .caches
            .tokenize_module
            .insert(module_id, result.clone());

        result
    }

    pub fn parse_module(self, module_id: ModuleId) -> Result<Steal<Ast>> {
        if let Some(cached) = self.gcx.query_engine.caches.parse_module.get(module_id) {
            return Ok(cached);
        }

        let result = (self.gcx.query_engine.providers.parse_module)(self, module_id)?;
        let result = Steal::new(result);

        self.gcx
            .query_engine
            .caches
            .parse_module
            .insert(module_id, result.clone());

        Ok(result)
    }

    pub fn get_module_source(self, module_id: ModuleId) -> Arc<String> {
        if let Some(info) = self.gcx.module_map.get_module_info(module_id) {
            return info.source.clone();
        }

        // TODO: when encountering new modules when doing queries, we need a mechanism to insert
        // modules on the fly
        unreachable!();
    }

    pub fn compile_module(self, module_id: ModuleId) -> Result<()> {
        (self.gcx.query_engine.providers.compile_module)(self, module_id)
    }
}

#[derive(Debug)]
pub struct QueryProviders {
    pub compile_module: for<'ctx> fn(TyCtx<'ctx>, module_id: ModuleId) -> Result<()>,
    pub tokenize_module: for<'ctx> fn(TyCtx<'ctx>, module_id: ModuleId) -> TokenStream,
    pub parse_module: for<'ctx> fn(TyCtx<'ctx>, module_id: ModuleId) -> Result<Ast>,
}

#[derive(Debug)]
pub struct QueryCache<K, V> {
    inner: RefCell<FxHashMap<K, V>>,
}

impl<K, V> Default for QueryCache<K, V> {
    fn default() -> Self {
        Self {
            inner: RefCell::new(FxHashMap::default()),
        }
    }
}

impl<K, V> QueryCache<K, V>
where
    K: Hash + Eq + Clone + Copy,
    V: Clone,
{
    pub fn get(&self, key: K) -> Option<V> {
        self.inner.borrow().get(&key).cloned()
    }

    pub fn insert(&self, key: K, value: V) {
        self.inner.borrow_mut().insert(key, value);
    }
}

#[derive(Debug, Clone)]
pub struct Steal<T> {
    value: Arc<parking_lot::RwLock<Option<T>>>,
}

impl<T> Steal<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Arc::new(parking_lot::RwLock::new(Some(value))),
        }
    }

    pub fn borrow(&self) -> MappedRwLockReadGuard<'_, RawRwLock, T> {
        let borrow = self.value.read();
        if borrow.is_none() {
            panic!(
                "attempted to read from stolen value: {}",
                std::any::type_name::<T>()
            );
        }

        parking_lot::RwLockReadGuard::map(borrow, |b| b.as_ref().unwrap())
    }

    pub fn steal(&self) -> T {
        let value_ref = &mut *self
            .value
            .try_write()
            .expect("stealing value which is locked");

        let value = value_ref.take();
        value.expect("attempt to steal from stolen value")
    }
}

#[derive(Debug, Default)]
pub struct QueryCaches {
    pub tokenize_module: QueryCache<ModuleId, Steal<TokenStream>>,
    pub parse_module: QueryCache<ModuleId, Steal<Ast>>,
}

impl Default for QueryProviders {
    fn default() -> Self {
        Self {
            compile_module: |_tcx, _module_id| panic!("compile_module provider not registered"),
            tokenize_module: |_tcx, _module_id| panic!("tokenize_module provider not registered"),
            parse_module: |_tcx, _module_id| panic!("parse_module provider not registered"),
        }
    }
}

impl<'ctx> Default for QueryEngine<'ctx> {
    fn default() -> Self {
        Self {
            providers: QueryProviders::default(),
            caches: QueryCaches::default(),
            _marker: std::marker::PhantomData,
        }
    }
}
