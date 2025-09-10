use std::cell::RefCell;
use std::path::PathBuf;
use std::sync::Arc;

use loy_ast::ast::Ast;
use loy_ast::result::Result;
use loy_ast::token::TokenStream;
use loy_typecheck_ast::modules::{ModuleId, ModuleInfo, ModuleMap, ResolvedImport};

use crate::query::QueryEngine;
use crate::query::steal::Steal;

pub mod query;

#[derive(Debug, Copy, Clone)]
pub struct TyCtx<'ctx> {
    gcx: &'ctx GlobalCtx<'ctx>,
}

#[derive(Debug)]
pub struct GlobalCtx<'ctx> {
    pub module_map: RefCell<ModuleMap>,
    pub query_engine: QueryEngine<'ctx>,
}

impl<'ctx> TyCtx<'ctx> {
    pub fn new(gcx: &'ctx GlobalCtx<'ctx>) -> Self {
        Self { gcx }
    }

    pub fn tokenize_module(self, module_id: ModuleId) -> Result<Steal<TokenStream>> {
        if let Some(cached) = self.gcx.query_engine.caches.tokenize_module.get(module_id) {
            return Ok(cached);
        }

        let result = (self.gcx.query_engine.providers.tokenize_module)(self, module_id)?;
        let result = Steal::new(result);

        self.gcx
            .query_engine
            .caches
            .tokenize_module
            .insert(module_id, result.clone());

        Ok(result)
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
        if let Some(info) = self.gcx.module_map.borrow().get_module_info(module_id) {
            return info.source.clone();
        }

        // TODO: when encountering new modules when doing queries, we need a mechanism to insert
        // modules on the fly
        unreachable!();
    }

    pub fn resolve_module(self, module_id: ModuleId) -> Arc<Vec<ResolvedImport>> {
        if let Some(module) = self.gcx.query_engine.caches.resolve_module.get(module_id) {
            return module.clone();
        }

        let result = (self.gcx.query_engine.providers.resolve_module)(self, module_id);

        self.gcx
            .query_engine
            .caches
            .resolve_module
            .insert(module_id, result.clone());

        result
    }

    pub fn register_module(self, path: PathBuf) -> ModuleId {
        if let Some(module) = self
            .gcx
            .module_map
            .borrow()
            .get_module_info(ModuleId::from_path(&path))
        {
            return module.id;
        }

        self.gcx.module_map.borrow_mut().add_file(path)
    }

    pub fn get_module_info(self, module_id: ModuleId) -> Option<ModuleInfo> {
        self.gcx
            .module_map
            .borrow()
            .get_module_info(module_id)
            .cloned()
    }

    pub fn compile_module(self, module_id: ModuleId) -> Result<()> {
        (self.gcx.query_engine.providers.compile_module)(self, module_id)
    }
}