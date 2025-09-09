use std::sync::Arc;

use loy_ast::ast::Ast;
use loy_ast::result::Result;
use loy_ast::token::TokenStream;

use crate::modules::{ModuleId, ModuleMap};
use crate::query::QueryEngine;
use crate::query::steal::Steal;

pub mod modules;
pub mod query;

#[derive(Debug, Copy, Clone)]
pub struct TyCtx<'ctx> {
    gcx: &'ctx GlobalCtx<'ctx>,
}

#[derive(Debug)]
pub struct GlobalCtx<'ctx> {
    pub module_map: ModuleMap,
    pub query_engine: QueryEngine<'ctx>,
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