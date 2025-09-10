use std::cell::RefCell;
use std::path::PathBuf;
use std::sync::Arc;

use fxhash::FxHashMap;
use loy_ast::ast::Ast;
use loy_ast::result::Result;
use loy_ast::token::TokenStream;
use loy_typecheck_ast::modules::declaration::{DeclId, Declaration};
use loy_typecheck_ast::modules::{ModuleId, ModuleInfo, ModuleMap, ResolvedImport, ResolvedModule};

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
    pub declarations: RefCell<FxHashMap<DeclId, Arc<Declaration>>>,
}

impl<'ctx> TyCtx<'ctx> {
    pub fn new(gcx: &'ctx GlobalCtx<'ctx>) -> Self {
        Self { gcx }
    }

    pub fn module_token_stream(self, module_id: ModuleId) -> Result<Steal<TokenStream>> {
        if let Some(cached) = self
            .gcx
            .query_engine
            .caches
            .module_token_stream
            .get(module_id)
        {
            return Ok(cached);
        }

        let result = (self.gcx.query_engine.providers.module_token_stream)(self, module_id)?;
        let result = Steal::new(result);

        self.gcx
            .query_engine
            .caches
            .module_token_stream
            .insert(module_id, result.clone());

        Ok(result)
    }

    pub fn module_ast(self, module_id: ModuleId) -> Result<Steal<Ast>> {
        if let Some(cached) = self.gcx.query_engine.caches.module_ast.get(module_id) {
            return Ok(cached);
        }

        let result = (self.gcx.query_engine.providers.module_ast)(self, module_id)?;
        let result = Steal::new(result);

        self.gcx
            .query_engine
            .caches
            .module_ast
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

    pub fn module_imports(self, module_id: ModuleId) -> Arc<Vec<ResolvedImport>> {
        if let Some(module) = self.gcx.query_engine.caches.module_imports.get(module_id) {
            return module.clone();
        }

        let result = (self.gcx.query_engine.providers.module_imports)(self, module_id);

        self.gcx
            .query_engine
            .caches
            .module_imports
            .insert(module_id, result.clone());

        result
    }

    pub fn module_declarations(self, module_id: ModuleId) -> Vec<DeclId> {
        let result = (self.gcx.query_engine.providers.module_declarations)(self, module_id);
        result
            .into_iter()
            .map(|declaration| self.register_declaration(declaration))
            .collect()
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

    pub fn module_resolve(self, module_id: ModuleId) -> Arc<ResolvedModule> {
        todo!()
    }

    pub fn module_exports(self, module_id: ModuleId) -> Arc<Vec<DeclId>> {
        if let Some(cached) = self.gcx.query_engine.caches.module_exports.get(module_id) {
            return cached.clone();
        };

        let result = (self.gcx.query_engine.providers.module_exports)(self, module_id);

        self.gcx
            .query_engine
            .caches
            .module_exports
            .insert(module_id, result.clone());

        result
    }

    pub fn get_declaration(self, decl_id: DeclId) -> Option<Arc<Declaration>> {
        self.gcx.declarations.borrow().get(&decl_id).cloned()
    }

    pub fn get_module_info(self, module_id: ModuleId) -> Option<ModuleInfo> {
        self.gcx
            .module_map
            .borrow()
            .get_module_info(module_id)
            .cloned()
    }

    pub fn register_declaration(self, declaration: Declaration) -> DeclId {
        let id = declaration.id;
        self.gcx
            .declarations
            .borrow_mut()
            .insert(id, Arc::new(declaration));
        id
    }

    pub fn module_compile(self, module_id: ModuleId) -> Result<()> {
        (self.gcx.query_engine.providers.module_compile)(self, module_id)
    }
}
