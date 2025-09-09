use loy_ast::ast::Ast;
use loy_ast::result::Result;
use loy_ast::token::TokenStream;

use crate::TyCtx;
use crate::modules::ModuleId;
use crate::query::cache::QueryCaches;

pub mod cache;
pub mod steal;

#[derive(Debug)]
pub struct QueryEngine<'ctx> {
    pub providers: QueryProviders,
    pub caches: QueryCaches,
    _marker: std::marker::PhantomData<&'ctx ()>,
}

#[derive(Debug)]
pub struct QueryProviders {
    pub compile_module: for<'ctx> fn(TyCtx<'ctx>, module_id: ModuleId) -> Result<()>,
    pub tokenize_module: for<'ctx> fn(TyCtx<'ctx>, module_id: ModuleId) -> TokenStream,
    pub parse_module: for<'ctx> fn(TyCtx<'ctx>, module_id: ModuleId) -> Result<Ast>,
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
