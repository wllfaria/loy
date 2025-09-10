pub mod cache;
pub mod steal;

#[macro_use]
mod plumbing;

use std::sync::Arc;

use loy_ast::ast::Ast;
use loy_ast::token::TokenStream;
use loy_typecheck_ast::modules::declaration::{DeclId, Declaration};
use loy_typecheck_ast::modules::{ModuleId, ResolvedImport, ResolvedModule};
use steal::Steal;

use crate::define_engine;

loy_macros::loy_queries! {
    query module_compile(module_id: ModuleId) -> loy_ast::result::Result<()>;
    query module_token_stream(module_id: ModuleId) -> loy_ast::result::Result<TokenStream> cache Steal<TokenStream>;
    query module_ast(module_id: ModuleId) -> loy_ast::result::Result<Ast> cache Steal<Ast>;
    query module_imports(module_id: ModuleId) -> Arc<Vec<ResolvedImport>> cache Arc<Vec<ResolvedImport>>;
    query module_resolve(module_id: ModuleId) -> Arc<ResolvedModule> cache Arc<ResolvedModule>;
    query module_exports(module_id: ModuleId) -> Arc<Vec<DeclId>> cache Arc<Vec<DeclId>>;
    query module_declarations(module_id: ModuleId) -> Vec<Declaration> cache Vec<DeclId>;
}

pub mod queries {
    use crate::define_modules;

    define_queries! { define_modules! }
}

define_queries! { define_engine! }