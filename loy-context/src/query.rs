pub mod cache;
pub mod steal;

#[macro_use]
mod plumbing;

use std::sync::Arc;

use loy_ast::ast::Ast;
use loy_ast::token::TokenStream;
use loy_typecheck_ast::modules::{ModuleId, ResolvedImport};
use steal::Steal;

use crate::define_engine;

loy_macros::loy_queries! {
    query compile_module(module_id: ModuleId) -> loy_ast::result::Result<()>;
    query tokenize_module(module_id: ModuleId) -> loy_ast::result::Result<TokenStream> cache Steal<TokenStream>;
    query parse_module(module_id: ModuleId) -> loy_ast::result::Result<Ast> cache Steal<Ast>;
    query resolve_module(module_id: ModuleId) -> Arc<Vec<ResolvedImport>> cache Arc<Vec<ResolvedImport>>;
}

pub mod queries {
    use crate::define_modules;

    define_queries! { define_modules! }
}

define_queries! { define_engine! }