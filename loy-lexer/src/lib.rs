mod cursor;
mod lexer;
mod result;

#[cfg(test)]
mod tests;

pub use lexer::Lexer;
pub use result::{Error, Result};

pub fn provide(providers: &mut loy_hir::QueryProviders) {
    providers.tokenize_module = tokenize_module;
}

fn tokenize_module(
    tcx: loy_hir::TyCtx,
    module_id: loy_hir::ModuleId,
) -> loy_ast::token::TokenStream {
    let source = tcx.get_module_source(module_id);
    Lexer::new(&source).lex().unwrap()
}