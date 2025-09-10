mod cursor;
mod lexer;
mod result;

#[cfg(test)]
mod tests;

pub use lexer::Lexer;
use loy_ast::result::ParseIssue;
use loy_ast::token::Span;
pub use result::{Error, Result};

pub fn provide(providers: &mut loy_context::query::QueryProviders) {
    providers.module_token_stream = module_token_stream;
}

fn module_token_stream(
    tcx: loy_context::TyCtx,
    module_id: loy_typecheck_ast::modules::ModuleId,
) -> loy_ast::result::Result<loy_ast::token::TokenStream> {
    let source = tcx.get_module_source(module_id);
    match Lexer::new(&source).lex() {
        Ok(stream) => Ok(stream),
        Err(error) => ParseIssue::new(error.to_string(), Span::default())
            .with_report_title("unknown token")
            .into_error()?,
    }
}
