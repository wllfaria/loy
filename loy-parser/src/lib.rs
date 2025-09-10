mod expr;
mod parser;
mod statement;

use loy_ast::result::Result;
pub use parser::{ParseContext, parse_token_stream};

pub fn provide(providers: &mut loy_context::query::QueryProviders) {
    providers.module_ast = module_ast;
}

fn module_ast(
    tcx: loy_context::TyCtx,
    module_id: loy_typecheck_ast::modules::ModuleId,
) -> Result<loy_ast::ast::Ast> {
    let module_source = tcx.get_module_source(module_id);
    let tokens = tcx.module_token_stream(module_id)?.steal();
    let mut context = ParseContext::new(tokens, &module_source);
    let ast = parse_token_stream(&mut context).unwrap();
    Ok(ast)
}
