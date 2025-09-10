use std::sync::Arc;

use loy_ast::ast::NodeVisibility;
use loy_context::TyCtx;
use loy_typecheck_ast::modules::ModuleId;
use loy_typecheck_ast::modules::declaration::DeclId;

pub fn module_exports(ctx: TyCtx<'_>, module_id: ModuleId) -> Arc<Vec<DeclId>> {
    let declarations = ctx.module_declarations(module_id);
    let exports = declarations
        .into_iter()
        .map(|id| ctx.get_declaration(id).expect("declaration must exist"))
        .filter(|decl| matches!(decl.visibility, NodeVisibility::Public))
        .map(|decl| decl.id)
        .collect::<Vec<_>>();

    Arc::new(exports)
}