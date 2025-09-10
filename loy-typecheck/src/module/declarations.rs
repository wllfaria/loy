use loy_ast::ast::AstNode;
use loy_ast::ast_visitor::OwnedAstVisitor;
use loy_context::TyCtx;
use loy_typecheck_ast::modules::ModuleId;
use loy_typecheck_ast::modules::declaration::{DeclId, DeclIndex, Declaration};

#[derive(Debug)]
pub struct DeclarationCollector<'ctx> {
    module: ModuleId,
    ctx: TyCtx<'ctx>,
    decl_index: DeclIndex,
    declarations: Vec<Declaration>,
}

impl<'ctx> DeclarationCollector<'ctx> {
    pub fn new(module: ModuleId, ctx: TyCtx<'ctx>, decl_index: DeclIndex) -> Self {
        Self {
            module,
            ctx,
            decl_index,
            declarations: vec![],
        }
    }

    fn make_declaration(&mut self, item: AstNode) {
        let index = self.decl_index.next();
        let decl_id = DeclId::new(index, self.module);
        let visibility = item.visibility();
        let declaration = Declaration::new(decl_id, item, visibility);
        self.declarations.push(declaration);
    }
}

impl OwnedAstVisitor for DeclarationCollector<'_> {
    fn visit_owned_struct(&mut self, s: loy_ast::ast::AstNodeStruct) {
        self.make_declaration(AstNode::Struct(s));
    }

    fn visit_owned_interface(&mut self, i: loy_ast::ast::AstNodeInterface) {
        self.make_declaration(AstNode::Interface(i));
    }

    fn visit_owned_function(&mut self, f: Box<loy_ast::ast::AstNodeFun>) {
        self.make_declaration(AstNode::Function(f));
    }

    fn visit_owned_enum(&mut self, e: loy_ast::ast::AstNodeEnum) {
        self.make_declaration(AstNode::Enum(e));
    }
}

pub fn module_declarations(ctx: TyCtx<'_>, module_id: ModuleId) -> Vec<Declaration> {
    let module_ast = ctx.module_ast(module_id).unwrap().steal();
    let mut collector = DeclarationCollector::new(module_id, ctx, DeclIndex::default());
    module_ast.accept_owned(&mut collector);
    collector.declarations
}