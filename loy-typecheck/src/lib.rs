mod module;

use std::path::PathBuf;
use std::sync::Arc;

use loy_ast::ast::AstNodeImport;
use loy_ast::ast_visitor::AstVisitor;
use loy_context::TyCtx;
use loy_context::query::QueryProviders;
use loy_typecheck_ast::modules::{
    EntireModuleImport, ImportedSymbols, ModuleId, ResolvedImport, SpecificModuleImport,
    SymbolImport,
};

use crate::module::declarations::module_declarations;
use crate::module::exports::module_exports;

pub fn provide(providers: &mut QueryProviders) {
    providers.module_imports = module_imports;
    providers.module_declarations = module_declarations;
    providers.module_exports = module_exports
}

#[derive(Debug)]
pub struct ImportCollector<'ctx> {
    ctx: TyCtx<'ctx>,
    module_id: ModuleId,
    source: Arc<String>,
    imports: Vec<ResolvedImport>,
}

impl<'ctx> ImportCollector<'ctx> {
    pub fn new(ctx: TyCtx<'ctx>, module_id: ModuleId, source: Arc<String>) -> Self {
        Self {
            ctx,
            module_id,
            source,
            imports: vec![],
        }
    }

    fn extract_path_string(&self, import: &AstNodeImport) -> &str {
        let path_text = &self.source[import.path.position.into_range()];
        path_text.trim_matches('"') // remove quotes from path string
    }

    fn resolve_path(&self, relative_path: &str) -> PathBuf {
        let current_module_info = self
            .ctx
            .get_module_info(self.module_id)
            .expect("current module should exist");

        let current_dir = current_module_info
            .absolute_path
            .parent()
            .expect("module should have parent directory");

        let path_with_extension = if relative_path.ends_with(".loy") {
            relative_path.to_string()
        } else {
            format!("{relative_path}.loy")
        };

        let absolute_path = current_dir.join(path_with_extension);
        absolute_path.canonicalize().unwrap_or(absolute_path) // fallback if canonicalize fails
    }

    fn parse_symbols(&self, import: &AstNodeImport) -> ImportedSymbols {
        let Some(methods) = &import.methods else {
            // no specific methods to import, so import the whole module
            return ImportedSymbols::All(EntireModuleImport {
                alias: import.alias.as_ref().map(|alias| alias.position),
            });
        };

        let symbol_imports: Vec<SymbolImport> = methods
            .methods
            .iter()
            .map(|method| SymbolImport {
                name: method.name.position,
                alias: method.alias.map(|alias| alias.position),
            })
            .collect();

        ImportedSymbols::Specific(SpecificModuleImport {
            alias: import.alias.as_ref().map(|alias| alias.position),
            methods: symbol_imports,
        })
    }
}

impl AstVisitor for ImportCollector<'_> {
    fn visit_import(&mut self, import: &AstNodeImport) {
        let path_string = self.extract_path_string(import);
        let absolute_path = self.resolve_path(path_string);
        let target_module = self.ctx.register_module(absolute_path);
        let symbols = self.parse_symbols(import);
        let alias = import.alias.as_ref().map(|alias| alias.position);
        let import = ResolvedImport::new(target_module, symbols, alias);
        self.imports.push(import);
    }
}

fn module_imports(ctx: TyCtx<'_>, module_id: ModuleId) -> Arc<Vec<ResolvedImport>> {
    let ast = ctx.module_ast(module_id).unwrap();
    let ast = ast.borrow();
    let source = ctx.get_module_source(module_id);
    let mut visitor = ImportCollector::new(ctx, module_id, source.clone());
    ast.accept(&mut visitor);
    Arc::new(visitor.imports)
}
