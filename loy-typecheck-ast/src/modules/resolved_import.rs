use loy_ast::token::Span;

use super::ModuleId;

#[derive(Debug)]
pub struct ResolvedImport {
    pub target_module: ModuleId,
    pub symbols: ImportedSymbols,
    pub alias: Option<Span>,
}

#[derive(Debug)]
pub enum ImportedSymbols {
    All(EntireModuleImport),
    Specific(SpecificModuleImport),
}

#[derive(Debug)]
pub struct EntireModuleImport {
    pub alias: Option<Span>,
}

#[derive(Debug)]
pub struct SpecificModuleImport {
    pub alias: Option<Span>,
    pub methods: Vec<SymbolImport>,
}

#[derive(Debug)]
pub struct SymbolImport {
    pub name: Span,
    pub alias: Option<Span>,
}
