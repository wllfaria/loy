use std::path::PathBuf;

use loy_ast::result::Result;
use loy_context::modules::{ModuleId, ModuleMap};
use loy_context::query::QueryEngine;
use loy_context::{GlobalCtx, TyCtx};

pub struct Driver<'ctx> {
    pub gcx: GlobalCtx<'ctx>,
}

impl<'ctx> Driver<'ctx> {
    pub fn new() -> Self {
        let mut gcx = GlobalCtx {
            module_map: ModuleMap::default(),
            query_engine: QueryEngine::default(),
        };

        loy_lexer::provide(&mut gcx.query_engine.providers);
        loy_parser::provide(&mut gcx.query_engine.providers);
        gcx.query_engine.providers.compile_module = compile_module;

        Self { gcx }
    }

    pub fn compile(&mut self, files: Vec<PathBuf>) -> Result<()> {
        for file in files {
            let module_id = self.gcx.module_map.add_file(file);
            let tcx = TyCtx::new(&self.gcx);
            tcx.compile_module(module_id)?;
        }

        Ok(())
    }
}

impl<'ctx> Default for Driver<'ctx> {
    fn default() -> Self {
        Self::new()
    }
}

fn compile_module(tcx: TyCtx<'_>, module_id: ModuleId) -> Result<()> {
    println!("Compiled module {module_id:?}");
    Ok(())
}