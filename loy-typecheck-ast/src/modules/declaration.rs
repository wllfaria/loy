use loy_ast::ast::{AstNode, NodeVisibility};

use crate::modules::ModuleId;

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct DeclIndex(u32);

impl DeclIndex {
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> DeclIndex {
        let current = *self;
        self.0 += 1;
        current
    }
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct DeclId {
    pub index: DeclIndex,
    pub module: ModuleId,
}

impl DeclId {
    pub fn new(index: DeclIndex, module: ModuleId) -> Self {
        Self { index, module }
    }
}

#[derive(Debug)]
pub struct Declaration {
    pub id: DeclId,
    pub node: AstNode,
    pub visibility: NodeVisibility,
}

impl Declaration {
    pub fn new(id: DeclId, node: AstNode, visibility: NodeVisibility) -> Self {
        Self {
            id,
            node,
            visibility,
        }
    }
}