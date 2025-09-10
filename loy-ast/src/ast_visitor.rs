use std::fmt::Debug;

use crate::ast::*;

#[allow(unused_variables)]
pub trait AstVisitor: Debug {
    fn visit_import(&mut self, import: &AstNodeImport) {}
    fn visit_interface(&mut self, import: &AstNodeInterface) {}
    fn visit_function(&mut self, import: &AstNodeFun) {}
    fn visit_enum(&mut self, import: &AstNodeEnum) {}
    fn visit_struct(&mut self, import: &AstNodeStruct) {}
    fn visit_typedef(&mut self, import: &AstNodeTypeDef) {}
}