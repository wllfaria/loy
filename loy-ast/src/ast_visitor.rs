use std::fmt::Debug;

use crate::ast::*;

#[allow(unused_variables)]
pub trait AstVisitor: Debug {
    fn visit_import(&mut self, i: &AstNodeImport) {}
    fn visit_interface(&mut self, i: &AstNodeInterface) {}
    fn visit_function(&mut self, f: &AstNodeFun) {}
    fn visit_enum(&mut self, e: &AstNodeEnum) {}
    fn visit_struct(&mut self, s: &AstNodeStruct) {}
}

#[allow(unused_variables)]
pub trait OwnedAstVisitor: Debug {
    fn visit_owned_import(&mut self, i: AstNodeImport) {}
    fn visit_owned_interface(&mut self, i: AstNodeInterface) {}
    fn visit_owned_function(&mut self, f: Box<AstNodeFun>) {}
    fn visit_owned_enum(&mut self, e: AstNodeEnum) {}
    fn visit_owned_struct(&mut self, s: AstNodeStruct) {}
}