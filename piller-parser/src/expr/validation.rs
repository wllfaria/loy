use crate::ast::{AstNode, Operator};

#[derive(Debug, Clone)]
pub struct OperatorValidation {
    pub valid_postfix: &'static [Operator],
    pub valid_prefix: &'static [Operator],
}

pub fn get_operator_context(lhs: &AstNode) -> OperatorValidation {
    use AstNode::*;
    use Operator::*;

    match lhs {
        Ident(_) => OperatorValidation {
            valid_postfix: &[LParen, LBracket, Dot, Increment, Decrement],
            valid_prefix: &[],
        },
        Number(_) => OperatorValidation {
            valid_postfix: &[Increment, Decrement],
            valid_prefix: &[],
        },
        Binary(_) | Unary(_) => OperatorValidation {
            valid_postfix: &[Increment, Decrement, LBracket, Dot],
            valid_prefix: &[],
        },
        Block(_) => OperatorValidation {
            valid_postfix: &[Increment, Decrement, LBracket, Dot],
            valid_prefix: &[],
        },
        FunctionCall(_) => OperatorValidation {
            valid_postfix: &[Increment, Decrement, LBracket, Dot, LParen],
            valid_prefix: &[],
        },
        ArrayAccess(_) => OperatorValidation {
            valid_postfix: &[Increment, Decrement, LBracket, Dot, LParen],
            valid_prefix: &[],
        },
        MemberAccess(_) => OperatorValidation {
            valid_postfix: &[Increment, Decrement, LBracket, Dot, LParen],
            valid_prefix: &[],
        },
        Binding(_) => OperatorValidation {
            valid_postfix: &[],
            valid_prefix: &[],
        },
        Struct(_) | Function(_) | TypeDecl(_) => OperatorValidation {
            valid_postfix: &[],
            valid_prefix: &[],
        },
    }
}

pub fn get_valid_prefix_operators() -> &'static [Operator] {
    &[
        Operator::Increment, // ++a
        Operator::Decrement, // --a
        Operator::Minus,     // -a
        Operator::Plus,      // +a
        Operator::Not,       // !a
        Operator::BitNot,    // ~a
    ]
}
