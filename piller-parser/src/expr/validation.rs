use crate::ast::{Expr, Operator};

#[derive(Debug, Clone)]
pub struct OperatorValidation {
    pub valid_postfix: &'static [Operator],
}

pub fn get_operator_context(lhs: &Expr) -> OperatorValidation {
    use Expr::*;
    use Operator::*;

    match lhs {
        Ident(_) => OperatorValidation {
            valid_postfix: &[LParen, LBracket, Dot, Increment, Decrement, LBrace],
        },
        Number(_) => OperatorValidation {
            valid_postfix: &[Increment, Decrement],
        },
        Binary(_) | Unary(_) => OperatorValidation {
            valid_postfix: &[Increment, Decrement, LBracket, Dot],
        },
        Block(_) => OperatorValidation {
            valid_postfix: &[Increment, Decrement, LBracket, Dot],
        },
        FunctionCall(_) => OperatorValidation {
            valid_postfix: &[Increment, Decrement, LBracket, Dot, LParen],
        },
        ArrayAccess(_) => OperatorValidation {
            valid_postfix: &[Increment, Decrement, LBracket, Dot, LParen],
        },
        MemberAccess(_) => OperatorValidation {
            valid_postfix: &[Increment, Decrement, LBracket, Dot, LParen],
        },
        Binding(_) => OperatorValidation { valid_postfix: &[] },
        If(_) | SemiColon(_) => OperatorValidation { valid_postfix: &[] },
        Bool(_) | While(_) | For(_) | String(_) => OperatorValidation { valid_postfix: &[] },
        Array(_) => OperatorValidation {
            valid_postfix: &[Increment, Decrement, LBracket, Dot, LParen],
        },
        StructInit(_) => OperatorValidation { valid_postfix: &[] },
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