use crate::ast::Operator;

#[allow(dead_code)]
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum OperatorLocation {
    #[default]
    Infix,
    Prefix,
    Suffix,
}

pub const PRECEDENCE_BASE: u8 = 255;
const PRECEDENCE_SUFFIX: u8 = 2;
const PRECEDENCE_PREFIX: u8 = 3;
const PRECEDENCE_MUL: u8 = 5;
const PRECEDENCE_ADD: u8 = 6;
const PRECEDENCE_COMPARISON: u8 = 9;
const PRECEDENCE_EQUALITY: u8 = 10;
const PRECEDENCE_BIT_AND: u8 = 11;
const PRECEDENCE_BIT_XOR: u8 = 12;
const PRECEDENCE_BIT_OR: u8 = 13;
const PRECEDENCE_AND: u8 = 14;
const PRECEDENCE_OR: u8 = 15;
const PRECEDENCE_ASSIGNMENT: u8 = 16;

const UNREACHABLE_INFIX: &str = "operator cannot be used as an infix operator";
const UNREACHABLE_PREFIX: &str = "operator cannot be used as an prefix operator";
const UNREACHABLE_SUFFIX: &str = "operator cannot be used as an suffix operator";

pub fn op_precedence(operator: Operator, location: OperatorLocation) -> u8 {
    use Operator::*;
    match operator {
        LBrace => match location {
            OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
            OperatorLocation::Infix => unreachable!("{UNREACHABLE_INFIX}"),
            OperatorLocation::Suffix => PRECEDENCE_SUFFIX,
        },
        Minus | Plus => match location {
            OperatorLocation::Prefix => PRECEDENCE_PREFIX,
            OperatorLocation::Infix => PRECEDENCE_ADD,
            OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
        },
        Not | BitNot => match location {
            OperatorLocation::Prefix => PRECEDENCE_PREFIX,
            OperatorLocation::Infix => unreachable!("{UNREACHABLE_INFIX}"),
            OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
        },
        Star | Div | Mod => match location {
            OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
            OperatorLocation::Infix => PRECEDENCE_MUL,
            OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
        },
        Increment | Decrement => match location {
            OperatorLocation::Prefix => PRECEDENCE_PREFIX,
            OperatorLocation::Infix => unreachable!("{UNREACHABLE_INFIX}"),
            OperatorLocation::Suffix => PRECEDENCE_SUFFIX,
        },
        Equal | NotEqual => match location {
            OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
            OperatorLocation::Infix => PRECEDENCE_EQUALITY,
            OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
        },
        BitAnd => match location {
            OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
            OperatorLocation::Infix => PRECEDENCE_BIT_AND,
            OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
        },
        BitXor => match location {
            OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
            OperatorLocation::Infix => PRECEDENCE_BIT_XOR,
            OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
        },
        BitOr => match location {
            OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
            OperatorLocation::Infix => PRECEDENCE_BIT_OR,
            OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
        },
        And => match location {
            OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
            OperatorLocation::Infix => PRECEDENCE_AND,
            OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
        },
        Or => match location {
            OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
            OperatorLocation::Infix => PRECEDENCE_OR,
            OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
        },
        LParen => match location {
            OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
            OperatorLocation::Infix => unreachable!("{UNREACHABLE_INFIX}"),
            OperatorLocation::Suffix => PRECEDENCE_SUFFIX,
        },
        Dot => match location {
            OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
            OperatorLocation::Infix => PRECEDENCE_SUFFIX,
            OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
        },
        LBracket => match location {
            OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
            OperatorLocation::Infix => PRECEDENCE_SUFFIX,
            OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
        },
        Lesser | Greater | LesserEqual | GreaterEqual => match location {
            OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
            OperatorLocation::Infix => PRECEDENCE_COMPARISON,
            OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
        },
        Assign | PlusAssign | MinusAssign | MulAssign | DivAssign | ModAssign | BitAndAssign
        | BitOrAssign | BitXorAssign | LShiftAssign | RShiftAssign => match location {
            OperatorLocation::Prefix => unreachable!("{UNREACHABLE_PREFIX}"),
            OperatorLocation::Infix => PRECEDENCE_ASSIGNMENT,
            OperatorLocation::Suffix => unreachable!("{UNREACHABLE_SUFFIX}"),
        },
    }
}
