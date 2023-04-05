use super::Environment;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub enum LispValue {
    Number(i64),
    Float(f64),
    Str(String),
    Symbol(String),
    List(Vec<LispValue>),
    Function(
        String,
        Rc<dyn Fn(&[LispValue]) -> Result<LispValue, LispError>>,
    ),
    Lambda(Lambda),
}

impl PartialEq for LispValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LispValue::Number(a), LispValue::Number(b)) => a == b,
            (LispValue::Str(a), LispValue::Str(b)) => a == b,
            (LispValue::Symbol(a), LispValue::Symbol(b)) => a == b,
            (LispValue::List(a), LispValue::List(b)) => a == b,
            (LispValue::Lambda(a), LispValue::Lambda(b)) => a == b,
            (LispValue::Function(_, _), LispValue::Function(_, _)) => false,
            _ => false,
        }
    }
}

impl fmt::Debug for LispValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LispValue::Number(n) => write!(f, "Number({})", n),
            LispValue::Float(n) => write!(f, "Float({})", n),
            LispValue::Str(s) => write!(f, "Str({:?})", s),
            LispValue::Symbol(s) => write!(f, "Symbol({})", s),
            LispValue::List(l) => write!(f, "List({:?})", l),
            LispValue::Function(name, _) => write!(f, "Function({})", name),
            LispValue::Lambda(lambda) => write!(f, "Lambda({:?})", lambda),
        }
    }
}

impl fmt::Display for LispValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LispValue::Number(n) => write!(f, "Number({})", n),
            LispValue::Float(n) => write!(f, "Float({})", n),
            LispValue::Str(s) => write!(f, "Str({:?})", s),
            LispValue::Symbol(s) => write!(f, "Symbol({})", s),
            LispValue::List(l) => write!(f, "List({:?})", l),
            LispValue::Function(name, _) => write!(f, "Function({})", name),
            LispValue::Lambda(lambda) => write!(f, "Lambda({:?})", lambda),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Lambda {
    pub params: Vec<String>,
    pub body: Vec<LispValue>,
}

#[derive(Debug, PartialEq)]
pub enum LispError {
    Generic(String),
}

pub type LispResult = Result<LispValue, LispError>;
