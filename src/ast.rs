use crate::prelude::Environment;
use std::collections::VecDeque;
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
        Rc<dyn Fn(&mut Environment, &[LispValue]) -> Result<LispValue, LispError>>,
    ),
    Lambda(Lambda),
    Pair(Box<(LispValue, LispValue)>),
    Nil(),
}

impl PartialEq for LispValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LispValue::Number(a), LispValue::Number(b)) => a == b,
            (LispValue::Float(a), LispValue::Float(b)) => a == b,
            (LispValue::Str(a), LispValue::Str(b)) => a == b,
            (LispValue::Symbol(a), LispValue::Symbol(b)) => a == b,
            (LispValue::List(a), LispValue::List(b)) => a == b,
            (LispValue::Lambda(a), LispValue::Lambda(b)) => a == b,
            (LispValue::Function(_, _), LispValue::Function(_, _)) => false,
            (LispValue::Nil(), LispValue::Nil()) => true,
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
            LispValue::Nil() => write!(f, "Nil()"),
            LispValue::Pair(pair) => {
                let (first, second) = *(pair.clone());
                write!(f, "Pair<{:?}, {:?}>", first.to_string(), second.to_string())
            }
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
            LispValue::Nil() => write!(f, "Nil()"),
            LispValue::Pair(pair) => {
                let (first, second) = *(pair.clone());
                write!(f, "Pair<{:?}, {:?}>", first.to_string(), second.to_string())
            }
        }
    }
}

impl IntoIterator for LispValue {
    type IntoIter = IntoIter;
    type Item = LispValue;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            LispValue::Pair(pair) => IntoIter {
                remaining: VecDeque::from([pair.0, pair.1]),
            },
            _ => IntoIter {
                remaining: VecDeque::from([self]),
            },
        }
    }
}

pub struct IntoIter {
    remaining: VecDeque<LispValue>,
}

impl Iterator for IntoIter {
    type Item = LispValue;

    fn next(&mut self) -> Option<Self::Item> {
        self.remaining.pop_front()
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

pub fn evaluate(expr: LispValue, env: &mut Environment) -> LispResult {
    match expr {
        LispValue::Number(_) | LispValue::Float(_) | LispValue::Str(_) => Ok(expr),
        LispValue::Symbol(name) => env.get(&name),
        LispValue::List(list) => evaluate_list(list, env),
        LispValue::Function(_, _) | LispValue::Lambda(_) | LispValue::Pair(_) => Err(
            LispError::Generic("Cannot evaluate a function or lambda directly".to_string()),
        ),
        LispValue::Nil() => Ok(LispValue::Nil()),
    }
}

fn evaluate_list(list: Vec<LispValue>, env: &mut Environment) -> LispResult {
    if list.is_empty() {
        return Err(LispError::Generic(
            "Cannot evaluate an empty list".to_string(),
        ));
    }

    let (first, args) = list.split_first().unwrap();
    match first {
        LispValue::Symbol(sym) if sym == "lambda" => {
            if args.len() != 2 {
                return Err(LispError::Generic(format!(
                    "Invalid number of arguments for lambda, expected 2, got {}",
                    args.len()
                )));
            }

            let params = match &args[0] {
                LispValue::List(params) => params
                    .iter()
                    .map(|p| match p {
                        LispValue::Symbol(s) => Ok(s.clone()),
                        _ => Err(LispError::Generic(
                            "Invalid parameter type, expected symbol".to_string(),
                        )),
                    })
                    .collect::<Result<Vec<String>, LispError>>()?,
                _ => {
                    return Err(LispError::Generic(
                        "Invalid argument type, expected list of symbols".to_string(),
                    ))
                }
            };

            let body = args[1].clone();
            Ok(LispValue::Lambda(Lambda {
                params,
                body: vec![body],
            }))
        }
        _ => {
            let function = evaluate(first.clone(), env)?;
            let evaluated_args = args
                .into_iter()
                .map(|arg| evaluate(arg.clone(), env))
                .collect::<Result<Vec<LispValue>, LispError>>()?;
            apply_function(function, &evaluated_args, env)
        }
    }
}

fn evaluate_lambda(lambda: Lambda, args: &[LispValue], env: &mut Environment) -> LispResult {
    if lambda.params.len() != args.len() {
        return Err(LispError::Generic(format!(
            "Invalid number of arguments, expected {}, got {}",
            lambda.params.len(),
            args.len()
        )));
    }

    let mut child_env = env.child(); // slow, but it works for now...
    for (param, arg) in lambda.params.iter().zip(args) {
        child_env.set(param.clone(), arg.clone());
    }

    let mut result = Err(LispError::Generic("Empty body in lambda".to_string()));

    for expr in &lambda.body {
        result = evaluate(expr.clone(), &mut child_env);
    }

    result
}

fn apply_function(function: LispValue, args: &[LispValue], env: &mut Environment) -> LispResult {
    match function {
        LispValue::Function(_, func) => func(env, args),
        LispValue::Lambda(lambda) => evaluate_lambda(
            lambda,
            &*(Vec::from(args)
                .into_iter()
                .flatten()
                .collect::<Vec<LispValue>>()),
            env, // lambda functions receive arguments unpacked
        ),
        _ => {
            let mut v = Vec::from([function]);
            v.extend(args.to_vec());
            Ok(LispValue::List(v))
        }
    }
}
