use crate::prelude::Environment;
use std::cmp::Ordering;
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

impl PartialOrd for LispValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (LispValue::Number(a), LispValue::Number(b)) => Some(a.cmp(b)),
            (LispValue::Float(a), LispValue::Float(b)) => Some(a.total_cmp(b)),
            (LispValue::Number(a), LispValue::Float(b)) => Some(b.total_cmp(&(*a as f64))),
            (LispValue::Float(a), LispValue::Number(b)) => Some(a.total_cmp(&(*b as f64))),
            (LispValue::Str(a), LispValue::Str(b)) => Some(a.cmp(b)),
            (LispValue::Symbol(a), LispValue::Symbol(b)) => Some(a.cmp(b)),
            (LispValue::List(_), LispValue::List(_)) => None,
            (LispValue::Lambda(_), LispValue::Lambda(_)) => None,
            (LispValue::Function(_, _), LispValue::Function(_, _)) => None,
            (LispValue::Nil(), LispValue::Nil()) => None,
            _ => None,
        }
    }

    fn lt(&self, other: &Self) -> bool {
        match (self, other) {
            (LispValue::Number(a), LispValue::Number(b)) => a < b,
            (LispValue::Float(a), LispValue::Float(b)) => a < b,
            (LispValue::Str(a), LispValue::Str(b)) => a < b,
            (LispValue::Symbol(a), LispValue::Symbol(b)) => a < b,
            (LispValue::List(a), LispValue::List(b)) => a < b,
            (LispValue::Lambda(_), LispValue::Lambda(_)) => false,
            (LispValue::Function(_, _), LispValue::Function(_, _)) => false,
            (LispValue::Nil(), LispValue::Nil()) => false,
            _ => false,
        }
    }
    fn le(&self, other: &Self) -> bool {
        match (self, other) {
            (LispValue::Number(a), LispValue::Number(b)) => a <= b,
            (LispValue::Float(a), LispValue::Float(b)) => a <= b,
            (LispValue::Str(a), LispValue::Str(b)) => a <= b,
            (LispValue::Symbol(a), LispValue::Symbol(b)) => a <= b,
            (LispValue::List(a), LispValue::List(b)) => a <= b,
            (LispValue::Lambda(_), LispValue::Lambda(_)) => false,
            (LispValue::Function(_, _), LispValue::Function(_, _)) => false,
            (LispValue::Nil(), LispValue::Nil()) => false,
            _ => false,
        }
    }
    fn gt(&self, other: &Self) -> bool {
        match (self, other) {
            (LispValue::Number(a), LispValue::Number(b)) => a > b,
            (LispValue::Float(a), LispValue::Float(b)) => a > b,
            (LispValue::Str(a), LispValue::Str(b)) => a > b,
            (LispValue::Symbol(a), LispValue::Symbol(b)) => a > b,
            (LispValue::List(a), LispValue::List(b)) => a > b,
            (LispValue::Lambda(_), LispValue::Lambda(_)) => false,
            (LispValue::Function(_, _), LispValue::Function(_, _)) => false,
            (LispValue::Nil(), LispValue::Nil()) => false,
            _ => false,
        }
    }
    fn ge(&self, other: &Self) -> bool {
        match (self, other) {
            (LispValue::Number(a), LispValue::Number(b)) => a >= b,
            (LispValue::Float(a), LispValue::Float(b)) => a >= b,
            (LispValue::Str(a), LispValue::Str(b)) => a >= b,
            (LispValue::Symbol(a), LispValue::Symbol(b)) => a >= b,
            (LispValue::List(a), LispValue::List(b)) => a >= b,
            (LispValue::Lambda(_), LispValue::Lambda(_)) => false,
            (LispValue::Function(_, _), LispValue::Function(_, _)) => false,
            (LispValue::Nil(), LispValue::Nil()) => false,
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
            LispValue::List(list) => IntoIter {
                remaining: VecDeque::from(list),
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
    EndOfExpression(),
    EndOfInput(),
}

pub type LispResult = Result<LispValue, LispError>;

pub fn evaluate(expr: LispValue, env: &mut Environment) -> LispResult {
    match expr {
        LispValue::Number(_) | LispValue::Float(_) | LispValue::Str(_) => Ok(expr),
        LispValue::Symbol(name) => match env.get(&name)? {
            LispValue::Function(_, _) => Ok(env.get(&name)?),
            _ => evaluate(env.get(&name)?, env),
        },
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
        LispValue::Symbol(sym) if sym == "set" => {
            if args.len() != 2 {
                return Err(LispError::Generic(format!(
                    "Invalid number of arguments for set, expected 2, got {}",
                    args.len()
                )));
            }
            match &args[0] {
                LispValue::Symbol(symbol) => {
                    if env.has(symbol) {
                        return Err(LispError::Generic(format!(
                            "Symbol '{}' already defined in this context",
                            symbol,
                        )));
                    } else {
                        let result = args[1].clone();
                        env.set(symbol.to_string(), result.clone());
                        return match result {
                            LispValue::Number(_) | LispValue::Float(_) | LispValue::Str(_) => {
                                Ok(result)
                            }
                            _ => Ok(LispValue::Nil()),
                        };
                    }
                }
                _ => Err(LispError::Generic(
                    "variable name must be a symbol".to_string(),
                )),
            }
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
    let (fixed_params, variadic) = split_params(&lambda.params);

    println!("{:?}", args);

    if !valid_argument_count(args.len(), fixed_params.len(), variadic) {
        return Err(LispError::Generic(format!(
            "Invalid number of arguments, expected {} required arguments, got {}",
            fixed_params.len(),
            args.len()
        )));
    }

    if let Err(err) = validate_variadic_position(&lambda.params) {
        return Err(err);
    }

    let mut child_env = env.child();
    bind_parameters(&fixed_params, args, &mut child_env);

    let lambda_body = unpack_lambda_body(&lambda.body, fixed_params.len(), args);
    evaluate(LispValue::List(lambda_body), &mut child_env)
}

fn split_params(params: &[String]) -> (Vec<String>, bool) {
    let fixed_params: Vec<String> = params.iter().filter(|x| x != &&"...").cloned().collect();
    let variadic = params.len() > fixed_params.len();
    (fixed_params, variadic)
}

fn valid_argument_count(arg_count: usize, fixed_param_count: usize, variadic: bool) -> bool {
    (arg_count == fixed_param_count && !variadic) || (arg_count >= fixed_param_count && variadic)
}

fn bind_parameters(fixed_params: &[String], args: &[LispValue], child_env: &mut Environment) {
    for (param, arg) in fixed_params.iter().zip(args) {
        child_env.set(param.clone(), arg.clone());
    }
}

fn unpack_lambda_body(
    lambda_body: &[LispValue],
    fixed_params_len: usize,
    args: &[LispValue],
) -> Vec<LispValue> {
    let mut lambda_body_q = VecDeque::from(lambda_body.to_vec());
    let mut unpacked_lambda_body = Vec::new();

    while !lambda_body_q.is_empty() {
        let item = lambda_body_q.pop_front().unwrap();
        match item {
            LispValue::Symbol(value) if value == "..." => {
                for arg in args.iter().skip(fixed_params_len) {
                    unpacked_lambda_body.push(arg.clone());
                }
            }
            LispValue::List(items) => items
                .iter()
                .rev()
                .for_each(|item| lambda_body_q.push_front(item.clone())),
            _ => unpacked_lambda_body.push(item),
        }
    }
    println!("{:?}", unpacked_lambda_body);
    unpacked_lambda_body
}

fn validate_variadic_position(params: &[String]) -> Result<(), LispError> {
    if let Some((idx, _)) = params.iter().enumerate().find(|(_, p)| p == &&"...") {
        if idx != params.len() - 1 {
            return Err(LispError::Generic(
                "Variadic argument '...' must be the last parameter".to_string(),
            ));
        }
    }
    Ok(())
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
