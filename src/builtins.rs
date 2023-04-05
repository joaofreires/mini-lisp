use crate::ast::{evaluate, LispError, LispResult, LispValue};
use crate::Environment;
use std::collections::HashMap;
use std::rc::Rc;

fn add(_: &mut Environment, args: &[LispValue]) -> LispResult {
    let sum = args
        .iter()
        .map(|arg| match arg {
            LispValue::Number(n) => Ok(LispValue::Number(*n)),
            LispValue::Float(f) => Ok(LispValue::Float(*f)),
            _ => Err(LispError::Generic("Expected a number".to_string())),
        })
        .try_fold(LispValue::Number(0), |acc, n| match (acc, n?) {
            (LispValue::Number(a), LispValue::Number(b)) => Ok(LispValue::Number(a + b)),
            (LispValue::Number(a), LispValue::Float(b)) => Ok(LispValue::Float(a as f64 + b)),
            (LispValue::Float(a), LispValue::Number(b)) => Ok(LispValue::Float(a + b as f64)),
            (LispValue::Float(a), LispValue::Float(b)) => Ok(LispValue::Float(a + b)),
            _ => Err(LispError::Generic("Unexpected value type".to_string())),
        })?;
    Ok(sum)
}

fn multiply(_: &mut Environment, args: &[LispValue]) -> LispResult {
    let product = args
        .iter()
        .map(|arg| match arg {
            LispValue::Number(n) => Ok(LispValue::Number(*n)),
            LispValue::Float(f) => Ok(LispValue::Float(*f)),
            _ => Err(LispError::Generic("Expected a number".to_string())),
        })
        .try_fold(LispValue::Number(1), |acc, n| match (acc, n?) {
            (LispValue::Number(a), LispValue::Number(b)) => Ok(LispValue::Number(a * b)),
            (LispValue::Number(a), LispValue::Float(b)) => Ok(LispValue::Float(a as f64 * b)),
            (LispValue::Float(a), LispValue::Number(b)) => Ok(LispValue::Float(a * b as f64)),
            (LispValue::Float(a), LispValue::Float(b)) => Ok(LispValue::Float(a * b)),
            _ => Err(LispError::Generic("Unexpected value type".to_string())),
        })?;
    Ok(product)
}

fn divide(_: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() != 2 {
        return Err(LispError::Generic("Expected 2 arguments".to_string()));
    }
    let dividend = match args[0] {
        LispValue::Number(n) => Ok(n as f64),
        LispValue::Float(f) => Ok(f),
        _ => Err(LispError::Generic("Expected a number".to_string())),
    }?;
    let divisor = match args[1] {
        LispValue::Number(n) if n != 0 => Ok(n as f64),
        LispValue::Float(f) if f != 0.0 => Ok(f),
        _ => Err(LispError::Generic("Expected a non-zero number".to_string())),
    }?;
    Ok(LispValue::Float(dividend / divisor))
}

fn subtract(_: &mut Environment, args: &[LispValue]) -> LispResult {
    let difference = args
        .iter()
        .map(|arg| match arg {
            LispValue::Number(n) => Ok(LispValue::Number(*n)),
            LispValue::Float(f) => Ok(LispValue::Float(*f)),
            _ => Err(LispError::Generic("Expected a number".to_string())),
        })
        .try_fold(None, |acc, n| match (acc, n?) {
            (None, n) => Ok(Some(n)),
            (Some(LispValue::Number(a)), LispValue::Number(b)) => {
                Ok(Some(LispValue::Number(a - b)))
            }
            (Some(LispValue::Number(a)), LispValue::Float(b)) => {
                Ok(Some(LispValue::Float(a as f64 - b)))
            }
            (Some(LispValue::Float(a)), LispValue::Number(b)) => {
                Ok(Some(LispValue::Float(a - b as f64)))
            }
            (Some(LispValue::Float(a)), LispValue::Float(b)) => Ok(Some(LispValue::Float(a - b))),
            _ => Err(LispError::Generic("Unexpected value type".to_string())),
        })?;
    match difference {
        Some(diff) => Ok(diff),
        None => Err(LispError::Generic(
            "Expected at least 1 argument".to_string(),
        )),
    }
}

fn floor(_: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() != 1 {
        return Err(LispError::Generic("Expected 1 argument".to_string()));
    }
    let arg = match args[0] {
        LispValue::Number(n) => n as f64,
        LispValue::Float(f) => f,
        _ => return Err(LispError::Generic("Expected a number".to_string())),
    };
    Ok(LispValue::Float(arg.floor()))
}

fn ceiling(_: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() != 1 {
        return Err(LispError::Generic("Expected 1 argument".to_string()));
    }
    let arg = match args[0] {
        LispValue::Number(n) => n as f64,
        LispValue::Float(f) => f,
        _ => return Err(LispError::Generic("Expected a number".to_string())),
    };
    Ok(LispValue::Float(arg.ceil()))
}

fn truncate(_: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() != 1 {
        return Err(LispError::Generic("Expected 1 argument".to_string()));
    }
    let arg = match args[0] {
        LispValue::Number(n) => n as f64,
        LispValue::Float(f) => f,
        _ => return Err(LispError::Generic("Expected a number".to_string())),
    };
    Ok(LispValue::Float(arg.trunc()))
}

fn round(_: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() != 1 {
        return Err(LispError::Generic("Expected 1 argument".to_string()));
    }
    let arg = match args[0] {
        LispValue::Number(n) => n as f64,
        LispValue::Float(f) => f,
        _ => return Err(LispError::Generic("Expected a number".to_string())),
    };
    Ok(LispValue::Float(arg.round()))
}

fn lisp_if(env: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() != 3 {
        return Err(LispError::Generic("Expected 3 arguments".to_string()));
    }
    let condition = match args[0] {
        LispValue::Number(0) => Ok(false),
        LispValue::Number(_) => Ok(true),
        LispValue::Float(f) => match f as i64 {
            0 => Ok(false),
            _ => Ok(true),
        },
        _ => Err(LispError::Generic(
            "Expected a boolean expression".to_string(),
        )),
    }?;
    if condition {
        evaluate(args[1].clone(), env)
    } else {
        evaluate(args[2].clone(), env)
    }
}

fn lisp_cond(env: &mut Environment, args: &[LispValue]) -> LispResult {
    for arg in args {
        match arg {
            LispValue::List(pair) if pair.len() == 2 => {
                let condition = evaluate(pair[0].clone(), env)?;
                let condition_bool = match condition {
                    LispValue::Number(0) => Ok(false),
                    LispValue::Number(_) => Ok(true),
                    LispValue::Float(f) => match f as i64 {
                        0 => Ok(false),
                        _ => Ok(true),
                    },
                    _ => Err(LispError::Generic(
                        "Expected a boolean expression".to_string(),
                    )),
                }?;
                if condition_bool {
                    return evaluate(pair[1].clone(), env);
                }
            }
            _ => {
                return Err(LispError::Generic(
                    "Expected a list of length 2".to_string(),
                ))
            }
        }
    }
    Err(LispError::Generic("No true conditions".to_string()))
}

fn lisp_and(env: &mut Environment, args: &[LispValue]) -> LispResult {
    let mut result = LispValue::Number(1);
    for arg in args {
        let value = evaluate(arg.clone(), env)?;
        result = match value {
            LispValue::Number(0) => return Ok(value),
            LispValue::Float(f) => match f as i64 {
                0 => return Ok(value),
                _ => value,
            },
            _ => value,
        }
    }
    Ok(result)
}

fn equalq(_: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() != 2 {
        return Err(LispError::Generic("Expected 2 arguments".to_string()));
    }
    Ok(LispValue::Number((args[0] == args[1]) as i64))
}

pub fn built_in_functions() -> HashMap<String, LispValue> {
    let mut functions = HashMap::new();

    // Arithmetic functions
    functions.insert(
        "+".to_string(),
        LispValue::Function("+".to_string(), Rc::new(add)),
    );
    functions.insert(
        "-".to_string(),
        LispValue::Function("-".to_string(), Rc::new(subtract)),
    );
    functions.insert(
        "*".to_string(),
        LispValue::Function("*".to_string(), Rc::new(multiply)),
    );
    functions.insert(
        "/".to_string(),
        LispValue::Function("/".to_string(), Rc::new(divide)),
    );

    functions.insert(
        "floor".to_string(),
        LispValue::Function("floor".to_string(), Rc::new(floor)),
    );
    functions.insert(
        "ceiling".to_string(),
        LispValue::Function("ceiling".to_string(), Rc::new(ceiling)),
    );
    functions.insert(
        "truncate".to_string(),
        LispValue::Function("truncate".to_string(), Rc::new(truncate)),
    );
    functions.insert(
        "round".to_string(),
        LispValue::Function("round".to_string(), Rc::new(round)),
    );
    // Predicates
    functions.insert(
        "eq".to_string(),
        LispValue::Function("eq".to_string(), Rc::new(equalq)),
    );
    functions.insert(
        "if".to_string(),
        LispValue::Function("if".to_string(), Rc::new(lisp_if)),
    );
    functions.insert(
        "cond".to_string(),
        LispValue::Function("cond".to_string(), Rc::new(lisp_cond)),
    );
    functions.insert(
        "and".to_string(),
        LispValue::Function("and".to_string(), Rc::new(lisp_and)),
    );

    functions
}
