use crate::ast::{evaluate, LispError, LispResult, LispValue};
use crate::prelude::Environment;
use std::collections::HashMap;
use std::rc::Rc;

fn expand_pairs_args(args: &[LispValue]) -> Vec<LispValue> {
    Vec::from(args).into_iter().flatten().collect()
}

fn add(_: &mut Environment, args: &[LispValue]) -> LispResult {
    let parsed_args = expand_pairs_args(args);
    let sum = parsed_args
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

fn divide(_: &mut Environment, uargs: &[LispValue]) -> LispResult {
    let args = expand_pairs_args(uargs);
    if args.len() < 2 {
        return Err(LispError::Generic("Expected 2 arguments".to_string()));
    }
    let mut dividend = match args[0] {
        LispValue::Number(n) => Ok(n as f64),
        LispValue::Float(f) => Ok(f),
        _ => Err(LispError::Generic("Expected a number".to_string())),
    }?;
    for arg in args[1..].iter() {
        let divisor = match *arg {
            LispValue::Number(n) if n != 0 => Ok(n as f64),
            LispValue::Float(f) if f != 0.0 => Ok(f),
            _ => Err(LispError::Generic("Expected a non-zero number".to_string())),
        }?;
        dividend = dividend / divisor;
    }
    Ok(LispValue::Float(dividend))
}

fn minus(_: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() == 1 {
        return match args[0] {
            LispValue::Number(n) => Ok(LispValue::Number(-n)),
            LispValue::Float(f) => Ok(LispValue::Float(-f)),
            _ => return Err(LispError::Generic("Expected a number".to_string())),
        };
    }
    let mut vargs = args.to_vec();
    for i in 0..(vargs.len()) {
        match vargs[i] {
            LispValue::Number(n) => vargs[i] = LispValue::Number(-n),
            LispValue::Float(f) => vargs[i] = LispValue::Float(-f),
            _ => return Err(LispError::Generic("Expected a number".to_string())),
        }
    }
    Ok(LispValue::List(vargs))
}

fn subtract(env: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() == 1 {
        return minus(env, args);
    }
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
    Ok(LispValue::Nil())
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
    if args.len() == 1 {
        return Ok(LispValue::Number(1));
    }
    let mut iter = args.into_iter();
    let first = iter.next().ok_or(LispError::Generic(
        "Equal operation needs arguments".to_string(),
    ))?;
    Ok(LispValue::Number(iter.all(|elem| elem == first) as i64))
}

fn gt(_: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() != 2 {
        return Err(LispError::Generic("Expected 2 arguments".to_string()));
    }
    Ok(LispValue::Number((args[0] > args[1]) as i64))
}

fn gte(_: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() != 2 {
        return Err(LispError::Generic("Expected 2 arguments".to_string()));
    }
    Ok(LispValue::Number((args[0] >= args[1]) as i64))
}

fn lt(_: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() != 2 {
        return Err(LispError::Generic("Expected 2 arguments".to_string()));
    }
    Ok(LispValue::Number((args[0] < args[1]) as i64))
}

fn lte(_: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() != 2 {
        return Err(LispError::Generic("Expected 2 arguments".to_string()));
    }
    Ok(LispValue::Number((args[0] <= args[1]) as i64))
}

fn cons(_: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() != 2 {
        return Err(LispError::Generic("Expected 2 arguments".to_string()));
    }
    Ok(LispValue::Pair(Box::new((
        args[0].clone(),
        args[1].clone(),
    ))))
}

fn car(_: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() != 1 {
        return Err(LispError::Generic("Expected 1 arguments".to_string()));
    }
    match &args[0] {
        LispValue::Pair(value) => Ok(value.0.clone()),
        value => Err(LispError::Generic(format!(
            "Expected cons structure found {:?}",
            value
        ))),
    }
}

fn cdr(_: &mut Environment, args: &[LispValue]) -> LispResult {
    if args.len() != 1 {
        return Err(LispError::Generic("Expected 1 arguments".to_string()));
    }
    match &args[0] {
        LispValue::Pair(value) => Ok(value.1.clone()),
        value => Err(LispError::Generic(format!(
            "Expected cons structure found {:?}",
            value
        ))),
    }
}

fn list(_: &mut Environment, args: &[LispValue]) -> LispResult {
    let v = Vec::from(args);
    Ok(LispValue::List(v))
}

fn lisp_else(_: &mut Environment, args: &[LispValue]) -> LispResult {
    let mut v = Vec::new();
    if args.len() == 0 {
        v.push(LispValue::Number(1));
    }
    v.extend(args.to_vec());
    Ok(LispValue::List(v))
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
        "cons".to_string(),
        LispValue::Function("cons".to_string(), Rc::new(cons)),
    );
    functions.insert(
        "=".to_string(),
        LispValue::Function("=".to_string(), Rc::new(equalq)),
    );
    functions.insert(
        "equal?".to_string(),
        LispValue::Function("equal?".to_string(), Rc::new(equalq)),
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
        "else".to_string(),
        LispValue::Function("else".to_string(), Rc::new(lisp_else)),
    );
    functions.insert(
        "and".to_string(),
        LispValue::Function("and".to_string(), Rc::new(lisp_and)),
    );
    functions.insert(
        "car".to_string(),
        LispValue::Function("car".to_string(), Rc::new(car)),
    );
    functions.insert(
        "cdr".to_string(),
        LispValue::Function("cdr".to_string(), Rc::new(cdr)),
    );
    functions.insert(
        "neg".to_string(),
        LispValue::Function("neg".to_string(), Rc::new(minus)),
    );
    functions.insert(
        ">".to_string(),
        LispValue::Function(">".to_string(), Rc::new(gt)),
    );
    functions.insert(
        ">=".to_string(),
        LispValue::Function(">=".to_string(), Rc::new(gte)),
    );
    functions.insert(
        "<".to_string(),
        LispValue::Function("<".to_string(), Rc::new(lt)),
    );
    functions.insert(
        "<=".to_string(),
        LispValue::Function("<=".to_string(), Rc::new(lte)),
    );
    functions.insert(
        "list".to_string(),
        LispValue::Function("list".to_string(), Rc::new(list)),
    );

    functions
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::evaluate;
    use crate::parser::parse;
    use crate::prelude::Environment;

    fn create_environment() -> Environment {
        let mut env = Environment::default();
        env.init();
        env
    }

    macro_rules! number {
        ($n:expr) => {
            LispValue::Number($n)
        };
    }

    macro_rules! float {
        ($f:expr) => {
            LispValue::Float($f)
        };
    }

    macro_rules! args {
        ($($arg:expr),*) => {
            vec![$($arg),*].as_slice()
        };
    }

    #[test]
    fn test_add() {
        let mut env = Environment::new();
        env.init();
        let input = parse("(+ 1 2 3)").unwrap();
        let expected = LispValue::Number(6);
        let result = evaluate(input, &mut env).unwrap();
        assert_eq!(result, expected);

        let input = parse("(+ 1.0 2)").unwrap();
        let expected = LispValue::Float(3.0);
        let result = evaluate(input, &mut env).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_subtract() {
        let mut env = Environment::new();
        env.init();
        let input = parse("(- 10 2 3)").unwrap();
        let expected = LispValue::Number(5);
        let result = evaluate(input, &mut env).unwrap();
        assert_eq!(result, expected);

        let input = parse("(- 10.0 3)").unwrap();
        let expected = LispValue::Float(7.0);
        let result = evaluate(input, &mut env).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_multiply() {
        let mut env = Environment::new();
        env.init();
        let input = parse("(* 2 3 4)").unwrap();
        let expected = LispValue::Number(24);
        let result = evaluate(input, &mut env).unwrap();
        assert_eq!(result, expected);

        let input = parse("(* 2.0 3)").unwrap();
        let expected = LispValue::Float(6.0);
        let result = evaluate(input, &mut env).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_divide() {
        let mut env = Environment::new();
        env.init();
        let input = parse("(/ 10 2)").unwrap();
        let expected = LispValue::Float(5.0);
        let result = evaluate(input, &mut env).unwrap();
        assert_eq!(result, expected);

        let input = parse("(/ 10.0 2)").unwrap();
        let expected = LispValue::Float(5.0);
        let result = evaluate(input, &mut env).unwrap();
        assert_eq!(result, expected);

        let input = parse("(/ 10 0)").unwrap();
        let result = evaluate(input, &mut env);
        assert!(result.is_err());

        let input = parse("(/ 10.0 0)").unwrap();
        let result = evaluate(input, &mut env);
        assert!(result.is_err());
    }

    #[test]
    fn test_floor() {
        let mut env = Environment::new();
        env.init();
        let input = parse("(floor 1.5)").unwrap();
        let expected = LispValue::Float(1.0);
        let result = evaluate(input, &mut env).unwrap();
        assert_eq!(result, expected);

        let input = parse("(floor (- 1.5))").unwrap();
        let expected = LispValue::Float(-2.0);
        let result = evaluate(input, &mut env).unwrap();
        assert_eq!(result, expected);

        let input = parse("(floor 10)").unwrap();
        let expected = LispValue::Float(10.0);
        let result = evaluate(input, &mut env).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_ceiling() {
        let mut env = create_environment();

        assert_eq!(ceiling(&mut env, args![number!(5)]).unwrap(), float!(5.0));
        assert_eq!(ceiling(&mut env, args![number!(-5)]).unwrap(), float!(-5.0));
        assert_eq!(ceiling(&mut env, args![float!(3.3)]).unwrap(), float!(4.0));
        assert_eq!(
            ceiling(&mut env, args![float!(-3.3)]).unwrap(),
            float!(-3.0)
        );
    }

    #[test]
    fn test_truncate() {
        let mut env = create_environment();

        assert_eq!(truncate(&mut env, args![number!(5)]).unwrap(), float!(5.0));
        assert_eq!(
            truncate(&mut env, args![number!(-5)]).unwrap(),
            float!(-5.0)
        );
        assert_eq!(truncate(&mut env, args![float!(3.3)]).unwrap(), float!(3.0));
        assert_eq!(
            truncate(&mut env, args![float!(-3.3)]).unwrap(),
            float!(-3.0)
        );
    }

    #[test]
    fn test_round() {
        let mut env = create_environment();

        assert_eq!(round(&mut env, args![number!(5)]).unwrap(), float!(5.0));
        assert_eq!(round(&mut env, args![number!(-5)]).unwrap(), float!(-5.0));
        assert_eq!(round(&mut env, args![float!(3.3)]).unwrap(), float!(3.0));
        assert_eq!(round(&mut env, args![float!(-3.3)]).unwrap(), float!(-3.0));
        assert_eq!(round(&mut env, args![float!(3.5)]).unwrap(), float!(4.0));
        assert_eq!(round(&mut env, args![float!(-3.5)]).unwrap(), float!(-4.0));
    }

    // Test 'cond' function
    #[test]
    fn test_lisp_cond() {
        let mut env = create_environment();
        let result = evaluate(
            parse("(cond ((= 1 2) \"no\") ((> 2 1) \"yes\"))").unwrap(),
            &mut env,
        )
        .unwrap();
        assert_eq!(result, LispValue::Str("yes".to_string()));
    }

    // Test 'and' function
    #[test]
    fn test_lisp_and() {
        let mut env = create_environment();
        let result = evaluate(parse("(and 1 1)").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(1));

        let result = evaluate(parse("(and 1 0)").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(0));
    }

    // Test 'equal?' function
    #[test]
    fn test_equalq() {
        let mut env = create_environment();
        let result = evaluate(parse("(equal? 1 1)").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(1));

        let result = evaluate(parse("(equal? 1 2)").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(0));
    }

    // Test '>' function
    #[test]
    fn test_gt() {
        let mut env = create_environment();
        let result = evaluate(parse("(> 2 1)").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(1));

        let result = evaluate(parse("(> 1 2)").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(0));
    }

    // Test '>=' function
    #[test]
    fn test_gte() {
        let mut env = create_environment();
        let result = evaluate(parse("(>= 2 1)").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(1));

        let result = evaluate(parse("(>= 1 1)").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(1));

        let result = evaluate(parse("(>= 1 2)").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(0));
    }

    // Test '<' function
    #[test]
    fn test_lt() {
        let mut env = create_environment();
        let result = evaluate(parse("(< 1 2)").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(1));

        let result = evaluate(parse("(< 2 1)").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(0));
    }

    // Test '<=' function
    #[test]
    fn test_lte() {
        let mut env = create_environment();
        let result = evaluate(parse("(<= 1 2)").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(1));

        let result = evaluate(parse("(<= 1 1)").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(1));

        let result = evaluate(parse("(<= 2 1)").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(0));
    }

    // Test 'cons' function
    #[test]
    fn test_cons() {
        let mut env = create_environment();
        let result = evaluate(parse("(cons 1 2)").unwrap(), &mut env).unwrap();
        assert_eq!(
            result,
            LispValue::Pair(Box::new((LispValue::Number(1), LispValue::Number(2))))
        );
    }

    // Test 'car' function
    #[test]
    fn test_car() {
        let mut env = create_environment();
        let result = evaluate(parse("(car (cons 1 2))").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(1));
    }

    // Test 'cdr' function
    #[test]
    fn test_cdr() {
        let mut env = create_environment();
        let result = evaluate(parse("(cdr (cons 1 2))").unwrap(), &mut env).unwrap();
        assert_eq!(result, LispValue::Number(2));
    }

    // Test 'list' function
    #[test]
    fn test_list() {
        let mut env = create_environment();
        let result = evaluate(parse("(list 1 2 3)").unwrap(), &mut env).unwrap();
        assert_eq!(
            result,
            LispValue::List(vec![
                LispValue::Number(1),
                LispValue::Number(2),
                LispValue::Number(3)
            ])
        );
    }

    // Test 'else' function
    #[test]
    fn test_lisp_else() {
        let mut env = create_environment();
        let result = evaluate(parse("(else 1 2 3)").unwrap(), &mut env).unwrap();
        assert_eq!(
            result,
            LispValue::List(vec![
                LispValue::Number(1),
                LispValue::Number(2),
                LispValue::Number(3)
            ])
        );
    }
}
