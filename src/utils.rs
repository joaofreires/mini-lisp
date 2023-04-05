use crate::ast::{evaluate, LispValue};
use crate::parser::parse;
use crate::prelude::Environment;
use std::io::{self, Write};

pub fn read_input(prompt: &str) -> Result<String, io::Error> {
    print!("{}", prompt);
    io::stdout().flush()?;

    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    Ok(input.trim().to_string())
}

#[allow(dead_code)]
pub fn check_expect(input: &str, expected: LispValue) {
    run_test(input, |result| result == expected, "Expected");
}

#[allow(dead_code)]
pub fn check_within(input: &str, expected: LispValue, delta: f64) {
    run_test(
        input,
        |result| {
            if let (LispValue::Number(a), LispValue::Number(b)) = (result, expected.clone()) {
                (a as f64 - b as f64).abs() <= delta
            } else {
                false
            }
        },
        "Expected within",
    );
}

#[allow(dead_code)]
fn run_test<F>(input: &str, check: F, description: &str)
where
    F: Fn(LispValue) -> bool,
{
    let result = parse(input).and_then(|expr| evaluate(expr, &mut Environment::default()));
    match result {
        Ok(res) if check(res.clone()) => println!("{}: {} - Test passed", description, input),
        Ok(res) => println!("{}: {} - Test failed, got {:?}", description, input, res),
        Err(err) => println!(
            "{}: {} - Test failed with error: {:?}",
            description, input, err
        ),
    }
}
