use crate::ast::{LispError, LispResult, LispValue};
use std::iter::Peekable;
use std::str::Chars;

pub fn parse(input: &str) -> LispResult {
    let mut chars = input.chars().peekable();
    parse_expression(&mut chars)
}

fn parse_expression(chars: &mut Peekable<Chars>) -> LispResult {
    let ch = chars
        .peek()
        .ok_or(LispError::Generic("Unexpected end of input".to_string()))?;

    match ch {
        '(' => parse_list(chars),
        '"' => parse_string(chars),
        '0'..='9' | '-' => parse_number(chars),
        ';' => {
            skip_comment(chars);
            parse_expression(chars)
        }
        _ => parse_symbol_or_function(chars),
    }
}

// Implement parse_list, parse_string, parse_number, skip_comment, and parse_symbol_or_function functions here.
fn parse_list(chars: &mut Peekable<Chars>) -> LispResult {
    let _ = chars.next(); // Consume opening parenthesis '('

    let mut list = Vec::new();

    loop {
        match chars.peek() {
            Some(')') => {
                chars.next(); // Consume closing parenthesis ')'
                break;
            }
            Some(_) => {
                let expr = parse_expression(chars)?;
                let force_space = match expr {
                    LispValue::Float(_) | LispValue::Number(_) | LispValue::Str(_) => true,
                    _ => false,
                };
                list.push(expr);
                match skip_whitespace(chars, force_space) {
                    Err(err) => return Err(err),
                    _ => (),
                };
            }
            None => {
                return Err(LispError::Generic(
                    "Unexpected end of input, missing ')'".to_string(),
                ))
            }
        }
    }

    Ok(LispValue::List(list))
}

fn parse_string(chars: &mut Peekable<Chars>) -> LispResult {
    let _ = chars.next(); // Consume opening quote '"'

    let mut string = String::new();

    while let Some(ch) = chars.next() {
        if ch == '"' {
            break;
        } else if ch == '\\' {
            if let Some(escaped_ch) = chars.next() {
                string.push(match escaped_ch {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    '"' => '"',
                    _ => {
                        return Err(LispError::Generic(format!(
                            "Invalid escape sequence: \\{}",
                            escaped_ch
                        )))
                    }
                });
            } else {
                return Err(LispError::Generic(
                    "Unexpected end of input, missing closing '\"'".to_string(),
                ));
            }
        } else {
            string.push(ch);
        }
    }

    Ok(LispValue::Str(string))
}

fn parse_number(chars: &mut Peekable<Chars>) -> LispResult {
    let mut number = String::new();
    let mut has_decimal = false;

    if let Some(&ch) = chars.peek() {
        if ch == '-' {
            number.push(ch);
            chars.next();
        }
    }

    while let Some(ch) = chars.peek() {
        if ch.is_digit(10) {
            number.push(*ch);
            chars.next();
        } else if *ch == '.' && !has_decimal {
            has_decimal = true;
            number.push(*ch);
            chars.next();
        } else {
            break;
        }
    }

    if has_decimal {
        number
            .parse::<f64>()
            .map(LispValue::Float)
            .map_err(|_| LispError::Generic("Failed to parse float".to_string()))
    } else {
        number
            .parse::<i64>()
            .map(LispValue::Number)
            .map_err(|_| LispError::Generic("Failed to parse number".to_string()))
    }
}

fn skip_comment(chars: &mut Peekable<Chars>) {
    while let Some(ch) = chars.next() {
        if ch == '\n' {
            break;
        }
    }
}

fn parse_symbol_or_function(chars: &mut Peekable<Chars>) -> LispResult {
    let mut symbol = String::new();

    while let Some(ch) = chars.peek() {
        if ch.is_alphanumeric()
            || *ch == '-'
            || *ch == '_'
            || *ch == '+'
            || *ch == '*'
            || *ch == '/'
        {
            symbol.push(*ch);
            chars.next();
        } else {
            break;
        }
    }

    if symbol.is_empty() {
        return Err(LispError::Generic("Unexpected character".to_string()));
    }

    Ok(LispValue::Symbol(symbol))
}

fn skip_whitespace(chars: &mut Peekable<Chars>, force_space: bool) -> Result<(), LispError> {
    let mut skipped = false;
    while let Some(ch) = chars.peek() {
        if ch.is_whitespace() {
            skipped = true;
            chars.next();
        } else if force_space && !skipped && *ch != ')' {
            return Err(LispError::Generic("Missing space error.".to_string()));
        } else {
            break;
        }
    }
    Ok(())
}
