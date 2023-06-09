use crate::ast::{LispError, LispResult, LispValue};
use std::iter::Peekable;
use std::str::Chars;

pub fn parse_multi(input: &str) -> LispResult {
    let mut result = LispValue::Nil();
    let mut chars = input.chars().peekable();
    loop {
        if let Some(_) = chars.peek() {
            match parse_expression(&mut chars) {
                Ok(value) => result = value,
                Err(LispError::EndOfInput()) => break,
                Err(err) => return Err(err),
            }
        } else {
            break;
        }
    }
    Ok(result)
}

pub fn parse(input: &str) -> LispResult {
    parse_multi(input)
}

fn parse_expression(chars: &mut Peekable<Chars>) -> LispResult {
    skip_whitespace(chars, false)?;
    let ch = chars.peek().ok_or(LispError::EndOfInput())?;

    match ch {
        '(' => parse_list(chars),
        '"' => parse_string(chars),
        '0'..='9' => parse_number(chars),
        ';' => {
            skip_comment(chars);
            match chars.peek() {
                Some(')') => Err(LispError::EndOfExpression()),
                _ => parse_expression(chars),
            }
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
                let expr = match parse_expression(chars) {
                    Err(LispError::EndOfExpression()) => {
                        chars.next();
                        break;
                    }
                    Ok(value) => value,
                    Err(err) => return Err(err),
                };
                let force_space = match expr {
                    LispValue::Float(_) | LispValue::Number(_) | LispValue::Str(_) => true,
                    _ => false,
                };
                list.push(expr);
                skip_whitespace(chars, force_space)?;
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
    skip_whitespace(chars, false)?;
    while let Some(ch) = chars.peek() {
        if ch.is_alphanumeric()
            || *ch == '-'
            || *ch == '_'
            || *ch == '+'
            || *ch == '*'
            || *ch == '/'
            || *ch == '='
            || *ch == '>'
            || *ch == '<'
            || *ch == '?'
            || *ch == '^'
            || *ch == '.'
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
    match symbol.as_str() {
        "nil" => Ok(LispValue::Nil()),
        _ => Ok(LispValue::Symbol(symbol)),
    }
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
