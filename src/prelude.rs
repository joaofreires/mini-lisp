use crate::ast::{evaluate, LispError, LispResult, LispValue};
use crate::builtins::built_in_functions;
use crate::parser::parse;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Environment {
    values: Rc<RefCell<HashMap<String, LispValue>>>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: Rc::new(RefCell::new(HashMap::new())),
            parent: None,
        }
    }

    pub fn child(&self) -> Self {
        Environment {
            values: Rc::new(RefCell::new(HashMap::new())),
            parent: Some(Rc::new(RefCell::new(self.clone()))),
        }
    }

    pub fn set(&mut self, name: String, value: LispValue) {
        self.values.borrow_mut().insert(name, value);
    }

    pub fn get(&self, name: &str) -> LispResult {
        if let Some(value) = self.values.borrow().get(name) {
            return Ok(value.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.borrow().get(name);
        }

        Err(LispError::Generic(format!("Variable '{}' not found", name)))
    }

    pub fn has(&self, name: &str) -> bool {
        if self.values.borrow().contains_key(name) {
            return true;
        }

        if let Some(parent) = &self.parent {
            return parent.borrow().has(name);
        }
        false
    }

    pub fn init(&mut self) {
        for (name, func) in built_in_functions() {
            self.set(name, func);
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Environment::new()
    }
}

pub struct LispEngine {
    env: Environment,
}

impl LispEngine {
    pub fn new() -> Self {
        let mut engine = LispEngine {
            env: Environment::new(),
        };
        engine.env.init();
        engine
    }

    pub fn parse(&mut self, expr: &str) -> Result<String, LispError> {
        let ast = parse(&expr)?;
        let result = evaluate(ast, &mut self.env)?;
        Ok(format!("{result}")
    }
}

impl Default for LispEngine {
    fn default() -> Self {
        LispEngine::new()
    }
}
