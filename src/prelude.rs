use crate::ast::{LispError, LispResult, LispValue};
use crate::builtins::built_in_functions;
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