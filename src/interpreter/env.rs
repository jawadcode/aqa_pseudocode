use std::collections::HashMap;

use super::{errors::RuntimeError, value::Value, ValueResult};

pub struct Env {
    scopes: Vec<HashMap<String, Value>>,
    depth: usize,
}

impl Default for Env {
    fn default() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            depth: 0,
        }
    }
}

impl Env {
    /// Introduce a new scope
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.depth += 1;
    }

    /// Exit the current scope
    pub fn pop_scope(&mut self) {
        self.scopes.pop();
        self.depth -= 1;
    }

    /// Insert `value` with a key of `ident` into the current scope
    pub fn set_variable(&mut self, ident: &str, value: &Value) {
        self.scopes
            .get_mut(self.depth)
            .unwrap()
            .insert(ident.to_string(), value.clone());
    }

    /// Get a variable named `ident` from the current scope, returning `RuntimeError::Undefined` if not found
    pub fn get_variable(&mut self, ident: &str) -> ValueResult {
        self.scopes[self.depth]
            .get(ident)
            .map(Clone::clone)
            .ok_or(RuntimeError::Undefined {
                ident: ident.to_string(),
            })
    }
}
