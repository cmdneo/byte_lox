use crate::scanner::Token;

#[derive(Default, Clone, Copy)]
pub struct Local {
    pub name: Token,
    pub depth: i32,
}

pub struct Locals {
    pub vars: Vec<Local>,
    pub scope_depth: i32,
}

impl Locals {
    pub fn new() -> Self {
        Locals {
            // Rarely we have more than 8 local variables
            vars: Vec::with_capacity(8),
            scope_depth: 0,
        }
    }

    pub fn add(&mut self, name: Token, depth: i32) {
        self.vars.push(Local { name, depth });
    }

    #[inline]
    pub fn last_depth(&self) -> i32 {
        self.vars.last().unwrap().depth
    }
}
