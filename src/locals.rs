use crate::scanner::Token;

#[derive(Default, Clone, Copy)]
pub struct Local {
    pub name: Token,
    pub depth: i32,
}

pub struct Locals {
    vars: Vec<Local>,
}

impl Locals {
    pub fn new() -> Self {
        Locals {
            // Rarely we have more than 8 local variables
            vars: Vec::with_capacity(8),
        }
    }

    pub fn push(&mut self, name: Token, depth: i32) {
        self.vars.push(Local { name, depth });
    }

    #[inline]
    pub fn last_depth(&self) -> i32 {
        self.vars.last().unwrap().depth
    }

    pub fn set_last_depth(&mut self, depth: i32) {
        self.vars.last_mut().unwrap().depth = depth;
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Local> {
        self.vars.iter()
    }

    pub fn is_empty(&self) -> bool {
        self.vars.is_empty()
    }

    pub fn len(&self) -> usize {
        self.vars.len()
    }

    pub fn pop(&mut self) {
        self.vars.pop();
    }
}
