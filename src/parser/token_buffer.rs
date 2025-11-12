#![allow(dead_code)]

use crate::lexer::LexToken;
use crate::parser::token_slice::TokenSlice;

pub struct TokenBuffer<Tok>
where
    Tok: LexToken,
{
    tokens: Vec<Tok>,
    offset: usize,
}

impl<Tok> TokenBuffer<Tok>
where
    Tok: LexToken,
{
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            offset: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.tokens.len().saturating_sub(self.offset)
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn push(&mut self, token: Tok) {
        self.tokens.push(token);
    }

    pub fn snapshot(&self) -> TokenSlice<'_, Tok> {
        TokenSlice::new(&self.tokens[self.offset..])
    }

    pub fn advance(&mut self, count: usize) {
        self.offset = (self.offset + count).min(self.tokens.len());

        if self.offset == self.tokens.len() {
            self.tokens.clear();
            self.offset = 0;
        } else if self.offset > 0 && self.offset > self.tokens.len() / 2 {
            self.tokens.drain(0..self.offset);
            self.offset = 0;
        }
    }

    pub fn clear(&mut self) {
        self.tokens.clear();
        self.offset = 0;
    }

    pub fn iter(&self) -> impl Iterator<Item = &Tok> {
        self.tokens[self.offset..].iter()
    }
}
