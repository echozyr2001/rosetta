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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{LexToken, Position};

    #[derive(Clone, Debug)]
    struct FakeToken {
        id: usize,
        eof: bool,
    }

    impl LexToken for FakeToken {
        fn position(&self) -> Option<Position> {
            None
        }

        fn is_eof(&self) -> bool {
            self.eof
        }

        fn is_newline(&self) -> bool {
            false
        }

        fn is_whitespace(&self) -> bool {
            false
        }

        fn is_indent(&self) -> bool {
            false
        }
    }

    fn make_token(id: usize) -> FakeToken {
        FakeToken { id, eof: false }
    }

    #[test]
    fn buffer_push_and_len() {
        let mut buffer = TokenBuffer::new();
        assert!(buffer.is_empty());

        buffer.push(make_token(1));
        buffer.push(make_token(2));
        assert_eq!(buffer.len(), 2);

        let snapshot = buffer.snapshot();
        let collected: Vec<_> = snapshot.tokens().iter().map(|tok| tok.id).collect();
        assert_eq!(collected, vec![1, 2]);
    }

    #[test]
    fn buffer_advance_drops_consumed_tokens() {
        let mut buffer = TokenBuffer::new();
        buffer.push(make_token(1));
        buffer.push(make_token(2));
        buffer.push(make_token(3));

        buffer.advance(1);
        assert_eq!(buffer.len(), 2);
        let snapshot = buffer.snapshot();
        assert_eq!(snapshot.tokens()[0].id, 2);

        buffer.advance(2);
        assert!(buffer.is_empty());
        assert_eq!(buffer.len(), 0);
    }

    #[test]
    fn buffer_clear_resets_state() {
        let mut buffer = TokenBuffer::new();
        buffer.push(make_token(1));
        buffer.push(make_token(2));
        buffer.clear();
        assert!(buffer.is_empty());
        buffer.push(make_token(3));
        assert_eq!(buffer.len(), 1);
        assert_eq!(buffer.snapshot().tokens()[0].id, 3);
    }
}
