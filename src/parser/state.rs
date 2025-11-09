use crate::lexer::LexToken;

/// ParserState keeps track of the collected token stream and the current cursor.
pub struct ParserState<Tok>
where
    Tok: LexToken,
{
    tokens: Vec<Tok>,
    offset: usize,
}

impl<Tok> ParserState<Tok>
where
    Tok: LexToken,
{
    pub fn new(tokens: Vec<Tok>) -> Self {
        Self { tokens, offset: 0 }
    }

    /// Returns the slice of tokens yet to be consumed.
    pub fn remaining(&self) -> &[Tok] {
        &self.tokens[self.offset..]
    }

    /// Advances the internal cursor by `count` tokens.
    pub fn advance(&mut self, count: usize) {
        self.offset = (self.offset + count).min(self.tokens.len());
    }

    /// Returns true when all tokens have been consumed.
    pub fn is_exhausted(&self) -> bool {
        self.offset >= self.tokens.len()
    }
}

impl<Tok> ParserState<Tok>
where
    Tok: LexToken,
{
    /// Consumes all remaining tokens.
    pub fn finish(&mut self) {
        self.offset = self.tokens.len();
    }
}
