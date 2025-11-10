use crate::lexer::LexToken;

/// ParserState keeps track of the remaining token slice and the current cursor.
pub struct ParserState<'a, Tok>
where
    Tok: LexToken,
{
    tokens: &'a [Tok],
    offset: usize,
}

impl<'a, Tok> ParserState<'a, Tok>
where
    Tok: LexToken,
{
    pub fn new(tokens: &'a [Tok]) -> Self {
        Self { tokens, offset: 0 }
    }

    /// Returns the slice of tokens yet to be consumed.
    pub fn remaining(&self) -> &'a [Tok] {
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

    /// Consumes all remaining tokens.
    pub fn finish(&mut self) {
        self.offset = self.tokens.len();
    }
}
