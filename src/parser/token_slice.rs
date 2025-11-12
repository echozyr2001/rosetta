use std::ops::{Range, RangeFrom, RangeTo};

use nom::{InputLength, InputTake, Slice};

#[derive(Debug, PartialEq)]
pub struct TokenSlice<'a, Tok> {
    tokens: &'a [Tok],
}

impl<'a, Tok> Copy for TokenSlice<'a, Tok> {}

impl<'a, Tok> Clone for TokenSlice<'a, Tok> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, Tok> TokenSlice<'a, Tok> {
    pub fn new(tokens: &'a [Tok]) -> Self {
        Self { tokens }
    }

    pub fn tokens(&self) -> &'a [Tok] {
        self.tokens
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    pub fn first(&self) -> Option<&'a Tok> {
        self.tokens.first()
    }
}

impl<'a, Tok> InputLength for TokenSlice<'a, Tok> {
    fn input_len(&self) -> usize {
        self.tokens.len()
    }
}

impl<'a, Tok> InputTake for TokenSlice<'a, Tok> {
    fn take(&self, count: usize) -> Self {
        let count = count.min(self.tokens.len());
        Self::new(&self.tokens[..count])
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let count = count.min(self.tokens.len());
        let (left, right) = self.tokens.split_at(count);
        (Self::new(right), Self::new(left))
    }
}

impl<'a, Tok> Slice<Range<usize>> for TokenSlice<'a, Tok> {
    fn slice(&self, range: Range<usize>) -> Self {
        Self::new(&self.tokens[range])
    }
}

impl<'a, Tok> Slice<RangeFrom<usize>> for TokenSlice<'a, Tok> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        Self::new(&self.tokens[range])
    }
}

impl<'a, Tok> Slice<RangeTo<usize>> for TokenSlice<'a, Tok> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Self::new(&self.tokens[range])
    }
}
