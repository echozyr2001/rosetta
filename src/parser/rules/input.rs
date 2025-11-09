use crate::lexer::token::Token;
use nom::{
    Err, IResult, InputLength, InputTake, InputTakeAtPosition, Needed, Slice,
    error::{ErrorKind, ParseError},
};
use std::ops::{Range, RangeFrom, RangeTo};

/// Wrapper around a token slice that implements nom input traits.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TokenSlice<'a>(pub &'a [Token<'a>]);

impl<'a> TokenSlice<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Self(tokens)
    }

    pub fn tokens(&self) -> &'a [Token<'a>] {
        self.0
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn first(&self) -> Option<&'a Token<'a>> {
        self.0.first()
    }
}

impl<'a> InputLength for TokenSlice<'a> {
    fn input_len(&self) -> usize {
        self.0.len()
    }
}

impl<'a> InputTake for TokenSlice<'a> {
    fn take(&self, count: usize) -> Self {
        Self(&self.0[..count.min(self.0.len())])
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let count = count.min(self.0.len());
        (Self(&self.0[count..]), Self(&self.0[..count]))
    }
}

impl<'a> Slice<Range<usize>> for TokenSlice<'a> {
    fn slice(&self, range: Range<usize>) -> Self {
        Self(&self.0[range])
    }
}

impl<'a> Slice<RangeFrom<usize>> for TokenSlice<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        Self(&self.0[range])
    }
}

impl<'a> Slice<RangeTo<usize>> for TokenSlice<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Self(&self.0[range])
    }
}

impl<'a> InputTakeAtPosition for TokenSlice<'a> {
    type Item = Token<'a>;

    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.iter().position(|token| predicate(token.clone())) {
            Some(i) => Ok((Self(&self.0[i..]), Self(&self.0[..i]))),
            None => Err(Err::Incomplete(Needed::new(1))),
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.iter().position(|token| predicate(token.clone())) {
            Some(0) => Err(Err::Error(E::from_error_kind(*self, e))),
            Some(i) => Ok((Self(&self.0[i..]), Self(&self.0[..i]))),
            None => {
                if self.0.is_empty() {
                    Err(Err::Error(E::from_error_kind(*self, e)))
                } else {
                    Ok((Self(&[]), *self))
                }
            }
        }
    }

    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.iter().position(|token| predicate(token.clone())) {
            Some(i) => Ok((Self(&self.0[i..]), Self(&self.0[..i]))),
            None => Ok((Self(&[]), *self)),
        }
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.iter().position(|token| predicate(token.clone())) {
            Some(0) => Err(Err::Error(E::from_error_kind(*self, e))),
            Some(i) => Ok((Self(&self.0[i..]), Self(&self.0[..i]))),
            None => {
                if self.0.is_empty() {
                    Err(Err::Error(E::from_error_kind(*self, e)))
                } else {
                    Ok((Self(&[]), *self))
                }
            }
        }
    }
}
