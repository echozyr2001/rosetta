/// Token-based parser using nom combinators
///
/// This module implements a parser that uses nom combinators to parse
/// Token sequences instead of strings, combining the power of nom with
/// the benefits of a separate lexical analysis phase.
use nom::{
    Err, IResult, InputLength, InputTake, InputTakeAtPosition, Needed, Slice,
    error::{ErrorKind, ParseError},
};
use std::ops::{Range, RangeFrom, RangeTo};

use crate::ast::{Block, Document, Inline, ListItem, ListKind, SourceMap};
use crate::error::{MarkdownError, Result};
use crate::lexer::{Position, token::Token};

/// Token slice wrapper that implements nom's input traits
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TokenSlice<'a>(&'a [Token<'a>]);

impl<'a> TokenSlice<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Self(tokens)
    }

    #[allow(dead_code)]
    pub fn tokens(&self) -> &'a [Token<'a>] {
        self.0
    }

    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn first(&self) -> Option<&'a Token<'a>> {
        self.0.first()
    }
}

// Implement nom input traits for TokenSlice

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

// Token-specific parser combinators

/// Parse a single token that matches the predicate
pub fn token<'a, F>(
    predicate: F,
) -> impl Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, &'a Token<'a>>
where
    F: Fn(&Token<'a>) -> bool,
{
    move |input: TokenSlice<'a>| {
        if let Some(first_token) = input.first() {
            if predicate(first_token) {
                Ok((input.slice(1..), first_token))
            } else {
                Err(Err::Error(nom::error::Error::new(input, ErrorKind::Tag)))
            }
        } else {
            Err(Err::Error(nom::error::Error::new(input, ErrorKind::Eof)))
        }
    }
}

/// Parse an ATX heading token
pub fn atx_heading(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, &Token<'_>> {
    token(|t| matches!(t, Token::AtxHeading(_)))(input)
}

/// Parse a setext heading token
pub fn setext_heading(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, &Token<'_>> {
    token(|t| matches!(t, Token::SetextHeading(_)))(input)
}

/// Parse a code block token
pub fn code_block(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, &Token<'_>> {
    token(|t| matches!(t, Token::CodeBlock(_)))(input)
}

/// Parse a blockquote token
pub fn blockquote_marker(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, &Token<'_>> {
    token(|t| matches!(t, Token::BlockQuote(_)))(input)
}

/// Parse a list marker token
pub fn list_marker(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, &Token<'_>> {
    token(|t| matches!(t, Token::ListMarker(_)))(input)
}

/// Parse a thematic break token
pub fn thematic_break(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, &Token<'_>> {
    token(|t| matches!(t, Token::ThematicBreak(_)))(input)
}

/// Parse a text token
pub fn text_token(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, &Token<'_>> {
    token(|t| matches!(t, Token::Text(_)))(input)
}

/// Parse whitespace tokens
#[allow(dead_code)]
pub fn whitespace(input: TokenSlice) -> IResult<TokenSlice, TokenSlice> {
    input.split_at_position_complete(|token| {
        !matches!(token, Token::Whitespace(_) | Token::Indent(_))
    })
}

/// Parse newline tokens
#[allow(dead_code)]
pub fn newlines(input: TokenSlice) -> IResult<TokenSlice, TokenSlice> {
    input.split_at_position_complete(|token| !matches!(token, Token::LineEnding(_)))
}

/// Skip whitespace and newlines
pub fn skip_ws_nl(input: TokenSlice) -> IResult<TokenSlice, ()> {
    let (input, _) = input.split_at_position_complete(|token| {
        !matches!(
            token,
            Token::Whitespace(_) | Token::Indent(_) | Token::LineEnding(_)
        )
    })?;
    Ok((input, ()))
}

// High-level parsing functions

use nom::{branch::alt, combinator::map, multi::many0, sequence::preceded};

/// Parse a document from tokens
pub fn parse_document(input: TokenSlice) -> IResult<TokenSlice, Document> {
    map(
        preceded(skip_ws_nl, many0(preceded(skip_ws_nl, parse_block))),
        |blocks| Document {
            blocks,
            source_map: SourceMap::new(),
        },
    )(input)
}

/// Parse a single block
pub fn parse_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    alt((
        parse_atx_heading_block,
        parse_setext_heading_block,
        parse_code_block_block,
        parse_blockquote_block,
        parse_list_block,
        parse_thematic_break_block,
        parse_paragraph_block,
    ))(input)
}

/// Parse ATX heading block
pub fn parse_atx_heading_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    map(atx_heading, |token| {
        if let Token::AtxHeading(heading_token) = token {
            Block::Heading {
                level: heading_token.level,
                content: vec![Inline::Text(heading_token.raw_content.to_string())],
                id: None,
                position: Some(heading_token.position),
            }
        } else {
            unreachable!("atx_heading parser should only return AtxHeading tokens")
        }
    })(input)
}

/// Parse setext heading block
pub fn parse_setext_heading_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    map(setext_heading, |token| {
        if let Token::SetextHeading(heading_token) = token {
            Block::Heading {
                level: heading_token.level,
                content: vec![Inline::Text("Setext Heading".to_string())], // TODO: get actual content
                id: None,
                position: Some(heading_token.position),
            }
        } else {
            unreachable!("setext_heading parser should only return SetextHeading tokens")
        }
    })(input)
}

/// Parse code block
pub fn parse_code_block_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    map(code_block, |token| {
        if let Token::CodeBlock(code_token) = token {
            Block::CodeBlock {
                info: code_token.info_string.map(|s| s.to_string()),
                content: code_token.raw_content.to_string(),
                language: code_token
                    .info_string
                    .and_then(|info| info.split_whitespace().next().map(|s| s.to_string())),
                position: Some(code_token.position),
            }
        } else {
            unreachable!("code_block parser should only return CodeBlock tokens")
        }
    })(input)
}

/// Parse blockquote block
pub fn parse_blockquote_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    map(blockquote_marker, |token| {
        if let Token::BlockQuote(bq_token) = token {
            Block::BlockQuote {
                content: vec![Block::Paragraph {
                    content: vec![Inline::Text("Blockquote content".to_string())],
                    position: Some(bq_token.position),
                }],
                position: Some(bq_token.position),
            }
        } else {
            unreachable!("blockquote_marker parser should only return BlockQuote tokens")
        }
    })(input)
}

/// Parse list block
pub fn parse_list_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    map(list_marker, |token| {
        if let Token::ListMarker(list_token) = token {
            let kind = match &list_token.marker {
                crate::lexer::token::ListKind::Bullet { marker } => {
                    ListKind::Bullet { marker: *marker }
                }
                crate::lexer::token::ListKind::Ordered { start, delimiter } => ListKind::Ordered {
                    start: *start,
                    delimiter: *delimiter,
                },
            };

            Block::List {
                kind,
                tight: true,
                items: vec![ListItem {
                    content: vec![Block::Paragraph {
                        content: vec![Inline::Text("List item content".to_string())],
                        position: Some(list_token.position),
                    }],
                    tight: true,
                    task_list_marker: None,
                }],
                position: Some(list_token.position),
            }
        } else {
            unreachable!("list_marker parser should only return ListMarker tokens")
        }
    })(input)
}

/// Parse thematic break block
pub fn parse_thematic_break_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    map(thematic_break, |token| {
        if let Token::ThematicBreak(tb_token) = token {
            Block::ThematicBreak {
                position: Some(tb_token.position),
            }
        } else {
            unreachable!("thematic_break parser should only return ThematicBreak tokens")
        }
    })(input)
}

/// Parse paragraph block (fallback)
pub fn parse_paragraph_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    map(text_token, |token| {
        if let Token::Text(text_token) = token {
            Block::Paragraph {
                content: vec![Inline::Text(text_token.lexeme.to_string())],
                position: Some(text_token.position),
            }
        } else {
            unreachable!("text_token parser should only return Text tokens")
        }
    })(input)
}

/// Main parsing function that takes a token slice and returns a Document
pub fn parse_tokens(tokens: &[Token]) -> Result<Document> {
    let input = TokenSlice::new(tokens);

    match parse_document(input) {
        Ok((_, document)) => Ok(document),
        Err(e) => Err(MarkdownError::parse_error(
            Position::new(),
            format!("Token parsing error: {:?}", e),
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token::{AtxHeadingToken, TextToken};

    #[test]
    fn test_token_slice_basic() {
        let tokens = vec![
            Token::Text(TextToken {
                lexeme: "hello",
                position: Position::new(),
            }),
            Token::Text(TextToken {
                lexeme: "world",
                position: Position::new(),
            }),
        ];

        let slice = TokenSlice::new(&tokens);
        assert_eq!(slice.len(), 2);
        assert!(!slice.is_empty());
    }

    #[test]
    fn test_parse_atx_heading() {
        let tokens = vec![Token::AtxHeading(AtxHeadingToken {
            level: 1,
            marker_count: 1,
            leading_whitespace: 0,
            raw_marker: "#",
            raw_content: "Hello World",
            closing_sequence: "",
            position: Position::new(),
        })];

        let input = TokenSlice::new(&tokens);
        let result = parse_atx_heading_block(input);

        assert!(result.is_ok());
        let (_, block) = result.unwrap();

        match block {
            Block::Heading { level, content, .. } => {
                assert_eq!(level, 1);
                assert_eq!(content.len(), 1);
                if let Inline::Text(text) = &content[0] {
                    assert_eq!(text, "Hello World");
                }
            }
            _ => panic!("Expected heading block"),
        }
    }

    #[test]
    fn test_parse_document() {
        let tokens = vec![
            Token::AtxHeading(AtxHeadingToken {
                level: 1,
                marker_count: 1,
                leading_whitespace: 0,
                raw_marker: "#",
                raw_content: "Title",
                closing_sequence: "",
                position: Position::new(),
            }),
            Token::Text(TextToken {
                lexeme: "Some text",
                position: Position::new(),
            }),
        ];

        let result = parse_tokens(&tokens);
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 2);

        // First block should be heading
        match &document.blocks[0] {
            Block::Heading { level, .. } => assert_eq!(*level, 1),
            _ => panic!("Expected heading block"),
        }

        // Second block should be paragraph
        match &document.blocks[1] {
            Block::Paragraph { content, .. } => {
                assert_eq!(content.len(), 1);
                if let Inline::Text(text) = &content[0] {
                    assert_eq!(text, "Some text");
                }
            }
            _ => panic!("Expected paragraph block"),
        }
    }
}
