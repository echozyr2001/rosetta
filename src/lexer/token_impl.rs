use crate::lexer::Position;
/// Implementation of CGP traits for the existing Token type
///
/// This module provides trait implementations that allow the existing
/// Token type to work with the CGP-based parser.
use crate::lexer::token::{
    AtxHeadingToken, CodeBlockToken, ListMarkerToken, SetextHeadingToken, TextToken, Token,
};
use crate::traits::token_traits::*;

// Implement TokenType for Token from token.rs
impl<'input> TokenType for Token<'input> {
    fn position(&self) -> Option<Position> {
        match self {
            Token::AtxHeading(token) => Some(token.position),
            Token::SetextHeading(token) => Some(token.position),
            Token::CodeBlock(token) => Some(token.position),
            Token::BlockQuote(token) => Some(token.position),
            Token::ListMarker(token) => Some(token.position),
            Token::ThematicBreak(token) => Some(token.position),
            Token::HtmlBlock(token) => Some(token.position),
            Token::LinkReferenceDefinition(token) => Some(token.position),
            Token::Text(token) => Some(token.position),
            Token::Whitespace(token) => Some(token.position),
            Token::SoftBreak(token) => Some(token.position),
            Token::HardBreak(token) => Some(token.position),
            Token::EmphasisDelimiter(token) => Some(token.position),
            Token::CodeSpan(token) => Some(token.position),
            Token::CodeSpanDelimiter(token) => Some(token.position),
            Token::Autolink(token) => Some(token.position),
            Token::Entity(token) => Some(token.position),
            Token::EscapeSequence(token) => Some(token.position),
            Token::HtmlInline(token) => Some(token.position),
            Token::LinkDestination(token) => Some(token.position),
            Token::LinkTitle(token) => Some(token.position),
            Token::ImageMarker(token) => Some(token.position),
            Token::LineEnding(token) => Some(token.position),
            Token::Indent(token) => Some(token.position),
            Token::Punctuation(token) => Some(token.position),
            Token::LinkLabelDelimiter(_) => None, // LinkLabelDelimiter doesn't have position yet
            Token::Eof => None,
        }
    }

    fn is_eof(&self) -> bool {
        matches!(self, Token::Eof)
    }

    fn is_newline(&self) -> bool {
        matches!(self, Token::LineEnding(_))
    }

    fn is_whitespace(&self) -> bool {
        matches!(self, Token::Whitespace(_))
    }

    fn is_indent(&self) -> bool {
        matches!(self, Token::Indent(_))
    }
}

// Implement data extraction traits for specific token types
impl<'input> AtxHeadingData for AtxHeadingToken<'input> {
    fn level(&self) -> u8 {
        self.level
    }

    fn content(&self) -> &str {
        self.raw_content
    }
}

impl<'input> SetextHeadingData for SetextHeadingToken<'input> {
    fn level(&self) -> u8 {
        self.level
    }

    fn content(&self) -> &str {
        self.raw_underline
    }
}

impl<'input> CodeBlockData for CodeBlockToken<'input> {
    fn info(&self) -> Option<&str> {
        self.info_string
    }

    fn content(&self) -> &str {
        self.raw_content
    }

    fn is_fenced(&self) -> bool {
        self.fence_char.is_some()
    }
}

impl<'input> ListMarkerData for ListMarkerToken<'input> {
    fn is_ordered(&self) -> bool {
        matches!(self.marker, crate::lexer::token::ListKind::Ordered { .. })
    }

    fn marker_char(&self) -> char {
        match &self.marker {
            crate::lexer::token::ListKind::Bullet { marker } => *marker,
            crate::lexer::token::ListKind::Ordered { delimiter, .. } => *delimiter,
        }
    }

    fn start_number(&self) -> Option<u32> {
        match &self.marker {
            crate::lexer::token::ListKind::Ordered { start, .. } => Some(*start),
            _ => None,
        }
    }

    fn indent(&self) -> usize {
        self.marker_offset
    }
}

impl<'input> TextData for TextToken<'input> {
    fn content(&self) -> &str {
        self.lexeme
    }
}
