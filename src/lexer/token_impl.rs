use crate::lexer::token::{
    AtxHeadingToken, CodeBlockToken, ListMarkerToken, SetextHeadingToken, TextToken, Token,
};
/// Implementation of CGP traits for the existing Token type
///
/// This module provides trait implementations that allow the existing
/// Token type to work with the CGP-based parser.
use crate::lexer::{Position, Token as LexerToken};
use crate::traits::token_traits::*;

// Implement TokenType for the Token in lexer.rs
impl<'input> TokenType for LexerToken<'input> {
    fn position(&self) -> Option<Position> {
        None // Simplified: lexer Token doesn't track position
    }

    fn is_eof(&self) -> bool {
        matches!(self, LexerToken::Eof)
    }

    fn is_newline(&self) -> bool {
        matches!(self, LexerToken::Newline)
    }

    fn is_whitespace(&self) -> bool {
        matches!(self, LexerToken::Whitespace(_))
    }

    fn is_indent(&self) -> bool {
        matches!(self, LexerToken::Indent(_))
    }
}

impl<'input> TokenType for Token<'input> {
    fn position(&self) -> Option<Position> {
        // For simplicity, return None for now
        // In a complete implementation, each token variant would store its position
        None
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
