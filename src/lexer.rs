/// Lexical analysis module for tokenizing Markdown input.
///
/// This module provides the `Lexer` struct and related types for breaking down
/// Markdown text into a stream of tokens that can be consumed by the parser.
use crate::error::Result;

/// Represents the position of a token in the source text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Position {
    pub fn new() -> Self {
        Position {
            line: 1,
            column: 1,
            offset: 0,
        }
    }
}

impl Default for Position {
    fn default() -> Self {
        Self::new()
    }
}

/// Enumeration of all possible token types in Markdown.
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'input> {
    // Block-level tokens
    AtxHeading {
        level: u8,
        content: &'input str,
    },
    SetextHeading {
        level: u8,
        content: &'input str,
    },
    CodeBlock {
        info: Option<&'input str>,
        content: &'input str,
    },
    BlockQuote,
    ListMarker {
        kind: ListKind,
        indent: usize,
    },
    ThematicBreak,

    // Inline tokens
    Text(&'input str),
    Emphasis {
        marker: char,
        count: usize,
    },
    CodeSpan(&'input str),
    Link {
        text: &'input str,
        dest: &'input str,
        title: Option<&'input str>,
    },
    Image {
        alt: &'input str,
        dest: &'input str,
        title: Option<&'input str>,
    },

    // Structural tokens
    Newline,
    Indent(usize),
    Eof,
}

/// Types of list markers.
#[derive(Debug, Clone, PartialEq)]
pub enum ListKind {
    Bullet { marker: char },
    Ordered { start: u32, delimiter: char },
}

/// The main lexer struct for tokenizing Markdown input.
pub struct Lexer<'input> {
    input: &'input str,
    position: Position,
    current_char: Option<char>,
    chars: std::str::Chars<'input>,
}

impl<'input> Lexer<'input> {
    /// Creates a new lexer for the given input string.
    pub fn new(input: &'input str) -> Self {
        let mut chars = input.chars();
        let current_char = chars.next();

        Lexer {
            input,
            position: Position::new(),
            current_char,
            chars,
        }
    }

    /// Returns the current position in the input.
    pub fn position(&self) -> Position {
        self.position
    }

    /// Advances to the next character in the input.
    fn advance(&mut self) {
        if let Some(ch) = self.current_char {
            self.position.offset += ch.len_utf8();
            if ch == '\n' {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += 1;
            }
        }
        self.current_char = self.chars.next();
    }

    /// Peeks at the next character without advancing.
    #[allow(dead_code)]
    fn peek(&self) -> Option<char> {
        self.chars.as_str().chars().next()
    }

    /// Consumes and returns the next token from the input.
    pub fn next_token(&mut self) -> Result<Token<'input>> {
        // Placeholder implementation - will be expanded in future tasks
        match self.current_char {
            None => Ok(Token::Eof),
            Some('\n') => {
                self.advance();
                Ok(Token::Newline)
            }
            Some(ch) if ch.is_whitespace() => {
                let start_offset = self.position.offset;
                while let Some(ch) = self.current_char {
                    if ch.is_whitespace() && ch != '\n' {
                        self.advance();
                    } else {
                        break;
                    }
                }
                let indent_size = self.position.offset - start_offset;
                Ok(Token::Indent(indent_size))
            }
            Some(_) => {
                // For now, treat everything else as text
                let start_offset = self.position.offset;
                while let Some(ch) = self.current_char {
                    if ch == '\n' || ch.is_whitespace() {
                        break;
                    }
                    self.advance();
                }
                let end_offset = self.position.offset;
                let text = &self.input[start_offset..end_offset];
                Ok(Token::Text(text))
            }
        }
    }

    /// Peeks at the next token without consuming it.
    pub fn peek_token(&mut self) -> Result<Token<'input>> {
        // Save current state
        let saved_position = self.position;
        let saved_current_char = self.current_char;
        let saved_chars = self.chars.clone();

        // Get next token
        let token = self.next_token();

        // Restore state
        self.position = saved_position;
        self.current_char = saved_current_char;
        self.chars = saved_chars;

        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_creation() {
        let input = "Hello, World!";
        let lexer = Lexer::new(input);
        assert_eq!(lexer.position().line, 1);
        assert_eq!(lexer.position().column, 1);
        assert_eq!(lexer.position().offset, 0);
    }

    #[test]
    fn test_position_tracking() {
        let position = Position::new();
        assert_eq!(position.line, 1);
        assert_eq!(position.column, 1);
        assert_eq!(position.offset, 0);
    }
}
