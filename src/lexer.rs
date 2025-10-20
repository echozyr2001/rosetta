/// Lexical analysis module for tokenizing Markdown input.
///
/// This module provides the `Lexer` struct and related types for breaking down
/// Markdown text into a stream of tokens that can be consumed by the parser.
use crate::error::Result;
use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};

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

/// Unicode-aware character stream for proper handling of grapheme clusters.
///
/// This struct provides an iterator over Unicode grapheme clusters, which is
/// important for correctly handling complex Unicode characters like emoji
/// and combining characters.
pub struct CharStream<'input> {
    input: &'input str,
    graphemes: GraphemeIndices<'input>,
    current_grapheme: Option<(usize, &'input str)>,
    position: Position,
}

impl<'input> CharStream<'input> {
    /// Creates a new CharStream for the given input string.
    pub fn new(input: &'input str) -> Self {
        let mut graphemes = input.grapheme_indices(true);
        let current_grapheme = graphemes.next();

        CharStream {
            input,
            graphemes,
            current_grapheme,
            position: Position::new(),
        }
    }

    /// Returns the current position in the input.
    pub fn position(&self) -> Position {
        self.position
    }

    /// Returns the current grapheme cluster without advancing.
    pub fn current(&self) -> Option<&'input str> {
        self.current_grapheme.map(|(_, grapheme)| grapheme)
    }

    /// Returns the byte offset of the current grapheme cluster.
    pub fn current_offset(&self) -> Option<usize> {
        self.current_grapheme.map(|(offset, _)| offset)
    }

    /// Advances to the next grapheme cluster.
    pub fn advance(&mut self) -> Option<&'input str> {
        if let Some((_, current)) = self.current_grapheme {
            // Update position based on the current grapheme
            if current == "\n" {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += 1;
            }
            self.position.offset += current.len();

            // Move to next grapheme
            self.current_grapheme = self.graphemes.next();
            Some(current)
        } else {
            None
        }
    }

    /// Peeks at the next grapheme cluster without advancing.
    pub fn peek(&self) -> Option<&'input str> {
        self.graphemes.as_str().graphemes(true).next()
    }

    /// Checks if the current grapheme matches the given character.
    pub fn current_is(&self, ch: char) -> bool {
        self.current().is_some_and(|g| g.starts_with(ch))
    }

    /// Checks if the current grapheme is whitespace.
    pub fn current_is_whitespace(&self) -> bool {
        self.current()
            .is_some_and(|g| g.chars().next().is_some_and(|ch| ch.is_whitespace()))
    }

    /// Checks if we've reached the end of the input.
    pub fn is_at_end(&self) -> bool {
        self.current_grapheme.is_none()
    }

    /// Returns a slice of the input from the given start offset to the current position.
    pub fn slice_from(&self, start_offset: usize) -> &'input str {
        let end_offset = self.current_offset().unwrap_or(self.input.len());
        &self.input[start_offset..end_offset]
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
    char_stream: CharStream<'input>,
}

impl<'input> Lexer<'input> {
    /// Creates a new lexer for the given input string.
    pub fn new(input: &'input str) -> Self {
        Lexer {
            char_stream: CharStream::new(input),
        }
    }

    /// Returns the current position in the input.
    pub fn position(&self) -> Position {
        self.char_stream.position()
    }

    /// Returns the current grapheme cluster.
    #[allow(dead_code)]
    fn current(&self) -> Option<&'input str> {
        self.char_stream.current()
    }

    /// Advances to the next grapheme cluster.
    fn advance(&mut self) -> Option<&'input str> {
        self.char_stream.advance()
    }

    /// Peeks at the next grapheme cluster without advancing.
    #[allow(dead_code)]
    fn peek(&self) -> Option<&'input str> {
        self.char_stream.peek()
    }

    /// Consumes and returns the next token from the input.
    pub fn next_token(&mut self) -> Result<Token<'input>> {
        // Skip whitespace but preserve it for indentation detection
        if !self.char_stream.is_at_end()
            && self.char_stream.current_is_whitespace()
            && !self.char_stream.current_is('\n')
        {
            // Handle indentation at start of line
            if self.is_at_line_start() {
                return self.tokenize_indent();
            } else {
                // Skip whitespace in middle of line
                self.skip_whitespace_except_newline();
            }
        }

        if self.char_stream.is_at_end() {
            return Ok(Token::Eof);
        }

        // Handle newlines
        if self.char_stream.current_is('\n') {
            self.advance();
            return Ok(Token::Newline);
        }

        // Handle list markers FIRST (before other checks that might consume the marker)
        if let Some(list_token) = self.try_tokenize_list_marker()? {
            return Ok(list_token);
        }

        // Handle ATX headings (# ## ### etc.) - only at start of line or after whitespace
        if self.char_stream.current_is('#') {
            return self.tokenize_atx_heading();
        }

        // Handle thematic breaks (--- *** ___)
        if self.is_thematic_break_start() {
            return self.tokenize_thematic_break();
        }

        // Handle code blocks and spans (```)
        if self.char_stream.current_is('`') {
            if self.is_code_fence_start() {
                return self.tokenize_code_block();
            } else {
                return self.tokenize_code_span();
            }
        }

        // Handle blockquotes (>)
        if self.char_stream.current_is('>') {
            self.advance();
            return Ok(Token::BlockQuote);
        }

        // Handle emphasis markers (* _ ** __)
        if self.is_emphasis_marker() {
            return self.tokenize_emphasis();
        }

        // Handle links and images ([ ![)
        if self.char_stream.current_is('[')
            || (self.char_stream.current_is('!') && self.peek_char() == Some('['))
        {
            return self.tokenize_link_or_image();
        }

        // Default: tokenize as text
        self.tokenize_text()
    }

    /// Peeks at the next token without consuming it.
    pub fn peek_token(&mut self) -> Result<Token<'input>> {
        // Create a clone of the current lexer state
        let mut temp_lexer = self.clone_state();
        temp_lexer.next_token()
    }

    /// Creates a clone of the current lexer state for lookahead operations.
    fn clone_state(&self) -> Lexer<'input> {
        let mut cloned_stream = CharStream::new(self.char_stream.input);

        // Advance the cloned stream to match current position
        while cloned_stream.position().offset < self.char_stream.position().offset {
            if cloned_stream.advance().is_none() {
                break;
            }
        }

        Lexer {
            char_stream: cloned_stream,
        }
    }

    /// Skips whitespace characters except newlines.
    fn skip_whitespace_except_newline(&mut self) {
        while !self.char_stream.is_at_end()
            && self.char_stream.current_is_whitespace()
            && !self.char_stream.current_is('\n')
        {
            self.advance();
        }
    }

    /// Checks if we're at the start of a line.
    fn is_at_line_start(&self) -> bool {
        self.char_stream.position().column == 1
    }

    /// Peeks at the next character without advancing.
    fn peek_char(&self) -> Option<char> {
        self.char_stream.peek()?.chars().next()
    }

    /// Tokenizes indentation at the start of a line.
    fn tokenize_indent(&mut self) -> Result<Token<'input>> {
        let mut indent_count = 0;
        while !self.char_stream.is_at_end() && self.char_stream.current_is(' ') {
            indent_count += 1;
            self.advance();
        }
        Ok(Token::Indent(indent_count))
    }

    /// Tokenizes ATX headings (# ## ### etc.).
    fn tokenize_atx_heading(&mut self) -> Result<Token<'input>> {
        let mut level = 0;
        let start_offset = self.char_stream.current_offset().unwrap_or(0);

        // Count # characters
        while !self.char_stream.is_at_end() && self.char_stream.current_is('#') && level < 6 {
            level += 1;
            self.advance();
        }

        // Must be followed by space or end of line for valid heading
        if !self.char_stream.is_at_end()
            && !self.char_stream.current_is_whitespace()
            && !self.char_stream.current_is('\n')
        {
            // Not a valid heading, treat as text
            let text = self.char_stream.slice_from(start_offset);
            return Ok(Token::Text(text));
        }

        // Skip optional space after #
        if self.char_stream.current_is(' ') {
            self.advance();
        }

        // Get heading content
        let content_start = self
            .char_stream
            .current_offset()
            .unwrap_or(self.char_stream.input.len());
        while !self.char_stream.is_at_end() && !self.char_stream.current_is('\n') {
            self.advance();
        }
        let content = self.char_stream.slice_from(content_start).trim_end();

        Ok(Token::AtxHeading {
            level: level as u8,
            content,
        })
    }

    /// Checks if current position could start a thematic break.
    fn is_thematic_break_start(&self) -> bool {
        if !matches!(
            self.char_stream.current(),
            Some("-") | Some("*") | Some("_")
        ) {
            return false;
        }

        // Look ahead to see if we have at least 3 consecutive markers
        let marker = self.char_stream.current().unwrap();
        let marker_char = marker.chars().next().unwrap();
        let current_offset = self.char_stream.current_offset().unwrap_or(0);
        let remaining = &self.char_stream.input[current_offset..];

        let mut count = 0;

        for ch in remaining.chars() {
            if ch == marker_char {
                count += 1;
            } else if ch == ' ' || ch == '\t' {
                continue;
            } else if ch == '\n' {
                break;
            } else {
                // Non-whitespace content on the line disqualifies a thematic break
                return false;
            }
        }

        count >= 3
    }

    /// Tokenizes thematic breaks (--- *** ___).
    fn tokenize_thematic_break(&mut self) -> Result<Token<'input>> {
        let marker = self.char_stream.current().unwrap_or("");
        let marker_char = marker.chars().next().unwrap();
        let mut count = 0;
        let start_offset = self.char_stream.current_offset().unwrap_or(0);

        // Count consecutive markers, allowing spaces
        while !self.char_stream.is_at_end() && !self.char_stream.current_is('\n') {
            if self.char_stream.current_is(marker_char) {
                count += 1;
                self.advance();
            } else if self.char_stream.current_is(' ') {
                self.advance();
            } else {
                // Invalid character, treat as text
                let text = self.char_stream.slice_from(start_offset);
                return Ok(Token::Text(text));
            }
        }

        if count >= 3 {
            Ok(Token::ThematicBreak)
        } else {
            // Not enough markers, treat as text
            let text = self.char_stream.slice_from(start_offset);
            Ok(Token::Text(text))
        }
    }

    /// Checks if current position starts a code fence.
    fn is_code_fence_start(&self) -> bool {
        // Look ahead to see if we have at least 3 backticks
        let mut temp_lexer = self.clone_state();
        let mut count = 0;
        while !temp_lexer.char_stream.is_at_end() && temp_lexer.char_stream.current_is('`') {
            count += 1;
            temp_lexer.advance();
        }
        count >= 3
    }

    /// Tokenizes code blocks (```).
    fn tokenize_code_block(&mut self) -> Result<Token<'input>> {
        let mut fence_count = 0;

        // Count opening fence
        while !self.char_stream.is_at_end() && self.char_stream.current_is('`') {
            fence_count += 1;
            self.advance();
        }

        if fence_count < 3 {
            // Not a code block, handle as code span or text
            return self.tokenize_code_span();
        }

        // Get info string (language identifier)
        let info_start = self
            .char_stream
            .current_offset()
            .unwrap_or(self.char_stream.input.len());
        while !self.char_stream.is_at_end() && !self.char_stream.current_is('\n') {
            self.advance();
        }
        let info_str = self.char_stream.slice_from(info_start).trim();
        let info = if info_str.is_empty() {
            None
        } else {
            Some(info_str)
        };

        // Skip newline after opening fence
        if self.char_stream.current_is('\n') {
            self.advance();
        }

        // Get code content
        let content_start = self
            .char_stream
            .current_offset()
            .unwrap_or(self.char_stream.input.len());
        let mut content_end = content_start;

        // Find closing fence
        while !self.char_stream.is_at_end() {
            if self.char_stream.current_is('`') {
                let fence_start = self.char_stream.current_offset().unwrap_or(0);
                let mut closing_count = 0;

                while !self.char_stream.is_at_end() && self.char_stream.current_is('`') {
                    closing_count += 1;
                    self.advance();
                }

                if closing_count >= fence_count {
                    // Found closing fence
                    content_end = fence_start;
                    break;
                }
            } else {
                self.advance();
            }
        }

        let content = &self.char_stream.input[content_start..content_end];
        Ok(Token::CodeBlock { info, content })
    }

    /// Tries to tokenize list markers.
    fn try_tokenize_list_marker(&mut self) -> Result<Option<Token<'input>>> {
        // List markers should only be detected at the beginning of a line or after whitespace
        // For now, we'll use a simple heuristic: check if we're at the start or after newline
        let position = self.char_stream.position();
        let is_at_line_start = position.column == 1;

        // Only check for list markers at the start of a line for now
        // This prevents conflicts with emphasis markers in the middle of text
        if !is_at_line_start {
            return Ok(None);
        }

        let current_offset = self.char_stream.current_offset().unwrap_or(0);

        // Bullet lists: - + *
        if matches!(
            self.char_stream.current(),
            Some("-") | Some("+") | Some("*")
        ) {
            let marker = self.char_stream.current().unwrap().chars().next().unwrap();

            // Check if the character after the marker is a space by looking at the input string directly
            let next_offset = current_offset + 1;
            if next_offset < self.char_stream.input.len() {
                let next_char = &self.char_stream.input[next_offset..next_offset + 1];
                if next_char == " " {
                    self.advance(); // Consume the marker
                    return Ok(Some(Token::ListMarker {
                        kind: ListKind::Bullet { marker },
                        indent: 0, // Will be calculated by parser
                    }));
                }
            }
        }

        // Ordered lists: 1. 2) etc.
        if let Some(current) = self.char_stream.current()
            && let Some(_digit) = current.chars().next().and_then(|c| c.to_digit(10))
        {
            // Use a simple string-based approach to check for ordered list pattern
            let remaining = &self.char_stream.input[current_offset..];

            // Look for pattern like "1. " or "123) "
            let mut chars = remaining.chars();
            let mut number_str = String::new();

            // Collect digits
            while let Some(ch) = chars.next() {
                if ch.is_ascii_digit() {
                    number_str.push(ch);
                } else if ch == '.' || ch == ')' {
                    // Check if followed by space
                    if chars.next() == Some(' ') {
                        let number = number_str.parse::<u32>().unwrap_or(0);

                        // Advance past the number and delimiter
                        for _ in 0..number_str.len() {
                            self.advance();
                        }
                        self.advance(); // delimiter

                        return Ok(Some(Token::ListMarker {
                            kind: ListKind::Ordered {
                                start: number,
                                delimiter: ch,
                            },
                            indent: 0,
                        }));
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
        }

        Ok(None)
    }

    /// Checks if current character is an emphasis marker.
    fn is_emphasis_marker(&self) -> bool {
        matches!(self.char_stream.current(), Some("*") | Some("_"))
    }

    /// Tokenizes emphasis markers (* _ ** __).
    fn tokenize_emphasis(&mut self) -> Result<Token<'input>> {
        let marker = self.char_stream.current().unwrap().chars().next().unwrap();
        let mut count = 0;

        while !self.char_stream.is_at_end() && self.char_stream.current_is(marker) {
            count += 1;
            self.advance();
        }

        Ok(Token::Emphasis { marker, count })
    }

    /// Tokenizes code spans (`code`).
    fn tokenize_code_span(&mut self) -> Result<Token<'input>> {
        let mut backtick_count = 0;

        // Count opening backticks
        while !self.char_stream.is_at_end() && self.char_stream.current_is('`') {
            backtick_count += 1;
            self.advance();
        }

        let content_start = self
            .char_stream
            .current_offset()
            .unwrap_or(self.char_stream.input.len());
        let mut content_end = content_start;

        // Find closing backticks
        while !self.char_stream.is_at_end() {
            if self.char_stream.current_is('`') {
                let closing_start = self.char_stream.current_offset().unwrap_or(0);
                let mut closing_count = 0;

                while !self.char_stream.is_at_end() && self.char_stream.current_is('`') {
                    closing_count += 1;
                    self.advance();
                }

                if closing_count == backtick_count {
                    content_end = closing_start;
                    break;
                }
            } else {
                self.advance();
            }
        }

        let content = &self.char_stream.input[content_start..content_end];
        Ok(Token::CodeSpan(content))
    }

    /// Tokenizes links and images ([ ![).
    fn tokenize_link_or_image(&mut self) -> Result<Token<'input>> {
        let is_image = self.char_stream.current_is('!');
        if is_image {
            self.advance(); // Skip !
        }

        if !self.char_stream.current_is('[') {
            // Not a valid link/image start
            return self.tokenize_text();
        }

        self.advance(); // Skip [

        // For now, create a simple placeholder implementation
        // Full link parsing will be implemented in later tasks
        let text_start = self
            .char_stream
            .current_offset()
            .unwrap_or(self.char_stream.input.len());

        // Find closing ]
        while !self.char_stream.is_at_end() && !self.char_stream.current_is(']') {
            self.advance();
        }

        let text_content = self.char_stream.slice_from(text_start);

        if self.char_stream.current_is(']') {
            self.advance(); // Skip ]
        }

        // Simplified link/image token for now
        if is_image {
            Ok(Token::Image {
                alt: text_content,
                dest: "", // Will be parsed properly in later tasks
                title: None,
            })
        } else {
            Ok(Token::Link {
                text: text_content,
                dest: "", // Will be parsed properly in later tasks
                title: None,
            })
        }
    }

    /// Tokenizes regular text.
    fn tokenize_text(&mut self) -> Result<Token<'input>> {
        let start_offset = self.char_stream.current_offset().unwrap_or(0);

        // Consume characters until we hit a special character or whitespace
        while !self.char_stream.is_at_end() {
            // Check if current character is a special Markdown character
            if self.char_stream.current_is('\n')
                || self.char_stream.current_is(' ')
                || self.char_stream.current_is('\t')
                || self.char_stream.current_is('#')
                || self.char_stream.current_is('*')
                || self.char_stream.current_is('_')
                || self.char_stream.current_is('`')
                || self.char_stream.current_is('[')
                || self.char_stream.current_is(']')
                || self.char_stream.current_is('!')
                || self.char_stream.current_is('>')
                || self.char_stream.current_is('-')
                || self.char_stream.current_is('+')
                || self.char_stream.current_is('=')
                || self.char_stream.current_is('~')
            {
                break;
            }

            self.advance();
        }

        let text = self.char_stream.slice_from(start_offset);
        if text.is_empty() {
            // If no text was consumed, we hit a special character immediately
            // This should not happen if the tokenizer is working correctly,
            // but we'll handle it gracefully by consuming one character
            self.advance();
            let single_char = self.char_stream.slice_from(start_offset);
            Ok(Token::Text(single_char))
        } else {
            Ok(Token::Text(text))
        }
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

    #[test]
    fn test_char_stream_creation() {
        let input = "Hello, 🌍!";
        let stream = CharStream::new(input);
        assert_eq!(stream.position().line, 1);
        assert_eq!(stream.position().column, 1);
        assert_eq!(stream.position().offset, 0);
        assert_eq!(stream.current(), Some("H"));
    }

    #[test]
    fn test_char_stream_unicode_handling() {
        let input = "🌍👋";
        let mut stream = CharStream::new(input);

        // First grapheme cluster (earth emoji)
        assert_eq!(stream.current(), Some("🌍"));
        assert_eq!(stream.position().column, 1);

        // Advance to next grapheme cluster
        stream.advance();
        assert_eq!(stream.current(), Some("👋"));
        assert_eq!(stream.position().column, 2);

        // Advance to end
        stream.advance();
        assert!(stream.is_at_end());
    }

    #[test]
    fn test_char_stream_newline_handling() {
        let input = "line1\nline2";
        let mut stream = CharStream::new(input);

        // Skip to newline
        while !stream.current_is('\n') && !stream.is_at_end() {
            stream.advance();
        }

        assert_eq!(stream.current(), Some("\n"));
        assert_eq!(stream.position().line, 1);
        assert_eq!(stream.position().column, 6);

        // Advance past newline
        stream.advance();
        assert_eq!(stream.position().line, 2);
        assert_eq!(stream.position().column, 1);
    }

    #[test]
    fn test_basic_tokenization() {
        let input = "Hello World";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token().unwrap();
        assert!(matches!(token1, Token::Text("Hello")));

        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::Text("World")));

        let token3 = lexer.next_token().unwrap();
        assert!(matches!(token3, Token::Eof));
    }

    #[test]
    fn test_newline_tokenization() {
        let input = "line1\nline2";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token().unwrap();
        assert!(matches!(token1, Token::Text("line1")));

        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::Newline));

        let token3 = lexer.next_token().unwrap();
        assert!(matches!(token3, Token::Text("line2")));
    }

    #[test]
    fn test_atx_heading_tokenization() {
        let input = "# Heading 1\n## Heading 2";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token().unwrap();
        if let Token::AtxHeading { level, content } = token1 {
            assert_eq!(level, 1);
            assert_eq!(content, "Heading 1");
        } else {
            panic!("Expected AtxHeading token, got {:?}", token1);
        }

        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::Newline));

        let token3 = lexer.next_token().unwrap();
        if let Token::AtxHeading { level, content } = token3 {
            assert_eq!(level, 2);
            assert_eq!(content, "Heading 2");
        } else {
            panic!("Expected AtxHeading token, got {:?}", token3);
        }
    }

    #[test]
    fn test_emphasis_tokenization() {
        // Test simple case first
        let input = "*test*";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token().unwrap();
        assert!(matches!(
            token1,
            Token::Emphasis {
                marker: '*',
                count: 1
            }
        ));

        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::Text("test")));

        let token3 = lexer.next_token().unwrap();
        assert!(matches!(
            token3,
            Token::Emphasis {
                marker: '*',
                count: 1
            }
        ));

        // Test more complex case
        let input2 = "*emphasis* **strong**";
        let mut lexer2 = Lexer::new(input2);

        let token1 = lexer2.next_token().unwrap();
        if let Token::Emphasis { marker, count } = token1 {
            assert_eq!(marker, '*');
            assert_eq!(count, 1);
        } else {
            panic!("Expected Emphasis token, got {:?}", token1);
        }

        let token2 = lexer2.next_token().unwrap();
        assert!(matches!(token2, Token::Text("emphasis")));

        let token3 = lexer2.next_token().unwrap();
        if let Token::Emphasis { marker, count } = token3 {
            assert_eq!(marker, '*');
            assert_eq!(count, 1);
        } else {
            panic!("Expected Emphasis token, got {:?}", token3);
        }

        let token4 = lexer2.next_token().unwrap();
        if let Token::Emphasis { marker, count } = token4 {
            assert_eq!(marker, '*');
            assert_eq!(count, 2);
        } else {
            panic!("Expected Emphasis token, got {:?}", token4);
        }
    }

    #[test]
    fn test_code_span_tokenization() {
        let input = "`code` and `more code`";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token().unwrap();
        if let Token::CodeSpan(content) = token1 {
            assert_eq!(content, "code");
        } else {
            panic!("Expected CodeSpan token, got {:?}", token1);
        }

        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::Text("and")));

        let token3 = lexer.next_token().unwrap();
        if let Token::CodeSpan(content) = token3 {
            assert_eq!(content, "more code");
        } else {
            panic!("Expected CodeSpan token, got {:?}", token3);
        }
    }

    #[test]
    fn test_list_marker_tokenization() {
        let input = "- item\n1. numbered";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token().unwrap();
        if let Token::ListMarker { kind, .. } = token1 {
            if let ListKind::Bullet { marker } = kind {
                assert_eq!(marker, '-');
            } else {
                panic!("Expected bullet list marker");
            }
        } else {
            panic!("Expected ListMarker token, got {:?}", token1);
        }

        // Skip to next line
        lexer.next_token().unwrap(); // "item"
        lexer.next_token().unwrap(); // Newline

        let token4 = lexer.next_token().unwrap();
        if let Token::ListMarker { kind, .. } = token4 {
            if let ListKind::Ordered { start, delimiter } = kind {
                assert_eq!(start, 1);
                assert_eq!(delimiter, '.');
            } else {
                panic!("Expected ordered list marker");
            }
        } else {
            panic!("Expected ListMarker token, got {:?}", token4);
        }
    }

    #[test]
    fn test_peek_token() {
        let input = "# Heading";
        let mut lexer = Lexer::new(input);

        // Peek should return the same token multiple times
        let peeked1 = lexer.peek_token().unwrap();
        let peeked2 = lexer.peek_token().unwrap();
        assert_eq!(peeked1, peeked2);

        // Next token should match what we peeked
        let next = lexer.next_token().unwrap();
        assert_eq!(next, peeked1);
    }

    #[test]
    fn test_unicode_text_tokenization() {
        let input = "Hello 🌍 World 👋";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token().unwrap();
        assert!(matches!(token1, Token::Text("Hello")));

        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::Text("🌍")));

        let token3 = lexer.next_token().unwrap();
        assert!(matches!(token3, Token::Text("World")));

        let token4 = lexer.next_token().unwrap();
        assert!(matches!(token4, Token::Text("👋")));
    }
}
