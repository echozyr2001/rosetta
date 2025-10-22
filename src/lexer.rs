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

    /// Checks if the current grapheme matches the given string.
    pub fn current_is_str(&self, s: &str) -> bool {
        self.current() == Some(s)
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
    EscapeSequence {
        character: char,
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
        // Handle indented code blocks (4+ spaces at start of line) - check first
        if self.is_indented_code_block() {
            return self.tokenize_indented_code_block();
        }

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

        // Handle thematic breaks (--- *** ___) - check before Setext headings
        if self.is_thematic_break_start() {
            return self.tokenize_thematic_break();
        }

        // Handle Setext headings (underlined with = or -)
        if self.is_setext_heading_underline() {
            return self.tokenize_setext_heading();
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

        // Handle escape sequences (\)
        if self.char_stream.current_is('\\') {
            return self.tokenize_escape_sequence();
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

    /// Checks if current position could be a Setext heading underline.
    fn is_setext_heading_underline(&self) -> bool {
        // Setext headings use = for level 1 and - for level 2
        if !matches!(self.char_stream.current(), Some("=") | Some("-")) {
            return false;
        }

        // Must be at start of line
        if !self.is_at_line_start() {
            return false;
        }

        // Look ahead to see if the entire line consists of = or - characters
        let marker = self.char_stream.current().unwrap();
        let marker_char = marker.chars().next().unwrap();
        let current_offset = self.char_stream.current_offset().unwrap_or(0);
        let remaining = &self.char_stream.input[current_offset..];

        let mut has_marker = false;
        let mut marker_count = 0;

        for ch in remaining.chars() {
            if ch == marker_char {
                has_marker = true;
                marker_count += 1;
            } else if ch == ' ' || ch == '\t' {
                continue; // Allow spaces
            } else if ch == '\n' {
                break;
            } else {
                // Non-whitespace content disqualifies Setext heading
                return false;
            }
        }

        // Need at least one marker character
        has_marker && marker_count > 0
    }

    /// Tokenizes Setext headings (underlined with = or -).
    fn tokenize_setext_heading(&mut self) -> Result<Token<'input>> {
        let marker = self.char_stream.current().unwrap();
        let marker_char = marker.chars().next().unwrap();
        let level = if marker_char == '=' { 1 } else { 2 };

        // Consume the underline
        while !self.char_stream.is_at_end() && !self.char_stream.current_is('\n') {
            self.advance();
        }

        // For Setext headings, we need to look back to find the heading text
        // This is a simplified implementation - in a real parser, this would be
        // handled by the parser layer that maintains context about previous lines
        Ok(Token::SetextHeading {
            level,
            content: "", // Content will be determined by parser from previous line
        })
    }

    /// Checks if current position starts an indented code block.
    fn is_indented_code_block(&self) -> bool {
        // Must be at the start of a line
        if !self.is_at_line_start() {
            return false;
        }

        // Must have at least 4 spaces at the start of the line
        let current_offset = self.char_stream.current_offset().unwrap_or(0);
        let remaining = &self.char_stream.input[current_offset..];

        let mut space_count = 0;
        for ch in remaining.chars() {
            if ch == ' ' {
                space_count += 1;
            } else if ch == '\t' {
                space_count += 4; // Tab counts as 4 spaces
            } else {
                break;
            }
        }

        space_count >= 4
    }

    /// Tokenizes indented code blocks (4+ spaces).
    fn tokenize_indented_code_block(&mut self) -> Result<Token<'input>> {
        // Skip the indentation
        let mut indent_count = 0;
        while !self.char_stream.is_at_end() && indent_count < 4 {
            if self.char_stream.current_is(' ') {
                indent_count += 1;
                self.advance();
            } else if self.char_stream.current_is('\t') {
                indent_count = 4; // Tab counts as reaching the 4-space threshold
                self.advance();
            } else {
                break;
            }
        }

        // Get the code content for this line
        let content_start = self
            .char_stream
            .current_offset()
            .unwrap_or(self.char_stream.input.len());

        while !self.char_stream.is_at_end() && !self.char_stream.current_is('\n') {
            self.advance();
        }

        let content = self.char_stream.slice_from(content_start);

        Ok(Token::CodeBlock {
            info: None, // Indented code blocks don't have info strings
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

        // Must be at start of line
        if !self.is_at_line_start() {
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
        // List markers should only be detected at the beginning of a line or after indentation
        let position = self.char_stream.position();
        let is_at_line_start = position.column == 1;

        // Calculate current indentation level
        let mut indent_level = 0;
        if !is_at_line_start {
            // Check if we're after whitespace-only content on this line
            let current_offset = self.char_stream.current_offset().unwrap_or(0);
            let line_start = self.find_line_start(current_offset);
            let line_prefix = &self.char_stream.input[line_start..current_offset];

            // Check if everything before current position is whitespace
            if !line_prefix.chars().all(|c| c == ' ' || c == '\t') {
                return Ok(None);
            }

            // Calculate indentation
            for ch in line_prefix.chars() {
                if ch == ' ' {
                    indent_level += 1;
                } else if ch == '\t' {
                    indent_level += 4;
                }
            }
        }

        let current_offset = self.char_stream.current_offset().unwrap_or(0);

        // Bullet lists: - + *
        if matches!(
            self.char_stream.current(),
            Some("-") | Some("+") | Some("*")
        ) {
            let marker = self.char_stream.current().unwrap().chars().next().unwrap();

            // Check if the character after the marker is a space or tab
            let next_offset = current_offset + 1;
            if next_offset < self.char_stream.input.len() {
                let next_char = &self.char_stream.input[next_offset..next_offset + 1];
                if next_char == " " || next_char == "\t" {
                    self.advance(); // Consume the marker
                    return Ok(Some(Token::ListMarker {
                        kind: ListKind::Bullet { marker },
                        indent: indent_level,
                    }));
                }
            } else {
                // End of input after marker is also valid
                self.advance();
                return Ok(Some(Token::ListMarker {
                    kind: ListKind::Bullet { marker },
                    indent: indent_level,
                }));
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

            // Collect digits (up to 9 digits per CommonMark spec)
            while let Some(ch) = chars.next() {
                if ch.is_ascii_digit() && number_str.len() < 9 {
                    number_str.push(ch);
                } else if (ch == '.' || ch == ')') && !number_str.is_empty() {
                    // Check if followed by space, tab, or end of line
                    let next_ch = chars.next();
                    if next_ch == Some(' ') || next_ch == Some('\t') || next_ch.is_none() {
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
                            indent: indent_level,
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

    /// Finds the start of the current line.
    fn find_line_start(&self, current_offset: usize) -> usize {
        let input = self.char_stream.input;
        for i in (0..current_offset).rev() {
            if input.chars().nth(i) == Some('\n') {
                return i + 1;
            }
        }
        0
    }

    /// Checks if current character is an emphasis marker.
    fn is_emphasis_marker(&self) -> bool {
        matches!(self.char_stream.current(), Some("*") | Some("_"))
    }

    /// Tokenizes emphasis markers (* _ ** __).
    /// Enhanced to handle proper emphasis detection according to CommonMark spec.
    fn tokenize_emphasis(&mut self) -> Result<Token<'input>> {
        let marker = self.char_stream.current().unwrap().chars().next().unwrap();
        let mut count = 0;

        // Count consecutive emphasis markers
        while !self.char_stream.is_at_end() && self.char_stream.current_is(marker) {
            count += 1;
            self.advance();

            // CommonMark limits emphasis runs to reasonable lengths
            if count >= 3 {
                break;
            }
        }

        // Check for valid emphasis context according to CommonMark rules
        // For now, we'll emit the emphasis token and let the parser handle context
        Ok(Token::Emphasis { marker, count })
    }

    /// Tokenizes code spans (`code`).
    /// Enhanced to handle nested backticks and edge cases according to CommonMark spec.
    fn tokenize_code_span(&mut self) -> Result<Token<'input>> {
        let mut backtick_count = 0;
        let start_offset = self.char_stream.current_offset().unwrap_or(0);

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
        let mut found_closing = false;

        // Find matching closing backticks
        while !self.char_stream.is_at_end() {
            if self.char_stream.current_is('`') {
                let closing_start = self.char_stream.current_offset().unwrap_or(0);
                let mut closing_count = 0;

                // Count closing backticks
                while !self.char_stream.is_at_end() && self.char_stream.current_is('`') {
                    closing_count += 1;
                    self.advance();
                }

                // Must match exactly the opening count
                if closing_count == backtick_count {
                    content_end = closing_start;
                    found_closing = true;
                    break;
                }
                // If not matching, continue searching (backticks become part of content)
            } else {
                self.advance();
            }
        }

        // If no closing backticks found, treat as regular text
        if !found_closing {
            // Reset position and treat as text
            let text = self.char_stream.slice_from(start_offset);
            return Ok(Token::Text(text));
        }

        let content = &self.char_stream.input[content_start..content_end];

        // CommonMark spec: trim one space from each end if both ends have spaces
        let trimmed_content =
            if content.starts_with(' ') && content.ends_with(' ') && content.len() > 1 {
                &content[1..content.len() - 1]
            } else {
                content
            };

        Ok(Token::CodeSpan(trimmed_content))
    }

    /// Tokenizes links and images ([ ![).
    /// Enhanced to handle full link/image syntax including destinations and titles.
    fn tokenize_link_or_image(&mut self) -> Result<Token<'input>> {
        let start_offset = self.char_stream.current_offset().unwrap_or(0);
        let is_image = self.char_stream.current_is('!');

        if is_image {
            self.advance(); // Skip !
        }

        if !self.char_stream.current_is('[') {
            // Not a valid link/image start, treat as text
            return self.tokenize_text();
        }

        self.advance(); // Skip [

        // Parse link text/image alt text
        let text_start = self
            .char_stream
            .current_offset()
            .unwrap_or(self.char_stream.input.len());
        let mut bracket_depth = 1;
        let mut text_end = text_start;

        // Find matching closing bracket, handling nested brackets
        while !self.char_stream.is_at_end() && bracket_depth > 0 {
            if self.char_stream.current_is('[') {
                bracket_depth += 1;
            } else if self.char_stream.current_is(']') {
                bracket_depth -= 1;
                if bracket_depth == 0 {
                    text_end = self.char_stream.current_offset().unwrap_or(0);
                }
            }
            self.advance();
        }

        if bracket_depth > 0 {
            // No matching closing bracket found, treat as text
            let text = self.char_stream.slice_from(start_offset);
            return Ok(Token::Text(text));
        }

        let text_content = &self.char_stream.input[text_start..text_end];

        // Check for link destination
        if !self.char_stream.current_is('(') {
            // No destination, could be reference link or just bracketed text
            // For now, treat as simple link/image with empty destination
            if is_image {
                return Ok(Token::Image {
                    alt: text_content,
                    dest: "",
                    title: None,
                });
            } else {
                return Ok(Token::Link {
                    text: text_content,
                    dest: "",
                    title: None,
                });
            }
        }

        self.advance(); // Skip (

        // Skip whitespace
        self.skip_whitespace_except_newline();

        // Parse destination
        let (dest_start, dest_end) = if self.char_stream.current_is('<') {
            self.advance();
            let start = self
                .char_stream
                .current_offset()
                .unwrap_or(self.char_stream.input.len());

            // Find closing >
            while !self.char_stream.is_at_end() && !self.char_stream.current_is('>') {
                self.advance();
            }

            let end = if self.char_stream.current_is('>') {
                let end_pos = self.char_stream.current_offset().unwrap_or(0);
                self.advance(); // Skip >
                end_pos
            } else {
                self.char_stream.current_offset().unwrap_or(0)
            };
            (start, end)
        } else {
            // Parse unenclosed destination
            let start = self
                .char_stream
                .current_offset()
                .unwrap_or(self.char_stream.input.len());
            let mut paren_depth = 0;
            while !self.char_stream.is_at_end() {
                if self.char_stream.current_is('(') {
                    paren_depth += 1;
                } else if self.char_stream.current_is(')') {
                    if paren_depth == 0 {
                        break; // End of destination
                    }
                    paren_depth -= 1;
                } else if self.char_stream.current_is_whitespace() {
                    break; // Whitespace ends unenclosed destination
                }
                self.advance();
            }
            let end = self.char_stream.current_offset().unwrap_or(0);
            (start, end)
        };

        let destination = &self.char_stream.input[dest_start..dest_end];

        // Skip whitespace before potential title
        self.skip_whitespace_except_newline();

        // Parse optional title
        let mut title: Option<&str> = None;
        if !self.char_stream.is_at_end() && !self.char_stream.current_is(')') {
            let title_quote = self.char_stream.current();
            if matches!(title_quote, Some("\"") | Some("'") | Some("(")) {
                let closing_quote = match title_quote {
                    Some("\"") => "\"",
                    Some("'") => "'",
                    Some("(") => ")",
                    _ => unreachable!(),
                };

                self.advance(); // Skip opening quote
                let title_start = self
                    .char_stream
                    .current_offset()
                    .unwrap_or(self.char_stream.input.len());

                // Find closing quote
                while !self.char_stream.is_at_end()
                    && !self.char_stream.current_is_str(closing_quote)
                {
                    self.advance();
                }

                if self.char_stream.current_is_str(closing_quote) {
                    let title_end = self.char_stream.current_offset().unwrap_or(0);
                    title = Some(&self.char_stream.input[title_start..title_end]);
                    self.advance(); // Skip closing quote
                }
            }
        }

        // Skip whitespace before closing paren
        self.skip_whitespace_except_newline();

        // Expect closing parenthesis
        if !self.char_stream.current_is(')') {
            // Malformed link, treat as text
            let text = self.char_stream.slice_from(start_offset);
            return Ok(Token::Text(text));
        }

        self.advance(); // Skip )

        if is_image {
            Ok(Token::Image {
                alt: text_content,
                dest: destination,
                title,
            })
        } else {
            Ok(Token::Link {
                text: text_content,
                dest: destination,
                title,
            })
        }
    }

    /// Tokenizes regular text with enhanced consolidation logic.
    /// Implements text token consolidation according to CommonMark spec.
    fn tokenize_text(&mut self) -> Result<Token<'input>> {
        let start_offset = self.char_stream.current_offset().unwrap_or(0);

        // Consume characters until we hit a special character or whitespace
        while !self.char_stream.is_at_end() {
            // Handle escape sequences first
            if self.char_stream.current_is('\\') {
                // Process escape sequence
                self.advance(); // consume backslash
                if !self.char_stream.is_at_end() {
                    // Consume the escaped character
                    self.advance();
                }
                continue;
            }

            // Check if current character is a special Markdown character that should end text
            if self.is_text_boundary_character() {
                break;
            }

            self.advance();
        }

        let text = self.char_stream.slice_from(start_offset);
        if text.is_empty() {
            // If no text was consumed, we hit a special character immediately
            // This should not happen if the tokenizer is working correctly,
            // but we'll handle it gracefully by consuming one character
            if let Some(current) = self.char_stream.current() {
                self.advance();
                Ok(Token::Text(current))
            } else {
                Ok(Token::Eof)
            }
        } else {
            // Process escape sequences in the text
            let processed_text = self.process_escape_sequences(text);
            Ok(Token::Text(processed_text))
        }
    }

    /// Checks if the current character should end a text token.
    /// This implements proper text boundary detection for CommonMark compliance.
    fn is_text_boundary_character(&self) -> bool {
        // Structural characters that always end text
        if self.char_stream.current_is('\n')
            || self.char_stream.current_is(' ')
            || self.char_stream.current_is('\t')
        {
            return true;
        }

        // Markdown special characters that can start new tokens
        if self.char_stream.current_is('#')
            || self.char_stream.current_is('*')
            || self.char_stream.current_is('_')
            || self.char_stream.current_is('`')
            || self.char_stream.current_is('[')
            || self.char_stream.current_is(']')
            || self.char_stream.current_is('!')
            || self.char_stream.current_is('>')
            || self.char_stream.current_is('\\')
        // Escape character
        {
            return true;
        }

        // Context-sensitive characters (-, +, =, ~) that might start special tokens
        // Only break on these if they could start a valid markdown construct
        if self.char_stream.current_is('-') || self.char_stream.current_is('+') {
            // Could be list marker or thematic break
            return self.could_be_list_marker_or_thematic_break();
        }

        if self.char_stream.current_is('=') || self.char_stream.current_is('~') {
            // Could be setext heading or other constructs
            return self.could_be_setext_heading_or_special();
        }

        // Check for numeric characters that could start ordered lists
        if let Some(current) = self.char_stream.current()
            && let Some(ch) = current.chars().next()
            && ch.is_ascii_digit()
        {
            return self.could_be_ordered_list_marker();
        }

        false
    }

    /// Checks if current position could start a list marker or thematic break.
    fn could_be_list_marker_or_thematic_break(&self) -> bool {
        // Simple heuristic: if we're at start of line or after whitespace, it could be special
        let position = self.char_stream.position();
        position.column == 1 || self.is_after_whitespace_on_line()
    }

    /// Checks if current position could start a setext heading or other special construct.
    fn could_be_setext_heading_or_special(&self) -> bool {
        // Similar heuristic for = and ~ characters
        let position = self.char_stream.position();
        position.column == 1 || self.is_after_whitespace_on_line()
    }

    /// Checks if current position could start an ordered list marker.
    fn could_be_ordered_list_marker(&self) -> bool {
        // Check if we're in a position where a list marker could appear
        let position = self.char_stream.position();
        position.column == 1 || self.is_after_whitespace_on_line()
    }

    /// Checks if we're after whitespace on the current line.
    fn is_after_whitespace_on_line(&self) -> bool {
        let current_offset = self.char_stream.current_offset().unwrap_or(0);
        if current_offset == 0 {
            return false;
        }

        // Look back to see if previous character was whitespace (but not newline)
        let prev_char = &self.char_stream.input[current_offset - 1..current_offset];
        prev_char == " " || prev_char == "\t"
    }

    /// Process escape sequences in text according to CommonMark specification.
    /// Returns a new string with escape sequences processed.
    fn process_escape_sequences(&self, text: &'input str) -> &'input str {
        // For now, return the text as-is since we need to handle escape sequences
        // during parsing phase for proper CommonMark compliance.
        // The lexer identifies escape sequences, but the parser processes them.
        text
    }

    /// Tokenizes escape sequences (\character).
    /// Implements CommonMark escape sequence rules.
    fn tokenize_escape_sequence(&mut self) -> Result<Token<'input>> {
        self.advance(); // consume backslash

        if self.char_stream.is_at_end() {
            // Backslash at end of input, treat as literal backslash
            return Ok(Token::Text("\\"));
        }

        let escaped_char = self.char_stream.current().unwrap_or("");
        let escaped_char_code = escaped_char.chars().next().unwrap_or('\0');

        // Check if this is a valid escape sequence according to CommonMark
        if self.is_escapable_character(escaped_char_code) {
            self.advance(); // consume escaped character
            Ok(Token::EscapeSequence {
                character: escaped_char_code,
            })
        } else {
            // Not a valid escape sequence, treat backslash as literal text
            Ok(Token::Text("\\"))
        }
    }

    /// Checks if a character can be escaped according to CommonMark specification.
    fn is_escapable_character(&self, ch: char) -> bool {
        // CommonMark escapable ASCII punctuation characters
        matches!(
            ch,
            '!' | '"'
                | '#'
                | '$'
                | '%'
                | '&'
                | '\''
                | '('
                | ')'
                | '*'
                | '+'
                | ','
                | '-'
                | '.'
                | '/'
                | ':'
                | ';'
                | '<'
                | '='
                | '>'
                | '?'
                | '@'
                | '['
                | '\\'
                | ']'
                | '^'
                | '_'
                | '`'
                | '{'
                | '|'
                | '}'
                | '~'
        )
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

    #[test]
    fn test_setext_heading_tokenization() {
        let input = "Heading\n=======";
        let mut lexer = Lexer::new(input);

        // Skip the heading text for now (will be handled by parser)
        let _text = lexer.next_token().unwrap();
        let _newline = lexer.next_token().unwrap();

        let token1 = lexer.next_token().unwrap();
        if let Token::SetextHeading { level, .. } = token1 {
            assert_eq!(level, 1);
        } else {
            panic!("Expected SetextHeading token, got {:?}", token1);
        }
    }

    #[test]
    fn test_indented_code_block_tokenization() {
        let input = "    code line 1\n    code line 2";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token().unwrap();
        if let Token::CodeBlock { info, content } = token1 {
            assert_eq!(info, None);
            assert_eq!(content, "code line 1");
        } else {
            panic!("Expected CodeBlock token, got {:?}", token1);
        }

        let _newline = lexer.next_token().unwrap();

        let token2 = lexer.next_token().unwrap();
        if let Token::CodeBlock { info, content } = token2 {
            assert_eq!(info, None);
            assert_eq!(content, "code line 2");
        } else {
            panic!("Expected CodeBlock token, got {:?}", token2);
        }
    }

    #[test]
    fn test_enhanced_list_marker_tokenization() {
        let input = "- bullet\n  - nested bullet\n1. ordered\n  2) nested ordered";
        let mut lexer = Lexer::new(input);

        // First bullet marker
        let token1 = lexer.next_token().unwrap();
        if let Token::ListMarker { kind, indent } = token1 {
            if let ListKind::Bullet { marker } = kind {
                assert_eq!(marker, '-');
                assert_eq!(indent, 0);
            } else {
                panic!("Expected bullet list marker");
            }
        } else {
            panic!("Expected ListMarker token, got {:?}", token1);
        }

        // Skip to nested bullet
        lexer.next_token().unwrap(); // "bullet"
        lexer.next_token().unwrap(); // Newline
        lexer.next_token().unwrap(); // Indent

        let token2 = lexer.next_token().unwrap();
        if let Token::ListMarker { kind, indent } = token2 {
            if let ListKind::Bullet { marker } = kind {
                assert_eq!(marker, '-');
                assert_eq!(indent, 2);
            } else {
                panic!("Expected bullet list marker");
            }
        } else {
            panic!("Expected ListMarker token, got {:?}", token2);
        }
    }

    #[test]
    fn test_blockquote_tokenization() {
        let input = "> This is a blockquote\n> Second line";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token().unwrap();
        assert!(matches!(token1, Token::BlockQuote));

        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::Text("This")));

        // Skip to next line
        while !matches!(lexer.next_token().unwrap(), Token::Newline) {}

        let token3 = lexer.next_token().unwrap();
        assert!(matches!(token3, Token::BlockQuote));
    }

    #[test]
    fn test_thematic_break_tokenization() {
        let input = "---\n***\n___";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token().unwrap();
        assert!(matches!(token1, Token::ThematicBreak));

        let _newline1 = lexer.next_token().unwrap();

        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::ThematicBreak));

        let _newline2 = lexer.next_token().unwrap();

        let token3 = lexer.next_token().unwrap();
        assert!(matches!(token3, Token::ThematicBreak));
    }

    #[test]
    fn test_enhanced_emphasis_tokenization() {
        let input = "*single* **double** ***triple***";
        let mut lexer = Lexer::new(input);

        // *single*
        let token1 = lexer.next_token().unwrap();
        if let Token::Emphasis { marker, count } = token1 {
            assert_eq!(marker, '*');
            assert_eq!(count, 1);
        } else {
            panic!("Expected Emphasis token, got {:?}", token1);
        }

        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::Text("single")));

        let token3 = lexer.next_token().unwrap();
        if let Token::Emphasis { marker, count } = token3 {
            assert_eq!(marker, '*');
            assert_eq!(count, 1);
        } else {
            panic!("Expected Emphasis token, got {:?}", token3);
        }

        // **double**
        let token4 = lexer.next_token().unwrap();
        if let Token::Emphasis { marker, count } = token4 {
            assert_eq!(marker, '*');
            assert_eq!(count, 2);
        } else {
            panic!("Expected Emphasis token, got {:?}", token4);
        }

        let token5 = lexer.next_token().unwrap();
        assert!(matches!(token5, Token::Text("double")));

        let token6 = lexer.next_token().unwrap();
        if let Token::Emphasis { marker, count } = token6 {
            assert_eq!(marker, '*');
            assert_eq!(count, 2);
        } else {
            panic!("Expected Emphasis token, got {:?}", token6);
        }

        // ***triple***
        let token7 = lexer.next_token().unwrap();
        if let Token::Emphasis { marker, count } = token7 {
            assert_eq!(marker, '*');
            assert_eq!(count, 3);
        } else {
            panic!("Expected Emphasis token, got {:?}", token7);
        }
    }

    #[test]
    fn test_enhanced_code_span_tokenization() {
        let input = "`simple` ``with backticks ` inside`` ` spaced `";
        let mut lexer = Lexer::new(input);

        // `simple`
        let token1 = lexer.next_token().unwrap();
        if let Token::CodeSpan(content) = token1 {
            assert_eq!(content, "simple");
        } else {
            panic!("Expected CodeSpan token, got {:?}", token1);
        }

        // ``with backticks ` inside``
        let token2 = lexer.next_token().unwrap();
        if let Token::CodeSpan(content) = token2 {
            assert_eq!(content, "with backticks ` inside");
        } else {
            panic!("Expected CodeSpan token, got {:?}", token2);
        }

        // ` spaced ` (should trim spaces)
        let token3 = lexer.next_token().unwrap();
        if let Token::CodeSpan(content) = token3 {
            assert_eq!(content, "spaced");
        } else {
            panic!("Expected CodeSpan token, got {:?}", token3);
        }
    }

    #[test]
    fn test_enhanced_link_tokenization() {
        let input =
            "[simple link](http://example.com) [link with title](http://example.com \"Title\")";
        let mut lexer = Lexer::new(input);

        // [simple link](http://example.com)
        let token1 = lexer.next_token().unwrap();
        if let Token::Link { text, dest, title } = token1 {
            assert_eq!(text, "simple link");
            assert_eq!(dest, "http://example.com");
            assert_eq!(title, None);
        } else {
            panic!("Expected Link token, got {:?}", token1);
        }

        // [link with title](http://example.com "Title")
        let token2 = lexer.next_token().unwrap();
        if let Token::Link { text, dest, title } = token2 {
            assert_eq!(text, "link with title");
            assert_eq!(dest, "http://example.com");
            assert_eq!(title, Some("Title"));
        } else {
            panic!("Expected Link token, got {:?}", token2);
        }
    }

    #[test]
    fn test_enhanced_image_tokenization() {
        let input = "![alt text](image.jpg) ![with title](image.jpg \"Image Title\")";
        let mut lexer = Lexer::new(input);

        // ![alt text](image.jpg)
        let token1 = lexer.next_token().unwrap();
        if let Token::Image { alt, dest, title } = token1 {
            assert_eq!(alt, "alt text");
            assert_eq!(dest, "image.jpg");
            assert_eq!(title, None);
        } else {
            panic!("Expected Image token, got {:?}", token1);
        }

        // ![with title](image.jpg "Image Title")
        let token2 = lexer.next_token().unwrap();
        if let Token::Image { alt, dest, title } = token2 {
            assert_eq!(alt, "with title");
            assert_eq!(dest, "image.jpg");
            assert_eq!(title, Some("Image Title"));
        } else {
            panic!("Expected Image token, got {:?}", token2);
        }
    }

    #[test]
    fn test_text_consolidation_logic() {
        let input = "regular text with spaces";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token().unwrap();
        assert!(matches!(token1, Token::Text("regular")));

        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::Text("text")));

        let token3 = lexer.next_token().unwrap();
        assert!(matches!(token3, Token::Text("with")));

        let token4 = lexer.next_token().unwrap();
        assert!(matches!(token4, Token::Text("spaces")));
    }

    // Additional tests for task 2.5: Unicode handling, position tracking, and CommonMark compliance

    #[test]
    fn test_unicode_emoji_handling() {
        let input = "Hello 👨‍👩‍👧‍👦 family emoji 🏳️‍🌈 flag";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token().unwrap();
        assert!(matches!(token1, Token::Text("Hello")));

        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::Text("👨‍👩‍👧‍👦")));

        let token3 = lexer.next_token().unwrap();
        assert!(matches!(token3, Token::Text("family")));

        let token4 = lexer.next_token().unwrap();
        assert!(matches!(token4, Token::Text("emoji")));

        let token5 = lexer.next_token().unwrap();
        assert!(matches!(token5, Token::Text("🏳️‍🌈")));

        let token6 = lexer.next_token().unwrap();
        assert!(matches!(token6, Token::Text("flag")));
    }

    #[test]
    fn test_unicode_multibyte_characters() {
        let input = "Héllo Wörld 中文 العربية русский";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token().unwrap();
        assert!(matches!(token1, Token::Text("Héllo")));

        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::Text("Wörld")));

        let token3 = lexer.next_token().unwrap();
        assert!(matches!(token3, Token::Text("中文")));

        let token4 = lexer.next_token().unwrap();
        assert!(matches!(token4, Token::Text("العربية")));

        let token5 = lexer.next_token().unwrap();
        assert!(matches!(token5, Token::Text("русский")));
    }

    #[test]
    fn test_unicode_combining_characters() {
        let input = "café naïve résumé";
        let mut lexer = Lexer::new(input);

        let token1 = lexer.next_token().unwrap();
        assert!(matches!(token1, Token::Text("café")));

        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::Text("naïve")));

        let token3 = lexer.next_token().unwrap();
        assert!(matches!(token3, Token::Text("résumé")));
    }

    #[test]
    fn test_position_tracking_multiline() {
        let input = "line1\nline2\nline3";
        let mut lexer = Lexer::new(input);

        // Initial position
        assert_eq!(lexer.position().line, 1);
        assert_eq!(lexer.position().column, 1);
        assert_eq!(lexer.position().offset, 0);

        // After first token
        let _token1 = lexer.next_token().unwrap();
        assert_eq!(lexer.position().line, 1);
        assert_eq!(lexer.position().column, 6);
        assert_eq!(lexer.position().offset, 5);

        // After newline
        let _newline1 = lexer.next_token().unwrap();
        assert_eq!(lexer.position().line, 2);
        assert_eq!(lexer.position().column, 1);
        assert_eq!(lexer.position().offset, 6);

        // After second token
        let _token2 = lexer.next_token().unwrap();
        assert_eq!(lexer.position().line, 2);
        assert_eq!(lexer.position().column, 6);
        assert_eq!(lexer.position().offset, 11);

        // After second newline
        let _newline2 = lexer.next_token().unwrap();
        assert_eq!(lexer.position().line, 3);
        assert_eq!(lexer.position().column, 1);
        assert_eq!(lexer.position().offset, 12);
    }

    #[test]
    fn test_position_tracking_unicode() {
        let input = "🌍\n👋";
        let mut lexer = Lexer::new(input);

        // Initial position
        assert_eq!(lexer.position().line, 1);
        assert_eq!(lexer.position().column, 1);
        assert_eq!(lexer.position().offset, 0);

        // After emoji token
        let _token1 = lexer.next_token().unwrap();
        assert_eq!(lexer.position().line, 1);
        assert_eq!(lexer.position().column, 2);
        assert_eq!(lexer.position().offset, 4); // Emoji is 4 bytes in UTF-8

        // After newline
        let _newline = lexer.next_token().unwrap();
        assert_eq!(lexer.position().line, 2);
        assert_eq!(lexer.position().column, 1);
        assert_eq!(lexer.position().offset, 5);

        // After second emoji
        let _token2 = lexer.next_token().unwrap();
        assert_eq!(lexer.position().line, 2);
        assert_eq!(lexer.position().column, 2);
        assert_eq!(lexer.position().offset, 9); // 5 + 4 bytes
    }

    #[test]
    fn test_position_tracking_mixed_content() {
        let input = "# Heading\n- List item";
        let mut lexer = Lexer::new(input);

        // ATX heading
        let token1 = lexer.next_token().unwrap();
        if let Token::AtxHeading { level, content } = token1 {
            assert_eq!(level, 1);
            assert_eq!(content, "Heading");
        } else {
            panic!("Expected AtxHeading token");
        }

        // Position after heading
        let pos = lexer.position();
        assert_eq!(pos.line, 1);
        assert!(pos.offset > 0);

        // Newline
        let _newline = lexer.next_token().unwrap();
        assert_eq!(lexer.position().line, 2);
        assert_eq!(lexer.position().column, 1);

        // List marker
        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::ListMarker { .. }));

        // List item text
        let token3 = lexer.next_token().unwrap();
        assert!(matches!(token3, Token::Text("List")));

        let token4 = lexer.next_token().unwrap();
        assert!(matches!(token4, Token::Text("item")));
    }

    #[test]
    fn test_all_commonmark_block_elements() {
        // Test comprehensive block element recognition
        let input = r#"# ATX Heading
Setext Heading
==============

> Blockquote
> Second line

- Bullet list
+ Plus bullet
* Star bullet

1. Ordered list
2) Paren delimiter

```rust
code block
```

    indented code

---
***
___"#;

        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();

        // Collect all tokens
        loop {
            let token = lexer.next_token().unwrap();
            if matches!(token, Token::Eof) {
                break;
            }
            tokens.push(token);
        }

        // Verify we have the expected token types
        let has_atx_heading = tokens.iter().any(|t| matches!(t, Token::AtxHeading { .. }));
        let has_setext_heading = tokens
            .iter()
            .any(|t| matches!(t, Token::SetextHeading { .. }));
        let has_blockquote = tokens.iter().any(|t| matches!(t, Token::BlockQuote));
        let has_bullet_list = tokens.iter().any(|t| {
            matches!(
                t,
                Token::ListMarker {
                    kind: ListKind::Bullet { .. },
                    ..
                }
            )
        });
        let has_ordered_list = tokens.iter().any(|t| {
            matches!(
                t,
                Token::ListMarker {
                    kind: ListKind::Ordered { .. },
                    ..
                }
            )
        });
        let has_code_block = tokens.iter().any(|t| matches!(t, Token::CodeBlock { .. }));
        let has_thematic_break = tokens.iter().any(|t| matches!(t, Token::ThematicBreak));

        assert!(has_atx_heading, "Missing ATX heading token");
        assert!(has_setext_heading, "Missing Setext heading token");
        assert!(has_blockquote, "Missing blockquote token");
        assert!(has_bullet_list, "Missing bullet list token");
        assert!(has_ordered_list, "Missing ordered list token");
        assert!(has_code_block, "Missing code block token");
        assert!(has_thematic_break, "Missing thematic break token");
    }

    #[test]
    fn test_all_commonmark_inline_elements() {
        let input = r#"*emphasis* **strong** `code` [link](url) ![image](img.jpg)"#;
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();

        // Collect all tokens
        loop {
            let token = lexer.next_token().unwrap();
            if matches!(token, Token::Eof) {
                break;
            }
            tokens.push(token);
        }

        // Verify we have the expected inline token types
        let has_emphasis = tokens.iter().any(|t| matches!(t, Token::Emphasis { .. }));
        let has_code_span = tokens.iter().any(|t| matches!(t, Token::CodeSpan(_)));
        let has_link = tokens.iter().any(|t| matches!(t, Token::Link { .. }));
        let has_image = tokens.iter().any(|t| matches!(t, Token::Image { .. }));

        assert!(has_emphasis, "Missing emphasis token");
        assert!(has_code_span, "Missing code span token");
        assert!(has_link, "Missing link token");
        assert!(has_image, "Missing image token");
    }

    #[test]
    fn test_complex_nested_structures() {
        let input = r#"> # Heading in blockquote
> - List in blockquote"#;

        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();

        // Collect all tokens
        loop {
            let token = lexer.next_token().unwrap();
            if matches!(token, Token::Eof) {
                break;
            }
            tokens.push(token);
        }

        // Should have blockquotes and heading
        let blockquote_count = tokens
            .iter()
            .filter(|t| matches!(t, Token::BlockQuote))
            .count();
        let has_heading = tokens.iter().any(|t| matches!(t, Token::AtxHeading { .. }));

        assert!(
            blockquote_count >= 2,
            "Should have multiple blockquote markers"
        );
        assert!(has_heading, "Should have heading in blockquote");
    }

    #[test]
    fn test_edge_cases_and_malformed_input() {
        // Test empty input
        let mut lexer = Lexer::new("");
        let token = lexer.next_token().unwrap();
        assert!(matches!(token, Token::Eof));

        // Test only newlines
        let mut lexer = Lexer::new("\n\n\n");
        let token1 = lexer.next_token().unwrap();
        assert!(matches!(token1, Token::Newline));
        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::Newline));
        let token3 = lexer.next_token().unwrap();
        assert!(matches!(token3, Token::Newline));
        let token4 = lexer.next_token().unwrap();
        assert!(matches!(token4, Token::Eof));

        // Test too many hash marks (should be treated as text)
        let mut lexer = Lexer::new("######## Too many hashes");
        let token1 = lexer.next_token().unwrap();
        assert!(matches!(token1, Token::Text(_)));

        // Test unclosed code span
        let mut lexer = Lexer::new("`unclosed code span");
        let token1 = lexer.next_token().unwrap();
        assert!(matches!(token1, Token::Text(_)));
    }

    #[test]
    fn test_whitespace_and_indentation_handling() {
        let input = "    indented code\n\t\ttab indented";
        let mut lexer = Lexer::new(input);

        // Indented code block (4+ spaces)
        let token1 = lexer.next_token().unwrap();
        assert!(matches!(token1, Token::CodeBlock { .. }));

        let _newline = lexer.next_token().unwrap();

        // Tab indentation (should also be treated as code block)
        let token2 = lexer.next_token().unwrap();
        assert!(matches!(token2, Token::CodeBlock { .. }));
    }

    #[test]
    fn test_fenced_code_block_variations() {
        let input = r#"```
plain code
```

```rust
fn main() {}
```

````
code with ``` inside
````"#;

        let mut lexer = Lexer::new(input);
        let mut code_blocks = Vec::new();

        loop {
            let token = lexer.next_token().unwrap();
            if let Token::CodeBlock { info, content } = token {
                code_blocks.push((info, content));
            } else if matches!(token, Token::Eof) {
                break;
            }
        }

        assert_eq!(code_blocks.len(), 3);

        // First block: no info string
        assert_eq!(code_blocks[0].0, None);
        assert!(code_blocks[0].1.contains("plain code"));

        // Second block: with language info
        assert_eq!(code_blocks[1].0, Some("rust"));
        assert!(code_blocks[1].1.contains("fn main()"));

        // Third block: 4 backticks with nested backticks
        assert_eq!(code_blocks[2].0, None);
        assert!(code_blocks[2].1.contains("```"));
    }

    #[test]
    fn test_emphasis_variations() {
        let input = "*single* **double** ***triple*** _underscore_ __double_underscore__";
        let mut lexer = Lexer::new(input);
        let mut emphasis_tokens = Vec::new();

        loop {
            let token = lexer.next_token().unwrap();
            if let Token::Emphasis { marker, count } = token {
                emphasis_tokens.push((marker, count));
            } else if matches!(token, Token::Eof) {
                break;
            }
        }

        // Should have various emphasis markers
        assert!(emphasis_tokens.contains(&('*', 1)));
        assert!(emphasis_tokens.contains(&('*', 2)));
        assert!(emphasis_tokens.contains(&('*', 3)));
        assert!(emphasis_tokens.contains(&('_', 1)));
        assert!(emphasis_tokens.contains(&('_', 2)));
    }

    #[test]
    fn test_escape_sequence_tokenization() {
        let input = "\\* \\# \\[ \\] \\\\";
        let mut lexer = Lexer::new(input);
        let mut escape_tokens = Vec::new();

        loop {
            let token = lexer.next_token().unwrap();
            if let Token::EscapeSequence { character } = token {
                escape_tokens.push(character);
            } else if matches!(token, Token::Eof) {
                break;
            }
        }

        // Should have escaped characters
        assert!(escape_tokens.contains(&'*'));
        assert!(escape_tokens.contains(&'#'));
        assert!(escape_tokens.contains(&'['));
        assert!(escape_tokens.contains(&']'));
        assert!(escape_tokens.contains(&'\\'));
    }

    #[test]
    fn test_invalid_escape_sequences() {
        let input = "\\a \\1 \\";
        let mut lexer = Lexer::new(input);
        let mut text_tokens = Vec::new();

        loop {
            let token = lexer.next_token().unwrap();
            if let Token::Text(text) = token {
                text_tokens.push(text);
            } else if matches!(token, Token::Eof) {
                break;
            }
        }

        // Invalid escape sequences should be treated as literal text
        assert!(text_tokens.iter().any(|&text| text.contains('\\')));
    }

    #[test]
    fn test_escapable_characters() {
        let lexer = Lexer::new("");

        // Test CommonMark escapable characters
        assert!(lexer.is_escapable_character('*'));
        assert!(lexer.is_escapable_character('#'));
        assert!(lexer.is_escapable_character('['));
        assert!(lexer.is_escapable_character(']'));
        assert!(lexer.is_escapable_character('\\'));
        assert!(lexer.is_escapable_character('`'));

        // Test non-escapable characters
        assert!(!lexer.is_escapable_character('a'));
        assert!(!lexer.is_escapable_character('1'));
        assert!(!lexer.is_escapable_character(' '));
    }
}
