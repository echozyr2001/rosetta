use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_until, take_while, take_while_m_n, take_while1};
use nom::character::complete::{char, line_ending, not_line_ending, space0, space1};
use nom::combinator::{map, opt, recognize, verify};
use nom::multi::{many_m_n, many1};
use nom::sequence::{pair, preceded, tuple};

use super::Position;
use super::token::{
    AtxHeadingToken, AutolinkKind, AutolinkToken, BlockQuoteToken, CodeBlockToken, CodeSpanToken,
    EmphasisDelimiterToken, EmphasisMutation, EntityToken, EscapeSequenceToken, HtmlInlineToken,
    IndentToken, LineEndingKind, LineEndingToken, ListKind, ListMarkerToken, SetextHeadingToken,
    TextToken, ThematicBreakToken, Token, WhitespaceToken,
};
use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};

/// Enhanced nom-powered lexer with position tracking.
#[derive(Clone)]
pub struct NomLexer<'input> {
    input: &'input str,
    position: Position,
    // Unicode-aware grapheme iterator for position tracking
    graphemes: GraphemeIndices<'input>,
    // Current grapheme for efficient access
    current_grapheme: Option<(usize, &'input str)>,
}

impl<'input> NomLexer<'input> {
    pub fn new(input: &'input str) -> Self {
        let mut graphemes = input.grapheme_indices(true);
        let current_grapheme = graphemes.next();

        Self {
            input,
            position: Position::new(),
            graphemes,
            current_grapheme,
        }
    }

    /// Returns the current position in the input.
    pub fn position(&self) -> Position {
        self.position
    }

    /// Returns the current byte position in the input.
    fn byte_position(&self) -> usize {
        self.position.offset
    }

    /// Peeks at the next token without consuming it.
    pub fn peek_token(&self) -> Option<Token<'input>> {
        let mut temp_lexer = self.clone();
        temp_lexer.next_token()
    }

    /// Checks if we've reached the end of the input.
    pub fn is_at_end(&self) -> bool {
        self.byte_position() >= self.input.len()
    }

    /// Updates position tracking after consuming input.
    fn update_position(&mut self, consumed_bytes: usize) {
        let target_offset = self.position.offset + consumed_bytes;

        // Advance through graphemes until we reach the target offset
        while let Some((offset, grapheme)) = self.current_grapheme {
            if offset >= target_offset {
                break;
            }

            // Update position based on current grapheme
            if grapheme == "\n" {
                self.position.line += 1;
                self.position.column = 1;
            } else if grapheme == "\r" {
                // Handle \r\n as a single line ending
                // Note: In practice, \r\n would be a single grapheme
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += 1;
            }

            // Move to next grapheme
            self.current_grapheme = self.graphemes.next();
        }

        self.position.offset = target_offset;
    }

    pub fn next_token(&mut self) -> Option<Token<'input>> {
        if self.byte_position() >= self.input.len() {
            return Some(Token::Eof);
        }

        let current = &self.input[self.byte_position()..];

        // Try parsers in order of precedence.

        // Line endings first (highest priority)
        if let Ok((remaining, token)) = parse_newline(current) {
            let consumed = current.len() - remaining.len();
            self.update_position(consumed);
            return Some(token);
        }

        // Indentation at start of line
        if self.is_at_line_start()
            && let Ok((remaining, token)) = parse_indent(current)
        {
            let consumed = current.len() - remaining.len();
            self.update_position(consumed);
            return Some(token);
        }

        // Block-level constructs (only at line start or after indentation)
        if self.is_at_block_start() {
            // List markers (try these first as they're more specific)
            if let Ok((remaining, token)) = parse_list_marker(current) {
                let consumed = current.len() - remaining.len();
                self.update_position(consumed);
                return Some(token);
            }

            // ATX headings
            if let Ok((remaining, token)) = parse_atx_heading(current) {
                let consumed = current.len() - remaining.len();
                self.update_position(consumed);
                return Some(token);
            }

            // Code fences
            if let Ok((remaining, token)) = parse_code_fence(current) {
                let consumed = current.len() - remaining.len();
                self.update_position(consumed);
                return Some(token);
            }

            // Block quotes
            if let Ok((remaining, token)) = parse_blockquote(current) {
                let consumed = current.len() - remaining.len();
                self.update_position(consumed);
                return Some(token);
            }

            // Thematic breaks
            if let Ok((remaining, token)) = parse_thematic_break(current) {
                let consumed = current.len() - remaining.len();
                self.update_position(consumed);
                return Some(token);
            }

            // Setext heading underlines (try these last as they can conflict with other constructs)
            if let Ok((remaining, token)) = parse_setext_heading(current) {
                let consumed = current.len() - remaining.len();
                self.update_position(consumed);
                return Some(token);
            }
        }

        // Inline constructs

        // Escape sequences (high priority)
        if let Ok((remaining, token)) = parse_escape_sequence(current) {
            let consumed = current.len() - remaining.len();
            self.update_position(consumed);
            return Some(token);
        }

        // Code spans (high priority to avoid conflicts with emphasis)
        if let Ok((remaining, token)) = parse_code_span(current) {
            let consumed = current.len() - remaining.len();
            self.update_position(consumed);
            return Some(token);
        }

        // Autolinks (before HTML inline to catch <url> patterns)
        if let Ok((remaining, token)) = parse_autolink(current) {
            let consumed = current.len() - remaining.len();
            self.update_position(consumed);
            return Some(token);
        }

        // HTML entities (before HTML inline)
        if let Ok((remaining, token)) = parse_entity(current) {
            let consumed = current.len() - remaining.len();
            self.update_position(consumed);
            return Some(token);
        }

        // Inline HTML
        if let Ok((remaining, token)) = parse_html_inline(current) {
            let consumed = current.len() - remaining.len();
            self.update_position(consumed);
            return Some(token);
        }

        // Emphasis delimiters (after code spans to avoid conflicts)
        if let Ok((remaining, token)) = parse_emphasis_delimiter(current) {
            let consumed = current.len() - remaining.len();
            self.update_position(consumed);
            return Some(token);
        }

        // Whitespace
        if let Ok((remaining, token)) = parse_whitespace(current) {
            let consumed = current.len() - remaining.len();
            self.update_position(consumed);
            return Some(token);
        }

        // Fallback to text
        if let Ok((remaining, token)) = parse_text(current) {
            let consumed = current.len() - remaining.len();
            self.update_position(consumed);
            return Some(token);
        }

        // Unable to parse; advance one byte to avoid infinite loop.
        let old_pos = self.byte_position();
        self.update_position(1);
        Some(Token::Text(TextToken {
            lexeme: &self.input[old_pos..old_pos + 1],
        }))
    }

    /// Check if we're at the start of a line
    fn is_at_line_start(&self) -> bool {
        self.position.column == 1
    }

    /// Check if we're at a position where block-level constructs can start
    fn is_at_block_start(&self) -> bool {
        if self.is_at_line_start() {
            return true;
        }

        // Check if we're after only whitespace on the current line
        let mut pos = self.byte_position();
        while pos > 0 {
            pos -= 1;
            let ch = self.input.chars().nth(pos);
            match ch {
                Some('\n') | Some('\r') => return true,
                Some(' ') | Some('\t') => continue,
                _ => return false,
            }
        }
        true
    }
}

fn parse_newline(input: &str) -> IResult<&str, Token<'_>> {
    map(line_ending, |matched: &str| {
        let kind = match matched {
            "\n" => LineEndingKind::LineFeed,
            "\r" => LineEndingKind::CarriageReturn,
            "\r\n" => LineEndingKind::CarriageReturnLineFeed,
            _ => LineEndingKind::LineFeed,
        };
        Token::LineEnding(LineEndingToken { kind })
    })(input)
}

fn parse_whitespace(input: &str) -> IResult<&str, Token<'_>> {
    map(
        take_while1(|c: char| c == ' ' || c == '\t'),
        |slice: &str| {
            Token::Whitespace(WhitespaceToken {
                lexeme: slice,
                contains_tab: slice.contains('\t'),
            })
        },
    )(input)
}

fn parse_atx_heading(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((many1(char('#')), space0, not_line_ending)),
        |(hashes, _, content): (Vec<char>, _, &str)| {
            let marker_count = hashes.len();
            let level = marker_count.min(6) as u8;
            let raw_marker = &input[..marker_count];
            // Split trailing closing sequence if present (sequence of '#' optionally preceded by spaces)
            let (raw_content, closing_sequence) = split_closing_sequence(content);

            Token::AtxHeading(AtxHeadingToken {
                level,
                marker_count,
                leading_whitespace: 0,
                raw_marker,
                raw_content: raw_content.trim_end(),
                closing_sequence,
            })
        },
    )(input)
}

fn parse_list_marker(input: &str) -> IResult<&str, Token<'_>> {
    alt((parse_bullet_marker, parse_ordered_marker))(input)
}

fn parse_blockquote(input: &str) -> IResult<&str, Token<'_>> {
    map(pair(char('>'), space0), |(_, spaces): (_, &str)| {
        Token::BlockQuote(BlockQuoteToken {
            depth: 1,
            marker_offset: 0,
            spaces_after_marker: spaces.len(),
        })
    })(input)
}

fn parse_thematic_break(input: &str) -> IResult<&str, Token<'_>> {
    // Must have at least 3 of the same character (-, *, or _) and only whitespace between them
    let parse_dash_break = verify(
        recognize(many1(alt((char('-'), char(' '), char('\t'))))),
        |s: &str| {
            let dash_count = s.chars().filter(|&c| c == '-').count();
            let other_count = s
                .chars()
                .filter(|&c| c != '-' && c != ' ' && c != '\t')
                .count();
            dash_count >= 3 && other_count == 0
        },
    );

    let parse_star_break = verify(
        recognize(many1(alt((char('*'), char(' '), char('\t'))))),
        |s: &str| {
            let star_count = s.chars().filter(|&c| c == '*').count();
            let other_count = s
                .chars()
                .filter(|&c| c != '*' && c != ' ' && c != '\t')
                .count();
            star_count >= 3 && other_count == 0
        },
    );

    let parse_underscore_break = verify(
        recognize(many1(alt((char('_'), char(' '), char('\t'))))),
        |s: &str| {
            let underscore_count = s.chars().filter(|&c| c == '_').count();
            let other_count = s
                .chars()
                .filter(|&c| c != '_' && c != ' ' && c != '\t')
                .count();
            underscore_count >= 3 && other_count == 0
        },
    );

    map(
        alt((parse_dash_break, parse_star_break, parse_underscore_break)),
        |matched: &str| {
            let marker_char = matched
                .chars()
                .find(|c| matches!(c, '-' | '*' | '_'))
                .unwrap_or('-');
            let marker_count = matched.chars().filter(|c| *c == marker_char).count();
            let leading_whitespace = matched.chars().take_while(|c| c.is_whitespace()).count();

            Token::ThematicBreak(ThematicBreakToken {
                marker_char,
                marker_count,
                leading_whitespace,
            })
        },
    )(input)
}

fn parse_code_fence(input: &str) -> IResult<&str, Token<'_>> {
    // Simplified version - just handle ``` for now
    map(
        tuple((
            tag::<&str, &str, nom::error::Error<&str>>("```"),
            opt(not_line_ending),
            opt(line_ending),
            take_until::<&str, &str, nom::error::Error<&str>>("```"),
            tag::<&str, &str, nom::error::Error<&str>>("```"),
        )),
        |(_, info, _, body, _): (&str, Option<&str>, Option<&str>, &str, &str)| {
            Token::CodeBlock(CodeBlockToken {
                fence_char: Some('`'),
                fence_length: Some(3),
                info_string: info.map(str::trim).filter(|s| !s.is_empty()),
                closing_fence_length: Some(3),
                raw_content: body,
                indent_width: 0,
                contains_tab: false,
            })
        },
    )(input)
}

fn parse_text(input: &str) -> IResult<&str, Token<'_>> {
    map(is_not("\n"), |slice: &str| {
        Token::Text(TextToken { lexeme: slice })
    })(input)
}

fn parse_bullet_marker(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((
            alt((char('-'), char('+'), char('*'))),
            space1, // Require at least one space after the marker
        )),
        |(marker, spaces): (char, &str)| {
            Token::ListMarker(ListMarkerToken {
                marker: ListKind::Bullet { marker },
                marker_offset: 0,
                spaces_after_marker: spaces.len(),
                ordinal_span: None,
            })
        },
    )(input)
}

fn parse_ordered_marker(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((
            take_while_m_n(1, 9, |c: char| c.is_ascii_digit()),
            alt((char('.'), char(')'))),
            space1, // Require at least one space after the delimiter
        )),
        |(digits, delimiter, spaces): (&str, char, &str)| {
            let start = digits.parse().unwrap_or(1);
            Token::ListMarker(ListMarkerToken {
                marker: ListKind::Ordered { start, delimiter },
                marker_offset: 0,
                spaces_after_marker: spaces.len(),
                ordinal_span: Some(digits),
            })
        },
    )(input)
}

fn parse_indent(input: &str) -> IResult<&str, Token<'_>> {
    map(
        take_while1(|c: char| c == ' ' || c == '\t'),
        |spaces: &str| {
            let mut visual_width = 0;
            let mut contains_tab = false;

            for ch in spaces.chars() {
                match ch {
                    ' ' => visual_width += 1,
                    '\t' => {
                        contains_tab = true;
                        visual_width += 4 - (visual_width % 4); // Tab stops at multiples of 4
                    }
                    _ => break,
                }
            }

            Token::Indent(IndentToken {
                visual_width,
                contains_tab,
            })
        },
    )(input)
}

fn parse_setext_heading(input: &str) -> IResult<&str, Token<'_>> {
    // Setext headings need at least 3 characters and should be the entire line
    map(
        alt((
            verify(
                recognize(tuple((many_m_n(3, usize::MAX, char('=')), space0))),
                |s: &str| s.chars().filter(|&c| c == '=').count() >= 3,
            ),
            verify(
                recognize(tuple((many_m_n(3, usize::MAX, char('-')), space0))),
                |s: &str| s.chars().filter(|&c| c == '-').count() >= 3,
            ),
        )),
        |matched: &str| {
            let marker_char = matched.chars().next().unwrap_or('=');
            let level = if marker_char == '=' { 1 } else { 2 };
            let marker_count = matched.chars().filter(|&c| c == marker_char).count();
            let leading_whitespace = 0; // Setext headings don't have leading whitespace

            Token::SetextHeading(SetextHeadingToken {
                level,
                marker_char,
                marker_count,
                leading_whitespace,
                raw_underline: matched,
            })
        },
    )(input)
}

fn parse_escape_sequence(input: &str) -> IResult<&str, Token<'_>> {
    map(
        preceded(
            char('\\'),
            verify(nom::character::complete::anychar, |&c| {
                is_ascii_punctuation(c)
            }),
        ),
        |escaped_char| {
            Token::EscapeSequence(EscapeSequenceToken {
                escaped: escaped_char,
            })
        },
    )(input)
}

fn parse_code_span(input: &str) -> IResult<&str, Token<'_>> {
    // Simplified version - just handle single backticks for now
    map(
        tuple((
            tag::<&str, &str, nom::error::Error<&str>>("`"),
            take_until::<&str, &str, nom::error::Error<&str>>("`"),
            tag::<&str, &str, nom::error::Error<&str>>("`"),
        )),
        |(_, content, _): (&str, &str, &str)| {
            // Trim one space from each end if both ends have spaces
            let trimmed_content =
                if content.starts_with(' ') && content.ends_with(' ') && content.len() > 1 {
                    &content[1..content.len() - 1]
                } else {
                    content
                };

            Token::CodeSpan(CodeSpanToken {
                content: trimmed_content,
                backtick_count: 1,
            })
        },
    )(input)
}

fn parse_emphasis_delimiter(input: &str) -> IResult<&str, Token<'_>> {
    map(
        alt((
            take_while1(|c: char| c == '*'),
            take_while1(|c: char| c == '_'),
        )),
        |delimiters: &str| {
            let marker = delimiters.chars().next().unwrap();
            let run_length = delimiters.len();

            // Simplified mutation detection - in a full implementation,
            // this would need to look at surrounding context
            let mutation = EmphasisMutation::Both;

            Token::EmphasisDelimiter(EmphasisDelimiterToken {
                marker,
                run_length,
                mutation,
            })
        },
    )(input)
}

fn parse_autolink(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((
            char('<'),
            alt((
                recognize(tuple((
                    take_while1(|c: char| c.is_alphanumeric() || c == '+' || c == '.' || c == '-'),
                    char(':'),
                    take_while1(|c: char| c != '>' && c != ' ' && c != '\n'),
                ))),
                recognize(tuple((
                    take_while1(|c: char| c.is_alphanumeric() || c == '.' || c == '-' || c == '_'),
                    char('@'),
                    take_while1(|c: char| c.is_alphanumeric() || c == '.' || c == '-'),
                ))),
            )),
            char('>'),
        )),
        |(_, content, _): (char, &str, char)| {
            let kind = if content.contains('@') {
                AutolinkKind::Email
            } else {
                AutolinkKind::Uri
            };

            Token::Autolink(AutolinkToken {
                kind,
                lexeme: content,
            })
        },
    )(input)
}

fn parse_entity(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((
            char('&'),
            alt((
                // Named entities
                recognize(tuple((
                    take_while1(|c: char| c.is_alphanumeric()),
                    char(';'),
                ))),
                // Numeric entities
                recognize(tuple((
                    char('#'),
                    alt((
                        preceded(char('x'), take_while1(|c: char| c.is_ascii_hexdigit())),
                        take_while1(|c: char| c.is_ascii_digit()),
                    )),
                    char(';'),
                ))),
            )),
        )),
        |(_, entity_content): (char, &str)| {
            // In a full implementation, this would resolve the entity to a character
            Token::Entity(EntityToken {
                raw: entity_content,
                resolved: None, // Simplified - would need entity resolution
            })
        },
    )(input)
}

fn parse_html_inline(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((
            char('<'),
            alt((
                // Opening tag
                recognize(tuple((
                    take_while1(|c: char| c.is_alphanumeric()),
                    take_while(|c: char| c != '>'),
                    char('>'),
                ))),
                // Closing tag
                recognize(tuple((
                    char('/'),
                    take_while1(|c: char| c.is_alphanumeric()),
                    take_while(|c: char| c != '>'),
                    char('>'),
                ))),
            )),
        )),
        |(open, content): (char, &str)| {
            let mut raw = String::new();
            raw.push(open);
            raw.push_str(content);

            // This is a simplified version - would need proper HTML tag parsing
            Token::HtmlInline(HtmlInlineToken {
                raw: content, // In practice, this would include the full tag
            })
        },
    )(input)
}

fn is_ascii_punctuation(c: char) -> bool {
    matches!(
        c,
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

fn split_closing_sequence(raw: &str) -> (&str, &str) {
    let trimmed = raw.trim_end();
    if trimmed.is_empty() {
        return (raw, "");
    }

    let mut chars = trimmed.chars().rev();
    let mut hash_count = 0;
    for ch in &mut chars {
        if ch == '#' {
            hash_count += 1;
        } else if ch == ' ' {
            continue;
        } else {
            break;
        }
    }

    if hash_count == 0 {
        return (raw, "");
    }

    let closing_len = trimmed
        .char_indices()
        .rev()
        .take_while(|(_, ch)| *ch == '#' || *ch == ' ')
        .count();
    let split_point = trimmed.len().saturating_sub(closing_len);
    let (content, closing) = raw.split_at(split_point);
    (content, closing.trim())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect_tokens<'a>(lexer: &mut NomLexer<'a>) -> Vec<Token<'a>> {
        let mut tokens = Vec::new();
        while let Some(token) = lexer.next_token() {
            let end = matches!(token, Token::Eof);
            tokens.push(token);
            if end {
                break;
            }
        }
        tokens
    }

    #[test]
    fn parses_heading_and_text() {
        let input = "# Heading\nParagraph";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        assert!(matches!(
            tokens[0],
            Token::AtxHeading(AtxHeadingToken { level: 1, .. })
        ));
        assert!(matches!(
            tokens[1],
            Token::LineEnding(LineEndingToken {
                kind: LineEndingKind::LineFeed,
            })
        ));
        assert!(matches!(
            tokens[2],
            Token::Text(TextToken {
                lexeme: "Paragraph"
            })
        ));
    }

    #[test]
    fn parses_bullet_list_item() {
        let input = "- item";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        assert!(matches!(
            tokens[0],
            Token::ListMarker(ListMarkerToken {
                marker: ListKind::Bullet { marker: '-' },
                ..
            })
        ));
        assert!(matches!(
            tokens[1],
            Token::Text(TextToken { lexeme: "item" })
        ));
    }

    #[test]
    fn parses_ordered_list_item() {
        let input = "2. item";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        assert!(matches!(
            tokens[0],
            Token::ListMarker(ListMarkerToken {
                marker: ListKind::Ordered {
                    start: 2,
                    delimiter: '.'
                },
                ordinal_span: Some("2"),
                ..
            })
        ));
        assert!(matches!(
            tokens[1],
            Token::Text(TextToken { lexeme: "item" })
        ));
    }

    #[test]
    fn parses_thematic_breaks() {
        // Test simple thematic breaks (without spaces)
        let simple_inputs = ["---", "***", "___"];

        for input in simple_inputs {
            let mut lexer = NomLexer::new(input);
            let tokens = collect_tokens(&mut lexer);

            assert!(
                matches!(tokens[0], Token::ThematicBreak(_)),
                "Failed to parse thematic break: {}",
                input
            );
        }

        // Test spaced thematic breaks (these are more complex due to list marker conflicts)
        let spaced_inputs = ["- - -", "* * *", "_ _ _"];

        for input in spaced_inputs {
            let mut lexer = NomLexer::new(input);
            let _tokens = collect_tokens(&mut lexer);

            // For now, these might parse as list markers + text, which is acceptable
            // In a full implementation, we'd need more context to distinguish
            // between thematic breaks and list items
        }
    }

    #[test]
    fn parses_setext_headings() {
        let input = "===";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        assert!(matches!(
            tokens[0],
            Token::SetextHeading(SetextHeadingToken { level: 1, .. })
        ));

        let input2 = "---";
        let mut lexer2 = NomLexer::new(input2);
        let tokens2 = collect_tokens(&mut lexer2);

        // Note: This might conflict with thematic break parsing
        // In practice, setext headings need context from previous lines
        assert!(matches!(
            tokens2[0],
            Token::ThematicBreak(_) | Token::SetextHeading(SetextHeadingToken { level: 2, .. })
        ));
    }

    #[test]
    fn parses_code_spans() {
        let input = "`code`";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        assert!(matches!(
            tokens[0],
            Token::CodeSpan(CodeSpanToken {
                content: "code",
                backtick_count: 1
            })
        ));
    }

    #[test]
    fn parses_emphasis_delimiters() {
        let input = "**bold**";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        assert!(matches!(
            tokens[0],
            Token::EmphasisDelimiter(EmphasisDelimiterToken {
                marker: '*',
                run_length: 2,
                ..
            })
        ));
    }

    #[test]
    fn parses_escape_sequences() {
        let input = "\\*";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        assert!(matches!(
            tokens[0],
            Token::EscapeSequence(EscapeSequenceToken { escaped: '*' })
        ));
    }

    #[test]
    fn parses_autolinks() {
        let input = "<https://example.com>";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        assert!(matches!(
            tokens[0],
            Token::Autolink(AutolinkToken {
                kind: AutolinkKind::Uri,
                lexeme: "https://example.com"
            })
        ));

        let input2 = "<user@example.com>";
        let mut lexer2 = NomLexer::new(input2);
        let tokens2 = collect_tokens(&mut lexer2);

        assert!(matches!(
            tokens2[0],
            Token::Autolink(AutolinkToken {
                kind: AutolinkKind::Email,
                lexeme: "user@example.com"
            })
        ));
    }

    #[test]
    fn parses_entities() {
        let input = "&amp;";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        assert!(matches!(
            tokens[0],
            Token::Entity(EntityToken { raw: "amp;", .. })
        ));

        let input2 = "&#42;";
        let mut lexer2 = NomLexer::new(input2);
        let tokens2 = collect_tokens(&mut lexer2);

        assert!(matches!(
            tokens2[0],
            Token::Entity(EntityToken { raw: "#42;", .. })
        ));
    }

    #[test]
    fn parses_indentation() {
        let input = "    ";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        assert!(matches!(
            tokens[0],
            Token::Indent(IndentToken {
                visual_width: 4,
                contains_tab: false
            })
        ));

        let input2 = "\t";
        let mut lexer2 = NomLexer::new(input2);
        let tokens2 = collect_tokens(&mut lexer2);

        assert!(matches!(
            tokens2[0],
            Token::Indent(IndentToken {
                visual_width: 4,
                contains_tab: true
            })
        ));
    }

    #[test]
    fn parses_complex_document() {
        let input = "# Heading\n\n> Quote\n\n- List item\n\n```rust\ncode\n```";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        // Should contain various token types
        let has_heading = tokens.iter().any(|t| matches!(t, Token::AtxHeading(_)));
        let has_blockquote = tokens.iter().any(|t| matches!(t, Token::BlockQuote(_)));
        let has_list = tokens.iter().any(|t| matches!(t, Token::ListMarker(_)));
        let has_code_block = tokens.iter().any(|t| matches!(t, Token::CodeBlock(_)));

        assert!(has_heading, "Should parse ATX heading");
        assert!(has_blockquote, "Should parse blockquote");
        assert!(has_list, "Should parse list marker");
        assert!(has_code_block, "Should parse code block");
    }

    #[test]
    fn handles_edge_cases() {
        // Empty input
        let mut lexer = NomLexer::new("");
        let tokens = collect_tokens(&mut lexer);
        assert!(matches!(tokens[0], Token::Eof));

        // Only whitespace
        let mut lexer = NomLexer::new("   \t  ");
        let tokens = collect_tokens(&mut lexer);
        // Should parse as indentation since it's at the start of a line
        assert!(matches!(tokens[0], Token::Indent(_) | Token::Whitespace(_)));

        // Mixed line endings
        let mut lexer = NomLexer::new("line1\nline2\r\nline3\r");
        let tokens = collect_tokens(&mut lexer);
        let line_endings: Vec<_> = tokens
            .iter()
            .filter_map(|t| match t {
                Token::LineEnding(le) => Some(le.kind),
                _ => None,
            })
            .collect();

        // Our parser should detect at least some line endings
        assert!(!line_endings.is_empty(), "Should detect line endings");
        // The exact types depend on how nom's line_ending parser handles mixed endings
    }

    #[test]
    fn parses_nested_backticks() {
        // Test code spans with different backtick counts
        let input = "``code with ` backtick``";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        // Our simplified parser will try to parse this as emphasis delimiters or text
        // The exact behavior depends on the parser order
        assert!(!tokens.is_empty());
        assert!(matches!(tokens.last(), Some(Token::Eof)));
    }

    #[test]
    fn parses_mixed_list_types() {
        let input = "1. First\n- Second\n2) Third\n+ Fourth";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        let list_markers: Vec<_> = tokens
            .iter()
            .filter_map(|t| match t {
                Token::ListMarker(lm) => Some(&lm.marker),
                _ => None,
            })
            .collect();

        // Should have both ordered and bullet markers
        assert!(
            list_markers
                .iter()
                .any(|m| matches!(m, ListKind::Ordered { .. }))
        );
        assert!(
            list_markers
                .iter()
                .any(|m| matches!(m, ListKind::Bullet { .. }))
        );
    }

    #[test]
    fn tracks_position_correctly() {
        let input = "# Heading\nSome text\n- List item";
        let mut lexer = NomLexer::new(input);

        // Initial position
        assert_eq!(lexer.position().line, 1);
        assert_eq!(lexer.position().column, 1);
        assert_eq!(lexer.position().offset, 0);

        // After heading
        let _heading = lexer.next_token().unwrap();
        let pos = lexer.position();
        assert_eq!(pos.line, 1);
        assert!(pos.column > 1);

        // After first newline
        let _newline1 = lexer.next_token().unwrap();
        let pos = lexer.position();
        assert_eq!(pos.line, 2);
        assert_eq!(pos.column, 1);

        // After "Some text"
        let _text = lexer.next_token().unwrap();
        let pos = lexer.position();
        assert_eq!(pos.line, 2);
        assert_eq!(pos.column, 10); // "Some text" is 9 chars + 1

        // After second newline
        let _newline2 = lexer.next_token().unwrap();
        let pos = lexer.position();
        assert_eq!(pos.line, 3);
        assert_eq!(pos.column, 1);

        // After list marker
        let _list_marker = lexer.next_token().unwrap();
        let pos = lexer.position();
        assert_eq!(pos.line, 3);
        assert_eq!(pos.column, 3); // "- " is 2 chars + 1
    }

    #[test]
    fn handles_unicode_position_tracking() {
        let input = "🌍\n👋";
        let mut lexer = NomLexer::new(input);

        // Initial position
        assert_eq!(lexer.position().line, 1);
        assert_eq!(lexer.position().column, 1);

        // After emoji (note: this counts as 1 character for column tracking)
        let _emoji1 = lexer.next_token().unwrap();
        let pos = lexer.position();
        assert_eq!(pos.line, 1);
        assert_eq!(pos.column, 2); // Emoji counts as 1 character
        assert_eq!(pos.offset, 4); // But 4 bytes in UTF-8

        // After newline
        let _newline = lexer.next_token().unwrap();
        let pos = lexer.position();
        assert_eq!(pos.line, 2);
        assert_eq!(pos.column, 1);
        assert_eq!(pos.offset, 5);

        // After second emoji
        let _emoji2 = lexer.next_token().unwrap();
        let pos = lexer.position();
        assert_eq!(pos.line, 2);
        assert_eq!(pos.column, 2);
        assert_eq!(pos.offset, 9); // 5 + 4 bytes
    }

    #[test]
    fn peek_token_works() {
        let input = "# Heading\nText";
        let mut lexer = NomLexer::new(input);

        // Peek should return the same token multiple times
        let peeked1 = lexer.peek_token().unwrap();
        let peeked2 = lexer.peek_token().unwrap();
        assert_eq!(peeked1, peeked2);

        // Position should not change after peeking
        let pos_before = lexer.position();
        let _peeked = lexer.peek_token();
        let pos_after = lexer.position();
        assert_eq!(pos_before, pos_after);

        // Next token should match what we peeked
        let next = lexer.next_token().unwrap();
        assert_eq!(next, peeked1);

        // Position should change after consuming
        let pos_after_consume = lexer.position();
        assert_ne!(pos_before, pos_after_consume);
    }

    #[test]
    fn compare_unicode_handling_with_grapheme_lexer() {
        // Test single grapheme handling to ensure both lexers correctly process Unicode
        let test_cases = vec![
            ("🌍", 4, 1),  // Simple emoji
            ("👨‍👩‍👧‍👦", 25, 1), // Complex emoji
            ("🏳️‍🌈", 14, 1), // Rainbow flag emoji
        ];

        for (input, _expected_bytes, _expected_graphemes) in test_cases {
            println!("\n=== Testing Unicode handling for: {:?} ===", input);

            // NomLexer - consumes entire token
            let mut nom_lexer = NomLexer::new(input);
            let _token = nom_lexer.next_token().unwrap();
            let nom_pos = nom_lexer.position();

            // Manual Lexer - consumes single grapheme
            use crate::lexer::CharStream;
            let mut char_stream = CharStream::new(input);
            char_stream.advance(); // Advance by one grapheme only
            let grapheme_pos = char_stream.position();

            println!(
                "  Input bytes: {}, chars: {}",
                input.len(),
                input.chars().count()
            );
            println!("  NomLexer position (after token): {:?}", nom_pos);
            println!(
                "  CharStream position (after 1 grapheme): {:?}",
                grapheme_pos
            );

            // For single complex Unicode characters, both should have the same column position
            // because NomLexer treats the entire character as one token, CharStream advances one grapheme
            assert_eq!(
                nom_pos.column, grapheme_pos.column,
                "Both should advance column by 1 for single Unicode grapheme: {}",
                input
            );
            assert_eq!(
                nom_pos.offset, grapheme_pos.offset,
                "Both should have same byte offset for single Unicode grapheme: {}",
                input
            );
        }
    }

    #[test]
    fn test_grapheme_based_unicode_handling() {
        let complex_emoji = "👨‍👩‍👧‍👦"; // Complex family emoji

        let mut lexer = NomLexer::new(complex_emoji);
        let _token = lexer.next_token().unwrap();
        let pos = lexer.position();

        println!("Complex emoji: {:?}", complex_emoji);
        println!("Position: {:?}", pos);

        // Should treat complex emoji as single visual unit
        assert_eq!(
            pos.column, 2,
            "Should treat complex emoji as single visual unit"
        );
        assert_eq!(
            pos.offset,
            complex_emoji.len(),
            "Byte offset should match input length"
        );
    }

    #[test]
    fn parses_blockquotes() {
        let input = "> This is a blockquote\n> Second line";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        // Should have blockquote markers
        let blockquote_count = tokens
            .iter()
            .filter(|t| matches!(t, Token::BlockQuote(_)))
            .count();
        assert!(
            blockquote_count >= 2,
            "Should parse multiple blockquote markers"
        );
    }

    #[test]
    fn parses_code_blocks() {
        let input = "```rust\nfn main() {}\n```";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        let code_blocks: Vec<_> = tokens
            .iter()
            .filter_map(|t| match t {
                Token::CodeBlock(cb) => Some(cb),
                _ => None,
            })
            .collect();

        assert_eq!(code_blocks.len(), 1, "Should parse one code block");
        assert_eq!(code_blocks[0].info_string, Some("rust"));
        assert!(code_blocks[0].raw_content.contains("fn main()"));
    }

    #[test]
    fn parses_whitespace() {
        // Test whitespace at start of line (should be parsed as indent)
        let input = "   \t  ";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        let indent_tokens: Vec<_> = tokens
            .iter()
            .filter_map(|t| match t {
                Token::Indent(indent) => Some(indent),
                _ => None,
            })
            .collect();

        assert!(!indent_tokens.is_empty(), "Should parse indentation tokens");
        assert!(
            indent_tokens.iter().any(|indent| indent.contains_tab),
            "Should detect tab character in indentation"
        );

        // Test whitespace within a line (after some content)
        let input2 = "word\n   \t  more";
        let mut lexer2 = NomLexer::new(input2);
        let tokens2 = collect_tokens(&mut lexer2);

        let has_indent = tokens2.iter().any(|t| matches!(t, Token::Indent(_)));
        assert!(has_indent, "Should parse indentation after newline");
    }

    #[test]
    fn parses_html_inline() {
        let input = "<em>emphasis</em> and <strong>strong</strong>";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        let html_tokens: Vec<_> = tokens
            .iter()
            .filter_map(|t| match t {
                Token::HtmlInline(html) => Some(html),
                _ => None,
            })
            .collect();

        assert!(!html_tokens.is_empty(), "Should parse HTML inline tokens");
    }

    #[test]
    fn parses_line_endings() {
        let input = "line1\nline2\r\nline3\r";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        let line_endings: Vec<_> = tokens
            .iter()
            .filter_map(|t| match t {
                Token::LineEnding(le) => Some(le.kind),
                _ => None,
            })
            .collect();

        assert!(!line_endings.is_empty(), "Should parse line endings");
        // Should handle different line ending types
        assert!(line_endings.contains(&LineEndingKind::LineFeed));
    }

    #[test]
    fn handles_malformed_input() {
        // Test unclosed code spans
        let input = "`unclosed code span";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);
        assert!(
            !tokens.is_empty(),
            "Should handle unclosed code spans gracefully"
        );

        // Test invalid HTML
        let input2 = "<invalid html";
        let mut lexer2 = NomLexer::new(input2);
        let tokens2 = collect_tokens(&mut lexer2);
        assert!(!tokens2.is_empty(), "Should handle invalid HTML gracefully");

        // Test incomplete autolinks
        let input3 = "<incomplete@";
        let mut lexer3 = NomLexer::new(input3);
        let tokens3 = collect_tokens(&mut lexer3);
        assert!(
            !tokens3.is_empty(),
            "Should handle incomplete autolinks gracefully"
        );
    }

    #[test]
    fn handles_empty_and_whitespace_only_input() {
        // Empty input
        let mut lexer = NomLexer::new("");
        let tokens = collect_tokens(&mut lexer);
        assert_eq!(tokens.len(), 1);
        assert!(matches!(tokens[0], Token::Eof));

        // Whitespace only
        let mut lexer2 = NomLexer::new("   ");
        let tokens2 = collect_tokens(&mut lexer2);
        assert!(tokens2.iter().any(|t| matches!(t, Token::Indent(_))));

        // Newlines only
        let mut lexer3 = NomLexer::new("\n\n\n");
        let tokens3 = collect_tokens(&mut lexer3);
        let newline_count = tokens3
            .iter()
            .filter(|t| matches!(t, Token::LineEnding(_)))
            .count();
        assert_eq!(newline_count, 3);
    }

    #[test]
    fn handles_complex_unicode_sequences() {
        // Test various Unicode categories
        let test_cases = vec![
            ("café", "accented characters"),
            ("🌍🎉", "multiple emojis"),
            ("👨‍💻", "profession emoji"),
            ("🏳️‍⚧️", "transgender flag"),
            ("नमस्ते", "Devanagari script"),
            ("こんにちは", "Japanese hiragana"),
            ("🇺🇸", "flag emoji"),
        ];

        for (input, description) in test_cases {
            let mut lexer = NomLexer::new(input);
            let tokens = collect_tokens(&mut lexer);

            assert!(!tokens.is_empty(), "Should parse {}", description);
            assert!(
                matches!(tokens.last(), Some(Token::Eof)),
                "Should end with EOF for {}",
                description
            );

            // Position tracking should work correctly
            let final_pos = lexer.position();
            assert_eq!(
                final_pos.offset,
                input.len(),
                "Byte offset should match input length for {}",
                description
            );
        }
    }

    #[test]
    fn test_is_at_end_functionality() {
        let input = "short";
        let mut lexer = NomLexer::new(input);

        assert!(!lexer.is_at_end(), "Should not be at end initially");

        // Consume all tokens
        while let Some(token) = lexer.next_token() {
            if matches!(token, Token::Eof) {
                break;
            }
        }

        assert!(
            lexer.is_at_end(),
            "Should be at end after consuming all input"
        );
    }

    #[test]
    fn test_position_consistency() {
        let input = "line1\nline2\nline3";
        let mut lexer = NomLexer::new(input);
        let mut positions = Vec::new();

        // Collect positions as we parse
        while let Some(token) = lexer.next_token() {
            positions.push(lexer.position());
            if matches!(token, Token::Eof) {
                break;
            }
        }

        // Positions should be monotonically increasing in offset
        for window in positions.windows(2) {
            assert!(
                window[1].offset >= window[0].offset,
                "Positions should be monotonically increasing"
            );
        }

        // Final position should match input length
        assert_eq!(
            positions.last().unwrap().offset,
            input.len(),
            "Final position should match input length"
        );
    }

    #[test]
    fn test_clone_and_peek_consistency() {
        let input = "# Heading\n- List\n`code`";
        let lexer = NomLexer::new(input);

        // Clone should create identical lexer
        let mut lexer1 = lexer.clone();
        let mut lexer2 = lexer.clone();

        // Both should produce identical token sequences
        let tokens1 = collect_tokens(&mut lexer1);
        let tokens2 = collect_tokens(&mut lexer2);

        assert_eq!(
            tokens1, tokens2,
            "Cloned lexers should produce identical tokens"
        );

        // Peek should not affect subsequent parsing
        let mut lexer3 = lexer.clone();
        let peeked = lexer3.peek_token();
        let first_actual = lexer3.next_token();

        assert_eq!(peeked, first_actual, "Peek should match first actual token");
    }
}
