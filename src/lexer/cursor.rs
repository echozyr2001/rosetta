use super::position::Position;
use super::rules::{
    parse_atx_heading, parse_autolink, parse_blockquote, parse_code_fence, parse_code_span,
    parse_emphasis_delimiter, parse_entity, parse_escape_sequence, parse_html_inline, parse_indent,
    parse_list_marker, parse_newline, parse_setext_heading, parse_text, parse_thematic_break,
    parse_whitespace,
};
use super::token::*;
use std::marker::PhantomData;
use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};

/// Stateless cursor that tracks byte offsets and line/column information.
/// This is the primitive shared by all lexer rule implementations.
#[derive(Clone)]
pub struct Cursor<'input> {
    input: &'input str,
    position: Position,
    graphemes: GraphemeIndices<'input>,
    current_grapheme: Option<(usize, &'input str)>,
}

impl<'input> Cursor<'input> {
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

    pub fn position(&self) -> Position {
        self.position
    }

    pub fn byte_offset(&self) -> usize {
        self.position.offset
    }

    pub fn rest(&self) -> &'input str {
        &self.input[self.byte_offset()..]
    }

    pub fn input(&self) -> &'input str {
        self.input
    }

    pub fn is_at_end(&self) -> bool {
        self.byte_offset() >= self.input.len()
    }

    pub fn is_at_line_start(&self) -> bool {
        self.position.column == 1
    }

    pub fn is_at_block_start(&self) -> bool {
        if self.is_at_line_start() {
            return true;
        }

        let mut pos = self.byte_offset();
        while pos > 0 {
            pos -= 1;
            let ch = self.input.as_bytes()[pos] as char;
            match ch {
                '\n' | '\r' => return true,
                ' ' | '\t' => continue,
                _ => return false,
            }
        }
        true
    }

    pub fn advance(&mut self, consumed_bytes: usize) {
        let target_offset = self.position.offset + consumed_bytes;

        while let Some((offset, grapheme)) = self.current_grapheme {
            if offset >= target_offset {
                break;
            }

            if grapheme == "\n" || grapheme == "\r" {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += 1;
            }

            self.current_grapheme = self.graphemes.next();
        }

        self.position.offset = target_offset;
    }
}

/// Trait describing a lexing rule set. Implementations receive a mutable cursor
/// and are responsible for consuming bytes and returning the next token.
pub trait LexingRule<'input, Tok> {
    fn next_token(&mut self, cursor: &mut Cursor<'input>) -> Option<Tok>;
}

/// Generic lexer driver that accepts an input string and a rule implementation.
pub struct Lexer<'input, Tok, Rule>
where
    Rule: LexingRule<'input, Tok>,
{
    cursor: Cursor<'input>,
    rules: Rule,
    token: PhantomData<Tok>,
}

impl<'input, Tok, Rule> Lexer<'input, Tok, Rule>
where
    Rule: LexingRule<'input, Tok>,
{
    pub fn with_rules(input: &'input str, rules: Rule) -> Self {
        Self {
            cursor: Cursor::new(input),
            rules,
            token: PhantomData,
        }
    }

    pub fn position(&self) -> Position {
        self.cursor.position()
    }

    pub fn is_at_end(&self) -> bool {
        self.cursor.is_at_end()
    }

    pub fn next_token(&mut self) -> Option<Tok> {
        self.rules.next_token(&mut self.cursor)
    }
}

impl<'input, Tok, Rule> Lexer<'input, Tok, Rule>
where
    Rule: LexingRule<'input, Tok> + Clone,
{
    pub fn peek_token(&self) -> Option<Tok> {
        let mut clone = Lexer {
            cursor: self.cursor.clone(),
            rules: self.rules.clone(),
            token: PhantomData,
        };
        clone.next_token()
    }
}

impl<'input, Tok, Rule> Clone for Lexer<'input, Tok, Rule>
where
    Rule: LexingRule<'input, Tok> + Clone,
{
    fn clone(&self) -> Self {
        Self {
            cursor: self.cursor.clone(),
            rules: self.rules.clone(),
            token: PhantomData,
        }
    }
}

/// Markdown-specific rule set that composes the existing parsing functions.
#[derive(Clone, Default)]
pub struct MarkdownRules;

impl MarkdownRules {
    fn add_position(token: Token<'_>, position: Position) -> Token<'_> {
        match token {
            Token::AtxHeading(mut t) => {
                t.position = position;
                Token::AtxHeading(t)
            }
            Token::SetextHeading(mut t) => {
                t.position = position;
                Token::SetextHeading(t)
            }
            Token::CodeBlock(mut t) => {
                t.position = position;
                Token::CodeBlock(t)
            }
            Token::BlockQuote(mut t) => {
                t.position = position;
                Token::BlockQuote(t)
            }
            Token::ListMarker(mut t) => {
                t.position = position;
                Token::ListMarker(t)
            }
            Token::ThematicBreak(mut t) => {
                t.position = position;
                Token::ThematicBreak(t)
            }
            Token::HtmlBlock(mut t) => {
                t.position = position;
                Token::HtmlBlock(t)
            }
            Token::LinkReferenceDefinition(mut t) => {
                t.position = position;
                Token::LinkReferenceDefinition(t)
            }
            Token::Text(mut t) => {
                t.position = position;
                Token::Text(t)
            }
            Token::Whitespace(mut t) => {
                t.position = position;
                Token::Whitespace(t)
            }
            Token::SoftBreak(_) => Token::SoftBreak(SoftBreakToken { position }),
            Token::HardBreak(_) => Token::HardBreak(HardBreakToken { position }),
            Token::EmphasisDelimiter(mut t) => {
                t.position = position;
                Token::EmphasisDelimiter(t)
            }
            Token::CodeSpan(mut t) => {
                t.position = position;
                Token::CodeSpan(t)
            }
            Token::CodeSpanDelimiter(mut t) => {
                t.position = position;
                Token::CodeSpanDelimiter(t)
            }
            Token::Autolink(mut t) => {
                t.position = position;
                Token::Autolink(t)
            }
            Token::Entity(mut t) => {
                t.position = position;
                Token::Entity(t)
            }
            Token::EscapeSequence(mut t) => {
                t.position = position;
                Token::EscapeSequence(t)
            }
            Token::HtmlInline(mut t) => {
                t.position = position;
                Token::HtmlInline(t)
            }
            Token::LinkDestination(mut t) => {
                t.position = position;
                Token::LinkDestination(t)
            }
            Token::LinkTitle(mut t) => {
                t.position = position;
                Token::LinkTitle(t)
            }
            Token::ImageMarker(_) => Token::ImageMarker(ImageMarkerToken { position }),
            Token::LineEnding(mut t) => {
                t.position = position;
                Token::LineEnding(t)
            }
            Token::Indent(mut t) => {
                t.position = position;
                Token::Indent(t)
            }
            Token::Punctuation(mut t) => {
                t.position = position;
                Token::Punctuation(t)
            }
            Token::LinkLabelDelimiter(_) | Token::Eof => token,
        }
    }
}

impl<'input> LexingRule<'input, Token<'input>> for MarkdownRules {
    fn next_token(&mut self, cursor: &mut Cursor<'input>) -> Option<Token<'input>> {
        if cursor.is_at_end() {
            return Some(Token::Eof);
        }

        let current = cursor.rest();
        let start_position = cursor.position();

        if let Ok((remaining, token)) = parse_newline(current) {
            let consumed = current.len() - remaining.len();
            cursor.advance(consumed);
            return Some(Self::add_position(token, start_position));
        }

        if cursor.is_at_line_start()
            && let Ok((remaining, token)) = parse_indent(current)
        {
            let consumed = current.len() - remaining.len();
            cursor.advance(consumed);
            return Some(Self::add_position(token, start_position));
        }

        if cursor.is_at_block_start() {
            if let Ok((remaining, token)) = parse_list_marker(current) {
                let consumed = current.len() - remaining.len();
                cursor.advance(consumed);
                return Some(Self::add_position(token, start_position));
            }

            if let Ok((remaining, token)) = parse_atx_heading(current) {
                let consumed = current.len() - remaining.len();
                cursor.advance(consumed);
                return Some(Self::add_position(token, start_position));
            }

            if let Ok((remaining, token)) = parse_code_fence(current) {
                let consumed = current.len() - remaining.len();
                cursor.advance(consumed);
                return Some(Self::add_position(token, start_position));
            }

            if let Ok((remaining, token)) = parse_blockquote(current) {
                let consumed = current.len() - remaining.len();
                cursor.advance(consumed);
                return Some(Self::add_position(token, start_position));
            }

            if let Ok((remaining, token)) = parse_thematic_break(current) {
                let consumed = current.len() - remaining.len();
                cursor.advance(consumed);
                return Some(Self::add_position(token, start_position));
            }

            if let Ok((remaining, mut token)) = parse_setext_heading(current) {
                // Extract the previous line's content for Setext heading
                let current_offset = cursor.byte_offset();
                if current_offset > 0 {
                    // Find the start of the previous line by going backwards
                    // First, skip any whitespace at the start of current line
                    let mut pos = current_offset;
                    
                    // Go back to find the newline that ends the previous line
                    let mut found_newline = false;
                    while pos > 0 {
                        pos -= 1;
                        let ch = cursor.input().as_bytes()[pos] as char;
                        if ch == '\n' || ch == '\r' {
                            found_newline = true;
                            // Handle \r\n
                            if ch == '\n' && pos > 0
                                && cursor.input().as_bytes()[pos - 1] as char == '\r'
                            {
                                pos -= 1;
                            }
                            break;
                        }
                    }
                    
                    if found_newline {
                        // Now find the start of the previous line
                        let line_end = pos;
                        let mut line_start = line_end;
                        while line_start > 0 {
                            let ch = cursor.input().as_bytes()[line_start - 1] as char;
                            if ch == '\n' || ch == '\r' {
                                break;
                            }
                            line_start -= 1;
                        }
                        
                        // Handle \r\n at line start
                        if line_start > 0
                            && cursor.input().as_bytes()[line_start] as char == '\r'
                            && line_start + 1 < cursor.input().len()
                            && cursor.input().as_bytes()[line_start + 1] as char == '\n'
                        {
                            line_start += 2;
                        } else if line_start < cursor.input().len()
                            && (cursor.input().as_bytes()[line_start] as char == '\n'
                                || cursor.input().as_bytes()[line_start] as char == '\r')
                        {
                            line_start += 1;
                        }
                        
                        if line_end > line_start {
                            let previous_line = &cursor.input()[line_start..line_end];
                            // Update the token with the previous line's content
                            if let Token::SetextHeading(ref t) = token {
                                let marker_char = t.marker_char;
                                let level = t.level;
                                let marker_count = t.marker_count;
                                let leading_whitespace = t.leading_whitespace;
                                let raw_underline = t.raw_underline;
                                
                                token = Token::SetextHeading(SetextHeadingToken {
                                    level,
                                    marker_char,
                                    marker_count,
                                    leading_whitespace,
                                    raw_underline,
                                    raw_content: previous_line.trim(),
                                    position: t.position,
                                });
                            }
                        }
                    }
                }
                
                let consumed = current.len() - remaining.len();
                cursor.advance(consumed);
                return Some(Self::add_position(token, start_position));
            }
        }

        if let Ok((remaining, token)) = parse_escape_sequence(current) {
            let consumed = current.len() - remaining.len();
            cursor.advance(consumed);
            return Some(Self::add_position(token, start_position));
        }

        if let Ok((remaining, token)) = parse_code_span(current) {
            let consumed = current.len() - remaining.len();
            cursor.advance(consumed);
            return Some(Self::add_position(token, start_position));
        }

        if let Ok((remaining, token)) = parse_autolink(current) {
            let consumed = current.len() - remaining.len();
            cursor.advance(consumed);
            return Some(Self::add_position(token, start_position));
        }

        if let Ok((remaining, token)) = parse_entity(current) {
            let consumed = current.len() - remaining.len();
            cursor.advance(consumed);
            return Some(Self::add_position(token, start_position));
        }

        if let Ok((remaining, token)) = parse_html_inline(current) {
            let consumed = current.len() - remaining.len();
            cursor.advance(consumed);
            return Some(Self::add_position(token, start_position));
        }

        if let Ok((remaining, token)) = parse_emphasis_delimiter(current) {
            let consumed = current.len() - remaining.len();
            cursor.advance(consumed);
            return Some(Self::add_position(token, start_position));
        }

        if let Ok((remaining, token)) = parse_whitespace(current) {
            let consumed = current.len() - remaining.len();
            cursor.advance(consumed);
            return Some(Self::add_position(token, start_position));
        }

        if let Ok((remaining, token)) = parse_text(current) {
            let consumed = current.len() - remaining.len();
            cursor.advance(consumed);
            return Some(Self::add_position(token, start_position));
        }

        let old_offset = cursor.byte_offset();
        cursor.advance(1);
        Some(Token::Text(TextToken {
            lexeme: &cursor.input()[old_offset..old_offset + 1],
            position: start_position,
        }))
    }
}

impl<'input> Lexer<'input, Token<'input>, MarkdownRules> {
    pub fn new(input: &'input str) -> Self {
        Lexer::with_rules(input, MarkdownRules)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect_tokens<'a>(lexer: &mut Lexer<'a, Token<'a>, MarkdownRules>) -> Vec<Token<'a>> {
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
        let mut lexer = Lexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        assert!(matches!(
            tokens[0],
            Token::AtxHeading(AtxHeadingToken { level: 1, .. })
        ));
        assert!(matches!(
            tokens[1],
            Token::LineEnding(LineEndingToken {
                kind: LineEndingKind::LineFeed,
                ..
            })
        ));
        assert!(matches!(
            tokens[2],
            Token::Text(TextToken {
                lexeme: "Paragraph",
                ..
            })
        ));
    }

    #[test]
    fn parses_bullet_list_item() {
        let input = "- item";
        let mut lexer = Lexer::new(input);
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
            Token::Text(TextToken { lexeme: "item", .. })
        ));
    }

    #[test]
    fn parses_ordered_list_item() {
        let input = "2. item";
        let mut lexer = Lexer::new(input);
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
            Token::Text(TextToken { lexeme: "item", .. })
        ));
    }

    #[test]
    fn parses_thematic_breaks() {
        let simple_inputs = ["---", "***", "___"];

        for input in simple_inputs {
            let mut lexer = Lexer::new(input);
            let tokens = collect_tokens(&mut lexer);
            assert!(matches!(tokens[0], Token::ThematicBreak(_)), "{}", input);
        }
    }

    #[test]
    fn parses_setext_headings() {
        let input = "===";
        let mut lexer = Lexer::new(input);
        let tokens = collect_tokens(&mut lexer);
        assert!(matches!(
            tokens[0],
            Token::SetextHeading(SetextHeadingToken { level: 1, .. })
        ));
    }

    #[test]
    fn parses_code_spans() {
        let input = "`code`";
        let mut lexer = Lexer::new(input);
        let tokens = collect_tokens(&mut lexer);
        assert!(matches!(
            tokens[0],
            Token::CodeSpan(CodeSpanToken {
                content: "code",
                backtick_count: 1,
                ..
            })
        ));
    }

    #[test]
    fn parses_emphasis_delimiters() {
        let input = "**bold**";
        let mut lexer = Lexer::new(input);
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
        let mut lexer = Lexer::new(input);
        let tokens = collect_tokens(&mut lexer);
        assert!(matches!(
            tokens[0],
            Token::EscapeSequence(EscapeSequenceToken { escaped: '*', .. })
        ));
    }

    #[test]
    fn parses_autolinks() {
        let input = "<https://example.com>";
        let mut lexer = Lexer::new(input);
        let tokens = collect_tokens(&mut lexer);
        assert!(matches!(
            tokens[0],
            Token::Autolink(AutolinkToken {
                kind: AutolinkKind::Uri,
                lexeme: "https://example.com",
                ..
            })
        ));
    }

    #[test]
    fn parses_entities() {
        let input = "&amp;";
        let mut lexer = Lexer::new(input);
        let tokens = collect_tokens(&mut lexer);
        assert!(matches!(
            tokens[0],
            Token::Entity(EntityToken { raw: "amp;", .. })
        ));
    }

    #[test]
    fn parses_indentation() {
        let input = "    ";
        let mut lexer = Lexer::new(input);
        let tokens = collect_tokens(&mut lexer);
        assert!(matches!(
            tokens[0],
            Token::Indent(IndentToken {
                visual_width: 4,
                contains_tab: false,
                ..
            })
        ));
    }

    #[test]
    fn parses_complex_document() {
        let input = "# Heading\n\n> Quote\n\n- List item\n\n```rust\ncode\n```";
        let mut lexer = Lexer::new(input);
        let tokens = collect_tokens(&mut lexer);
        assert!(tokens.iter().any(|t| matches!(t, Token::AtxHeading(_))));
        assert!(tokens.iter().any(|t| matches!(t, Token::BlockQuote(_))));
        assert!(tokens.iter().any(|t| matches!(t, Token::ListMarker(_))));
        assert!(tokens.iter().any(|t| matches!(t, Token::CodeBlock(_))));
    }

    #[test]
    fn handles_edge_cases() {
        let mut lexer = Lexer::new("");
        let tokens = collect_tokens(&mut lexer);
        assert!(matches!(tokens[0], Token::Eof));

        let mut lexer = Lexer::new("   \t  ");
        let tokens = collect_tokens(&mut lexer);
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t, Token::Indent(_) | Token::Whitespace(_)))
        );
    }

    #[test]
    fn tracks_position_correctly() {
        let input = "# Heading\nSome text\n- List item";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.position().line, 1);
        assert_eq!(lexer.position().column, 1);

        let _ = lexer.next_token().unwrap();
        assert_eq!(lexer.position().line, 1);

        let _ = lexer.next_token().unwrap();
        assert_eq!(lexer.position().line, 2);
    }

    #[test]
    fn handles_unicode_position_tracking() {
        let input = "🌍\n👋";
        let mut lexer = Lexer::new(input);
        let _ = lexer.next_token().unwrap();
        let pos = lexer.position();
        assert_eq!(pos.line, 1);
        assert_eq!(pos.column, 2);
        assert_eq!(pos.offset, 4);
    }

    #[test]
    fn peek_token_works() {
        let input = "# Heading\nText";
        let mut lexer = Lexer::new(input);
        let peeked1 = lexer.peek_token().unwrap();
        let peeked2 = lexer.peek_token().unwrap();
        assert_eq!(peeked1, peeked2);
        let next = lexer.next_token().unwrap();
        assert_eq!(peeked1, next);
    }
}
