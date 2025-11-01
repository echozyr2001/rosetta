use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_while1};
use nom::character::complete::{char, digit1, line_ending, not_line_ending, space0};
use nom::combinator::{map, opt, recognize};
use nom::multi::many1;
use nom::sequence::{pair, tuple};

use super::token::{
    AtxHeadingToken, BlockQuoteToken, CodeBlockToken, LineEndingKind, LineEndingToken, ListKind,
    ListMarkerToken, TextToken, ThematicBreakToken, Token, WhitespaceToken,
};

/// Lightweight nom-powered lexer for experimentation.
pub struct NomLexer<'input> {
    input: &'input str,
    position: usize,
}

impl<'input> NomLexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self { input, position: 0 }
    }

    pub fn next_token(&mut self) -> Option<Token<'input>> {
        if self.position >= self.input.len() {
            return Some(Token::Eof);
        }

        let current = &self.input[self.position..];

        // Try parsers in order of precedence.
        if let Ok((remaining, token)) = parse_newline(current) {
            let consumed = current.len() - remaining.len();
            self.position += consumed;
            return Some(token);
        }

        if let Ok((remaining, token)) = parse_atx_heading(current) {
            let consumed = current.len() - remaining.len();
            self.position += consumed;
            return Some(token);
        }

        if let Ok((remaining, token)) = parse_blockquote(current) {
            let consumed = current.len() - remaining.len();
            self.position += consumed;
            return Some(token);
        }

        if let Ok((remaining, token)) = parse_thematic_break(current) {
            let consumed = current.len() - remaining.len();
            self.position += consumed;
            return Some(token);
        }

        if let Ok((remaining, token)) = parse_code_fence(current) {
            let consumed = current.len() - remaining.len();
            self.position += consumed;
            return Some(token);
        }

        if let Ok((remaining, token)) = parse_list_marker(current) {
            let consumed = current.len() - remaining.len();
            self.position += consumed;
            return Some(token);
        }

        if let Ok((remaining, token)) = parse_whitespace(current) {
            let consumed = current.len() - remaining.len();
            self.position += consumed;
            return Some(token);
        }

        // Fallback to text
        if let Ok((remaining, token)) = parse_text(current) {
            let consumed = current.len() - remaining.len();
            self.position += consumed;
            return Some(token);
        }

        // Unable to parse; advance one byte to avoid infinite loop.
        self.position += 1;
        Some(Token::Text(TextToken {
            lexeme: &self.input[self.position - 1..self.position],
        }))
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
    map(
        alt((
            recognize(tuple((many1(char('-')), space0, many1(char('-'))))),
            recognize(tuple((many1(char('*')), space0, many1(char('*'))))),
        )),
        |matched: &str| {
            let marker_char = matched
                .chars()
                .find(|c| *c == '-' || *c == '*')
                .unwrap_or('-');
            let marker_count = matched.chars().filter(|c| *c == marker_char).count();
            Token::ThematicBreak(ThematicBreakToken {
                marker_char,
                marker_count,
                leading_whitespace: 0,
            })
        },
    )(input)
}

fn parse_code_fence(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((
            tag("```"),
            opt(not_line_ending),
            line_ending,
            take_while1(|c| c != '`'),
            tag("```"),
        )),
        |(_, info, _, body, _)| {
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
    map(char('-'), |_| {
        Token::ListMarker(ListMarkerToken {
            marker: ListKind::Bullet { marker: '-' },
            marker_offset: 0,
            spaces_after_marker: 0,
            ordinal_span: None,
        })
    })(input)
}

fn parse_ordered_marker(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((digit1::<&str, nom::error::Error<&str>>, opt(char('.')))),
        |(digits, delimiter)| {
            let start = digits.parse().unwrap_or(1);
            let delimiter = delimiter.unwrap_or('.');
            Token::ListMarker(ListMarkerToken {
                marker: ListKind::Ordered { start, delimiter },
                marker_offset: 0,
                spaces_after_marker: 0,
                ordinal_span: Some(digits),
            })
        },
    )(input)
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
            Token::Whitespace(WhitespaceToken { lexeme: " ", .. })
        ));
        assert!(matches!(
            tokens[2],
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
            Token::Whitespace(WhitespaceToken { lexeme: " ", .. })
        ));
        assert!(matches!(
            tokens[2],
            Token::Text(TextToken { lexeme: "item" })
        ));
    }
}
