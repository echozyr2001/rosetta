use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_while1};
use nom::character::complete::{char, digit1, line_ending, not_line_ending, space0};
use nom::combinator::{map, opt, recognize};
use nom::multi::many1;
use nom::sequence::{pair, tuple};
use nom::IResult;

use super::{ListKind, Token};

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
        Some(Token::Text(&self.input[self.position - 1..self.position]))
    }
}

fn parse_newline(input: &str) -> IResult<&str, Token<'_>> {
    map(line_ending, |_| Token::Newline)(input)
}

fn parse_whitespace(input: &str) -> IResult<&str, Token<'_>> {
    map(take_while1(|c: char| c == ' ' || c == '\t'), Token::Whitespace)(input)
}

fn parse_atx_heading(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((many1(char('#')), space0, not_line_ending)),
        |(hashes, _, content): (Vec<char>, _, &str)| Token::AtxHeading {
            level: hashes.len() as u8,
            content: content.trim(),
        },
    )(input)
}

fn parse_list_marker(input: &str) -> IResult<&str, Token<'_>> {
    alt((parse_bullet_marker, parse_ordered_marker))(input)
}

fn parse_blockquote(input: &str) -> IResult<&str, Token<'_>> {
    map(pair(char('>'), space0), |_| Token::BlockQuote)(input)
}

fn parse_thematic_break(input: &str) -> IResult<&str, Token<'_>> {
    map(
        alt((
            recognize(tuple((many1(char('-')), space0, many1(char('-'))))),
            recognize(tuple((many1(char('*')), space0, many1(char('*'))))),
        )),
        |_| Token::ThematicBreak,
    )(input)
}

fn parse_code_fence(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((tag("```"), opt(not_line_ending), line_ending, take_while1(|c| c != '`'), tag("```"))),
        |(_, info, _, body, _)| Token::CodeBlock {
            info,
            content: body,
        },
    )(input)
}

fn parse_text(input: &str) -> IResult<&str, Token<'_>> {
    map(is_not("\n"), Token::Text)(input)
}

fn parse_bullet_marker(input: &str) -> IResult<&str, Token<'_>> {
    map(char('-'), |_| Token::ListMarker {
        kind: ListKind::Bullet { marker: '-' },
        indent: 0,
    })(input)
}

fn parse_ordered_marker(input: &str) -> IResult<&str, Token<'_>> {
    map(tuple((digit1::<&str, nom::error::Error<&str>>, opt(char('.')))), |(digits, delimiter)| {
        let start = digits.parse().unwrap_or(1);
        let delimiter = delimiter.unwrap_or('.');
        Token::ListMarker {
            kind: ListKind::Ordered { start, delimiter },
            indent: 0,
        }
    })(input)
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

        assert!(matches!(tokens[0], Token::AtxHeading { level: 1, content } if content == "Heading"));
        assert!(matches!(tokens[1], Token::Newline));
        assert!(matches!(tokens[2], Token::Text("Paragraph")));
    }

    #[test]
    fn parses_bullet_list_item() {
        let input = "- item";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        assert!(matches!(tokens[0], Token::ListMarker { kind: ListKind::Bullet { marker: '-' }, indent: 0 }));
        assert!(matches!(tokens[1], Token::Whitespace(" ")));
        assert!(matches!(tokens[2], Token::Text("item")));
    }

    #[test]
    fn parses_ordered_list_item() {
        let input = "2. item";
        let mut lexer = NomLexer::new(input);
        let tokens = collect_tokens(&mut lexer);

        assert!(matches!(tokens[0], Token::ListMarker { kind: ListKind::Ordered { start: 2, delimiter: '.' }, indent: 0 }));
        assert!(matches!(tokens[1], Token::Whitespace(" ")));
        assert!(matches!(tokens[2], Token::Text("item")));
    }
}
