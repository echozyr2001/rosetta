use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_until, take_while, take_while_m_n, take_while1};
use nom::character::complete::{char, line_ending, not_line_ending, space0, space1};
use nom::combinator::{map, opt, recognize, verify};
use nom::multi::{many_m_n, many1};
use nom::sequence::{pair, preceded, tuple};

use super::position::Position;
use super::token::*;

pub(super) fn parse_newline(input: &str) -> IResult<&str, Token<'_>> {
    map(line_ending, |matched: &str| {
        let kind = match matched {
            "\n" => LineEndingKind::LineFeed,
            "\r" => LineEndingKind::CarriageReturn,
            "\r\n" => LineEndingKind::CarriageReturnLineFeed,
            _ => LineEndingKind::LineFeed,
        };
        Token::LineEnding(LineEndingToken {
            kind,
            position: Position::default(),
        })
    })(input)
}

pub(super) fn parse_whitespace(input: &str) -> IResult<&str, Token<'_>> {
    map(
        take_while1(|c: char| c == ' ' || c == '\t'),
        |slice: &str| {
            Token::Whitespace(WhitespaceToken {
                lexeme: slice,
                contains_tab: slice.contains('\t'),
                position: Position::default(),
            })
        },
    )(input)
}

pub(super) fn parse_atx_heading(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((many1(char('#')), space0, not_line_ending)),
        |(hashes, _, content): (Vec<char>, _, &str)| {
            let marker_count = hashes.len();
            let level = marker_count.min(6) as u8;
            let raw_marker = &input[..marker_count];
            let (raw_content, closing_sequence) = split_closing_sequence(content);

            Token::AtxHeading(AtxHeadingToken {
                level,
                marker_count,
                leading_whitespace: 0,
                raw_marker,
                raw_content: raw_content.trim_end(),
                closing_sequence,
                position: Position::default(),
            })
        },
    )(input)
}

pub(super) fn parse_list_marker(input: &str) -> IResult<&str, Token<'_>> {
    alt((parse_bullet_marker, parse_ordered_marker))(input)
}

pub(super) fn parse_blockquote(input: &str) -> IResult<&str, Token<'_>> {
    map(pair(char('>'), space0), |(_, spaces): (_, &str)| {
        Token::BlockQuote(BlockQuoteToken {
            depth: 1,
            marker_offset: 0,
            spaces_after_marker: spaces.len(),
            position: Position::default(),
        })
    })(input)
}

pub(super) fn parse_thematic_break(input: &str) -> IResult<&str, Token<'_>> {
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
                position: Position::default(),
            })
        },
    )(input)
}

pub(super) fn parse_code_fence(input: &str) -> IResult<&str, Token<'_>> {
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
                position: Position::default(),
            })
        },
    )(input)
}

pub(super) fn parse_text(input: &str) -> IResult<&str, Token<'_>> {
    map(is_not("\n"), |slice: &str| {
        Token::Text(TextToken {
            lexeme: slice,
            position: Position::default(),
        })
    })(input)
}

pub(super) fn parse_indent(input: &str) -> IResult<&str, Token<'_>> {
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
                        visual_width += 4 - (visual_width % 4);
                    }
                    _ => break,
                }
            }

            Token::Indent(IndentToken {
                visual_width,
                contains_tab,
                position: Position::default(),
            })
        },
    )(input)
}

pub(super) fn parse_setext_heading(input: &str) -> IResult<&str, Token<'_>> {
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

            Token::SetextHeading(SetextHeadingToken {
                level,
                marker_char,
                marker_count,
                leading_whitespace: 0,
                raw_underline: matched,
                position: Position::default(),
            })
        },
    )(input)
}

pub(super) fn parse_escape_sequence(input: &str) -> IResult<&str, Token<'_>> {
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
                position: Position::default(),
            })
        },
    )(input)
}

pub(super) fn parse_code_span(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((
            tag::<&str, &str, nom::error::Error<&str>>("`"),
            take_until::<&str, &str, nom::error::Error<&str>>("`"),
            tag::<&str, &str, nom::error::Error<&str>>("`"),
        )),
        |(_, content, _): (&str, &str, &str)| {
            let trimmed_content =
                if content.starts_with(' ') && content.ends_with(' ') && content.len() > 1 {
                    &content[1..content.len() - 1]
                } else {
                    content
                };

            Token::CodeSpan(CodeSpanToken {
                content: trimmed_content,
                backtick_count: 1,
                position: Position::default(),
            })
        },
    )(input)
}

pub(super) fn parse_emphasis_delimiter(input: &str) -> IResult<&str, Token<'_>> {
    map(
        alt((
            take_while1(|c: char| c == '*'),
            take_while1(|c: char| c == '_'),
        )),
        |delimiters: &str| {
            let marker = delimiters.chars().next().unwrap();
            let run_length = delimiters.len();

            Token::EmphasisDelimiter(EmphasisDelimiterToken {
                marker,
                run_length,
                mutation: EmphasisMutation::Both,
                position: Position::default(),
            })
        },
    )(input)
}

pub(super) fn parse_autolink(input: &str) -> IResult<&str, Token<'_>> {
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
                position: Position::default(),
            })
        },
    )(input)
}

pub(super) fn parse_entity(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((
            char('&'),
            alt((
                recognize(tuple((
                    take_while1(|c: char| c.is_alphanumeric()),
                    char(';'),
                ))),
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
            Token::Entity(EntityToken {
                raw: entity_content,
                resolved: None,
                position: Position::default(),
            })
        },
    )(input)
}

pub(super) fn parse_html_inline(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((
            char('<'),
            alt((
                recognize(tuple((
                    take_while1(|c: char| c.is_alphanumeric()),
                    take_while(|c: char| c != '>'),
                    char('>'),
                ))),
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

            Token::HtmlInline(HtmlInlineToken {
                raw: content,
                position: Position::default(),
            })
        },
    )(input)
}

fn parse_bullet_marker(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((alt((char('-'), char('+'), char('*'))), space1)),
        |(marker, spaces): (char, &str)| {
            Token::ListMarker(ListMarkerToken {
                marker: ListKind::Bullet { marker },
                marker_offset: 0,
                spaces_after_marker: spaces.len(),
                ordinal_span: None,
                position: Position::default(),
            })
        },
    )(input)
}

fn parse_ordered_marker(input: &str) -> IResult<&str, Token<'_>> {
    map(
        tuple((
            take_while_m_n(1, 9, |c: char| c.is_ascii_digit()),
            alt((char('.'), char(')'))),
            space1,
        )),
        |(digits, delimiter, spaces): (&str, char, &str)| {
            let start = digits.parse().unwrap_or(1);
            Token::ListMarker(ListMarkerToken {
                marker: ListKind::Ordered { start, delimiter },
                marker_offset: 0,
                spaces_after_marker: spaces.len(),
                ordinal_span: Some(digits),
                position: Position::default(),
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
