use crate::lexer::token::{
    BlockQuoteToken, CodeBlockToken, ListKind as LexerListKind, ListMarkerToken, TextToken,
    ThematicBreakToken, Token,
};
use crate::parser::ast::{Block, Document, Inline, ListItem, ListKind};
use crate::parser::token_slice::TokenSlice;
use crate::parser::traits::ParseRule;
use nom::{Err as NomErr, IResult, error::ErrorKind};

pub type ParseInput<'slice, 'input> = TokenSlice<'slice, Token<'input>>;
pub type ParseOutput<'slice, 'input, T> = IResult<ParseInput<'slice, 'input>, T>;

#[derive(Clone, Copy, Default)]
pub struct MarkdownParseRule;

fn nom_error<'slice, 'input>(
    input: ParseInput<'slice, 'input>,
) -> NomErr<nom::error::Error<ParseInput<'slice, 'input>>> {
    NomErr::Error(nom::error::Error::new(input, ErrorKind::Tag))
}

fn advance<'slice, 'input>(
    input: ParseInput<'slice, 'input>,
    count: usize,
) -> ParseInput<'slice, 'input> {
    let tokens = input.tokens();
    if count >= tokens.len() {
        TokenSlice::new(&[])
    } else {
        TokenSlice::new(&tokens[count..])
    }
}

fn skip_separators<'slice, 'input>(
    mut input: ParseInput<'slice, 'input>,
) -> ParseInput<'slice, 'input> {
    loop {
        let Some(token) = input.first() else {
            break;
        };
        match token {
            Token::Whitespace(_)
            | Token::LineEnding(_)
            | Token::Indent(_)
            | Token::SoftBreak(_)
            | Token::HardBreak(_) => {
                input = advance(input, 1);
            }
            _ => break,
        }
    }
    input
}

fn parse_atx_heading_block<'slice, 'input>(
    input: ParseInput<'slice, 'input>,
) -> ParseOutput<'slice, 'input, Block>
where
    'input: 'slice,
{
    match input.first() {
        Some(Token::AtxHeading(token)) => {
            let rest = advance(input, 1);
            let text = token.raw_content.trim().to_string();
            let inline = Inline::Text(text);
            Ok((
                rest,
                Block::Heading {
                    level: token.level,
                    content: vec![inline],
                    id: None,
                    position: Some(token.position),
                },
            ))
        }
        _ => Err(nom_error(input)),
    }
}

fn parse_setext_heading_block<'slice, 'input>(
    input: ParseInput<'slice, 'input>,
) -> ParseOutput<'slice, 'input, Block>
where
    'input: 'slice,
{
    match input.first() {
        Some(Token::SetextHeading(token)) => {
            let rest = advance(input, 1);
            let inline = Inline::Text(token.raw_underline.trim().to_string());
            Ok((
                rest,
                Block::Heading {
                    level: token.level,
                    content: vec![inline],
                    id: None,
                    position: Some(token.position),
                },
            ))
        }
        _ => Err(nom_error(input)),
    }
}

fn parse_code_block_block<'slice, 'input>(
    input: ParseInput<'slice, 'input>,
) -> ParseOutput<'slice, 'input, Block>
where
    'input: 'slice,
{
    match input.first() {
        Some(Token::CodeBlock(CodeBlockToken {
            info_string,
            raw_content,
            position,
            ..
        })) => {
            let rest = advance(input, 1);
            let info = info_string.map(|s| s.to_string());
            let language = info_string
                .and_then(|s| s.split_whitespace().next())
                .map(|s| s.to_string());
            Ok((
                rest,
                Block::CodeBlock {
                    info,
                    content: raw_content.to_string(),
                    language,
                    position: Some(*position),
                },
            ))
        }
        _ => Err(nom_error(input)),
    }
}

fn parse_blockquote_block<'slice, 'input>(
    input: ParseInput<'slice, 'input>,
) -> ParseOutput<'slice, 'input, Block>
where
    'input: 'slice,
{
    match input.first() {
        Some(Token::BlockQuote(BlockQuoteToken { position, .. })) => {
            let rest = advance(input, 1);
            Ok((
                rest,
                Block::BlockQuote {
                    content: vec![],
                    position: Some(*position),
                },
            ))
        }
        _ => Err(nom_error(input)),
    }
}

fn map_list_kind(kind: &LexerListKind) -> ListKind {
    match kind {
        LexerListKind::Bullet { marker } => ListKind::Bullet { marker: *marker },
        LexerListKind::Ordered { start, delimiter } => ListKind::Ordered {
            start: *start,
            delimiter: *delimiter,
        },
    }
}

fn parse_list_block<'slice, 'input>(
    input: ParseInput<'slice, 'input>,
) -> ParseOutput<'slice, 'input, Block>
where
    'input: 'slice,
{
    match input.first() {
        Some(Token::ListMarker(ListMarkerToken {
            marker, position, ..
        })) => {
            let rest = advance(input, 1);
            let block = Block::List {
                kind: map_list_kind(marker),
                tight: true,
                items: vec![ListItem {
                    content: vec![],
                    tight: true,
                    task_list_marker: None,
                }],
                position: Some(*position),
            };
            Ok((rest, block))
        }
        _ => Err(nom_error(input)),
    }
}

fn parse_thematic_break_block<'slice, 'input>(
    input: ParseInput<'slice, 'input>,
) -> ParseOutput<'slice, 'input, Block>
where
    'input: 'slice,
{
    match input.first() {
        Some(Token::ThematicBreak(ThematicBreakToken { position, .. })) => {
            let rest = advance(input, 1);
            Ok((
                rest,
                Block::ThematicBreak {
                    position: Some(*position),
                },
            ))
        }
        _ => Err(nom_error(input)),
    }
}

fn parse_paragraph_block<'slice, 'input>(
    input: ParseInput<'slice, 'input>,
) -> ParseOutput<'slice, 'input, Block>
where
    'input: 'slice,
{
    let tokens = input.tokens();
    if tokens.is_empty() {
        return Err(nom_error(input));
    }

    let mut idx = 0;
    let mut text = String::new();
    let mut position = None;
    let mut found_text = false;

    while idx < tokens.len() {
        match &tokens[idx] {
            Token::Text(TextToken {
                lexeme,
                position: pos,
            }) => {
                if position.is_none() {
                    position = Some(*pos);
                }
                text.push_str(lexeme);
                found_text = true;
                idx += 1;
            }
            Token::Whitespace(token) => {
                if found_text {
                    text.push_str(token.lexeme);
                }
                idx += 1;
            }
            Token::LineEnding(_) | Token::SoftBreak(_) => {
                if found_text {
                    text.push('\n');
                }
                idx += 1;
            }
            Token::HardBreak(_) => {
                if found_text {
                    text.push_str("\\n");
                }
                idx += 1;
            }
            Token::Indent(_) => {
                if found_text {
                    text.push_str("    ");
                }
                idx += 1;
            }
            _ => break,
        }
    }

    if !found_text {
        return Err(nom_error(input));
    }

    let rest = advance(input, idx);
    Ok((
        rest,
        Block::Paragraph {
            content: vec![Inline::Text(text)],
            position,
        },
    ))
}

fn parse_block<'slice, 'input>(
    input: ParseInput<'slice, 'input>,
) -> ParseOutput<'slice, 'input, Block>
where
    'input: 'slice,
{
    parse_atx_heading_block(input)
        .or_else(|_| parse_setext_heading_block(input))
        .or_else(|_| parse_code_block_block(input))
        .or_else(|_| parse_blockquote_block(input))
        .or_else(|_| parse_list_block(input))
        .or_else(|_| parse_thematic_break_block(input))
        .or_else(|_| parse_paragraph_block(input))
}

fn parse_blocks<'slice, 'input>(
    mut input: ParseInput<'slice, 'input>,
) -> ParseOutput<'slice, 'input, Vec<Block>>
where
    'input: 'slice,
{
    let mut blocks = Vec::new();
    input = skip_separators(input);

    while !input.is_empty() {
        match parse_block(input) {
            Ok((rest, block)) => {
                if rest.len() == input.len() {
                    return Err(nom_error(input));
                }
                blocks.push(block);
                input = skip_separators(rest);
            }
            Err(NomErr::Error(_)) => break,
            Err(err) => return Err(err),
        }
    }

    Ok((input, blocks))
}

impl<'slice, 'input> ParseRule<'slice, Token<'input>, Document> for MarkdownParseRule
where
    'input: 'slice,
{
    fn parse(
        &self,
        input: TokenSlice<'slice, Token<'input>>,
    ) -> IResult<TokenSlice<'slice, Token<'input>>, Document> {
        parse_blocks(input).map(|(rest, blocks)| (rest, Document { blocks }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Position;
    use crate::lexer::token::{
        AtxHeadingToken, BlockQuoteToken, CodeBlockToken, ListKind as LexerListKind,
        ListMarkerToken, TextToken, ThematicBreakToken,
    };
    use crate::parser::ast::Block;

    fn make_slice<'a>(tokens: &'a [Token<'static>]) -> TokenSlice<'a, Token<'static>> {
        TokenSlice::new(tokens)
    }

    #[test]
    fn parse_rule_handles_basic_blocks() {
        let tokens = vec![
            Token::AtxHeading(AtxHeadingToken {
                level: 1,
                marker_count: 1,
                leading_whitespace: 0,
                raw_marker: "#",
                raw_content: "Title",
                closing_sequence: "",
                position: Position::default(),
            }),
            Token::ThematicBreak(ThematicBreakToken {
                marker_char: '-',
                marker_count: 3,
                leading_whitespace: 0,
                position: Position::default(),
            }),
            Token::CodeBlock(CodeBlockToken {
                fence_char: Some('`'),
                fence_length: Some(3),
                info_string: Some("rust"),
                closing_fence_length: Some(3),
                raw_content: "fn main() {}",
                indent_width: 0,
                contains_tab: false,
                position: Position::default(),
            }),
            Token::BlockQuote(BlockQuoteToken {
                depth: 1,
                marker_offset: 0,
                spaces_after_marker: 1,
                position: Position::default(),
            }),
            Token::ListMarker(ListMarkerToken {
                marker: LexerListKind::Bullet { marker: '-' },
                marker_offset: 0,
                spaces_after_marker: 1,
                ordinal_span: None,
                position: Position::default(),
            }),
            Token::Text(TextToken {
                lexeme: "Paragraph text",
                position: Position::default(),
            }),
            Token::Eof,
        ];

        let rule = MarkdownParseRule;
        let (_rest, document) = rule
            .parse(make_slice(&tokens))
            .expect("rule should parse token sequence");

        assert!(document.blocks.len() >= 5);
        assert!(matches!(document.blocks[0], Block::Heading { .. }));
        assert!(matches!(document.blocks[1], Block::ThematicBreak { .. }));
        assert!(matches!(document.blocks[2], Block::CodeBlock { .. }));
        assert!(matches!(document.blocks[3], Block::BlockQuote { .. }));
        assert!(matches!(document.blocks[4], Block::List { .. }));
    }

    #[test]
    fn parse_rule_requires_consumable_tokens() {
        let tokens = vec![Token::Eof];
        let rule = MarkdownParseRule;
        let result = rule.parse(make_slice(&tokens));
        assert!(result.is_ok(), "empty document with EOF should succeed");
    }
}
