use crate::lexer::token::{CodeBlockToken, ListKind as LexerListKind, ThematicBreakToken, Token};
use crate::parser::ast::{Block, Document, Inline, ListItem, ListKind};
use crate::parser::inline;
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
            Ok((
                rest,
                Block::Heading {
                    level: token.level,
                    content: inline::parse_inlines_from_text(token.raw_content.trim()),
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
            Ok((
                rest,
                Block::Heading {
                    level: token.level,
                    content: inline::parse_inlines_from_text(token.raw_content),
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
    let tokens = input.tokens();
    let Some(Token::BlockQuote(first_marker)) = tokens.first() else {
        return Err(nom_error(input));
    };

    let mut position = Some(first_marker.position);
    let mut body: Vec<Token<'input>> = Vec::new();
    let mut idx = 0usize;

    while idx < tokens.len() {
        match &tokens[idx] {
            Token::BlockQuote(marker) => {
                if position.is_none() {
                    position = Some(marker.position);
                }
                idx += 1;

                if idx < tokens.len()
                    && matches!(
                        tokens[idx],
                        Token::Whitespace(_) | Token::Indent(_) | Token::SoftBreak(_)
                    )
                {
                    idx += 1;
                }

                let line_start = idx;
                while idx < tokens.len()
                    && !matches!(tokens[idx], Token::LineEnding(_) | Token::Eof)
                {
                    idx += 1;
                }

                body.extend_from_slice(&tokens[line_start..idx]);

                if idx < tokens.len() {
                    if let Token::LineEnding(_) = &tokens[idx] {
                        body.push(tokens[idx].clone());
                        idx += 1;

                        if idx < tokens.len() && !matches!(tokens[idx], Token::BlockQuote(_)) {
                            break;
                        }
                        continue;
                    }

                    if let Token::Eof = tokens[idx] {
                        break;
                    }
                }
            }
            _ => break,
        }
    }

    let rest = TokenSlice::new(&tokens[idx..]);

    let child_slice = TokenSlice::new(body.as_slice());
    let content = if child_slice.is_empty() {
        Vec::new()
    } else {
        match parse_blocks(child_slice) {
            Ok((remaining, parsed)) if remaining.is_empty() => parsed,
            _ => return Err(nom_error(input)),
        }
    };

    Ok((rest, Block::BlockQuote { content, position }))
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
    let tokens = input.tokens();
    let Some(Token::ListMarker(first_marker)) = tokens.first() else {
        return Err(nom_error(input));
    };

    let mut items = Vec::new();
    let mut idx = 0usize;
    let mut list_tight = true;
    let mut position = Some(first_marker.position);
    let list_kind = map_list_kind(&first_marker.marker);

    while idx < tokens.len() {
        let marker_token = match &tokens[idx] {
            Token::ListMarker(marker) if map_list_kind(&marker.marker) == list_kind => marker,
            Token::LineEnding(_) => {
                idx += 1;
                continue;
            }
            _ => break,
        };

        if position.is_none() {
            position = Some(marker_token.position);
        }

        idx += 1;

        while idx < tokens.len() && matches!(tokens[idx], Token::Whitespace(_) | Token::Indent(_)) {
            idx += 1;
        }

        let mut item_tokens = Vec::new();
        let mut saw_blank_line = false;
        let mut end_list = false;

        while idx < tokens.len() {
            if matches!(tokens.get(idx), Some(Token::Eof)) {
                idx = tokens.len();
                break;
            }

            match &tokens[idx] {
                Token::LineEnding(_) => {
                    item_tokens.push(tokens[idx].clone());

                    let mut lookahead_idx = idx + 1;
                    let mut blank_line = false;

                    while lookahead_idx < tokens.len()
                        && matches!(
                            tokens[lookahead_idx],
                            Token::Whitespace(_) | Token::LineEnding(_)
                        )
                    {
                        if let Token::LineEnding(_) = &tokens[lookahead_idx] {
                            blank_line = true;
                        }
                        item_tokens.push(tokens[lookahead_idx].clone());
                        lookahead_idx += 1;
                    }

                    if blank_line {
                        saw_blank_line = true;
                        list_tight = false;

                        match tokens.get(lookahead_idx) {
                            Some(Token::ListMarker(next_marker))
                                if map_list_kind(&next_marker.marker) == list_kind =>
                            {
                                idx = lookahead_idx;
                                break;
                            }
                            Some(Token::Indent(_)) => {
                                idx = lookahead_idx;
                                continue;
                            }
                            _ => {
                                idx = lookahead_idx;
                                end_list = true;
                                break;
                            }
                        }
                    } else {
                        idx = lookahead_idx;
                    }
                    continue;
                }
                _ => {
                    item_tokens.push(tokens[idx].clone());
                    idx += 1;
                }
            }
        }

        let child_slice = TokenSlice::new(item_tokens.as_slice());
        let content = if child_slice.is_empty() {
            Vec::new()
        } else {
            match parse_blocks(child_slice) {
                Ok((remaining, blocks)) if remaining.is_empty() => blocks,
                _ => return Err(nom_error(input)),
            }
        };

        items.push(ListItem {
            content,
            tight: !saw_blank_line,
            task_list_marker: None,
        });

        if end_list {
            break;
        }
    }

    if items.is_empty() {
        return Err(nom_error(input));
    }

    let rest = TokenSlice::new(&tokens[idx..]);

    Ok((
        rest,
        Block::List {
            kind: list_kind,
            tight: list_tight,
            items,
            position,
        },
    ))
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
    let mut position = None;
    let mut found_content = false;
    let mut text_buffer = String::new();
    let mut inlines: Vec<Inline> = Vec::new();

    while idx < tokens.len() {
        match &tokens[idx] {
            Token::Text(token) => {
                if position.is_none() {
                    position = Some(token.position);
                }
                text_buffer.push_str(token.lexeme);
                found_content = true;
                idx += 1;
            }
            Token::Whitespace(token) => {
                if found_content {
                    text_buffer.push_str(token.lexeme);
                }
                idx += 1;
            }
            Token::Indent(token) => {
                if found_content {
                    for _ in 0..token.visual_width {
                        text_buffer.push(' ');
                    }
                }
                idx += 1;
            }
            Token::EscapeSequence(token) => {
                if position.is_none() {
                    position = Some(token.position);
                }
                text_buffer.push(token.escaped);
                found_content = true;
                idx += 1;
            }
            Token::Entity(token) => {
                if position.is_none() {
                    position = Some(token.position);
                }
                if let Some(resolved) = token.resolved {
                    text_buffer.push(resolved);
                } else {
                    text_buffer.push('&');
                    text_buffer.push_str(token.raw);
                }
                found_content = true;
                idx += 1;
            }
            Token::Punctuation(token) => {
                if position.is_none() {
                    position = Some(token.position);
                }
                text_buffer.push(token.ch);
                found_content = true;
                idx += 1;
            }
            Token::EmphasisDelimiter(token) => {
                if position.is_none() {
                    position = Some(token.position);
                }
                for _ in 0..token.run_length {
                    text_buffer.push(token.marker);
                }
                found_content = true;
                idx += 1;
            }
            Token::CodeSpanDelimiter(token) => {
                if position.is_none() {
                    position = Some(token.position);
                }
                for _ in 0..token.backtick_count {
                    text_buffer.push('`');
                }
                found_content = true;
                idx += 1;
            }
            Token::CodeSpan(token) => {
                if position.is_none() {
                    position = Some(token.position);
                }
                if !text_buffer.is_empty() {
                    inlines.extend(inline::parse_inlines_from_text(&text_buffer));
                    text_buffer.clear();
                }
                inlines.push(Inline::Code(token.content.to_string()));
                found_content = true;
                idx += 1;
            }
            Token::HtmlInline(token) => {
                if position.is_none() {
                    position = Some(token.position);
                }
                if !text_buffer.is_empty() {
                    inlines.extend(inline::parse_inlines_from_text(&text_buffer));
                    text_buffer.clear();
                }
                inlines.push(Inline::HtmlInline(token.raw.to_string()));
                found_content = true;
                idx += 1;
            }
            Token::Autolink(token) => {
                if position.is_none() {
                    position = Some(token.position);
                }
                if !text_buffer.is_empty() {
                    inlines.extend(inline::parse_inlines_from_text(&text_buffer));
                    text_buffer.clear();
                }
                let text = Inline::Text(token.lexeme.to_string());
                inlines.push(Inline::Link {
                    text: vec![text],
                    destination: token.lexeme.to_string(),
                    title: None,
                });
                found_content = true;
                idx += 1;
            }
            Token::LineEnding(token) => {
                if !found_content {
                    break;
                }
                if position.is_none() {
                    position = Some(token.position);
                }
                if !text_buffer.is_empty() {
                    inlines.extend(inline::parse_inlines_from_text(&text_buffer));
                    text_buffer.clear();
                }
                inlines.push(Inline::SoftBreak);
                idx += 1;
            }
            Token::SoftBreak(token) => {
                if !found_content {
                    break;
                }
                if position.is_none() {
                    position = Some(token.position);
                }
                if !text_buffer.is_empty() {
                    inlines.extend(inline::parse_inlines_from_text(&text_buffer));
                    text_buffer.clear();
                }
                inlines.push(Inline::SoftBreak);
                idx += 1;
            }
            Token::HardBreak(token) => {
                if !found_content {
                    break;
                }
                if position.is_none() {
                    position = Some(token.position);
                }
                if !text_buffer.is_empty() {
                    inlines.extend(inline::parse_inlines_from_text(&text_buffer));
                    text_buffer.clear();
                }
                inlines.push(Inline::HardBreak);
                idx += 1;
            }
            Token::Eof => break,
            _ => break,
        }
    }

    if !text_buffer.is_empty() {
        inlines.extend(inline::parse_inlines_from_text(&text_buffer));
        text_buffer.clear();
    }

    if !found_content && inlines.is_empty() {
        return Err(nom_error(input));
    }

    let rest = advance(input, idx);

    // Check if the next token is a SetextHeadingToken
    // If so, convert this paragraph to a heading using the content from the token
    if let Some(Token::SetextHeading(token)) = rest.first() {
        // Use the raw_content from the SetextHeadingToken which was extracted by Lexer
        return Ok((
            advance(rest, 1),
            Block::Heading {
                level: token.level,
                content: inline::parse_inlines_from_text(token.raw_content),
                id: None,
                position,
            },
        ));
    }

    Ok((
        rest,
        Block::Paragraph {
            content: inlines,
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

        assert!(document.blocks.len() >= 4);
        assert!(matches!(document.blocks[0], Block::Heading { .. }));
        assert!(matches!(document.blocks[1], Block::ThematicBreak { .. }));
        assert!(matches!(document.blocks[2], Block::CodeBlock { .. }));
        assert!(matches!(document.blocks[3], Block::BlockQuote { .. }));

        if let Block::BlockQuote { content, .. } = &document.blocks[3] {
            assert_eq!(content.len(), 1);
            assert!(matches!(content[0], Block::List { .. }));
        } else {
            panic!("expected blockquote with nested list");
        }
    }

    #[test]
    fn parse_rule_requires_consumable_tokens() {
        let tokens = vec![Token::Eof];
        let rule = MarkdownParseRule;
        let result = rule.parse(make_slice(&tokens));
        assert!(result.is_ok(), "empty document with EOF should succeed");
    }
}
