use crate::lexer::token::Token;
use crate::parser::ast::{Block, Inline, ListItem, ListKind};
use crate::parser::rules::input::TokenSlice;
use nom::{IResult, combinator::map};

use nom::{Err, InputTakeAtPosition, Slice};

pub fn token<'a, F>(
    predicate: F,
) -> impl Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, &'a Token<'a>>
where
    F: Fn(&Token<'a>) -> bool,
{
    move |input: TokenSlice<'a>| {
        if let Some(first_token) = input.first() {
            if predicate(first_token) {
                Ok((input.slice(1..), first_token))
            } else {
                Err(Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Tag,
                )))
            }
        } else {
            Err(Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Eof,
            )))
        }
    }
}

pub fn atx_heading(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, &Token<'_>> {
    token(|t| matches!(t, Token::AtxHeading(_)))(input)
}

pub fn setext_heading(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, &Token<'_>> {
    token(|t| matches!(t, Token::SetextHeading(_)))(input)
}

pub fn code_block(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, &Token<'_>> {
    token(|t| matches!(t, Token::CodeBlock(_)))(input)
}

pub fn blockquote_marker(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, &Token<'_>> {
    token(|t| matches!(t, Token::BlockQuote(_)))(input)
}

pub fn list_marker(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, &Token<'_>> {
    token(|t| matches!(t, Token::ListMarker(_)))(input)
}

pub fn thematic_break(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, &Token<'_>> {
    token(|t| matches!(t, Token::ThematicBreak(_)))(input)
}

pub fn text_token(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, &Token<'_>> {
    token(|t| matches!(t, Token::Text(_)))(input)
}

pub fn skip_ws_nl(input: TokenSlice) -> IResult<TokenSlice, ()> {
    let (input, _) = input.split_at_position_complete(|token| {
        !matches!(
            token,
            Token::Whitespace(_) | Token::Indent(_) | Token::LineEnding(_)
        )
    })?;
    Ok((input, ()))
}

pub fn parse_atx_heading_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    map(atx_heading, |token| {
        if let Token::AtxHeading(heading_token) = token {
            Block::Heading {
                level: heading_token.level,
                content: vec![Inline::Text(heading_token.raw_content.to_string())],
                id: None,
                position: Some(heading_token.position),
            }
        } else {
            unreachable!("atx_heading parser should only return AtxHeading tokens")
        }
    })(input)
}

pub fn parse_setext_heading_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    map(setext_heading, |token| {
        if let Token::SetextHeading(heading_token) = token {
            Block::Heading {
                level: heading_token.level,
                content: vec![Inline::Text("Setext Heading".to_string())], // TODO: get actual content
                id: None,
                position: Some(heading_token.position),
            }
        } else {
            unreachable!("setext_heading parser should only return SetextHeading tokens")
        }
    })(input)
}

pub fn parse_code_block_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    map(code_block, |token| {
        if let Token::CodeBlock(code_token) = token {
            Block::CodeBlock {
                info: code_token.info_string.map(|s| s.to_string()),
                content: code_token.raw_content.to_string(),
                language: code_token
                    .info_string
                    .and_then(|info| info.split_whitespace().next().map(|s| s.to_string())),
                position: Some(code_token.position),
            }
        } else {
            unreachable!("code_block parser should only return CodeBlock tokens")
        }
    })(input)
}

pub fn parse_blockquote_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    map(blockquote_marker, |token| {
        if let Token::BlockQuote(bq_token) = token {
            Block::BlockQuote {
                content: vec![Block::Paragraph {
                    content: vec![Inline::Text("Blockquote content".to_string())],
                    position: Some(bq_token.position),
                }],
                position: Some(bq_token.position),
            }
        } else {
            unreachable!("blockquote_marker parser should only return BlockQuote tokens")
        }
    })(input)
}

pub fn parse_list_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    map(list_marker, |token| {
        if let Token::ListMarker(list_token) = token {
            let kind = match &list_token.marker {
                crate::lexer::token::ListKind::Bullet { marker } => {
                    ListKind::Bullet { marker: *marker }
                }
                crate::lexer::token::ListKind::Ordered { start, delimiter } => ListKind::Ordered {
                    start: *start,
                    delimiter: *delimiter,
                },
            };

            Block::List {
                kind,
                tight: true,
                items: vec![ListItem {
                    content: vec![Block::Paragraph {
                        content: vec![Inline::Text("List item content".to_string())],
                        position: Some(list_token.position),
                    }],
                    tight: true,
                    task_list_marker: None,
                }],
                position: Some(list_token.position),
            }
        } else {
            unreachable!("list_marker parser should only return ListMarker tokens")
        }
    })(input)
}

pub fn parse_thematic_break_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    map(thematic_break, |token| {
        if let Token::ThematicBreak(tb_token) = token {
            Block::ThematicBreak {
                position: Some(tb_token.position),
            }
        } else {
            unreachable!("thematic_break parser should only return ThematicBreak tokens")
        }
    })(input)
}

pub fn parse_paragraph_block(input: TokenSlice) -> IResult<TokenSlice, Block> {
    map(text_token, |token| {
        if let Token::Text(text_token) = token {
            Block::Paragraph {
                content: vec![Inline::Text(text_token.lexeme.to_string())],
                position: Some(text_token.position),
            }
        } else {
            unreachable!("text_token parser should only return Text tokens")
        }
    })(input)
}
