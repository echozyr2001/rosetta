use crate::parser::ast::Block;
use crate::parser::rules::block_patterns::{
    parse_atx_heading_block, parse_blockquote_block, parse_code_block_block, parse_list_block,
    parse_paragraph_block, parse_setext_heading_block, parse_thematic_break_block,
};
use crate::parser::rules::input::TokenSlice;
use nom::IResult;

/// Result alias for block parsing attempts.
pub type BlockParseResult<'a> = IResult<TokenSlice<'a>, Block>;

/// Function pointer type for block parsers.
pub type BlockParserFn = for<'a> fn(TokenSlice<'a>) -> BlockParseResult<'a>;

/// Ordered Markdown block parsers, from highest to lowest precedence.
pub const MARKDOWN_BLOCK_PARSERS: &[BlockParserFn] = &[
    parse_atx_heading_block,
    parse_setext_heading_block,
    parse_code_block_block,
    parse_blockquote_block,
    parse_list_block,
    parse_thematic_break_block,
    parse_paragraph_block,
];
