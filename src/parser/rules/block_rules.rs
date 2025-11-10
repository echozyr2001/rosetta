use crate::parser::ast::Block;
use crate::parser::rules::block_patterns::{
    parse_atx_heading_block, parse_blockquote_block, parse_code_block_block, parse_list_block,
    parse_paragraph_block, parse_setext_heading_block, parse_thematic_break_block,
};
use crate::parser::rules::input::TokenSlice;
use nom::{Err as NomErr, IResult, error::ErrorKind};

/// Result alias for block parsing attempts.
pub type BlockParseResult<'a> = IResult<TokenSlice<'a>, Block>;

/// Collection of block parsing functions.
///
/// The set is evaluated in order, allowing consumers to customise
/// precedence or introduce new block rules.
#[derive(Clone)]
pub struct BlockRuleSet {
    parsers: Vec<BlockParserFn>,
}

type BlockParserFn = for<'a> fn(TokenSlice<'a>) -> BlockParseResult<'a>;

impl BlockRuleSet {
    pub fn new(parsers: Vec<BlockParserFn>) -> Self {
        Self { parsers }
    }

    pub fn parse<'a>(&self, input: TokenSlice<'a>) -> BlockParseResult<'a> {
        let mut last_err = NomErr::Error(nom::error::Error::new(input, ErrorKind::Alt));

        for parser in &self.parsers {
            match parser(input) {
                Ok(result) => return Ok(result),
                Err(NomErr::Error(e)) => last_err = NomErr::Error(e),
                Err(NomErr::Incomplete(needed)) => return Err(NomErr::Incomplete(needed)),
                Err(NomErr::Failure(e)) => return Err(NomErr::Failure(e)),
            }
        }

        Err(last_err)
    }
}

/// Default Markdown block rule ordering.
#[derive(Clone)]
pub struct MarkdownBlockRules {
    rules: BlockRuleSet,
}

impl MarkdownBlockRules {
    pub fn new() -> Self {
        Self {
            rules: BlockRuleSet::new(vec![
                parse_atx_heading_block,
                parse_setext_heading_block,
                parse_code_block_block,
                parse_blockquote_block,
                parse_list_block,
                parse_thematic_break_block,
                parse_paragraph_block,
            ]),
        }
    }

    pub fn parse<'a>(&self, input: TokenSlice<'a>) -> BlockParseResult<'a> {
        self.rules.parse(input)
    }
}

impl Default for MarkdownBlockRules {
    fn default() -> Self {
        Self::new()
    }
}
