use crate::error::MarkdownError;
use crate::lexer::{Position, token::Token};
use crate::parser::ast::{Document, SourceMap};
use crate::parser::rules::block_patterns::skip_ws_nl;
use crate::parser::rules::block_rules::{BlockParseResult, BlockParserFn, MARKDOWN_BLOCK_PARSERS};
use crate::parser::rules::input::TokenSlice;
use crate::parser::state::ParserState;
use crate::parser::traits::ParseRule;
use nom::{Err as NomErr, error::ErrorKind};

#[derive(Clone, Copy)]
pub struct MarkdownParseRule {
    block_parsers: &'static [BlockParserFn],
}

impl Default for MarkdownParseRule {
    fn default() -> Self {
        Self {
            block_parsers: MARKDOWN_BLOCK_PARSERS,
        }
    }
}

impl MarkdownParseRule {
    fn parse_with_rules<'a>(&self, input: TokenSlice<'a>) -> BlockParseResult<'a> {
        let mut last_err = NomErr::Error(nom::error::Error::new(input, ErrorKind::Alt));

        for parser in self.block_parsers {
            match parser(input) {
                Ok(result) => return Ok(result),
                Err(NomErr::Error(e)) => last_err = NomErr::Error(e),
                Err(other @ NomErr::Incomplete(_)) | Err(other @ NomErr::Failure(_)) => {
                    return Err(other);
                }
            }
        }

        Err(last_err)
    }
}

impl<'input> ParseRule<Token<'input>, Document> for MarkdownParseRule {
    fn parse(&mut self, state: &mut ParserState<Token<'input>>) -> crate::error::Result<Document> {
        let mut blocks = Vec::new();

        loop {
            if state.is_exhausted() {
                break;
            }

            let remaining = state.remaining();
            let start_len = remaining.len();
            let slice = TokenSlice::new(remaining);

            let (after_ws, _) = match skip_ws_nl(slice) {
                Ok(res) => res,
                Err(NomErr::Error(_)) | Err(NomErr::Incomplete(_)) => {
                    state.finish();
                    break;
                }
                Err(err) => {
                    return Err(MarkdownError::parse_error(
                        Position::new(),
                        format!("Failed to skip whitespace: {:?}", err),
                    ));
                }
            };

            if after_ws.is_empty() {
                state.finish();
                break;
            }

            let (after_block, block) = match self.parse_with_rules(after_ws) {
                Ok(res) => res,
                Err(NomErr::Error(_)) | Err(NomErr::Incomplete(_)) => {
                    state.finish();
                    break;
                }
                Err(err) => {
                    return Err(MarkdownError::parse_error(
                        Position::new(),
                        format!("Failed to parse block: {:?}", err),
                    ));
                }
            };

            blocks.push(block);

            let consumed = start_len - after_block.len();
            state.advance(consumed);
        }

        Ok(Document {
            blocks,
            source_map: SourceMap::new(),
        })
    }
}
