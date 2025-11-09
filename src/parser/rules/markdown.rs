use crate::ast::{Document, SourceMap};
use crate::error::MarkdownError;
use crate::lexer::{Position, token::Token};
use crate::parser::rules::block_patterns::skip_ws_nl;
use crate::parser::rules::block_rules::MarkdownBlockRules;
use crate::parser::rules::input::TokenSlice;
use crate::parser::state::ParserState;
use crate::parser::traits::ParseRule;
use nom::Err as NomErr;

#[derive(Clone, Default)]
pub struct MarkdownParseRule {
    block_rules: MarkdownBlockRules,
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

            let (after_block, block) = match self.block_rules.parse(after_ws) {
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
