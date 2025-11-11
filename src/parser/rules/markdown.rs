use crate::error::Result;
use crate::lexer::token::{ListKind as LexerListKind, Token};
use crate::parser::state::ParserState;
use crate::parser::traits::{ListItem, ListKind, ParseRule};
use crate::traits::ParsingContext;

#[derive(Clone, Copy, Default)]
pub struct MarkdownParseRule;

impl MarkdownParseRule {
    fn skip_trivia<'ctx, 'input, C>(&self, state: &mut ParserState<'ctx, C>) -> Result<()>
    where
        C: ParsingContext<Token = Token<'input>>,
    {
        loop {
            let should_skip = match state.current_token() {
                Some(token) => matches!(
                    token,
                    Token::Whitespace(_)
                        | Token::LineEnding(_)
                        | Token::SoftBreak(_)
                        | Token::HardBreak(_)
                ),
                None => false,
            };

            if !should_skip {
                break;
            }

            state.advance()?;

            if state.is_exhausted() {
                break;
            }
        }

        Ok(())
    }

    fn parse_block<'ctx, 'input, C>(
        &self,
        state: &mut ParserState<'ctx, C>,
    ) -> Result<Option<C::Block>>
    where
        C: ParsingContext<Token = Token<'input>>,
    {
        let token = match state.current_token() {
            Some(token) => token,
            None => return Ok(None),
        };

        match token {
            Token::AtxHeading(heading) => {
                let level = heading.level;
                let text = heading.raw_content.trim().to_string();
                let position = Some(heading.position);

                state.advance()?;

                let builder = state.ast_builder();
                let content = vec![builder.build_text(text)];
                Ok(Some(builder.build_heading(level, content, position)))
            }
            Token::SetextHeading(heading) => {
                let level = heading.level;
                let text = "Setext Heading".to_string();
                let position = Some(heading.position);

                state.advance()?;

                let builder = state.ast_builder();
                let content = vec![builder.build_text(text)];
                Ok(Some(builder.build_heading(level, content, position)))
            }
            Token::CodeBlock(code) => {
                let info = code.info_string.map(|s| s.to_string());
                let language = code
                    .info_string
                    .and_then(|info| info.split_whitespace().next().map(str::to_string));
                let content = code.raw_content.to_string();
                let position = Some(code.position);

                state.advance()?;

                let builder = state.ast_builder();
                Ok(Some(
                    builder.build_code_block(info, content, language, position),
                ))
            }
            Token::BlockQuote(block_quote) => {
                let position = Some(block_quote.position);

                state.advance()?;

                let builder = state.ast_builder();
                let paragraph = builder.build_paragraph(
                    vec![builder.build_text("Blockquote content".to_string())],
                    position,
                );
                Ok(Some(builder.build_block_quote(vec![paragraph], position)))
            }
            Token::ListMarker(marker) => {
                let position = Some(marker.position);
                let list_kind = match &marker.marker {
                    LexerListKind::Bullet { marker } => ListKind::Bullet { marker: *marker },
                    LexerListKind::Ordered { start, delimiter } => ListKind::Ordered {
                        start: *start,
                        delimiter: *delimiter,
                    },
                };

                state.advance()?;

                let builder = state.ast_builder();
                let paragraph = builder.build_paragraph(
                    vec![builder.build_text("List item content".to_string())],
                    position,
                );
                let item = ListItem {
                    content: vec![paragraph],
                    tight: true,
                    task_list_marker: None,
                };
                Ok(Some(builder.build_list(
                    list_kind,
                    true,
                    vec![item],
                    position,
                )))
            }
            Token::ThematicBreak(break_token) => {
                let position = Some(break_token.position);
                state.advance()?;
                let builder = state.ast_builder();
                Ok(Some(builder.build_thematic_break(position)))
            }
            Token::Text(text) => {
                let content = text.lexeme.to_string();
                let position = Some(text.position);

                state.advance()?;

                let builder = state.ast_builder();
                let inline = builder.build_text(content);
                Ok(Some(builder.build_paragraph(vec![inline], position)))
            }
            Token::HtmlBlock(html) => {
                let content = html.raw.to_string();
                let position = Some(html.position);

                state.advance()?;

                let builder = state.ast_builder();
                Ok(Some(builder.build_html_block(content, position)))
            }
            Token::Eof => Ok(None),
            _ => {
                state.advance()?;
                Ok(None)
            }
        }
    }
}

impl<'input, C> ParseRule<C> for MarkdownParseRule
where
    C: ParsingContext<Token = Token<'input>>,
{
    fn parse(&mut self, state: &mut ParserState<'_, C>) -> Result<C::Document> {
        let mut blocks = Vec::new();

        loop {
            self.skip_trivia(state)?;

            if state.is_exhausted() {
                break;
            }

            match self.parse_block(state)? {
                Some(block) => blocks.push(block),
                None => {
                    if state.is_exhausted() {
                        break;
                    }
                }
            }
        }

        let builder = state.ast_builder();
        Ok(builder.build_document(blocks))
    }
}
