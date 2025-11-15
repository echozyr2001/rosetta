use crate::lexer::{LexToken, Lexer, token::Token};
use crate::parser::ast::Document;
use crate::parser::core::{ParseAttempt, Parser, ParserError};
use crate::parser::traits::ParseRule;

/// Errors that can be produced while driving the streaming parser via the controller.
#[derive(Debug)]
pub enum ControllerError {
    /// Wrapper around [`ParserError`] reported by the streaming parser.
    Parser(ParserError),
    /// The lexer reported end-of-input before the parser completed.
    LexerExhausted,
    /// Internal invariant failed while orchestrating the parser/lexer interaction.
    UnexpectedState(&'static str),
}

impl From<ParserError> for ControllerError {
    fn from(value: ParserError) -> Self {
        ControllerError::Parser(value)
    }
}

/// Streaming controller that coordinates the lexer and parser.
///
/// The controller feeds tokens from the lexer into the parser until the parser produces a document
/// or reports a fatal error. It exposes a convenience API for reacting to intermediate
/// [`ParseAttempt::Pending`] results (useful for incremental UIs).
pub struct ParserController<'input, Rule>
where
    Rule: for<'a> ParseRule<'a, Token<'input>, Document>,
{
    parser: Parser<'input, Rule>,
    lexer: Lexer<'input>,
    eof_sent: bool,
    last_pending: Option<Document>,
}

impl<'input, Rule> ParserController<'input, Rule>
where
    Rule: for<'a> ParseRule<'a, Token<'input>, Document>,
{
    /// Creates a new controller for the provided input and parsing rule.
    pub fn new(input: &'input str, rule: Rule) -> Self {
        Self {
            parser: Parser::new(rule),
            lexer: Lexer::new(input),
            eof_sent: false,
            last_pending: None,
        }
    }

    /// Returns the most recent pending document produced by the parser, if any.
    pub fn last_pending(&self) -> Option<&Document> {
        self.last_pending.as_ref()
    }

    /// Drives the streaming parser to completion while invoking `on_pending` for intermediate
    /// documents emitted via [`ParseAttempt::Pending`].
    pub fn parse_with_listener<F>(mut self, mut on_pending: F) -> Result<Document, ControllerError>
    where
        F: FnMut(&Document),
    {
        loop {
            let attempt = self.parser.try_parse().map_err(ControllerError::Parser)?;
            match attempt {
                ParseAttempt::Complete(document) => return Ok(document),
                ParseAttempt::Pending { document, .. } => {
                    on_pending(&document);
                    self.last_pending = Some(document);
                    self.pull_next_token()?;
                }
                ParseAttempt::NeedMoreTokens {
                    must_end_with_eof, ..
                } => {
                    if must_end_with_eof {
                        self.inject_eof()?;
                    } else {
                        self.pull_next_token()?;
                    }
                }
            }
        }
    }

    /// Convenience helper for callers who are not interested in intermediate documents.
    pub fn parse(self) -> Result<Document, ControllerError> {
        self.parse_with_listener(|_| {})
    }
}

impl<'input, Rule> ParserController<'input, Rule>
where
    Rule: for<'a> ParseRule<'a, Token<'input>, Document>,
{
    fn pull_next_token(&mut self) -> Result<(), ControllerError> {
        let token = self
            .lexer
            .next_token()
            .ok_or(ControllerError::LexerExhausted)?;
        self.eof_sent |= token.is_eof();
        self.parser
            .push_token(token)
            .map_err(ControllerError::Parser)?;
        Ok(())
    }

    fn inject_eof(&mut self) -> Result<(), ControllerError> {
        if self.eof_sent {
            return Err(ControllerError::UnexpectedState(
                "parser requested EOF after EOF was already sent",
            ));
        }

        self.parser
            .push_token(Token::Eof)
            .map_err(ControllerError::Parser)?;
        self.eof_sent = true;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::rules::MarkdownParseRule;
    use crate::parser::traits::ParseRule;
    use nom::{Err as NomErr, IResult, error::ErrorKind as NomErrorKind};

    #[test]
    fn controller_parses_document() {
        let controller = ParserController::new("# Heading\n\nText", MarkdownParseRule);
        let document = controller.parse().expect("controller parses markdown");
        assert!(!document.blocks.is_empty());
    }

    #[test]
    fn controller_reacts_to_pending_updates() {
        let controller = ParserController::new("# Heading\n\nParagraph", MarkdownParseRule);
        let mut pending_called = false;
        let document = controller
            .parse_with_listener(|doc| {
                pending_called = true;
                assert!(!doc.blocks.is_empty());
            })
            .expect("controller completes");
        assert!(pending_called, "expected at least one pending callback");
        assert!(!document.blocks.is_empty());
    }

    #[derive(Clone)]
    struct ErrorRule;

    impl<'slice, 'input> ParseRule<'slice, Token<'input>, Document> for ErrorRule
    where
        'input: 'slice,
    {
        fn parse(
            &self,
            input: crate::parser::token_slice::TokenSlice<'slice, Token<'input>>,
        ) -> IResult<crate::parser::token_slice::TokenSlice<'slice, Token<'input>>, Document>
        {
            Err(NomErr::Error(nom::error::Error::new(
                input,
                NomErrorKind::Tag,
            )))
        }
    }

    #[test]
    fn controller_propagates_parser_errors() {
        let controller = ParserController::new("content", ErrorRule);
        let err = controller
            .parse()
            .expect_err("controller should surface parser error");
        match err {
            ControllerError::Parser(ParserError::NomError { kind }) => {
                assert_eq!(kind, NomErrorKind::Tag);
            }
            other => panic!("unexpected controller error: {other:?}"),
        }
    }
}
