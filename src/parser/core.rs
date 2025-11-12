// use super::config::ParserConfig;
// use crate::adapters::DefaultParsingContext;
// use crate::error::{ErrorHandler, Result};
// use crate::lexer::Position;
// use crate::traits::*;
use super::token_buffer::TokenBuffer;
use crate::lexer::{LexToken, token::Token};
use crate::parser::ast::Document;
use crate::parser::token_slice::TokenSlice;
use crate::parser::traits::ParseRule;
use nom::{Err as NomErr, error::ErrorKind as NomErrorKind};

/// Outcome of a single `Parser::try_parse` attempt.
#[derive(Debug)]
pub enum ParseAttempt {
    /// Parser needs more tokens to make progress.
    NeedMoreTokens,
    /// Parsing completed successfully and produced a document.
    Complete(Document),
}

/// Errors that can occur while running the parser.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    /// Parsing finished already; pushing more tokens is not allowed.
    AlreadyFinished,
    /// `nom` reported a syntax error while parsing the buffered tokens.
    NomError { kind: NomErrorKind },
    /// The rule returned successfully but left trailing tokens unconsumed.
    TrailingTokens { remaining: usize },
}

impl ParserError {
    fn from_nom(kind: NomErrorKind) -> Self {
        ParserError::NomError { kind }
    }
}

/// Streaming parser that consumes tokens from a [`TokenBuffer`] using a pluggable [`ParseRule`].
///
/// The parser is intentionally lightweight: it owns the token buffer, receives tokens from an
/// external controller, and attempts to parse the accumulated input whenever `try_parse` is
/// invoked. If the underlying rule needs additional tokens, the parser reports
/// [`ParseAttempt::NeedMoreTokens`] without consuming the buffer. Once the rule succeeds and
/// consumes the entire buffer, the parser returns the resulting [`Document`] and enters the
/// finished state.
pub struct Parser<'input, Rule>
where
    Rule: for<'a> ParseRule<'a, Token<'input>, Document>,
{
    buffer: TokenBuffer<Token<'input>>,
    rule: Rule,
    finished: bool,
}

impl<'input, Rule> Parser<'input, Rule>
where
    Rule: for<'a> ParseRule<'a, Token<'input>, Document>,
{
    /// Creates a new parser with the provided rule.
    pub fn new(rule: Rule) -> Self {
        Self {
            buffer: TokenBuffer::new(),
            rule,
            finished: false,
        }
    }

    /// Returns true once the parser has produced a document successfully.
    pub fn is_finished(&self) -> bool {
        self.finished
    }

    /// Returns the number of buffered tokens still awaiting parsing.
    pub fn buffered_len(&self) -> usize {
        self.buffer.len()
    }

    /// Clears the internal buffer and resets the finished flag. This allows the parser to be
    /// reused for a new document.
    pub fn reset(&mut self) {
        self.buffer.clear();
        self.finished = false;
    }

    /// Adds a token to the internal buffer. Returns an error if the parser has already completed
    /// parsing a document.
    pub fn push_token(&mut self, token: Token<'input>) -> Result<(), ParserError> {
        if self.finished {
            return Err(ParserError::AlreadyFinished);
        }
        self.buffer.push(token);
        Ok(())
    }

    /// Attempts to parse the currently buffered tokens.
    ///
    /// - On success, returns [`ParseAttempt::Complete`] and marks the parser as finished.
    /// - If the rule reports `Incomplete`, the parser keeps the buffer untouched and returns
    ///   [`ParseAttempt::NeedMoreTokens`].
    /// - On syntax errors, the buffer is left untouched and a [`ParserError`] is returned.
    pub fn try_parse(&mut self) -> Result<ParseAttempt, ParserError> {
        if self.finished {
            return Err(ParserError::AlreadyFinished);
        }

        let snapshot: TokenSlice<'_, Token<'input>> = self.buffer.snapshot();

        if snapshot.is_empty() {
            return Ok(ParseAttempt::NeedMoreTokens);
        }

        let tokens = snapshot.tokens();
        let saw_eof = tokens.iter().any(|tok| tok.is_eof());

        let initial_len = snapshot.len();
        let parse_result = self.rule.parse(snapshot);

        match parse_result {
            Ok((rest, document)) => {
                let remaining = rest.len();
                let trailing_tokens = rest.tokens();

                if trailing_tokens.is_empty() && !saw_eof {
                    // Parsed everything currently buffered but have not observed EOF yet.
                    // Keep the buffer as-is so additional tokens can be appended and retried.
                    return Ok(ParseAttempt::NeedMoreTokens);
                }

                if trailing_tokens.iter().any(|tok| !tok.is_eof()) {
                    return Err(ParserError::TrailingTokens { remaining });
                }

                if !saw_eof {
                    return Ok(ParseAttempt::NeedMoreTokens);
                }

                self.buffer.advance(initial_len);
                self.finished = true;
                Ok(ParseAttempt::Complete(document))
            }
            Err(NomErr::Incomplete(_)) => Ok(ParseAttempt::NeedMoreTokens),
            Err(NomErr::Error(err)) | Err(NomErr::Failure(err)) => {
                Err(ParserError::from_nom(err.code))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token::{AtxHeadingToken, TextToken, Token};
    use crate::lexer::{Lexer, Position};
    use crate::parser::ast::{Block, Document};
    use crate::parser::rules::MarkdownParseRule;
    use crate::parser::traits::ParseRule;
    use nom::{Err as NomErr, IResult, error::Error as NomError};

    #[test]
    fn parser_requires_eof_before_completion() {
        let mut parser = Parser::new(MarkdownParseRule);
        let mut lexer = Lexer::new("# Heading");

        let first = lexer.next_token().expect("token");
        assert!(!first.is_eof());
        parser.push_token(first).unwrap();

        assert!(
            matches!(parser.try_parse().unwrap(), ParseAttempt::NeedMoreTokens),
            "parser should request more tokens before EOF"
        );

        let eof = lexer.next_token().expect("eof token");
        assert!(eof.is_eof());
        parser.push_token(eof).unwrap();

        match parser.try_parse().unwrap() {
            ParseAttempt::Complete(document) => {
                assert_eq!(document.blocks.len(), 1);
                assert!(matches!(document.blocks[0], Block::Heading { .. }));
            }
            ParseAttempt::NeedMoreTokens => panic!("expected completion after EOF"),
        }
    }

    #[derive(Clone, Debug)]
    struct EchoRule;

    impl<'a, 'input> ParseRule<'a, Token<'input>, Document> for EchoRule
    where
        'input: 'a,
    {
        fn parse(
            &self,
            input: TokenSlice<'a, Token<'input>>,
        ) -> IResult<TokenSlice<'a, Token<'input>>, Document> {
            Ok((input, Document { blocks: Vec::new() }))
        }
    }

    #[test]
    fn parser_reports_trailing_tokens_when_rule_leaves_data() {
        let mut parser = Parser::new(EchoRule);
        let token = Token::Text(TextToken {
            lexeme: "text",
            position: Position::default(),
        });
        parser.push_token(token).unwrap();

        let err = parser
            .try_parse()
            .expect_err("expected trailing token error");
        assert!(matches!(err, ParserError::TrailingTokens { remaining } if remaining == 1));
    }

    #[derive(Clone, Debug)]
    struct ErrorRule;

    impl<'a, 'input> ParseRule<'a, Token<'input>, Document> for ErrorRule
    where
        'input: 'a,
    {
        fn parse(
            &self,
            input: TokenSlice<'a, Token<'input>>,
        ) -> IResult<TokenSlice<'a, Token<'input>>, Document> {
            Err(NomErr::Error(NomError::new(input, NomErrorKind::Tag)))
        }
    }

    #[test]
    fn parser_converts_nom_errors() {
        let mut parser = Parser::new(ErrorRule);
        let token = Token::Text(TextToken {
            lexeme: "text",
            position: Position::default(),
        });
        parser.push_token(token).unwrap();

        let err = parser.try_parse().expect_err("expected nom error");
        assert!(matches!(
            err,
            ParserError::NomError {
                kind: NomErrorKind::Tag
            }
        ));
    }

    #[test]
    fn parser_rejects_tokens_after_completion() {
        let mut parser = Parser::new(MarkdownParseRule);
        let mut lexer = Lexer::new("# Heading\n");

        loop {
            let token = lexer.next_token().expect("token");
            let eof = token.is_eof();
            parser.push_token(token).unwrap();
            if let ParseAttempt::Complete(_) = parser.try_parse().unwrap() {
                assert!(eof, "completion must occur on EOF token");
                break;
            }
        }

        let result = parser.push_token(Token::AtxHeading(AtxHeadingToken {
            level: 1,
            marker_count: 1,
            leading_whitespace: 0,
            raw_marker: "#",
            raw_content: "again",
            closing_sequence: "",
            position: Position::default(),
        }));
        assert!(matches!(result, Err(ParserError::AlreadyFinished)));
    }
}

// impl<Rule> Parser<Rule> {
//     pub fn new(rules: Rule) -> Self {
//         Self { rules }
//     }

//     pub fn parse(
//         self,
//         input: TokenSlice<'a, Token<'a>>,
//     ) -> IResult<TokenSlice<'a, Token<'a>>, Document> {
//         self.rules.parse(input)
//     }
// }

// impl<'input> Parser<DefaultParsingContext<'input>> {
//     /// Creates a new parser instance with the given configuration.
//     pub fn new(input: &'input str, config: ParserConfig) -> Result<Self> {
//         let context = DefaultParsingContext::new(input, config)?;
//         Ok(Self { context })
//     }

//     /// Convenience helper that uses default parser configuration.
//     pub fn with_defaults(input: &'input str) -> Self {
//         Self::new(input, ParserConfig::default()).expect("Failed to create parser with defaults")
//     }

//     /// Creates a parser with a custom error handler.
//     pub fn with_error_handler(
//         input: &'input str,
//         config: ParserConfig,
//         _error_handler: Box<dyn ErrorHandler>,
//     ) -> Self {
//         // Note: Custom error handler support can be added to DefaultParsingContext
//         Self::new(input, config).expect("Failed to create parser with error handler")
//     }
// }

// impl<C> Parser<C> {
//     /// Construct a parser from an explicit context.
//     pub fn from_context(context: C) -> Self {
//         Self { context }
//     }

//     /// Consume the parser and return the inner context.
//     pub fn into_context(self) -> C {
//         self.context
//     }
// }

// impl<'input, C> Parser<C>
// where
//     C: ParsingContext<
//             Token = crate::lexer::token::Token<'input>,
//             Document = crate::parser::ast::Document,
//         > + CanAccessParserConfig<Config = ParserConfig>,
// {
//     /// Returns the current position in the input.
//     pub fn position(&self) -> Position {
//         self.context.current_position()
//     }

//     /// Returns a reference to the collected reference definitions.
//     pub fn reference_definitions(
//         &self,
//     ) -> &std::collections::HashMap<String, crate::parser::ast::LinkReference> {
//         // Return empty map for now - can be extended later
//         static EMPTY_MAP: std::sync::LazyLock<
//             std::collections::HashMap<String, crate::parser::ast::LinkReference>,
//         > = std::sync::LazyLock::new(std::collections::HashMap::new);
//         &EMPTY_MAP
//     }

//     /// Parses the input into a `Document` AST using the configured context.
//     pub fn parse(mut self) -> Result<C::Document> {
//         crate::parser::token_pipeline::parse_document(&mut self.context)
//     }

//     /// Returns a reference to the parser configuration.
//     pub fn config(&self) -> &ParserConfig {
//         self.context.parser_config()
//     }

//     /// Handle a parsing warning with the error handler
//     pub fn handle_parse_warning(&mut self, message: String, _input: &str) {
//         let error_info =
//             crate::error::ErrorInfo::new(crate::error::ErrorSeverity::Warning, message)
//                 .with_position(self.position());

//         self.context.error_handler_mut().handle_error(&error_info);
//     }

//     /// Handle a parsing error with the error handler
//     pub fn handle_parse_error(&mut self, message: String, _input: &str) {
//         let error_info = crate::error::ErrorInfo::new(crate::error::ErrorSeverity::Error, message)
//             .with_position(self.position());

//         self.context.error_handler_mut().handle_error(&error_info);
//     }
// }

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_parse_document() {
//         let input = "# Title\n\nThis is a paragraph";
//         let parser = Parser::with_defaults(input);
//         let result = parser.parse();
//         assert!(
//             result.is_ok(),
//             "Failed to parse document: {:?}",
//             result.err()
//         );

//         let document = result.unwrap();
//         assert!(!document.blocks.is_empty(), "Document should have blocks");
//     }

//     #[test]
//     fn test_error_handler_usage() {
//         use crate::error::DefaultErrorHandler;

//         // Test with a custom error handler
//         let error_handler = DefaultErrorHandler::new();
//         let config = ParserConfig::default();

//         // Create a parser with the custom error handler
//         let parser = Parser::with_error_handler(
//             "# Valid heading\n\nSome content",
//             config,
//             Box::new(error_handler),
//         );

//         let result = parser.parse();
//         assert!(result.is_ok());
//     }

//     #[test]
//     fn test_position_information_in_parsed_ast() {
//         let input = "# First Heading\n\nSome paragraph text\n\n## Second Heading";
//         let parser = Parser::with_defaults(input);
//         let document = parser.parse().unwrap();

//         // Check that blocks are parsed correctly
//         assert!(!document.blocks.is_empty(), "Document should have blocks");
//     }
// }
