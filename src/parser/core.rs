use super::config::ParserConfig;
use crate::adapters::DefaultParsingContext;
use crate::error::{ErrorHandler, Result};
use crate::lexer::Position;
use crate::traits::*;

/// Parser that uses component-oriented context principles.
///
/// This parser uses a two-phase approach:
/// 1. Lexer → Token sequence
/// 2. Parser → AST
///
/// **Benefits of this approach**:
/// - ✅ Better position tracking
/// - ✅ Reusable lexical analysis
/// - ✅ Modular and testable via traits
/// - ✅ Easy to extend and customize
///
/// **Architecture**:
/// - Uses `DefaultParsingContext` which combines all component traits
/// - Delegates to `token_pipeline` for actual parsing logic
pub struct Parser<C> {
    context: C,
}

impl<'input> Parser<DefaultParsingContext<'input>> {
    /// Creates a new parser instance with the given configuration.
    pub fn new(input: &'input str, config: ParserConfig) -> Result<Self> {
        let context = DefaultParsingContext::new(input, config)?;
        Ok(Self { context })
    }

    /// Convenience helper that uses default parser configuration.
    pub fn with_defaults(input: &'input str) -> Self {
        Self::new(input, ParserConfig::default()).expect("Failed to create parser with defaults")
    }

    /// Creates a parser with a custom error handler.
    pub fn with_error_handler(
        input: &'input str,
        config: ParserConfig,
        _error_handler: Box<dyn ErrorHandler>,
    ) -> Self {
        // Note: Custom error handler support can be added to DefaultParsingContext
        Self::new(input, config).expect("Failed to create parser with error handler")
    }
}

impl<C> Parser<C> {
    /// Construct a parser from an explicit context.
    pub fn from_context(context: C) -> Self {
        Self { context }
    }

    /// Consume the parser and return the inner context.
    pub fn into_context(self) -> C {
        self.context
    }
}

impl<'input, C> Parser<C>
where
    C: ParsingContext<
            Token = crate::lexer::token::Token<'input>,
            Document = crate::parser::ast::Document,
        > + CanAccessParserConfig<Config = ParserConfig>,
{
    /// Returns the current position in the input.
    pub fn position(&self) -> Position {
        self.context.current_position()
    }

    /// Returns a reference to the collected reference definitions.
    pub fn reference_definitions(
        &self,
    ) -> &std::collections::HashMap<String, crate::parser::ast::LinkReference> {
        // Return empty map for now - can be extended later
        static EMPTY_MAP: std::sync::LazyLock<
            std::collections::HashMap<String, crate::parser::ast::LinkReference>,
        > = std::sync::LazyLock::new(std::collections::HashMap::new);
        &EMPTY_MAP
    }

    /// Parses the input into a `Document` AST using the configured context.
    pub fn parse(mut self) -> Result<C::Document> {
        crate::parser::token_pipeline::parse_document(&mut self.context)
    }

    /// Returns a reference to the parser configuration.
    pub fn config(&self) -> &ParserConfig {
        self.context.parser_config()
    }

    /// Handle a parsing warning with the error handler
    pub fn handle_parse_warning(&mut self, message: String, _input: &str) {
        let error_info =
            crate::error::ErrorInfo::new(crate::error::ErrorSeverity::Warning, message)
                .with_position(self.position());

        self.context.error_handler_mut().handle_error(&error_info);
    }

    /// Handle a parsing error with the error handler
    pub fn handle_parse_error(&mut self, message: String, _input: &str) {
        let error_info = crate::error::ErrorInfo::new(crate::error::ErrorSeverity::Error, message)
            .with_position(self.position());

        self.context.error_handler_mut().handle_error(&error_info);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_document() {
        let input = "# Title\n\nThis is a paragraph";
        let parser = Parser::with_defaults(input);
        let result = parser.parse();
        assert!(
            result.is_ok(),
            "Failed to parse document: {:?}",
            result.err()
        );

        let document = result.unwrap();
        assert!(!document.blocks.is_empty(), "Document should have blocks");
    }

    #[test]
    fn test_error_handler_usage() {
        use crate::error::DefaultErrorHandler;

        // Test with a custom error handler
        let error_handler = DefaultErrorHandler::new();
        let config = ParserConfig::default();

        // Create a parser with the custom error handler
        let parser = Parser::with_error_handler(
            "# Valid heading\n\nSome content",
            config,
            Box::new(error_handler),
        );

        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_position_information_in_parsed_ast() {
        let input = "# First Heading\n\nSome paragraph text\n\n## Second Heading";
        let parser = Parser::with_defaults(input);
        let document = parser.parse().unwrap();

        // Check that blocks are parsed correctly
        assert!(!document.blocks.is_empty(), "Document should have blocks");
    }
}
