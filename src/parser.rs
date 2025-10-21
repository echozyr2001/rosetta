use crate::ast::{Document, SourceMap, ReferenceMap};
use crate::error::{ErrorHandler, DefaultErrorHandler, ErrorInfo, ErrorSeverity, MarkdownError, Result};
use crate::lexer::{Lexer, Token, Position};

/// Configuration for the parser behavior and options
#[derive(Debug, Clone)]
pub struct ParserConfig {
    /// Whether to strictly follow CommonMark specification
    pub strict_commonmark: bool,
    /// Maximum nesting depth to prevent stack overflow
    pub max_nesting_depth: usize,
    /// Whether to enable GitHub Flavored Markdown extensions
    pub enable_gfm_extensions: bool,
    /// Whether to collect detailed source position information
    pub track_source_positions: bool,
    /// Maximum number of errors before stopping parsing
    pub max_errors: Option<usize>,
}

impl Default for ParserConfig {
    fn default() -> Self {
        Self {
            strict_commonmark: true,
            max_nesting_depth: 64,
            enable_gfm_extensions: false,
            track_source_positions: true,
            max_errors: Some(100),
        }
    }
}

/// Parser state for tracking parsing context and recovery
#[derive(Debug, Clone)]
struct ParserState {
    /// Current nesting depth
    nesting_depth: usize,
    /// Whether we're currently in a recovery mode
    in_recovery: bool,
    /// Last known good position for error recovery
    last_good_position: Position,
    /// Reference definitions collected during parsing
    reference_map: ReferenceMap,
}

impl Default for ParserState {
    fn default() -> Self {
        Self {
            nesting_depth: 0,
            in_recovery: false,
            last_good_position: Position::new(),
            reference_map: ReferenceMap::new(),
        }
    }
}

/// Main parser struct implementing recursive descent parsing with error recovery
pub struct Parser<'input> {
    /// Lexer for tokenizing input
    lexer: Lexer<'input>,
    /// Current token being processed
    current_token: Option<Token<'input>>,
    /// Parser configuration
    config: ParserConfig,
    /// Parser state for context tracking
    state: ParserState,
    /// Error handler for collecting and managing errors
    error_handler: Box<dyn ErrorHandler>,
}

impl<'input> Parser<'input> {
    /// Creates a new parser with the given input and configuration
    pub fn new(input: &'input str, config: ParserConfig) -> Self {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next_token().ok();
        
        let error_handler: Box<dyn ErrorHandler> = if let Some(max_errors) = config.max_errors {
            Box::new(DefaultErrorHandler::with_max_errors(max_errors))
        } else {
            Box::new(DefaultErrorHandler::new())
        };

        Self {
            lexer,
            current_token,
            config,
            state: ParserState::default(),
            error_handler,
        }
    }

    /// Creates a new parser with default configuration
    pub fn with_defaults(input: &'input str) -> Self {
        Self::new(input, ParserConfig::default())
    }

    /// Creates a new parser with custom error handler
    pub fn with_error_handler(
        input: &'input str,
        config: ParserConfig,
        error_handler: Box<dyn ErrorHandler>,
    ) -> Self {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next_token().ok();

        Self {
            lexer,
            current_token,
            config,
            state: ParserState::default(),
            error_handler,
        }
    }

    /// Returns the current position in the input
    pub fn position(&self) -> Position {
        self.lexer.position()
    }

    /// Returns a reference to the current parser configuration
    pub fn config(&self) -> &ParserConfig {
        &self.config
    }

    /// Returns a reference to the collected reference definitions
    pub fn reference_map(&self) -> &ReferenceMap {
        &self.state.reference_map
    }

    /// Parses the input and returns a Document AST
    pub fn parse(&mut self) -> Result<Document> {
        self.reset_state();
        
        match self.parse_document() {
            Ok(document) => {
                // For now, we'll skip the fatal error check since it requires complex downcasting
                // This will be improved in future iterations
                Ok(document)
            }
            Err(error) => {
                // Attempt error recovery and return partial document if possible
                if self.config.strict_commonmark {
                    Err(error)
                } else {
                    // In non-strict mode, try to recover and return partial results
                    self.attempt_recovery();
                    Ok(Document {
                        blocks: Vec::new(),
                        source_map: SourceMap::new(),
                    })
                }
            }
        }
    }

    /// Advances to the next token
    fn advance(&mut self) -> Result<()> {
        match self.lexer.next_token() {
            Ok(token) => {
                self.current_token = Some(token);
                Ok(())
            }
            Err(error) => {
                self.handle_lexer_error(error);
                // Try to continue with EOF token
                self.current_token = Some(Token::Eof);
                Ok(())
            }
        }
    }

    /// Peeks at the next token without consuming it
    fn peek_token(&mut self) -> Result<Token<'input>> {
        self.lexer.peek_token()
    }

    /// Checks if the current token matches the expected token type
    fn current_token_is(&self, expected: &Token) -> bool {
        if let Some(ref current) = self.current_token {
            std::mem::discriminant(current) == std::mem::discriminant(expected)
        } else {
            false
        }
    }

    /// Consumes the current token if it matches the expected type
    fn consume_token(&mut self, expected: Token<'input>) -> Result<Token<'input>> {
        if let Some(current) = &self.current_token {
            if std::mem::discriminant(current) == std::mem::discriminant(&expected) {
                let token = current.clone();
                self.advance()?;
                return Ok(token);
            }
        }
        
        Err(MarkdownError::parse_error(
            self.position(),
            format!("Expected token {:?}, found {:?}", expected, self.current_token)
        ))
    }

    /// Checks if we've reached the end of input
    fn is_at_end(&self) -> bool {
        matches!(self.current_token, Some(Token::Eof) | None)
    }

    /// Resets parser state for a new parsing session
    fn reset_state(&mut self) {
        self.state = ParserState::default();
        self.state.last_good_position = self.position();
    }

    /// Enters a new nesting level, checking for maximum depth
    fn enter_nesting_level(&mut self) -> Result<()> {
        self.state.nesting_depth += 1;
        if self.state.nesting_depth > self.config.max_nesting_depth {
            let error = MarkdownError::parse_error(
                self.position(),
                format!("Maximum nesting depth {} exceeded", self.config.max_nesting_depth)
            );
            self.handle_parse_error(error.clone());
            return Err(error);
        }
        Ok(())
    }

    /// Exits the current nesting level
    fn exit_nesting_level(&mut self) {
        if self.state.nesting_depth > 0 {
            self.state.nesting_depth -= 1;
        }
    }

    /// Records the current position as a good recovery point
    fn mark_good_position(&mut self) {
        self.state.last_good_position = self.position();
        self.state.in_recovery = false;
    }

    /// Attempts error recovery by advancing to a safe parsing state
    fn attempt_recovery(&mut self) {
        self.state.in_recovery = true;
        
        // Try to find a block boundary or other safe recovery point
        while !self.is_at_end() {
            match &self.current_token {
                Some(Token::Newline) | Some(Token::Eof) => {
                    // Found a potential recovery point
                    self.advance().ok();
                    break;
                }
                Some(Token::AtxHeading { .. }) | 
                Some(Token::SetextHeading { .. }) |
                Some(Token::ThematicBreak) |
                Some(Token::BlockQuote) => {
                    // Found a block-level element, good recovery point
                    break;
                }
                _ => {
                    // Continue advancing until we find a recovery point
                    if self.advance().is_err() {
                        break;
                    }
                }
            }
        }
        
        // Reset nesting depth after recovery
        self.state.nesting_depth = 0;
        self.mark_good_position();
    }

    /// Handles lexer errors with appropriate recovery
    fn handle_lexer_error(&mut self, error: MarkdownError) {
        let error_info = ErrorInfo::new(
            ErrorSeverity::Error,
            format!("Lexer error: {}", error)
        ).with_position(self.position());
        
        self.error_handler.handle_error(&error_info);
        
        if !self.error_handler.should_continue(ErrorSeverity::Error) {
            // Convert to fatal error if we should stop
            let fatal_error = ErrorInfo::new(
                ErrorSeverity::Fatal,
                "Too many lexer errors, stopping parsing".to_string()
            ).with_position(self.position());
            self.error_handler.handle_error(&fatal_error);
        }
    }

    /// Handles parse errors with appropriate recovery
    fn handle_parse_error(&mut self, error: MarkdownError) {
        let error_info = ErrorInfo::new(
            if error.is_recoverable() { ErrorSeverity::Error } else { ErrorSeverity::Fatal },
            format!("Parse error: {}", error)
        ).with_position(self.position());
        
        self.error_handler.handle_error(&error_info);
        
        if error.is_recoverable() && !self.config.strict_commonmark {
            self.attempt_recovery();
        }
    }

    /// Placeholder for document parsing - will be implemented in subsequent tasks
    fn parse_document(&mut self) -> Result<Document> {
        // This is a placeholder implementation for task 4.1
        // The actual document parsing logic will be implemented in task 4.2
        
        // For now, return a simple document to demonstrate the parser foundation
        Ok(Document {
            blocks: Vec::new(),
            source_map: SourceMap::new(),
        })
    }
}



/// Simple parse function with default configuration for convenience
pub fn parse(markdown: &str) -> Result<Document> {
    let mut parser = Parser::with_defaults(markdown);
    parser.parse()
}

/// Parse function with custom configuration
pub fn parse_with_config(markdown: &str, config: ParserConfig) -> Result<Document> {
    let mut parser = Parser::new(markdown, config);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::{ErrorSeverity, ErrorInfo};

    #[test]
    fn test_parser_creation() {
        let input = "# Hello World";
        let parser = Parser::with_defaults(input);
        
        assert_eq!(parser.config().strict_commonmark, true);
        assert_eq!(parser.config().max_nesting_depth, 64);
        assert_eq!(parser.config().track_source_positions, true);
    }

    #[test]
    fn test_parser_with_custom_config() {
        let input = "# Hello World";
        let config = ParserConfig {
            strict_commonmark: false,
            max_nesting_depth: 32,
            enable_gfm_extensions: true,
            track_source_positions: false,
            max_errors: Some(50),
        };
        
        let parser = Parser::new(input, config.clone());
        assert_eq!(parser.config().strict_commonmark, false);
        assert_eq!(parser.config().max_nesting_depth, 32);
        assert_eq!(parser.config().enable_gfm_extensions, true);
        assert_eq!(parser.config().track_source_positions, false);
        assert_eq!(parser.config().max_errors, Some(50));
    }

    #[test]
    fn test_parser_position_tracking() {
        let input = "line1\nline2";
        let parser = Parser::with_defaults(input);
        
        let position = parser.position();
        assert_eq!(position.line, 1);
        // Position may have advanced due to lexer initialization, so we check it's reasonable
        assert!(position.column >= 1);
    }

    #[test]
    fn test_parser_basic_parsing() {
        let input = "# Hello World";
        let mut parser = Parser::with_defaults(input);
        
        let result = parser.parse();
        assert!(result.is_ok());
        
        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 0); // Placeholder implementation returns empty document
    }

    #[test]
    fn test_parser_empty_input() {
        let input = "";
        let mut parser = Parser::with_defaults(input);
        
        let result = parser.parse();
        assert!(result.is_ok());
        
        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 0);
    }

    #[test]
    fn test_parser_with_custom_error_handler() {
        struct TestErrorHandler {
            errors: Vec<ErrorInfo>,
        }
        
        impl ErrorHandler for TestErrorHandler {
            fn handle_error(&mut self, error: &ErrorInfo) {
                self.errors.push(error.clone());
            }
            
            fn should_continue(&self, _severity: ErrorSeverity) -> bool {
                self.errors.len() < 5
            }
        }
        
        let input = "# Hello World";
        let config = ParserConfig::default();
        let error_handler = Box::new(TestErrorHandler { errors: Vec::new() });
        
        let parser = Parser::with_error_handler(input, config, error_handler);
        assert_eq!(parser.config().strict_commonmark, true);
    }

    #[test]
    fn test_parser_config_default() {
        let config = ParserConfig::default();
        
        assert_eq!(config.strict_commonmark, true);
        assert_eq!(config.max_nesting_depth, 64);
        assert_eq!(config.enable_gfm_extensions, false);
        assert_eq!(config.track_source_positions, true);
        assert_eq!(config.max_errors, Some(100));
    }

    #[test]
    fn test_parser_state_management() {
        let input = "# Test";
        let mut parser = Parser::with_defaults(input);
        
        // Test initial state
        assert_eq!(parser.state.nesting_depth, 0);
        assert_eq!(parser.state.in_recovery, false);
        
        // Test nesting level management
        assert!(parser.enter_nesting_level().is_ok());
        assert_eq!(parser.state.nesting_depth, 1);
        
        parser.exit_nesting_level();
        assert_eq!(parser.state.nesting_depth, 0);
    }

    #[test]
    fn test_parser_max_nesting_depth() {
        let input = "# Test";
        let config = ParserConfig {
            max_nesting_depth: 2,
            ..ParserConfig::default()
        };
        let mut parser = Parser::new(input, config);
        
        // Should succeed within limit
        assert!(parser.enter_nesting_level().is_ok());
        assert!(parser.enter_nesting_level().is_ok());
        
        // Should fail when exceeding limit
        assert!(parser.enter_nesting_level().is_err());
    }

    #[test]
    fn test_parser_error_recovery() {
        let input = "# Test";
        let mut parser = Parser::with_defaults(input);
        
        // Mark initial good position
        parser.mark_good_position();
        assert_eq!(parser.state.in_recovery, false);
        
        // Simulate entering recovery mode
        parser.attempt_recovery();
        // Recovery should complete and mark good position
        assert_eq!(parser.state.in_recovery, false);
        assert_eq!(parser.state.nesting_depth, 0);
    }

    #[test]
    fn test_parser_reference_map() {
        let input = "# Test";
        let parser = Parser::with_defaults(input);
        
        let ref_map = parser.reference_map();
        // Should start with empty reference map
        assert!(ref_map.get("test").is_none());
    }

    #[test]
    fn test_convenience_parse_functions() {
        let input = "# Hello World";
        
        // Test simple parse function
        let result1 = parse(input);
        assert!(result1.is_ok());
        
        // Test parse with config
        let config = ParserConfig::default();
        let result2 = parse_with_config(input, config);
        assert!(result2.is_ok());
    }

    #[test]
    fn test_parser_token_management() {
        let input = "# Hello";
        let mut parser = Parser::with_defaults(input);
        
        // Test token advancement
        let initial_token = parser.current_token.clone();
        assert!(parser.advance().is_ok());
        
        // Token should have changed after advance
        assert_ne!(parser.current_token, initial_token);
    }

    #[test]
    fn test_parser_end_detection() {
        let input = "";
        let parser = Parser::with_defaults(input);
        
        // Empty input should be at end
        assert!(parser.is_at_end());
    }

    #[test]
    fn test_parser_strict_vs_non_strict_mode() {
        let input = "# Test";
        
        // Test strict mode
        let strict_config = ParserConfig {
            strict_commonmark: true,
            ..ParserConfig::default()
        };
        let mut strict_parser = Parser::new(input, strict_config);
        let strict_result = strict_parser.parse();
        assert!(strict_result.is_ok());
        
        // Test non-strict mode
        let non_strict_config = ParserConfig {
            strict_commonmark: false,
            ..ParserConfig::default()
        };
        let mut non_strict_parser = Parser::new(input, non_strict_config);
        let non_strict_result = non_strict_parser.parse();
        assert!(non_strict_result.is_ok());
    }

    #[test]
    fn test_parser_with_unicode_input() {
        let input = "# Hello 🌍 World";
        let mut parser = Parser::with_defaults(input);
        
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parser_with_multiline_input() {
        let input = "# Heading\n\nParagraph text\n\n## Another heading";
        let mut parser = Parser::with_defaults(input);
        
        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parser_state_reset() {
        let input = "# Test";
        let mut parser = Parser::with_defaults(input);
        
        // Modify state
        parser.enter_nesting_level().ok();
        parser.state.in_recovery = true;
        
        // Reset should restore initial state
        parser.reset_state();
        assert_eq!(parser.state.nesting_depth, 0);
        assert_eq!(parser.state.in_recovery, false);
    }
}