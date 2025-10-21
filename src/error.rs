/// Error handling module for the Markdown engine.
///
/// This module defines error types and utilities for comprehensive error reporting
/// throughout the parsing and generation pipeline.
use crate::lexer::Position;
use thiserror::Error;

/// Main error type for the Markdown engine.
#[derive(Debug, Error)]
pub enum MarkdownError {
    /// Lexical analysis errors (tokenization phase).
    #[error("Lexical error at line {}, column {}: {message}", position.line, position.column)]
    Lex { position: Position, message: String },

    /// Parsing errors (AST construction phase).
    #[error("Parse error at line {}, column {}: {message}", position.line, position.column)]
    Parse { position: Position, message: String },

    /// DOM transformation errors.
    #[error("Transform error: {message}")]
    Transform { message: String },

    /// HTML generation errors.
    #[error("Generation error: {message}")]
    Generation { message: String },

    /// I/O related errors.
    #[error("IO error: {source}")]
    Io {
        #[from]
        source: std::io::Error,
    },

    /// UTF-8 encoding errors.
    #[error("UTF-8 encoding error: {source}")]
    Utf8 {
        #[from]
        source: std::str::Utf8Error,
    },

    /// String conversion errors.
    #[error("String conversion error: {source}")]
    FromUtf8 {
        #[from]
        source: std::string::FromUtf8Error,
    },
}

/// Convenience type alias for Results in the Markdown engine.
pub type Result<T> = std::result::Result<T, MarkdownError>;

impl MarkdownError {
    /// Creates a new lexical error with position information.
    pub fn lex_error(position: Position, message: impl Into<String>) -> Self {
        MarkdownError::Lex {
            position,
            message: message.into(),
        }
    }

    /// Creates a new parse error with position information.
    pub fn parse_error(position: Position, message: impl Into<String>) -> Self {
        MarkdownError::Parse {
            position,
            message: message.into(),
        }
    }

    /// Creates a new transform error.
    pub fn transform_error(message: impl Into<String>) -> Self {
        MarkdownError::Transform {
            message: message.into(),
        }
    }

    /// Creates a new generation error.
    pub fn generation_error(message: impl Into<String>) -> Self {
        MarkdownError::Generation {
            message: message.into(),
        }
    }

    /// Returns the position associated with this error, if any.
    pub fn position(&self) -> Option<Position> {
        match self {
            MarkdownError::Lex { position, .. } => Some(*position),
            MarkdownError::Parse { position, .. } => Some(*position),
            _ => None,
        }
    }

    /// Returns true if this is a recoverable error.
    pub fn is_recoverable(&self) -> bool {
        match self {
            MarkdownError::Lex { .. } => true,
            MarkdownError::Parse { .. } => true,
            MarkdownError::Transform { .. } => false,
            MarkdownError::Generation { .. } => false,
            MarkdownError::Io { .. } => false,
            MarkdownError::Utf8 { .. } => false,
            MarkdownError::FromUtf8 { .. } => false,
        }
    }
}

impl Clone for MarkdownError {
    fn clone(&self) -> Self {
        match self {
            MarkdownError::Lex { position, message } => MarkdownError::Lex {
                position: *position,
                message: message.clone(),
            },
            MarkdownError::Parse { position, message } => MarkdownError::Parse {
                position: *position,
                message: message.clone(),
            },
            MarkdownError::Transform { message } => MarkdownError::Transform {
                message: message.clone(),
            },
            MarkdownError::Generation { message } => MarkdownError::Generation {
                message: message.clone(),
            },
            MarkdownError::Io { source } => MarkdownError::Io {
                source: std::io::Error::new(source.kind(), source.to_string()),
            },
            MarkdownError::Utf8 { source } => MarkdownError::Utf8 {
                source: *source,
            },
            MarkdownError::FromUtf8 { source } => MarkdownError::FromUtf8 {
                source: source.clone(),
            },
        }
    }
}

/// Error severity levels for different types of issues.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorSeverity {
    /// Fatal errors that prevent further processing.
    Fatal,
    /// Errors that can be recovered from.
    Error,
    /// Warnings about potentially problematic input.
    Warning,
    /// Informational messages.
    Info,
}

/// Detailed error information with severity and context.
#[derive(Debug, Clone)]
pub struct ErrorInfo {
    pub severity: ErrorSeverity,
    pub position: Option<Position>,
    pub message: String,
    pub context: Option<String>,
}

impl ErrorInfo {
    /// Creates a new error info with the given severity and message.
    pub fn new(severity: ErrorSeverity, message: String) -> Self {
        ErrorInfo {
            severity,
            position: None,
            message,
            context: None,
        }
    }

    /// Sets the position for this error.
    pub fn with_position(mut self, position: Position) -> Self {
        self.position = Some(position);
        self
    }

    /// Sets additional context for this error.
    pub fn with_context(mut self, context: String) -> Self {
        self.context = Some(context);
        self
    }
}

/// Error handler trait for custom error processing.
pub trait ErrorHandler {
    /// Called when an error occurs during processing.
    fn handle_error(&mut self, error: &ErrorInfo);

    /// Returns whether processing should continue after an error.
    fn should_continue(&self, severity: ErrorSeverity) -> bool;
}

/// Default error handler that collects errors in a vector.
#[derive(Debug, Default)]
pub struct DefaultErrorHandler {
    pub errors: Vec<ErrorInfo>,
    pub max_errors: Option<usize>,
}

impl DefaultErrorHandler {
    /// Creates a new default error handler.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new error handler with a maximum error count.
    pub fn with_max_errors(max_errors: usize) -> Self {
        DefaultErrorHandler {
            errors: Vec::new(),
            max_errors: Some(max_errors),
        }
    }

    /// Returns true if there are any fatal errors.
    pub fn has_fatal_errors(&self) -> bool {
        self.errors
            .iter()
            .any(|e| e.severity == ErrorSeverity::Fatal)
    }

    /// Returns the number of errors of a given severity.
    pub fn count_by_severity(&self, severity: ErrorSeverity) -> usize {
        self.errors
            .iter()
            .filter(|e| e.severity == severity)
            .count()
    }
}

impl ErrorHandler for DefaultErrorHandler {
    fn handle_error(&mut self, error: &ErrorInfo) {
        self.errors.push(error.clone());
    }

    fn should_continue(&self, severity: ErrorSeverity) -> bool {
        if severity == ErrorSeverity::Fatal {
            return false;
        }

        if let Some(max) = self.max_errors {
            self.errors.len() < max
        } else {
            true
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let position = Position {
            line: 1,
            column: 5,
            offset: 4,
        };
        let error = MarkdownError::Lex {
            position,
            message: "Unexpected character".to_string(),
        };

        let error_str = format!("{}", error);
        assert!(error_str.contains("line 1"));
        assert!(error_str.contains("column 5"));
        assert!(error_str.contains("Unexpected character"));
    }

    #[test]
    fn test_error_constructors() {
        let position = Position {
            line: 2,
            column: 10,
            offset: 15,
        };

        let lex_error = MarkdownError::lex_error(position, "Invalid token");
        assert_eq!(lex_error.position(), Some(position));
        assert!(lex_error.is_recoverable());

        let parse_error = MarkdownError::parse_error(position, "Syntax error");
        assert_eq!(parse_error.position(), Some(position));
        assert!(parse_error.is_recoverable());

        let transform_error = MarkdownError::transform_error("Transform failed");
        assert_eq!(transform_error.position(), None);
        assert!(!transform_error.is_recoverable());

        let generation_error = MarkdownError::generation_error("Generation failed");
        assert_eq!(generation_error.position(), None);
        assert!(!generation_error.is_recoverable());
    }

    #[test]
    fn test_error_conversions() {
        // Test IO error conversion
        let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "File not found");
        let markdown_error: MarkdownError = io_error.into();
        assert!(!markdown_error.is_recoverable());

        // Test UTF-8 error conversion with invalid continuation byte
        let mut invalid_utf8 = vec![0xC0, 0x80]; // Invalid UTF-8: overlong encoding
        invalid_utf8[0] = 0xFF; // Make it definitely invalid at runtime
        let utf8_error = std::str::from_utf8(&invalid_utf8).unwrap_err();
        let markdown_error: MarkdownError = utf8_error.into();
        assert!(!markdown_error.is_recoverable());
    }

    #[test]
    fn test_error_handler() {
        let mut handler = DefaultErrorHandler::new();
        let error_info = ErrorInfo::new(ErrorSeverity::Error, "Test error".to_string());

        handler.handle_error(&error_info);
        assert_eq!(handler.errors.len(), 1);
        assert!(!handler.has_fatal_errors());
    }

    #[test]
    fn test_error_severity() {
        let mut handler = DefaultErrorHandler::new();

        let warning = ErrorInfo::new(ErrorSeverity::Warning, "Warning".to_string());
        let error = ErrorInfo::new(ErrorSeverity::Error, "Error".to_string());
        let fatal = ErrorInfo::new(ErrorSeverity::Fatal, "Fatal".to_string());

        handler.handle_error(&warning);
        handler.handle_error(&error);
        handler.handle_error(&fatal);

        assert_eq!(handler.count_by_severity(ErrorSeverity::Warning), 1);
        assert_eq!(handler.count_by_severity(ErrorSeverity::Error), 1);
        assert_eq!(handler.count_by_severity(ErrorSeverity::Fatal), 1);
        assert!(handler.has_fatal_errors());
    }

    #[test]
    fn test_error_handler_max_errors() {
        let mut handler = DefaultErrorHandler::with_max_errors(2);
        let error_info = ErrorInfo::new(ErrorSeverity::Error, "Test error".to_string());

        handler.handle_error(&error_info);
        assert!(handler.should_continue(ErrorSeverity::Error));

        handler.handle_error(&error_info);
        assert!(!handler.should_continue(ErrorSeverity::Error));
    }
}
