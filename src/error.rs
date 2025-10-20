/// Error handling module for the Markdown engine.
///
/// This module defines error types and utilities for comprehensive error reporting
/// throughout the parsing and generation pipeline.
use crate::lexer::Position;
use std::fmt;

/// Main error type for the Markdown engine.
#[derive(Debug)]
pub enum MarkdownError {
    /// Lexical analysis errors (tokenization phase).
    Lex { position: Position, message: String },

    /// Parsing errors (AST construction phase).
    Parse { position: Position, message: String },

    /// DOM transformation errors.
    Transform { message: String },

    /// HTML generation errors.
    Generation { message: String },

    /// I/O related errors.
    Io { source: std::io::Error },
}

impl fmt::Display for MarkdownError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MarkdownError::Lex { position, message } => {
                write!(
                    f,
                    "Lexical error at line {}, column {}: {}",
                    position.line, position.column, message
                )
            }
            MarkdownError::Parse { position, message } => {
                write!(
                    f,
                    "Parse error at line {}, column {}: {}",
                    position.line, position.column, message
                )
            }
            MarkdownError::Transform { message } => {
                write!(f, "Transform error: {}", message)
            }
            MarkdownError::Generation { message } => {
                write!(f, "Generation error: {}", message)
            }
            MarkdownError::Io { source } => {
                write!(f, "IO error: {}", source)
            }
        }
    }
}

impl std::error::Error for MarkdownError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            MarkdownError::Io { source } => Some(source),
            _ => None,
        }
    }
}

impl From<std::io::Error> for MarkdownError {
    fn from(error: std::io::Error) -> Self {
        MarkdownError::Io { source: error }
    }
}

/// Convenience type alias for Results in the Markdown engine.
pub type Result<T> = std::result::Result<T, MarkdownError>;

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
}
