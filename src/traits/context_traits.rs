/// Context traits that tie together different capabilities
///
/// This module defines the core context traits that combine token provision,
/// AST building, and other capabilities following CGP principles.
use crate::error::{ErrorHandler, Result};

/// Trait for contexts that provide parser configuration
pub trait HasConfig {
    type Config;

    /// Get a reference to the configuration
    fn config(&self) -> &Self::Config;
}

/// Trait for contexts that provide error handling
pub trait HasErrorHandler {
    /// Get a reference to the error handler
    fn error_handler(&self) -> &dyn ErrorHandler;

    /// Get a mutable reference to the error handler
    fn error_handler_mut(&mut self) -> &mut dyn ErrorHandler;
}

/// Main parsing context trait that combines all capabilities
pub trait ParsingContext:
    super::HasTokenProvider + super::HasAstBuilder + HasConfig + HasErrorHandler
{
    /// Advance to the next token in the stream
    fn advance(&mut self) -> Result<()>;

    /// Check if we've reached the end of input
    fn is_at_end(&self) -> bool;

    /// Get the current nesting depth
    fn nesting_depth(&self) -> usize;

    /// Increment the nesting depth
    fn increment_nesting(&mut self) -> Result<()>;

    /// Decrement the nesting depth
    fn decrement_nesting(&mut self);

    /// Mark the current position as a good recovery point
    fn mark_good_position(&mut self);

    /// Attempt error recovery
    fn attempt_recovery(&mut self);
}

/// Trait for creating a parsing context from components
pub trait ContextBuilder {
    type Context: ParsingContext;

    /// Build a context from its components
    fn build(self) -> Self::Context;
}
