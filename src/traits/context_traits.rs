/// Context traits that tie together different capabilities
///
/// This module defines the core context traits that combine token provision,
/// AST building, and other capabilities using component abstractions.
use crate::error::{ErrorHandler, Result};

use super::component::{DelegateComponent, HasProvider, IsProviderFor};

/// Component marker for parser configuration access.
pub struct ParserConfigComponent;

/// Consumer trait for accessing parser configuration.
pub trait CanAccessParserConfig: HasProvider {
    type Config;

    fn parser_config(&self) -> &Self::Config;
}

/// Provider trait for parser configuration access.
pub trait ParserConfigProvider<Context>: IsProviderFor<ParserConfigComponent, Context> {
    type Config;

    #[allow(clippy::needless_lifetimes)]
    fn parser_config<'a>(context: &'a Context) -> &'a Self::Config;
}

impl<Context, Component> ParserConfigProvider<Context> for Component
where
    Context: Sized,
    Component:
        DelegateComponent<ParserConfigComponent> + IsProviderFor<ParserConfigComponent, Context>,
    Component::Delegate: ParserConfigProvider<Context>,
{
    type Config = <Component::Delegate as ParserConfigProvider<Context>>::Config;

    #[allow(clippy::needless_lifetimes)]
    fn parser_config<'a>(context: &'a Context) -> &'a Self::Config {
        <Component::Delegate as ParserConfigProvider<Context>>::parser_config(context)
    }
}

impl<Context> CanAccessParserConfig for Context
where
    Context: HasProvider,
    Context::Components: ParserConfigProvider<Context>,
{
    type Config = <Context::Components as ParserConfigProvider<Context>>::Config;

    fn parser_config(&self) -> &Self::Config {
        <Context::Components as ParserConfigProvider<Context>>::parser_config(self)
    }
}

/// Component marker for error handling access.
pub struct ErrorHandlerComponent;

/// Consumer trait for error handling access.
pub trait CanHandleErrors: HasProvider {
    fn error_handler(&self) -> &dyn ErrorHandler;
    fn error_handler_mut(&mut self) -> &mut dyn ErrorHandler;
}

/// Provider trait for error handling access.
pub trait ErrorHandlerProvider<Context>: IsProviderFor<ErrorHandlerComponent, Context> {
    #[allow(clippy::needless_lifetimes)]
    fn error_handler<'a>(context: &'a Context) -> &'a dyn ErrorHandler;
    #[allow(clippy::needless_lifetimes)]
    fn error_handler_mut<'a>(context: &'a mut Context) -> &'a mut dyn ErrorHandler;
}

impl<Context, Component> ErrorHandlerProvider<Context> for Component
where
    Context: Sized,
    Component:
        DelegateComponent<ErrorHandlerComponent> + IsProviderFor<ErrorHandlerComponent, Context>,
    Component::Delegate: ErrorHandlerProvider<Context>,
{
    #[allow(clippy::needless_lifetimes)]
    fn error_handler<'a>(context: &'a Context) -> &'a dyn ErrorHandler {
        <Component::Delegate as ErrorHandlerProvider<Context>>::error_handler(context)
    }

    #[allow(clippy::needless_lifetimes)]
    fn error_handler_mut<'a>(context: &'a mut Context) -> &'a mut dyn ErrorHandler {
        <Component::Delegate as ErrorHandlerProvider<Context>>::error_handler_mut(context)
    }
}

impl<Context> CanHandleErrors for Context
where
    Context: HasProvider,
    Context::Components: ErrorHandlerProvider<Context>,
{
    fn error_handler(&self) -> &dyn ErrorHandler {
        <Context::Components as ErrorHandlerProvider<Context>>::error_handler(self)
    }

    fn error_handler_mut(&mut self) -> &mut dyn ErrorHandler {
        <Context::Components as ErrorHandlerProvider<Context>>::error_handler_mut(self)
    }
}

/// Main parsing context trait that combines all capabilities
pub trait ParsingContext:
    super::CanProvideTokens + super::CanBuildAst + CanAccessParserConfig + CanHandleErrors
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
