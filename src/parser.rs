// Parser module with submodules for better organization using Rust 2018 module system
mod config;
mod core;
mod token_parser;
mod utils;

pub mod token_pipeline;

#[cfg(test)]
mod tests;

// Re-export main types and functions
pub use config::ParserConfig;
pub use core::Parser;
pub use utils::ParagraphBuilder;

use crate::ast::Document;
use crate::error::{ErrorHandler, Result};

/// Parse Markdown using the component-driven context approach.
///
/// This function creates a `DefaultParsingContext` (including running `Lexer` to generate
/// all tokens), then uses nom combinators to parse the Token sequence.
/// This represents the token-driven parsing paradigm built from swappable components.
pub fn parse(markdown: &str) -> Result<Document> {
    Parser::with_defaults(markdown).parse()
}

/// Parse function with custom configuration.
pub fn parse_with_config(markdown: &str, config: ParserConfig) -> Result<Document> {
    let parser = Parser::new(markdown, config)?;
    parser.parse()
}

/// Parse function with custom error handler.
pub fn parse_with_error_handler(
    markdown: &str,
    config: ParserConfig,
    error_handler: Box<dyn ErrorHandler>,
) -> Result<Document> {
    let parser = Parser::with_error_handler(markdown, config, error_handler);
    parser.parse()
}
