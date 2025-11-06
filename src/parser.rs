// Parser module with submodules for better organization using Rust 2018 module system
mod config;
mod core;
mod utils;

#[cfg(test)]
mod tests;

// Re-export main types and functions
pub use config::ParserConfig;
pub use core::Parser;
pub use utils::ParagraphBuilder;
// NomParser is now the main Parser

use crate::ast::Document;
use crate::error::{ErrorHandler, Result};

/// Parse Markdown using the default parser configuration.
pub fn parse(markdown: &str) -> Result<Document> {
    Parser::with_defaults(markdown).parse()
}

/// Parse function with custom configuration.
pub fn parse_with_config(markdown: &str, config: ParserConfig) -> Result<Document> {
    let parser = Parser::new(markdown, config);
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
