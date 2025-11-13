//! Streaming Markdown parser built on top of the lexer tokens.
//!
//! The parser is split into small components:
//! - `ast`: data structures representing documents, blocks, and inlines.
//! - `core`: streaming parser driver that consumes tokens via `Parser`.
//! - `rules`: token-level parsing rules backed by `nom`.
//! - `token_buffer` / `token_slice`: lightweight helpers for buffering tokens.
//! - `traits`: shared parser traits used by the rule implementations.

pub mod ast;
pub mod core;
pub mod rules;
mod token_buffer;
pub mod token_slice;
pub mod traits;

pub use core::{ParseAttempt, Parser, ParserError};
pub use rules::MarkdownParseRule;

pub use ast::{Block, Document, Inline, ListItem, ListKind};
pub use traits::{AstNode, ParseRule};
#[cfg(test)]
mod tests;
