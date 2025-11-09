//! Lexer module organised around the component layering model.
//!
//! The module is split into focused submodules to make the pipeline clearer:
//! - `position`: purely tracks source offsets.
//! - `cursor`: stateful lexer that drives token emission using parsing rules.
//! - `rules`: nom-powered token parsers shared between adapters and the lexer.
//!
//! This keeps the boundary between pure parsing rules and stateful execution
//! explicit—`cursor` implements the concrete lexer, while adapters in
//! `crate::adapters` expose the trait-based components.

pub mod token;

mod cursor;
mod position;
mod rules;
mod traits;

pub use cursor::{Cursor, Lexer as GenericLexer, LexingRule, MarkdownRules};
pub type Lexer<'input> = cursor::Lexer<'input, token::Token<'input>, MarkdownRules>;
pub use position::Position;
pub use traits::LexToken;
