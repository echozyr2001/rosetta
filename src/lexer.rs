//! Lexer module following the CGP layering model.
//!
//! The module is split into focused submodules to make the pipeline clearer:
//! - `position`: purely tracks source offsets.
//! - `cursor`: stateful lexer that drives token emission using parsing rules.
//! - `rules`: nom-powered token parsers shared between CGP adapters and the lexer.
//!
//! This keeps the CGP boundary explicit—`cursor` implements the concrete lexer,
//! while adapters in `crate::adapters` expose the CGP-friendly traits.

pub mod token;

mod cursor;
mod position;
mod rules;
mod token_impl;

pub use cursor::Lexer;
pub use position::Position;
