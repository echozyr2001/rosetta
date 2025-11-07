/// Core Context-Generic Programming traits for the Markdown parser
///
/// This module defines the fundamental capabilities that decouple the parsing
/// pipeline following CGP principles. These traits represent "abilities" that
/// different contexts can provide.
use crate::error::{MarkdownError, Result};
use crate::lexer::Position;
use crate::parser::ParserConfig;

// --- Token-related capabilities ---
// Reuse existing TokenType trait
pub use crate::traits::token_traits::TokenType;

/// Provides token stream capability.
pub trait HasTokenProvider {
    type Token: TokenType;

    fn current_token(&self) -> Option<&Self::Token>;
    fn next_token(&mut self) -> Result<Self::Token>;
    fn peek_token(&self) -> Option<&Self::Token>; // Peek at next token without consuming it
    fn current_position(&self) -> Position; // Get position, preferably from current_token()
}

// --- AST/IR building capabilities ---

/// Provides Abstract Syntax Tree (AST) building capability.
pub trait AstBuilder {
    type Inline;
    type Block;
    type Document;

    fn build_document(&self, blocks: Vec<Self::Block>) -> Self::Document;
    fn build_paragraph(&self, content: Vec<Self::Inline>, pos: Option<Position>) -> Self::Block;
    fn build_heading(
        &self,
        level: u8,
        content: Vec<Self::Inline>,
        pos: Option<Position>,
    ) -> Self::Block;
    fn build_code_block(
        &self,
        info: Option<String>,
        content: String,
        language: Option<String>,
        pos: Option<Position>,
    ) -> Self::Block;
    fn build_blockquote(&self, content: Vec<Self::Block>, pos: Option<Position>) -> Self::Block;
    fn build_list(
        &self,
        kind: crate::ast::ListKind,
        tight: bool,
        items: Vec<crate::ast::ListItem>,
        pos: Option<Position>,
    ) -> Self::Block;
    fn build_thematic_break(&self, pos: Option<Position>) -> Self::Block;
    fn build_html_block(&self, content: String, pos: Option<Position>) -> Self::Block;

    // Inline builders
    fn build_text(&self, content: String) -> Self::Inline;
    fn build_emphasis(&self, strong: bool, content: Vec<Self::Inline>) -> Self::Inline;
    fn build_code(&self, content: String) -> Self::Inline;
    fn build_link(
        &self,
        text: Vec<Self::Inline>,
        destination: String,
        title: Option<String>,
    ) -> Self::Inline;
    fn build_image(&self, alt: String, destination: String, title: Option<String>) -> Self::Inline;
    fn build_html_inline(&self, content: String) -> Self::Inline;
    fn build_soft_break(&self) -> Self::Inline;
    fn build_hard_break(&self) -> Self::Inline;
}

/// Provides Intermediate Representation (IR) building capability.
/// This is for future analysis phases.
pub trait IrBuilder {
    // Define IR-related types and building methods
    // Currently left empty for future extension
}

// --- Diagnostic and environment capabilities ---

/// Provides error and warning reporting capability.
pub trait HasErrorHandler {
    fn report_error(&mut self, error: MarkdownError);
    fn report_warning(&mut self, warning: String, position: Option<Position>);
}

/// Provides source map and position lookup capability.
pub trait HasSourceMap {
    fn lookup_position(&self, position: Position) -> SourceLocation;
}

/// Provides configuration access capability.
pub trait HasConfig {
    fn get_config(&self) -> &ParserConfig;
}

/// Source location information
#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file_name: Option<String>,
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl From<Position> for SourceLocation {
    fn from(pos: Position) -> Self {
        Self {
            file_name: None,
            line: pos.line,
            column: pos.column,
            offset: pos.offset,
        }
    }
}
