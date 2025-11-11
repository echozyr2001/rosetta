use crate::error::Result;
use crate::lexer::Position;

/// Unified AST node abstraction used by the parser.
///
/// All concrete AST nodes (documents, blocks, and inlines) implement this trait,
/// keeping the parser generic over specific node implementations.
pub trait AstNode: Clone + std::fmt::Debug {
    fn position(&self) -> Option<Position>;
}

/// AST builder abstraction used by the parser to construct nodes
/// without depending on concrete implementations.
pub trait AstBuilder {
    type Inline: AstNode;
    type Block: AstNode;
    type Document: AstNode;

    // Document builders
    fn build_document(&self, blocks: Vec<Self::Block>) -> Self::Document;

    // Block builders
    fn build_heading(
        &self,
        level: u8,
        content: Vec<Self::Inline>,
        position: Option<Position>,
    ) -> Self::Block;

    fn build_paragraph(
        &self,
        content: Vec<Self::Inline>,
        position: Option<Position>,
    ) -> Self::Block;

    fn build_code_block(
        &self,
        info: Option<String>,
        content: String,
        language: Option<String>,
        position: Option<Position>,
    ) -> Self::Block;

    fn build_block_quote(
        &self,
        content: Vec<Self::Block>,
        position: Option<Position>,
    ) -> Self::Block;

    fn build_list(
        &self,
        kind: ListKind,
        tight: bool,
        items: Vec<ListItem<Self::Block>>,
        position: Option<Position>,
    ) -> Self::Block;

    fn build_thematic_break(&self, position: Option<Position>) -> Self::Block;

    fn build_html_block(&self, content: String, position: Option<Position>) -> Self::Block;

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

/// Generic list kind independent of concrete AST representation.
#[derive(Debug, Clone, PartialEq)]
pub enum ListKind {
    Bullet { marker: char },
    Ordered { start: u32, delimiter: char },
}

/// Generic list item structure independent of concrete AST representation.
#[derive(Debug, Clone)]
pub struct ListItem<Block> {
    pub content: Vec<Block>,
    pub tight: bool,
    pub task_list_marker: Option<bool>,
}

/// Trait representing a reusable parsing rule set.
///
/// Implementations can offer alternative parsing strategies that
/// consume token sequences and produce AST documents.
use super::state::ParserState;
use crate::traits::ParsingContext;

pub trait ParseRule<C>
where
    C: ParsingContext,
{
    fn parse(&mut self, state: &mut ParserState<'_, C>) -> Result<C::Document>;
}
