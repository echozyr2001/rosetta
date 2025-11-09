use crate::error::Result;
use crate::lexer::Position;

/// Trait representing an inline node within the parser.
/// Concrete inline types implement this so the parser can stay generic.
pub trait InlineNode: Clone + std::fmt::Debug {
    fn position(&self) -> Option<Position>;
}

/// Trait representing a block-level node within the parser.
pub trait BlockNode: Clone + std::fmt::Debug {
    fn position(&self) -> Option<Position>;
}

/// Trait representing the root document node within the parser.
pub trait DocumentNode: Clone + std::fmt::Debug {
    type Block: BlockNode;

    fn blocks(&self) -> &[Self::Block];
}

/// AST builder abstraction used by the parser to construct nodes
/// without depending on concrete implementations.
pub trait AstBuilder {
    type Inline: InlineNode;
    type Block: BlockNode;
    type Document: DocumentNode<Block = Self::Block>;

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

pub trait ParseRule<Tok, Document>
where
    Tok: crate::lexer::LexToken,
    Document: DocumentNode,
{
    fn parse(&mut self, state: &mut ParserState<Tok>) -> Result<Document>;
}
