/// Context-Generic Programming traits for AST layer decoupling
///
/// This module defines builder and visitor traits that decouple the parser
/// from specific AST implementations following CGP principles.
use crate::error::Result;
use crate::lexer::Position;

/// Trait representing an inline element in the AST.
/// This allows different inline implementations to be used.
pub trait InlineNode: Clone + std::fmt::Debug {
    /// Get the position of this inline node
    fn position(&self) -> Option<Position>;
}

/// Trait representing a block element in the AST.
/// This allows different block implementations to be used.
pub trait BlockNode: Clone + std::fmt::Debug {
    /// Get the position of this block node
    fn position(&self) -> Option<Position>;
}

/// Trait representing a document in the AST.
/// This allows different document implementations to be used.
pub trait DocumentNode: Clone + std::fmt::Debug {
    type Block: BlockNode;

    /// Get all blocks in the document
    fn blocks(&self) -> &[Self::Block];
}

/// Provider trait for contexts that can build AST nodes.
/// This decouples the parser from the specific AST implementation.
pub trait HasAstBuilder {
    type Inline: InlineNode;
    type Block: BlockNode;
    type Document: DocumentNode<Block = Self::Block>;

    /// Get a reference to the AST builder
    fn ast_builder(
        &self,
    ) -> &dyn AstBuilder<Inline = Self::Inline, Block = Self::Block, Document = Self::Document>;
}

/// Builder trait for constructing AST nodes.
/// This allows the parser to build AST nodes generically.
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

/// List kind enumeration that's independent of specific AST implementations
#[derive(Debug, Clone, PartialEq)]
pub enum ListKind {
    Bullet { marker: char },
    Ordered { start: u32, delimiter: char },
}

/// List item structure that's independent of specific AST implementations
#[derive(Debug, Clone)]
pub struct ListItem<Block> {
    pub content: Vec<Block>,
    pub tight: bool,
    pub task_list_marker: Option<bool>,
}

/// Trait for contexts that support AST transformation.
/// This allows post-processing of AST nodes generically.
pub trait AstTransformer<Context>
where
    Context: HasAstBuilder,
{
    /// Transform a block node
    fn transform_block(context: &Context, block: Context::Block) -> Result<Context::Block>;

    /// Transform an inline node
    fn transform_inline(context: &Context, inline: Context::Inline) -> Result<Context::Inline>;

    /// Transform a document
    fn transform_document(
        context: &Context,
        document: Context::Document,
    ) -> Result<Context::Document>;
}

/// Trait for contexts that support AST validation.
/// This allows validation of AST nodes generically.
pub trait AstValidator<Context>
where
    Context: HasAstBuilder,
{
    /// Validate a block node
    fn validate_block(context: &Context, block: &Context::Block) -> Result<()>;

    /// Validate an inline node
    fn validate_inline(context: &Context, inline: &Context::Inline) -> Result<()>;

    /// Validate a document
    fn validate_document(context: &Context, document: &Context::Document) -> Result<()>;
}

/// Default implementation for list items
impl<Block> ListItem<Block> {
    pub fn new(content: Vec<Block>, tight: bool) -> Self {
        Self {
            content,
            tight,
            task_list_marker: None,
        }
    }

    pub fn with_task_marker(mut self, checked: bool) -> Self {
        self.task_list_marker = Some(checked);
        self
    }
}
