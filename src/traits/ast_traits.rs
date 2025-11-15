use super::component::{DelegateComponent, HasProvider, IsProviderFor};
/// Component-based traits for AST layer decoupling
///
/// This module defines builder and visitor traits that decouple the parser
/// from specific AST implementations while using component abstractions.
use crate::error::Result;
pub use crate::parser::traits::{AstBuilder, AstNode, ListItem, ListKind};

/// Component marker for AST construction.
pub struct AstBuilderComponent;

/// Consumer trait for contexts capable of building AST nodes.
pub trait CanBuildAst: HasProvider {
    type Inline: AstNode;
    type Block: AstNode;
    type Document: AstNode;

    fn ast_builder(
        &self,
    ) -> &dyn AstBuilder<Inline = Self::Inline, Block = Self::Block, Document = Self::Document>;
}

/// Provider trait implemented by component registries that offer AST building
/// capabilities.
pub trait AstBuilderProvider<Context>: IsProviderFor<AstBuilderComponent, Context> {
    type Inline: AstNode;
    type Block: AstNode;
    type Document: AstNode;

    #[allow(clippy::needless_lifetimes)]
    fn ast_builder<'a>(
        context: &'a Context,
    ) -> &'a dyn AstBuilder<Inline = Self::Inline, Block = Self::Block, Document = Self::Document>;
}

impl<Context, Component> AstBuilderProvider<Context> for Component
where
    Context: Sized,
    Component: DelegateComponent<AstBuilderComponent> + IsProviderFor<AstBuilderComponent, Context>,
    Component::Delegate: AstBuilderProvider<Context>,
{
    type Inline = <Component::Delegate as AstBuilderProvider<Context>>::Inline;
    type Block = <Component::Delegate as AstBuilderProvider<Context>>::Block;
    type Document = <Component::Delegate as AstBuilderProvider<Context>>::Document;

    #[allow(clippy::needless_lifetimes)]
    fn ast_builder<'a>(
        context: &'a Context,
    ) -> &'a dyn AstBuilder<Inline = Self::Inline, Block = Self::Block, Document = Self::Document>
    {
        <Component::Delegate as AstBuilderProvider<Context>>::ast_builder(context)
    }
}

impl<Context> CanBuildAst for Context
where
    Context: HasProvider,
    Context::Components: AstBuilderProvider<Context>,
{
    type Inline = <Context::Components as AstBuilderProvider<Context>>::Inline;
    type Block = <Context::Components as AstBuilderProvider<Context>>::Block;
    type Document = <Context::Components as AstBuilderProvider<Context>>::Document;

    fn ast_builder(
        &self,
    ) -> &dyn AstBuilder<Inline = Self::Inline, Block = Self::Block, Document = Self::Document>
    {
        <Context::Components as AstBuilderProvider<Context>>::ast_builder(self)
    }
}

/// Trait for contexts that support AST transformation.
/// This allows post-processing of AST nodes generically.
pub trait AstTransformer<Context>
where
    Context: CanBuildAst,
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
    Context: CanBuildAst,
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
