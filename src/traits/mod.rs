/// Component-based traits module
///
/// This module provides trait-based abstractions that decouple the lexer,
/// parser, and AST implementations using shared component principles.
pub mod ast_traits;
pub mod component;
pub mod context_traits;
pub mod support_traits;
pub mod token_traits;

// Re-export core component infrastructure
pub use component::{DelegateComponent, HasProvider, IsProviderFor};

// Re-export token layer traits
pub use token_traits::{CanProvideTokens, TokenProvider, TokenProviderComponent};

// Re-export AST layer traits
pub use ast_traits::{
    AstBuilder, AstBuilderComponent, AstBuilderProvider, AstTransformer, AstValidator, BlockNode,
    CanBuildAst, DocumentNode, InlineNode, ListItem, ListKind,
};

// Re-export context coordination traits
pub use context_traits::{
    CanAccessParserConfig, CanHandleErrors, ErrorHandlerComponent, ErrorHandlerProvider,
    ParserConfigComponent, ParserConfigProvider, ParsingContext,
};

// Re-export miscellaneous helpers
pub use support_traits::{HasSourceMap, IrBuilder, SourceLocation};
