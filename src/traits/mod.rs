/// Context-Generic Programming traits module
///
/// This module provides trait-based abstractions that decouple the lexer,
/// parser, and AST implementations following CGP (Context-Generic Programming)
/// principles.
pub mod ast_traits;
pub mod cgp_traits;
pub mod context_traits;
pub mod token_traits;

// Re-export commonly used traits
pub use ast_traits::{
    AstBuilder, AstTransformer, AstValidator, BlockNode, DocumentNode, HasAstBuilder, InlineNode,
    ListItem, ListKind,
};
pub use context_traits::{HasConfig, HasErrorHandler, ParsingContext};
pub use token_traits::{
    AtxHeadingData, CodeBlockData, HasTokenProvider, ListMarkerData, SetextHeadingData, TextData,
    TokenAdvancer, TokenDataExtractor, TokenInspector, TokenType,
};

// Re-export new CGP traits
pub use cgp_traits::{
    AstBuilder as CgpAstBuilder, HasConfig as CgpHasConfig, HasErrorHandler as CgpHasErrorHandler,
    HasSourceMap, HasTokenProvider as CgpHasTokenProvider, IrBuilder, SourceLocation,
};
