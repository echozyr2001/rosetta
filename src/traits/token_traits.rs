/// Context-Generic Programming traits for Token layer decoupling
///
/// This module defines provider and consumer traits that decouple the lexer,
/// parser, and token implementations following CGP principles.
use crate::error::Result;
use crate::lexer::Position;

/// Trait representing a token in the parsing pipeline.
/// This is a marker trait that allows different token implementations
/// to be used interchangeably through trait objects or generics.
pub trait TokenType: Clone + std::fmt::Debug {
    /// Returns the position of this token in the source
    fn position(&self) -> Option<Position>;

    /// Returns true if this token represents end-of-file
    fn is_eof(&self) -> bool;

    /// Returns true if this token represents a newline
    fn is_newline(&self) -> bool;

    /// Returns true if this token represents whitespace
    fn is_whitespace(&self) -> bool;

    /// Returns true if this token represents indentation
    fn is_indent(&self) -> bool;
}

/// Provider trait for contexts that can supply tokens.
/// This decouples the parser from the specific lexer implementation.
pub trait HasTokenProvider {
    type Token: TokenType;

    /// Get the current token without consuming it
    fn current_token(&self) -> Option<&Self::Token>;

    /// Advance to the next token and return it
    fn next_token(&mut self) -> Result<Self::Token>;

    /// Peek at the next token without consuming it
    fn peek_token(&mut self) -> Result<Self::Token>;

    /// Get the current position in the input
    fn current_position(&self) -> Position;
}

/// Consumer trait for contexts that need to inspect tokens.
/// This allows the parser to query token properties generically.
pub trait TokenInspector<Context>
where
    Context: HasTokenProvider,
{
    /// Check if the current token is an ATX heading
    fn is_atx_heading(context: &Context) -> bool;

    /// Check if the current token is a setext heading
    fn is_setext_heading(context: &Context) -> bool;

    /// Check if the current token is a code block
    fn is_code_block(context: &Context) -> bool;

    /// Check if the current token is a block quote
    fn is_block_quote(context: &Context) -> bool;

    /// Check if the current token is a list marker
    fn is_list_marker(context: &Context) -> bool;

    /// Check if the current token is a thematic break
    fn is_thematic_break(context: &Context) -> bool;

    /// Check if the current token is text
    fn is_text(context: &Context) -> bool;
}

/// Trait for extracting data from ATX heading tokens
pub trait AtxHeadingData {
    fn level(&self) -> u8;
    fn content(&self) -> &str;
}

/// Trait for extracting data from setext heading tokens
pub trait SetextHeadingData {
    fn level(&self) -> u8;
    fn content(&self) -> &str;
}

/// Trait for extracting data from code block tokens
pub trait CodeBlockData {
    fn info(&self) -> Option<&str>;
    fn content(&self) -> &str;
    fn is_fenced(&self) -> bool;
}

/// Trait for extracting data from list marker tokens
pub trait ListMarkerData {
    fn is_ordered(&self) -> bool;
    fn marker_char(&self) -> char;
    fn start_number(&self) -> Option<u32>;
    fn indent(&self) -> usize;
}

/// Trait for extracting data from text tokens
pub trait TextData {
    fn content(&self) -> &str;
}

/// Trait for contexts that support token data extraction.
/// This allows the parser to extract structured data from tokens generically.
pub trait TokenDataExtractor<Context>
where
    Context: HasTokenProvider,
{
    /// Extract ATX heading data from the current token
    fn extract_atx_heading(context: &Context) -> Option<&dyn AtxHeadingData>;

    /// Extract setext heading data from the current token
    fn extract_setext_heading(context: &Context) -> Option<&dyn SetextHeadingData>;

    /// Extract code block data from the current token
    fn extract_code_block(context: &Context) -> Option<&dyn CodeBlockData>;

    /// Extract list marker data from the current token
    fn extract_list_marker(context: &Context) -> Option<&dyn ListMarkerData>;

    /// Extract text data from the current token
    fn extract_text(context: &Context) -> Option<&dyn TextData>;
}

/// Trait for advancing through tokens with additional semantics
pub trait TokenAdvancer<Context>
where
    Context: HasTokenProvider,
{
    /// Skip all whitespace and newline tokens
    fn skip_whitespace(context: &mut Context) -> Result<()>;

    /// Skip until a block boundary is found
    fn skip_to_block_boundary(context: &mut Context) -> Result<()>;

    /// Consume the current token if it matches the predicate
    fn consume_if<F>(context: &mut Context, predicate: F) -> Result<bool>
    where
        F: FnOnce(&Context::Token) -> bool;
}
