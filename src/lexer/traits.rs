use super::position::Position;

/// Marker trait implemented by tokens produced by the lexer.
///
/// This trait stays within the lexer module so that core lexical concepts
/// remain decoupled from the component-layer abstractions.
pub trait LexToken: Clone + std::fmt::Debug {
    /// Returns the position of this token in the source.
    fn position(&self) -> Option<Position>;

    /// Returns true if this token represents end-of-file.
    fn is_eof(&self) -> bool;

    /// Returns true if this token represents a newline.
    fn is_newline(&self) -> bool;

    /// Returns true if this token represents whitespace.
    fn is_whitespace(&self) -> bool;

    /// Returns true if this token represents indentation.
    fn is_indent(&self) -> bool;
}
