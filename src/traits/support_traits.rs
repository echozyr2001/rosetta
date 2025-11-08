/// Core component helpers that do not belong to a specific module.
use crate::lexer::Position;

/// Provides Intermediate Representation (IR) building capability.
/// This is for future analysis phases.
pub trait IrBuilder {
    // Define IR-related types and building methods
    // Currently left empty for future extension
}

/// Provides source map and position lookup capability.
pub trait HasSourceMap {
    fn lookup_position(&self, position: Position) -> SourceLocation;
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
