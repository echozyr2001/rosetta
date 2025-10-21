use crate::ast::{Block, Document, Inline, SourceMap};

/// Parses a Markdown string into an Abstract Syntax Tree (AST).
///
/// This function will eventually contain the logic for tokenizing the input
/// and constructing the AST according to CommonMark rules.
///
/// For now, it returns a fixed, hardcoded AST for demonstration purposes.
pub fn parse(_markdown: &str) -> Document {
    // Placeholder: A real implementation will parse the `_markdown` input.
    // This dummy AST corresponds to: `# Hello, World!`
    let heading = Block::Heading {
        level: 1,
        content: vec![Inline::Text("Hello, World!".to_string())],
        id: None,
        position: None,
    };

    Document {
        blocks: vec![heading],
        source_map: SourceMap::new(),
    }
}
