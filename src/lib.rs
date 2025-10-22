// Core modules
pub mod ast;
pub mod codegen;
pub mod dom;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod performance;

// Re-export key types for public API
pub use ast::{Block, Document, Inline, Node, Visitable, Visitor};
pub use codegen::{
    CustomRenderer, HtmlGenerator, HtmlValidator, HtmlWriter, OutputConfig, OutputConfigBuilder,
};
pub use dom::DomNode;
pub use error::{MarkdownError, Result};
pub use lexer::{Lexer, Position, Token};
pub use parser::ParserConfig;
pub use performance::{ParallelConfig, PerformanceOptimizer, ZeroCopyStr};

// Public API configuration structs
pub use config::{DomConfig, EngineConfig, GenerationConfig, LexerConfig, MarkdownEngine};

// Advanced API exports
pub use streaming::{ChunkResult, StreamingConfig, StreamingParser};
// pub use extensions::{SyntaxExtension, ExtensionRegistry, ExtensionManager};

// Configuration module for public API
pub mod config;

// Advanced API modules
pub mod extensions;
pub mod streaming;

/// Simple function to parse Markdown text and return HTML.
///
/// This is the simplest entry point for the Markdown engine, providing
/// a one-function interface for basic Markdown to HTML conversion.
/// Uses default configuration for all components.
///
/// # Arguments
///
/// * `markdown` - The input Markdown text to convert
///
/// # Returns
///
/// Returns the generated HTML as a String
///
/// # Examples
///
/// ```
/// use rosetta::parse_markdown;
///
/// let html = parse_markdown("# Hello, World!");
/// assert!(html.contains("Hello, World!"));
/// ```
pub fn parse_markdown(markdown: &str) -> String {
    let engine = config::MarkdownEngine::new();
    engine.parse_to_html(markdown).unwrap_or_else(|_| {
        // Fallback to empty content on error
        String::new()
    })
}

/// Converts a Markdown string to an HTML string.
///
/// This is the main entry point for the Markdown engine. It orchestrates
/// the complete compilation pipeline:
/// 1. **Lexical Analysis:** Tokenize the input Markdown text
/// 2. **Parsing:** Parse tokens into an Abstract Syntax Tree (AST)
/// 3. **Intermediate Representation:** Convert AST to Document Object Model (DOM)
/// 4. **Code Generation:** Generate HTML from the DOM
///
/// # Arguments
///
/// * `markdown` - The input Markdown text to convert
///
/// # Returns
///
/// Returns the generated HTML as a String
///
/// # Examples
///
/// ```
/// use rosetta::to_html;
///
/// let markdown = "# Hello, World!";
/// let html = to_html(markdown);
/// assert_eq!(html, "<div class=\"markdown-content\"><h1 class=\"heading\">Hello, World!</h1>\n</div>\n");
/// ```
pub fn to_html(markdown: &str) -> String {
    // 1. Parsing (includes lexical analysis internally)
    let root = parser::parse(markdown).unwrap_or_else(|_| ast::Document {
        blocks: Vec::new(),
        source_map: ast::SourceMap::new(),
    });

    // 2. AST to DOM conversion (Intermediate Representation)
    let dom = dom::from_ast(root).unwrap_or_else(|_| {
        // Fallback to empty div on transformation error
        dom::DomNode::new("div")
    });

    // 3. Target Code Generation
    codegen::generate_html(dom)
}

/// Parse Markdown text into an Abstract Syntax Tree.
///
/// This function provides direct access to the AST representation
/// for applications that need to work with the parsed structure.
///
/// # Arguments
///
/// * `markdown` - The input Markdown text to parse
///
/// # Returns
///
/// Returns the root Document of the AST
pub fn parse_to_ast(markdown: &str) -> Document {
    parser::parse(markdown).unwrap_or_else(|_| ast::Document {
        blocks: Vec::new(),
        source_map: ast::SourceMap::new(),
    })
}

/// Convert an AST to a DOM representation.
///
/// This function provides access to the DOM transformation step
/// for applications that need the intermediate representation.
///
/// # Arguments
///
/// * `ast` - The AST root document to convert
///
/// # Returns
///
/// Returns the root DomNode of the DOM tree, or an error if transformation fails
pub fn ast_to_dom(ast: Document) -> Result<DomNode> {
    dom::from_ast(ast)
}

/// Generate HTML from a DOM representation.
///
/// This function provides direct access to the HTML generation step
/// for applications working with DOM representations.
///
/// # Arguments
///
/// * `dom` - The DOM root node to convert to HTML
///
/// # Returns
///
/// Returns the generated HTML as a String
pub fn dom_to_html(dom: DomNode) -> String {
    codegen::generate_html(dom)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let markdown = "# Hello, World!";
        let expected_html =
            "<div class=\"markdown-content\"><h1 class=\"heading\">Hello, World!</h1>\n</div>\n";
        assert_eq!(to_html(markdown), expected_html);
    }

    #[test]
    fn test_parse_markdown_function() {
        let markdown = "# Hello, World!";
        let html = parse_markdown(markdown);
        assert!(html.contains("Hello, World!"));
    }

    #[test]
    fn test_parse_to_ast_function() {
        let markdown = "# Hello, World!";
        let document = parse_to_ast(markdown);
        assert_eq!(document.blocks.len(), 1);
        assert!(matches!(document.blocks[0], ast::Block::Heading { .. }));
    }
}
