// Core modules
pub mod ast;
pub mod codegen;
pub mod dom;
pub mod error;
pub mod lexer;
pub mod parser;

// Re-export key types for public API
pub use ast::{Block, Document, Inline, Node, Visitable, Visitor};
pub use dom::DomNode;
pub use error::{MarkdownError, Result};
pub use lexer::{Lexer, Position, Token};

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
/// assert_eq!(html, "<h1>Hello, World!</h1>");
/// ```
pub fn to_html(markdown: &str) -> String {
    // 1. Parsing (includes lexical analysis internally)
    let root = parser::parse(markdown);

    // 2. AST to DOM conversion (Intermediate Representation)
    let dom = dom::from_ast(root);

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
pub fn parse_markdown(markdown: &str) -> Document {
    parser::parse(markdown)
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
/// Returns the root DomNode of the DOM tree
pub fn ast_to_dom(ast: Document) -> DomNode {
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
        let expected_html = "<h1>Hello, World!</h1>";
        assert_eq!(to_html(markdown), expected_html);
    }
}
