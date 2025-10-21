use crate::ast;
use std::collections::HashMap;

/// Represents a node in the Document Object Model (DOM).
/// This is a generic, intermediate representation that decouples the AST
/// from the final output format (e.g., HTML, XML).
#[derive(Debug, PartialEq)]
pub struct DomNode {
    pub tag_name: String,
    pub attributes: HashMap<String, String>,
    pub children: Vec<DomNode>,
    pub text: Option<String>,
}

impl DomNode {
    /// Creates a new DOM node with a given tag name (e.g., "h1", "p").
    pub fn new(tag_name: &str) -> Self {
        DomNode {
            tag_name: tag_name.to_string(),
            attributes: HashMap::new(),
            children: Vec::new(),
            text: None,
        }
    }
}

/// Converts an Abstract Syntax Tree (AST) into a Document Object Model (DOM).
///
/// This function will eventually walk the AST and convert each node into its
/// corresponding DOM representation.
///
/// For now, it returns a fixed, hardcoded DOM for demonstration purposes.
pub fn from_ast(_ast_root: ast::Document) -> DomNode {
    // Placeholder: A real implementation will convert the `_ast_root`.
    // This dummy DOM corresponds to the AST for: `# Hello, World!`
    let mut dom_root = DomNode::new("div"); // A virtual root for the DOM

    let mut h1 = DomNode::new("h1");

    let mut text_node = DomNode::new("#text"); // Using #text convention for text nodes
    text_node.text = Some("Hello, World!".to_string());

    h1.children.push(text_node);
    dom_root.children.push(h1);

    dom_root
}
