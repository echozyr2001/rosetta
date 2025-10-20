use crate::ast::{Node, NodeType};

/// Parses a Markdown string into an Abstract Syntax Tree (AST).
///
/// This function will eventually contain the logic for tokenizing the input
/// and constructing the AST according to CommonMark rules.
///
/// For now, it returns a fixed, hardcoded AST for demonstration purposes.
pub fn parse(_markdown: &str) -> Node {
    // Placeholder: A real implementation will parse the `_markdown` input.
    // This dummy AST corresponds to: `# Hello, World!`
    let mut root = Node::new(NodeType::Document);

    let mut heading = Node::new(NodeType::Heading);
    heading.level = Some(1);

    let mut text = Node::new(NodeType::Text);
    text.text = Some("Hello, World!".to_string());

    heading.children.push(text);
    root.children.push(heading);

    root
}
