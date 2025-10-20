/// Represents a node in the Abstract Syntax Tree (AST).
/// The AST is a direct representation of the Markdown document structure.
#[derive(Debug, PartialEq)]
pub struct Node {
    pub node_type: NodeType,
    pub children: Vec<Node>,
    pub text: Option<String>,
    // Properties specific to certain node types, e.g., heading level.
    pub level: Option<usize>,
}

/// The type of a node in the AST, corresponding to CommonMark elements.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NodeType {
    Document,
    Heading,
    Paragraph,
    Text,
    // TODO: Add other CommonMark node types (e.g., BlockQuote, List, Image).
}

impl Node {
    /// Creates a new AST node of a given type.
    pub fn new(node_type: NodeType) -> Self {
        Node {
            node_type,
            children: Vec::new(),
            text: None,
            level: None,
        }
    }
}
