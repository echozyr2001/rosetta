use crate::lexer::Position;
use std::collections::HashMap;

/// Base trait for all AST nodes providing basic node functionality
pub trait Node {
    /// Get source position if available
    fn source_position(&self) -> Option<Position>;
}

/// Trait for visitor pattern support - separate from Node to maintain dyn compatibility
pub trait Visitable {
    /// Accept a visitor for traversal
    fn accept<V: Visitor>(&self, visitor: &mut V);
}

/// Visitor trait for AST traversal
pub trait Visitor {
    fn visit_document(&mut self, document: &Document);
    fn visit_block(&mut self, block: &Block);
    fn visit_inline(&mut self, inline: &Inline);
}

/// Root document node containing the entire parsed Markdown document
#[derive(Debug, Clone)]
pub struct Document {
    pub blocks: Vec<Block>,
    pub source_map: SourceMap,
}

/// Block-level elements as defined by CommonMark specification
#[derive(Debug, Clone)]
pub enum Block {
    /// ATX or Setext heading with level (1-6) and inline content
    Heading { 
        level: u8, 
        content: Vec<Inline>, 
        id: Option<String>,
        position: Option<Position>,
    },
    /// Paragraph containing inline elements
    Paragraph { 
        content: Vec<Inline>,
        position: Option<Position>,
    },
    /// Fenced or indented code block
    CodeBlock { 
        info: Option<String>, 
        content: String, 
        language: Option<String>,
        position: Option<Position>,
    },
    /// Blockquote containing nested blocks
    BlockQuote { 
        content: Vec<Block>,
        position: Option<Position>,
    },
    /// Ordered or unordered list
    List { 
        kind: ListKind, 
        tight: bool, 
        items: Vec<ListItem>,
        position: Option<Position>,
    },
    /// Thematic break (horizontal rule)
    ThematicBreak {
        position: Option<Position>,
    },
    /// Raw HTML block
    HtmlBlock { 
        content: String,
        position: Option<Position>,
    },
}

/// Inline elements as defined by CommonMark specification
#[derive(Debug, Clone)]
pub enum Inline {
    /// Plain text content
    Text(String),
    /// Emphasis (italic) or strong emphasis (bold)
    Emphasis { 
        strong: bool, 
        content: Vec<Inline> 
    },
    /// Inline code span
    Code(String),
    /// Link with text, destination, and optional title
    Link { 
        text: Vec<Inline>, 
        destination: String, 
        title: Option<String> 
    },
    /// Image with alt text, destination, and optional title
    Image { 
        alt: String, 
        destination: String, 
        title: Option<String> 
    },
    /// Raw HTML inline
    HtmlInline(String),
    /// Soft line break
    SoftBreak,
    /// Hard line break
    HardBreak,
}

/// List type and configuration
#[derive(Debug, Clone, PartialEq)]
pub enum ListKind {
    /// Bullet list with marker character (-, +, *)
    Bullet { marker: char },
    /// Ordered list with start number and delimiter (. or ))
    Ordered { start: u32, delimiter: char },
}

/// Individual list item
#[derive(Debug, Clone)]
pub struct ListItem {
    pub content: Vec<Block>,
    pub tight: bool,
    pub task_list_marker: Option<bool>, // None, Some(false) for unchecked, Some(true) for checked
}

/// Link reference definition
#[derive(Debug, Clone)]
pub struct LinkReference {
    pub label: String,
    pub destination: String,
    pub title: Option<String>,
}

/// Map of link reference definitions
#[derive(Debug, Clone)]
pub struct ReferenceMap {
    definitions: HashMap<String, LinkReference>,
}

/// Source position mapping for nodes
#[derive(Debug, Clone)]
pub struct SourceMap {
    positions: HashMap<NodeId, Position>,
}

/// Unique identifier for AST nodes
pub type NodeId = usize;

// Implement Node trait for Document
impl Node for Document {
    fn source_position(&self) -> Option<Position> {
        None // Document doesn't have a specific position
    }
}

// Implement Visitable trait for Document
impl Visitable for Document {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_document(self);
        for block in &self.blocks {
            block.accept(visitor);
        }
    }
}

// Implement Node trait for Block
impl Node for Block {
    fn source_position(&self) -> Option<Position> {
        match self {
            Block::Heading { position, .. } |
            Block::Paragraph { position, .. } |
            Block::CodeBlock { position, .. } |
            Block::BlockQuote { position, .. } |
            Block::List { position, .. } |
            Block::ThematicBreak { position, .. } |
            Block::HtmlBlock { position, .. } => *position,
        }
    }
}

// Implement Visitable trait for Block
impl Visitable for Block {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_block(self);
        match self {
            Block::Heading { content, .. } => {
                for inline in content {
                    inline.accept(visitor);
                }
            }
            Block::Paragraph { content, .. } => {
                for inline in content {
                    inline.accept(visitor);
                }
            }
            Block::BlockQuote { content, .. } => {
                for block in content {
                    block.accept(visitor);
                }
            }
            Block::List { items, .. } => {
                for item in items {
                    for block in &item.content {
                        block.accept(visitor);
                    }
                }
            }
            Block::CodeBlock { .. } | Block::ThematicBreak { .. } | Block::HtmlBlock { .. } => {
                // These blocks don't have child nodes
            }
        }
    }
}

// Implement Node trait for Inline
impl Node for Inline {
    fn source_position(&self) -> Option<Position> {
        None // Inline elements don't currently track positions
    }
}

// Implement Visitable trait for Inline
impl Visitable for Inline {
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_inline(self);
        match self {
            Inline::Emphasis { content, .. } => {
                for inline in content {
                    inline.accept(visitor);
                }
            }
            Inline::Link { text, .. } => {
                for inline in text {
                    inline.accept(visitor);
                }
            }
            Inline::Text(_) | Inline::Code(_) | Inline::Image { .. } | 
            Inline::HtmlInline(_) | Inline::SoftBreak | Inline::HardBreak => {
                // These inlines don't have child nodes
            }
        }
    }
}

// Implementation for ReferenceMap
impl ReferenceMap {
    pub fn new() -> Self {
        Self {
            definitions: HashMap::new(),
        }
    }
    
    pub fn insert(&mut self, label: String, reference: LinkReference) {
        self.definitions.insert(label.to_lowercase(), reference);
    }
    
    pub fn get(&self, label: &str) -> Option<&LinkReference> {
        self.definitions.get(&label.to_lowercase())
    }
}

// Implementation for SourceMap
impl SourceMap {
    pub fn new() -> Self {
        Self {
            positions: HashMap::new(),
        }
    }
    
    pub fn insert(&mut self, node_id: NodeId, position: Position) {
        self.positions.insert(node_id, position);
    }
    
    pub fn get(&self, node_id: NodeId) -> Option<Position> {
        self.positions.get(&node_id).copied()
    }
}

// Default implementations
impl Default for ReferenceMap {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for SourceMap {
    fn default() -> Self {
        Self::new()
    }
}