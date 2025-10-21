use crate::lexer::Position;
use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicUsize, Ordering};

/// Base trait for all AST nodes providing basic node functionality
pub trait Node {
    /// Get source position if available
    fn source_position(&self) -> Option<Position>;

    /// Get a reference to Any for downcasting
    fn as_any(&self) -> &dyn std::any::Any;
}

/// Trait for visitor pattern support - separate from Node to maintain dyn compatibility
pub trait Visitable {
    /// Accept a visitor for immutable traversal
    fn accept<V: Visitor>(&self, visitor: &mut V);
    /// Accept a mutable visitor for traversal with modification capabilities
    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V);
}

/// Visitor trait for immutable AST traversal
pub trait Visitor {
    fn visit_document(&mut self, document: &Document);
    fn visit_block(&mut self, block: &Block);
    fn visit_inline(&mut self, inline: &Inline);
}

/// Mutable visitor trait for AST traversal with modification capabilities
pub trait MutVisitor {
    fn visit_document(&mut self, document: &mut Document);
    fn visit_block(&mut self, block: &mut Block);
    fn visit_inline(&mut self, inline: &mut Inline);
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
    ThematicBreak { position: Option<Position> },
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
    Emphasis { strong: bool, content: Vec<Inline> },
    /// Inline code span
    Code(String),
    /// Link with text, destination, and optional title
    Link {
        text: Vec<Inline>,
        destination: String,
        title: Option<String>,
    },
    /// Image with alt text, destination, and optional title
    Image {
        alt: String,
        destination: String,
        title: Option<String>,
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

/// Global counter for generating unique node IDs
static NODE_ID_COUNTER: AtomicUsize = AtomicUsize::new(1);

/// Generate a unique node ID
pub fn generate_node_id() -> NodeId {
    NODE_ID_COUNTER.fetch_add(1, Ordering::Relaxed)
}

// Implement Node trait for Document
impl Node for Document {
    fn source_position(&self) -> Option<Position> {
        None // Document doesn't have a specific position
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
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

    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V) {
        visitor.visit_document(self);
        for block in &mut self.blocks {
            block.accept_mut(visitor);
        }
    }
}

// Implement Node trait for Block
impl Node for Block {
    fn source_position(&self) -> Option<Position> {
        match self {
            Block::Heading { position, .. }
            | Block::Paragraph { position, .. }
            | Block::CodeBlock { position, .. }
            | Block::BlockQuote { position, .. }
            | Block::List { position, .. }
            | Block::ThematicBreak { position, .. }
            | Block::HtmlBlock { position, .. } => *position,
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
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

    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V) {
        visitor.visit_block(self);
        match self {
            Block::Heading { content, .. } => {
                for inline in content {
                    inline.accept_mut(visitor);
                }
            }
            Block::Paragraph { content, .. } => {
                for inline in content {
                    inline.accept_mut(visitor);
                }
            }
            Block::BlockQuote { content, .. } => {
                for block in content {
                    block.accept_mut(visitor);
                }
            }
            Block::List { items, .. } => {
                for item in items {
                    for block in &mut item.content {
                        block.accept_mut(visitor);
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

    fn as_any(&self) -> &dyn std::any::Any {
        self
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
            Inline::Text(_)
            | Inline::Code(_)
            | Inline::Image { .. }
            | Inline::HtmlInline(_)
            | Inline::SoftBreak
            | Inline::HardBreak => {
                // These inlines don't have child nodes
            }
        }
    }

    fn accept_mut<V: MutVisitor>(&mut self, visitor: &mut V) {
        visitor.visit_inline(self);
        match self {
            Inline::Emphasis { content, .. } => {
                for inline in content {
                    inline.accept_mut(visitor);
                }
            }
            Inline::Link { text, .. } => {
                for inline in text {
                    inline.accept_mut(visitor);
                }
            }
            Inline::Text(_)
            | Inline::Code(_)
            | Inline::Image { .. }
            | Inline::HtmlInline(_)
            | Inline::SoftBreak
            | Inline::HardBreak => {
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

/// AST utility functions for node creation, relationship management, and traversal
pub mod utils {
    use super::*;

    /// Node creation helper functions
    pub struct NodeBuilder;

    impl NodeBuilder {
        /// Create a new heading block
        pub fn heading(level: u8, content: Vec<Inline>, position: Option<Position>) -> Block {
            Block::Heading {
                level,
                content,
                id: None,
                position,
            }
        }

        /// Create a new paragraph block
        pub fn paragraph(content: Vec<Inline>, position: Option<Position>) -> Block {
            Block::Paragraph { content, position }
        }

        /// Create a new code block
        pub fn code_block(
            info: Option<String>,
            content: String,
            language: Option<String>,
            position: Option<Position>,
        ) -> Block {
            Block::CodeBlock {
                info,
                content,
                language,
                position,
            }
        }

        /// Create a new blockquote
        pub fn blockquote(content: Vec<Block>, position: Option<Position>) -> Block {
            Block::BlockQuote { content, position }
        }

        /// Create a new list
        pub fn list(
            kind: ListKind,
            tight: bool,
            items: Vec<ListItem>,
            position: Option<Position>,
        ) -> Block {
            Block::List {
                kind,
                tight,
                items,
                position,
            }
        }

        /// Create a new thematic break
        pub fn thematic_break(position: Option<Position>) -> Block {
            Block::ThematicBreak { position }
        }

        /// Create a new HTML block
        pub fn html_block(content: String, position: Option<Position>) -> Block {
            Block::HtmlBlock { content, position }
        }

        /// Create a new text inline
        pub fn text(content: String) -> Inline {
            Inline::Text(content)
        }

        /// Create a new emphasis inline
        pub fn emphasis(strong: bool, content: Vec<Inline>) -> Inline {
            Inline::Emphasis { strong, content }
        }

        /// Create a new code span inline
        pub fn code_span(content: String) -> Inline {
            Inline::Code(content)
        }

        /// Create a new link inline
        pub fn link(text: Vec<Inline>, destination: String, title: Option<String>) -> Inline {
            Inline::Link {
                text,
                destination,
                title,
            }
        }

        /// Create a new image inline
        pub fn image(alt: String, destination: String, title: Option<String>) -> Inline {
            Inline::Image {
                alt,
                destination,
                title,
            }
        }

        /// Create a new HTML inline
        pub fn html_inline(content: String) -> Inline {
            Inline::HtmlInline(content)
        }

        /// Create a soft break
        pub fn soft_break() -> Inline {
            Inline::SoftBreak
        }

        /// Create a hard break
        pub fn hard_break() -> Inline {
            Inline::HardBreak
        }

        /// Create a new list item
        pub fn list_item(
            content: Vec<Block>,
            tight: bool,
            task_list_marker: Option<bool>,
        ) -> ListItem {
            ListItem {
                content,
                tight,
                task_list_marker,
            }
        }

        /// Create a new document
        pub fn document(blocks: Vec<Block>) -> Document {
            Document {
                blocks,
                source_map: SourceMap::new(),
            }
        }
    }

    /// Parent-child relationship management utilities
    pub struct RelationshipManager;

    impl RelationshipManager {
        /// Get all child blocks from a block
        pub fn get_child_blocks(block: &Block) -> Vec<&Block> {
            match block {
                Block::BlockQuote { content, .. } => content.iter().collect(),
                Block::List { items, .. } => {
                    items.iter().flat_map(|item| item.content.iter()).collect()
                }
                _ => Vec::new(),
            }
        }

        /// Get all child inlines from a block
        pub fn get_child_inlines(block: &Block) -> Vec<&Inline> {
            match block {
                Block::Heading { content, .. } | Block::Paragraph { content, .. } => {
                    content.iter().collect()
                }
                _ => Vec::new(),
            }
        }

        /// Get all child inlines from an inline element
        pub fn get_child_inlines_from_inline(inline: &Inline) -> Vec<&Inline> {
            match inline {
                Inline::Emphasis { content, .. } | Inline::Link { text: content, .. } => {
                    content.iter().collect()
                }
                _ => Vec::new(),
            }
        }

        /// Count total blocks in a document
        pub fn count_blocks(document: &Document) -> usize {
            let mut count = 0;
            for block in &document.blocks {
                count += Self::count_blocks_recursive(block);
            }
            count
        }

        /// Recursively count blocks
        fn count_blocks_recursive(block: &Block) -> usize {
            let mut count = 1; // Count the current block
            match block {
                Block::BlockQuote { content, .. } => {
                    for child_block in content {
                        count += Self::count_blocks_recursive(child_block);
                    }
                }
                Block::List { items, .. } => {
                    for item in items {
                        for child_block in &item.content {
                            count += Self::count_blocks_recursive(child_block);
                        }
                    }
                }
                _ => {}
            }
            count
        }

        /// Count total inline elements in a document
        pub fn count_inlines(document: &Document) -> usize {
            let mut count = 0;
            for block in &document.blocks {
                count += Self::count_inlines_in_block(block);
            }
            count
        }

        /// Count inline elements in a block
        fn count_inlines_in_block(block: &Block) -> usize {
            let mut count = 0;
            match block {
                Block::Heading { content, .. } | Block::Paragraph { content, .. } => {
                    for inline in content {
                        count += Self::count_inlines_recursive(inline);
                    }
                }
                Block::BlockQuote { content, .. } => {
                    for child_block in content {
                        count += Self::count_inlines_in_block(child_block);
                    }
                }
                Block::List { items, .. } => {
                    for item in items {
                        for child_block in &item.content {
                            count += Self::count_inlines_in_block(child_block);
                        }
                    }
                }
                _ => {}
            }
            count
        }

        /// Recursively count inline elements
        fn count_inlines_recursive(inline: &Inline) -> usize {
            let mut count = 1; // Count the current inline
            match inline {
                Inline::Emphasis { content, .. } | Inline::Link { text: content, .. } => {
                    for child_inline in content {
                        count += Self::count_inlines_recursive(child_inline);
                    }
                }
                _ => {}
            }
            count
        }
    }

    /// Source position mapping utilities
    pub struct PositionMapper;

    impl PositionMapper {
        /// Create a new source map with positions for all nodes in a document
        pub fn create_source_map(document: &Document) -> SourceMap {
            let mut source_map = SourceMap::new();
            let mut node_id = 1;

            for block in &document.blocks {
                Self::map_block_positions(block, &mut source_map, &mut node_id);
            }

            source_map
        }

        /// Map positions for a block and its children
        fn map_block_positions(block: &Block, source_map: &mut SourceMap, node_id: &mut NodeId) {
            let current_id = *node_id;
            *node_id += 1;

            if let Some(position) = block.source_position() {
                source_map.insert(current_id, position);
            }

            match block {
                Block::Heading { content, .. } | Block::Paragraph { content, .. } => {
                    for inline in content {
                        Self::map_inline_positions(inline, source_map, node_id);
                    }
                }
                Block::BlockQuote { content, .. } => {
                    for child_block in content {
                        Self::map_block_positions(child_block, source_map, node_id);
                    }
                }
                Block::List { items, .. } => {
                    for item in items {
                        for child_block in &item.content {
                            Self::map_block_positions(child_block, source_map, node_id);
                        }
                    }
                }
                _ => {}
            }
        }

        /// Map positions for an inline element and its children
        fn map_inline_positions(inline: &Inline, source_map: &mut SourceMap, node_id: &mut NodeId) {
            let current_id = *node_id;
            *node_id += 1;

            if let Some(position) = inline.source_position() {
                source_map.insert(current_id, position);
            }

            match inline {
                Inline::Emphasis { content, .. } | Inline::Link { text: content, .. } => {
                    for child_inline in content {
                        Self::map_inline_positions(child_inline, source_map, node_id);
                    }
                }
                _ => {}
            }
        }

        /// Find all nodes at a specific position
        pub fn find_nodes_at_position(
            document: &Document,
            target_position: Position,
        ) -> Vec<NodeId> {
            let mut matching_nodes = Vec::new();
            let mut node_id = 1;

            for block in &document.blocks {
                Self::find_blocks_at_position(
                    block,
                    target_position,
                    &mut matching_nodes,
                    &mut node_id,
                );
            }

            matching_nodes
        }

        /// Find blocks at a specific position
        fn find_blocks_at_position(
            block: &Block,
            target_position: Position,
            matching_nodes: &mut Vec<NodeId>,
            node_id: &mut NodeId,
        ) {
            let current_id = *node_id;
            *node_id += 1;

            if let Some(position) = block.source_position() {
                if position == target_position {
                    matching_nodes.push(current_id);
                }
            }

            match block {
                Block::Heading { content, .. } | Block::Paragraph { content, .. } => {
                    for inline in content {
                        Self::find_inlines_at_position(
                            inline,
                            target_position,
                            matching_nodes,
                            node_id,
                        );
                    }
                }
                Block::BlockQuote { content, .. } => {
                    for child_block in content {
                        Self::find_blocks_at_position(
                            child_block,
                            target_position,
                            matching_nodes,
                            node_id,
                        );
                    }
                }
                Block::List { items, .. } => {
                    for item in items {
                        for child_block in &item.content {
                            Self::find_blocks_at_position(
                                child_block,
                                target_position,
                                matching_nodes,
                                node_id,
                            );
                        }
                    }
                }
                _ => {}
            }
        }

        /// Find inline elements at a specific position
        fn find_inlines_at_position(
            inline: &Inline,
            target_position: Position,
            matching_nodes: &mut Vec<NodeId>,
            node_id: &mut NodeId,
        ) {
            let current_id = *node_id;
            *node_id += 1;

            if let Some(position) = inline.source_position() {
                if position == target_position {
                    matching_nodes.push(current_id);
                }
            }

            match inline {
                Inline::Emphasis { content, .. } | Inline::Link { text: content, .. } => {
                    for child_inline in content {
                        Self::find_inlines_at_position(
                            child_inline,
                            target_position,
                            matching_nodes,
                            node_id,
                        );
                    }
                }
                _ => {}
            }
        }
    }

    /// AST traversal utilities implementing depth-first and breadth-first iteration
    pub struct Traversal;

    /// Visitor-based traversal utilities
    pub struct VisitorTraversal;

    impl Traversal {
        /// Perform depth-first traversal of the document
        pub fn depth_first<F>(document: &Document, mut visit_fn: F)
        where
            F: FnMut(&dyn Node),
        {
            visit_fn(document);
            for block in &document.blocks {
                Self::depth_first_block(block, &mut visit_fn);
            }
        }

        /// Depth-first traversal of a block
        fn depth_first_block<F>(block: &Block, visit_fn: &mut F)
        where
            F: FnMut(&dyn Node),
        {
            visit_fn(block);
            match block {
                Block::Heading { content, .. } | Block::Paragraph { content, .. } => {
                    for inline in content {
                        Self::depth_first_inline(inline, visit_fn);
                    }
                }
                Block::BlockQuote { content, .. } => {
                    for child_block in content {
                        Self::depth_first_block(child_block, visit_fn);
                    }
                }
                Block::List { items, .. } => {
                    for item in items {
                        for child_block in &item.content {
                            Self::depth_first_block(child_block, visit_fn);
                        }
                    }
                }
                _ => {}
            }
        }

        /// Depth-first traversal of an inline element
        fn depth_first_inline<F>(inline: &Inline, visit_fn: &mut F)
        where
            F: FnMut(&dyn Node),
        {
            visit_fn(inline);
            match inline {
                Inline::Emphasis { content, .. } | Inline::Link { text: content, .. } => {
                    for child_inline in content {
                        Self::depth_first_inline(child_inline, visit_fn);
                    }
                }
                _ => {}
            }
        }

        /// Perform breadth-first traversal of the document
        pub fn breadth_first<F>(document: &Document, mut visit_fn: F)
        where
            F: FnMut(&dyn Node),
        {
            let mut queue: VecDeque<&dyn Node> = VecDeque::new();
            queue.push_back(document);

            while let Some(node) = queue.pop_front() {
                visit_fn(node);

                // Add children to queue based on node type
                if let Some(document) = node.as_any().downcast_ref::<Document>() {
                    for block in &document.blocks {
                        queue.push_back(block);
                    }
                } else if let Some(block) = node.as_any().downcast_ref::<Block>() {
                    match block {
                        Block::Heading { content, .. } | Block::Paragraph { content, .. } => {
                            for inline in content {
                                queue.push_back(inline);
                            }
                        }
                        Block::BlockQuote { content, .. } => {
                            for child_block in content {
                                queue.push_back(child_block);
                            }
                        }
                        Block::List { items, .. } => {
                            for item in items {
                                for child_block in &item.content {
                                    queue.push_back(child_block);
                                }
                            }
                        }
                        _ => {}
                    }
                } else if let Some(inline) = node.as_any().downcast_ref::<Inline>() {
                    match inline {
                        Inline::Emphasis { content, .. } | Inline::Link { text: content, .. } => {
                            for child_inline in content {
                                queue.push_back(child_inline);
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        /// Collect all nodes in depth-first order
        pub fn collect_depth_first<'a>(document: &'a Document) -> Vec<&'a dyn Node> {
            let mut nodes = Vec::new();
            Self::depth_first_collect(document, &mut nodes);
            nodes
        }

        /// Collect all nodes in breadth-first order  
        pub fn collect_breadth_first<'a>(document: &'a Document) -> Vec<&'a dyn Node> {
            let mut nodes = Vec::new();
            Self::breadth_first_collect(document, &mut nodes);
            nodes
        }

        /// Helper function for depth-first collection
        fn depth_first_collect<'a>(document: &'a Document, nodes: &mut Vec<&'a dyn Node>) {
            nodes.push(document);
            for block in &document.blocks {
                Self::depth_first_block_collect(block, nodes);
            }
        }

        /// Helper function for depth-first block collection
        fn depth_first_block_collect<'a>(block: &'a Block, nodes: &mut Vec<&'a dyn Node>) {
            nodes.push(block);
            match block {
                Block::Heading { content, .. } | Block::Paragraph { content, .. } => {
                    for inline in content {
                        Self::depth_first_inline_collect(inline, nodes);
                    }
                }
                Block::BlockQuote { content, .. } => {
                    for child_block in content {
                        Self::depth_first_block_collect(child_block, nodes);
                    }
                }
                Block::List { items, .. } => {
                    for item in items {
                        for child_block in &item.content {
                            Self::depth_first_block_collect(child_block, nodes);
                        }
                    }
                }
                _ => {}
            }
        }

        /// Helper function for depth-first inline collection
        fn depth_first_inline_collect<'a>(inline: &'a Inline, nodes: &mut Vec<&'a dyn Node>) {
            nodes.push(inline);
            match inline {
                Inline::Emphasis { content, .. } | Inline::Link { text: content, .. } => {
                    for child_inline in content {
                        Self::depth_first_inline_collect(child_inline, nodes);
                    }
                }
                _ => {}
            }
        }

        /// Helper function for breadth-first collection
        fn breadth_first_collect<'a>(document: &'a Document, nodes: &mut Vec<&'a dyn Node>) {
            let mut queue: VecDeque<&'a dyn Node> = VecDeque::new();
            queue.push_back(document);

            while let Some(node) = queue.pop_front() {
                nodes.push(node);

                // Add children to queue based on node type
                if let Some(document) = node.as_any().downcast_ref::<Document>() {
                    for block in &document.blocks {
                        queue.push_back(block);
                    }
                } else if let Some(block) = node.as_any().downcast_ref::<Block>() {
                    match block {
                        Block::Heading { content, .. } | Block::Paragraph { content, .. } => {
                            for inline in content {
                                queue.push_back(inline);
                            }
                        }
                        Block::BlockQuote { content, .. } => {
                            for child_block in content {
                                queue.push_back(child_block);
                            }
                        }
                        Block::List { items, .. } => {
                            for item in items {
                                for child_block in &item.content {
                                    queue.push_back(child_block);
                                }
                            }
                        }
                        _ => {}
                    }
                } else if let Some(inline) = node.as_any().downcast_ref::<Inline>() {
                    match inline {
                        Inline::Emphasis { content, .. } | Inline::Link { text: content, .. } => {
                            for child_inline in content {
                                queue.push_back(child_inline);
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    impl VisitorTraversal {
        /// Perform depth-first traversal using visitor pattern
        pub fn depth_first_visitor<V: Visitor>(document: &Document, visitor: &mut V) {
            Self::depth_first_document(document, visitor);
        }

        /// Perform breadth-first traversal using visitor pattern
        pub fn breadth_first_visitor<V: Visitor>(document: &Document, visitor: &mut V) {
            let mut queue: VecDeque<VisitorNode> = VecDeque::new();
            queue.push_back(VisitorNode::Document(document));

            while let Some(node) = queue.pop_front() {
                match node {
                    VisitorNode::Document(doc) => {
                        visitor.visit_document(doc);
                        for block in &doc.blocks {
                            queue.push_back(VisitorNode::Block(block));
                        }
                    }
                    VisitorNode::Block(block) => {
                        visitor.visit_block(block);
                        match block {
                            Block::Heading { content, .. } | Block::Paragraph { content, .. } => {
                                for inline in content {
                                    queue.push_back(VisitorNode::Inline(inline));
                                }
                            }
                            Block::BlockQuote { content, .. } => {
                                for child_block in content {
                                    queue.push_back(VisitorNode::Block(child_block));
                                }
                            }
                            Block::List { items, .. } => {
                                for item in items {
                                    for child_block in &item.content {
                                        queue.push_back(VisitorNode::Block(child_block));
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    VisitorNode::Inline(inline) => {
                        visitor.visit_inline(inline);
                        match inline {
                            Inline::Emphasis { content, .. }
                            | Inline::Link { text: content, .. } => {
                                for child_inline in content {
                                    queue.push_back(VisitorNode::Inline(child_inline));
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        /// Perform depth-first traversal using mutable visitor pattern
        pub fn depth_first_mut_visitor<V: MutVisitor>(document: &mut Document, visitor: &mut V) {
            Self::depth_first_document_mut(document, visitor);
        }

        /// Perform breadth-first traversal using mutable visitor pattern
        /// Note: Due to Rust's borrowing rules, mutable breadth-first traversal
        /// uses the accept_mut method which provides depth-first semantics
        pub fn breadth_first_mut_visitor<V: MutVisitor>(document: &mut Document, visitor: &mut V) {
            // For mutable visitors, we use the accept_mut method which provides
            // depth-first traversal due to Rust's borrowing constraints
            document.accept_mut(visitor);
        }

        // Helper functions for depth-first traversal
        fn depth_first_document<V: Visitor>(document: &Document, visitor: &mut V) {
            visitor.visit_document(document);
            for block in &document.blocks {
                Self::depth_first_block(block, visitor);
            }
        }

        fn depth_first_block<V: Visitor>(block: &Block, visitor: &mut V) {
            visitor.visit_block(block);
            match block {
                Block::Heading { content, .. } | Block::Paragraph { content, .. } => {
                    for inline in content {
                        Self::depth_first_inline(inline, visitor);
                    }
                }
                Block::BlockQuote { content, .. } => {
                    for child_block in content {
                        Self::depth_first_block(child_block, visitor);
                    }
                }
                Block::List { items, .. } => {
                    for item in items {
                        for child_block in &item.content {
                            Self::depth_first_block(child_block, visitor);
                        }
                    }
                }
                _ => {}
            }
        }

        fn depth_first_inline<V: Visitor>(inline: &Inline, visitor: &mut V) {
            visitor.visit_inline(inline);
            match inline {
                Inline::Emphasis { content, .. } | Inline::Link { text: content, .. } => {
                    for child_inline in content {
                        Self::depth_first_inline(child_inline, visitor);
                    }
                }
                _ => {}
            }
        }

        // Helper functions for mutable depth-first traversal
        fn depth_first_document_mut<V: MutVisitor>(document: &mut Document, visitor: &mut V) {
            visitor.visit_document(document);
            for block in &mut document.blocks {
                Self::depth_first_block_mut(block, visitor);
            }
        }

        fn depth_first_block_mut<V: MutVisitor>(block: &mut Block, visitor: &mut V) {
            visitor.visit_block(block);
            match block {
                Block::Heading { content, .. } | Block::Paragraph { content, .. } => {
                    for inline in content {
                        Self::depth_first_inline_mut(inline, visitor);
                    }
                }
                Block::BlockQuote { content, .. } => {
                    for child_block in content {
                        Self::depth_first_block_mut(child_block, visitor);
                    }
                }
                Block::List { items, .. } => {
                    for item in items {
                        for child_block in &mut item.content {
                            Self::depth_first_block_mut(child_block, visitor);
                        }
                    }
                }
                _ => {}
            }
        }

        fn depth_first_inline_mut<V: MutVisitor>(inline: &mut Inline, visitor: &mut V) {
            visitor.visit_inline(inline);
            match inline {
                Inline::Emphasis { content, .. } | Inline::Link { text: content, .. } => {
                    for child_inline in content {
                        Self::depth_first_inline_mut(child_inline, visitor);
                    }
                }
                _ => {}
            }
        }
    }

    /// Helper enum for visitor-based traversal
    enum VisitorNode<'a> {
        Document(&'a Document),
        Block(&'a Block),
        Inline(&'a Inline),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Position;

    #[test]
    fn test_node_builder_functions() {
        // Test creating various node types
        let text_inline = utils::NodeBuilder::text("Hello world".to_string());
        assert!(matches!(text_inline, Inline::Text(_)));

        let emphasis = utils::NodeBuilder::emphasis(false, vec![text_inline]);
        assert!(matches!(emphasis, Inline::Emphasis { strong: false, .. }));

        let paragraph = utils::NodeBuilder::paragraph(
            vec![emphasis],
            Some(Position {
                line: 1,
                column: 1,
                offset: 0,
            }),
        );
        assert!(matches!(paragraph, Block::Paragraph { .. }));

        let document = utils::NodeBuilder::document(vec![paragraph]);
        assert_eq!(document.blocks.len(), 1);
    }

    #[test]
    fn test_relationship_management() {
        // Create a simple document structure
        let text = utils::NodeBuilder::text("Hello".to_string());
        let paragraph = utils::NodeBuilder::paragraph(vec![text], None);
        let document = utils::NodeBuilder::document(vec![paragraph]);

        // Test counting functions
        assert_eq!(utils::RelationshipManager::count_blocks(&document), 1);
        assert_eq!(utils::RelationshipManager::count_inlines(&document), 1);

        // Test getting child inlines
        if let Block::Paragraph { content, .. } = &document.blocks[0] {
            let child_inlines = utils::RelationshipManager::get_child_inlines(&document.blocks[0]);
            assert_eq!(child_inlines.len(), content.len());
        }
    }

    #[test]
    fn test_traversal_methods() {
        // Create a document with nested structure
        let text1 = utils::NodeBuilder::text("Hello".to_string());
        let text2 = utils::NodeBuilder::text("World".to_string());
        let emphasis = utils::NodeBuilder::emphasis(false, vec![text2]);
        let paragraph = utils::NodeBuilder::paragraph(vec![text1, emphasis], None);
        let document = utils::NodeBuilder::document(vec![paragraph]);

        // Test depth-first traversal
        let mut visited_count = 0;
        utils::Traversal::depth_first(&document, |_node| {
            visited_count += 1;
        });
        assert!(visited_count > 0);

        // Test breadth-first traversal
        let mut visited_count_bf = 0;
        utils::Traversal::breadth_first(&document, |_node| {
            visited_count_bf += 1;
        });
        assert_eq!(visited_count, visited_count_bf); // Should visit same number of nodes

        // Test collection methods
        let depth_first_nodes = utils::Traversal::collect_depth_first(&document);
        let breadth_first_nodes = utils::Traversal::collect_breadth_first(&document);
        assert_eq!(depth_first_nodes.len(), breadth_first_nodes.len());
        assert!(depth_first_nodes.len() > 0);
    }

    #[test]
    fn test_position_mapping() {
        let position = Position {
            line: 1,
            column: 5,
            offset: 4,
        };
        let text = utils::NodeBuilder::text("Hello".to_string());
        let paragraph = utils::NodeBuilder::paragraph(vec![text], Some(position));
        let document = utils::NodeBuilder::document(vec![paragraph]);

        // Test source map creation
        let _source_map = utils::PositionMapper::create_source_map(&document);

        // Test finding nodes at position
        let matching_nodes = utils::PositionMapper::find_nodes_at_position(&document, position);
        assert!(matching_nodes.len() > 0);
    }

    #[test]
    fn test_node_id_generation() {
        let id1 = generate_node_id();
        let id2 = generate_node_id();
        assert_ne!(id1, id2);
        assert!(id2 > id1);
    }

    #[test]
    fn test_visitor_pattern() {
        // Create a test document with nested structure
        let text1 = utils::NodeBuilder::text("Hello".to_string());
        let text2 = utils::NodeBuilder::text("World".to_string());
        let emphasis = utils::NodeBuilder::emphasis(false, vec![text2]);
        let paragraph = utils::NodeBuilder::paragraph(vec![text1, emphasis], None);
        let document = utils::NodeBuilder::document(vec![paragraph]);

        // Test immutable visitor
        struct CountingVisitor {
            document_count: usize,
            block_count: usize,
            inline_count: usize,
        }

        impl Visitor for CountingVisitor {
            fn visit_document(&mut self, _document: &Document) {
                self.document_count += 1;
            }

            fn visit_block(&mut self, _block: &Block) {
                self.block_count += 1;
            }

            fn visit_inline(&mut self, _inline: &Inline) {
                self.inline_count += 1;
            }
        }

        let mut visitor = CountingVisitor {
            document_count: 0,
            block_count: 0,
            inline_count: 0,
        };

        document.accept(&mut visitor);

        assert_eq!(visitor.document_count, 1);
        assert_eq!(visitor.block_count, 1);
        assert_eq!(visitor.inline_count, 3); // "Hello", emphasis, and "World" inside emphasis
    }

    #[test]
    fn test_mutable_visitor_pattern() {
        // Create a test document
        let text = utils::NodeBuilder::text("hello".to_string());
        let paragraph = utils::NodeBuilder::paragraph(vec![text], None);
        let mut document = utils::NodeBuilder::document(vec![paragraph]);

        // Test mutable visitor that capitalizes text
        struct CapitalizingVisitor;

        impl MutVisitor for CapitalizingVisitor {
            fn visit_document(&mut self, _document: &mut Document) {
                // No action needed for document
            }

            fn visit_block(&mut self, _block: &mut Block) {
                // No action needed for block
            }

            fn visit_inline(&mut self, inline: &mut Inline) {
                if let Inline::Text(text) = inline {
                    *text = text.to_uppercase();
                }
            }
        }

        let mut visitor = CapitalizingVisitor;
        document.accept_mut(&mut visitor);

        // Check that text was capitalized
        if let Block::Paragraph { content, .. } = &document.blocks[0] {
            if let Inline::Text(text) = &content[0] {
                assert_eq!(text, "HELLO");
            } else {
                panic!("Expected text inline");
            }
        } else {
            panic!("Expected paragraph block");
        }
    }

    #[test]
    fn test_depth_first_visitor_traversal() {
        // Create a document with nested structure
        let text1 = utils::NodeBuilder::text("First".to_string());
        let text2 = utils::NodeBuilder::text("Second".to_string());
        let emphasis = utils::NodeBuilder::emphasis(false, vec![text2]);
        let paragraph = utils::NodeBuilder::paragraph(vec![text1, emphasis], None);
        let document = utils::NodeBuilder::document(vec![paragraph]);

        // Test depth-first traversal order
        struct OrderTrackingVisitor {
            visit_order: Vec<String>,
        }

        impl Visitor for OrderTrackingVisitor {
            fn visit_document(&mut self, _document: &Document) {
                self.visit_order.push("Document".to_string());
            }

            fn visit_block(&mut self, block: &Block) {
                match block {
                    Block::Paragraph { .. } => self.visit_order.push("Paragraph".to_string()),
                    _ => {}
                }
            }

            fn visit_inline(&mut self, inline: &Inline) {
                match inline {
                    Inline::Text(text) => self.visit_order.push(format!("Text({})", text)),
                    Inline::Emphasis { .. } => self.visit_order.push("Emphasis".to_string()),
                    _ => {}
                }
            }
        }

        let mut visitor = OrderTrackingVisitor {
            visit_order: Vec::new(),
        };

        utils::VisitorTraversal::depth_first_visitor(&document, &mut visitor);

        // Verify depth-first order: Document -> Paragraph -> Text(First) -> Emphasis -> Text(Second)
        assert_eq!(visitor.visit_order.len(), 5);
        assert_eq!(visitor.visit_order[0], "Document");
        assert_eq!(visitor.visit_order[1], "Paragraph");
        assert_eq!(visitor.visit_order[2], "Text(First)");
        assert_eq!(visitor.visit_order[3], "Emphasis");
        assert_eq!(visitor.visit_order[4], "Text(Second)");
    }

    #[test]
    fn test_breadth_first_visitor_traversal() {
        // Create a document with nested structure
        let text1 = utils::NodeBuilder::text("First".to_string());
        let text2 = utils::NodeBuilder::text("Second".to_string());
        let emphasis = utils::NodeBuilder::emphasis(false, vec![text2]);
        let paragraph = utils::NodeBuilder::paragraph(vec![text1, emphasis], None);
        let document = utils::NodeBuilder::document(vec![paragraph]);

        // Test breadth-first traversal order
        struct OrderTrackingVisitor {
            visit_order: Vec<String>,
        }

        impl Visitor for OrderTrackingVisitor {
            fn visit_document(&mut self, _document: &Document) {
                self.visit_order.push("Document".to_string());
            }

            fn visit_block(&mut self, block: &Block) {
                match block {
                    Block::Paragraph { .. } => self.visit_order.push("Paragraph".to_string()),
                    _ => {}
                }
            }

            fn visit_inline(&mut self, inline: &Inline) {
                match inline {
                    Inline::Text(text) => self.visit_order.push(format!("Text({})", text)),
                    Inline::Emphasis { .. } => self.visit_order.push("Emphasis".to_string()),
                    _ => {}
                }
            }
        }

        let mut visitor = OrderTrackingVisitor {
            visit_order: Vec::new(),
        };

        utils::VisitorTraversal::breadth_first_visitor(&document, &mut visitor);

        // Verify breadth-first order: Document -> Paragraph -> Text(First) -> Emphasis -> Text(Second)
        assert_eq!(visitor.visit_order.len(), 5);
        assert_eq!(visitor.visit_order[0], "Document");
        assert_eq!(visitor.visit_order[1], "Paragraph");
        // In breadth-first, all inlines at the same level should be visited before their children
        assert!(visitor.visit_order.contains(&"Text(First)".to_string()));
        assert!(visitor.visit_order.contains(&"Emphasis".to_string()));
        assert!(visitor.visit_order.contains(&"Text(Second)".to_string()));
    }

    #[test]
    fn test_complex_nested_structure() {
        // Test more complex nested structures with blockquotes and lists
        let text1 = utils::NodeBuilder::text("Item 1".to_string());
        let text2 = utils::NodeBuilder::text("Item 2".to_string());
        let paragraph1 = utils::NodeBuilder::paragraph(vec![text1], None);
        let paragraph2 = utils::NodeBuilder::paragraph(vec![text2], None);
        
        let list_item1 = utils::NodeBuilder::list_item(vec![paragraph1], true, None);
        let list_item2 = utils::NodeBuilder::list_item(vec![paragraph2], true, None);
        
        let list = utils::NodeBuilder::list(
            ListKind::Bullet { marker: '-' },
            true,
            vec![list_item1, list_item2],
            None,
        );
        
        let blockquote = utils::NodeBuilder::blockquote(vec![list], None);
        let document = utils::NodeBuilder::document(vec![blockquote]);

        // Test relationship management with complex structure
        assert_eq!(utils::RelationshipManager::count_blocks(&document), 4); // blockquote + list + 2 paragraphs
        assert_eq!(utils::RelationshipManager::count_inlines(&document), 2); // 2 text inlines

        // Test traversal with complex structure
        let mut node_count = 0;
        utils::Traversal::depth_first(&document, |_node| {
            node_count += 1;
        });
        assert_eq!(node_count, 7); // document + blockquote + list + 2 paragraphs + 2 text inlines
    }

    #[test]
    fn test_source_position_preservation() {
        // Test that source positions are properly preserved and accessible
        let position1 = Position { line: 1, column: 1, offset: 0 };
        let position2 = Position { line: 2, column: 1, offset: 10 };
        let position3 = Position { line: 3, column: 5, offset: 25 };

        let text = utils::NodeBuilder::text("Hello".to_string());
        let paragraph = utils::NodeBuilder::paragraph(vec![text], Some(position1));
        let code_block = utils::NodeBuilder::code_block(
            Some("rust".to_string()),
            "fn main() {}".to_string(),
            Some("rust".to_string()),
            Some(position2),
        );
        let heading = utils::NodeBuilder::heading(
            1,
            vec![utils::NodeBuilder::text("Title".to_string())],
            Some(position3),
        );

        let document = utils::NodeBuilder::document(vec![paragraph, code_block, heading]);

        // Verify positions are preserved
        assert_eq!(document.blocks[0].source_position(), Some(position1));
        assert_eq!(document.blocks[1].source_position(), Some(position2));
        assert_eq!(document.blocks[2].source_position(), Some(position3));

        // Test position mapping utilities
        let _source_map = utils::PositionMapper::create_source_map(&document);
        let nodes_at_pos1 = utils::PositionMapper::find_nodes_at_position(&document, position1);
        let nodes_at_pos2 = utils::PositionMapper::find_nodes_at_position(&document, position2);
        let nodes_at_pos3 = utils::PositionMapper::find_nodes_at_position(&document, position3);

        assert!(!nodes_at_pos1.is_empty());
        assert!(!nodes_at_pos2.is_empty());
        assert!(!nodes_at_pos3.is_empty());
    }

    #[test]
    fn test_node_type_downcasting() {
        // Test that nodes can be properly downcast to their concrete types
        let text = utils::NodeBuilder::text("Hello".to_string());
        let paragraph = utils::NodeBuilder::paragraph(vec![text], None);
        let document = utils::NodeBuilder::document(vec![paragraph]);

        // Test downcasting through the Node trait
        let node: &dyn Node = &document;
        assert!(node.as_any().downcast_ref::<Document>().is_some());
        assert!(node.as_any().downcast_ref::<Block>().is_none());

        let block_node: &dyn Node = &document.blocks[0];
        assert!(block_node.as_any().downcast_ref::<Block>().is_some());
        assert!(block_node.as_any().downcast_ref::<Document>().is_none());
    }

    #[test]
    fn test_list_structures() {
        // Test different list types and configurations
        let text1 = utils::NodeBuilder::text("First item".to_string());
        let text2 = utils::NodeBuilder::text("Second item".to_string());
        let paragraph1 = utils::NodeBuilder::paragraph(vec![text1], None);
        let paragraph2 = utils::NodeBuilder::paragraph(vec![text2], None);

        // Test bullet list
        let bullet_item1 = utils::NodeBuilder::list_item(vec![paragraph1.clone()], true, None);
        let bullet_item2 = utils::NodeBuilder::list_item(vec![paragraph2.clone()], true, Some(false)); // unchecked task
        let bullet_list = utils::NodeBuilder::list(
            ListKind::Bullet { marker: '*' },
            true,
            vec![bullet_item1, bullet_item2],
            None,
        );

        // Test ordered list
        let ordered_item1 = utils::NodeBuilder::list_item(vec![paragraph1], false, None);
        let ordered_item2 = utils::NodeBuilder::list_item(vec![paragraph2], false, Some(true)); // checked task
        let ordered_list = utils::NodeBuilder::list(
            ListKind::Ordered { start: 1, delimiter: '.' },
            false,
            vec![ordered_item1, ordered_item2],
            None,
        );

        let document = utils::NodeBuilder::document(vec![bullet_list, ordered_list]);

        // Verify list structure
        if let Block::List { kind, tight, items, .. } = &document.blocks[0] {
            assert!(matches!(kind, ListKind::Bullet { marker: '*' }));
            assert!(*tight);
            assert_eq!(items.len(), 2);
            assert_eq!(items[1].task_list_marker, Some(false));
        } else {
            panic!("Expected bullet list");
        }

        if let Block::List { kind, tight, items, .. } = &document.blocks[1] {
            assert!(matches!(kind, ListKind::Ordered { start: 1, delimiter: '.' }));
            assert!(!*tight);
            assert_eq!(items.len(), 2);
            assert_eq!(items[1].task_list_marker, Some(true));
        } else {
            panic!("Expected ordered list");
        }
    }

    #[test]
    fn test_reference_map() {
        // Test link reference definition storage and retrieval
        let mut ref_map = ReferenceMap::new();
        
        let link_ref = LinkReference {
            label: "example".to_string(),
            destination: "https://example.com".to_string(),
            title: Some("Example Site".to_string()),
        };

        ref_map.insert("Example".to_string(), link_ref.clone());
        
        // Test case-insensitive lookup
        assert!(ref_map.get("example").is_some());
        assert!(ref_map.get("EXAMPLE").is_some());
        assert!(ref_map.get("Example").is_some());
        assert!(ref_map.get("nonexistent").is_none());

        let retrieved = ref_map.get("example").unwrap();
        assert_eq!(retrieved.destination, "https://example.com");
        assert_eq!(retrieved.title, Some("Example Site".to_string()));
    }

    #[test]
    fn test_source_map_operations() {
        // Test source map insertion and retrieval
        let mut source_map = SourceMap::new();
        let position1 = Position { line: 1, column: 1, offset: 0 };
        let position2 = Position { line: 2, column: 5, offset: 15 };

        let node_id1 = generate_node_id();
        let node_id2 = generate_node_id();

        source_map.insert(node_id1, position1);
        source_map.insert(node_id2, position2);

        assert_eq!(source_map.get(node_id1), Some(position1));
        assert_eq!(source_map.get(node_id2), Some(position2));
        assert_eq!(source_map.get(999999), None); // Non-existent node ID
    }
}
