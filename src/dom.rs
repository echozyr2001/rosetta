use crate::lexer::Position;
use std::collections::HashMap;

/// Represents a node in the Document Object Model (DOM).
/// This is a generic, intermediate representation that decouples the AST
/// from the final output format (e.g., HTML, XML).
#[derive(Debug, Clone, PartialEq)]
pub struct DomNode {
    /// The HTML tag name for this element (e.g., "h1", "p", "div")
    pub tag: String,
    /// HTML attributes as key-value pairs
    pub attributes: HashMap<String, String>,
    /// Child nodes (elements and text)
    pub children: Vec<DomChild>,
    /// Optional source position for debugging and tooling
    pub source_position: Option<Position>,
}

/// Represents a child node in the DOM tree.
/// Can be either an element node or a text node.
#[derive(Debug, Clone, PartialEq)]
pub enum DomChild {
    /// An element node containing a DomNode
    Element(DomNode),
    /// A text node containing string content
    Text(String),
}

impl DomNode {
    /// Creates a new DOM node with the given tag name.
    pub fn new(tag: &str) -> Self {
        DomNode {
            tag: tag.to_string(),
            attributes: HashMap::new(),
            children: Vec::new(),
            source_position: None,
        }
    }

    /// Creates a new DOM node with tag name and source position.
    pub fn new_with_position(tag: &str, position: Position) -> Self {
        DomNode {
            tag: tag.to_string(),
            attributes: HashMap::new(),
            children: Vec::new(),
            source_position: Some(position),
        }
    }

    /// Adds an attribute to this DOM node.
    pub fn add_attribute(&mut self, key: &str, value: &str) {
        self.attributes.insert(key.to_string(), value.to_string());
    }

    /// Sets multiple attributes at once.
    pub fn set_attributes(&mut self, attributes: HashMap<String, String>) {
        self.attributes = attributes;
    }

    /// Adds a child element to this DOM node.
    pub fn add_child(&mut self, child: DomChild) {
        self.children.push(child);
    }

    /// Adds a child element node to this DOM node.
    pub fn add_element_child(&mut self, element: DomNode) {
        self.children.push(DomChild::Element(element));
    }

    /// Adds a text child to this DOM node.
    pub fn add_text_child(&mut self, text: &str) {
        self.children.push(DomChild::Text(text.to_string()));
    }

    /// Returns true if this node has any children.
    pub fn has_children(&self) -> bool {
        !self.children.is_empty()
    }

    /// Returns the number of children.
    pub fn child_count(&self) -> usize {
        self.children.len()
    }

    /// Returns an iterator over the children.
    pub fn children_iter(&self) -> std::slice::Iter<DomChild> {
        self.children.iter()
    }

    /// Returns a mutable iterator over the children.
    pub fn children_iter_mut(&mut self) -> std::slice::IterMut<DomChild> {
        self.children.iter_mut()
    }

    /// Removes all children from this node.
    pub fn clear_children(&mut self) {
        self.children.clear();
    }

    /// Gets the value of an attribute by key.
    pub fn get_attribute(&self, key: &str) -> Option<&String> {
        self.attributes.get(key)
    }

    /// Removes an attribute by key.
    pub fn remove_attribute(&mut self, key: &str) -> Option<String> {
        self.attributes.remove(key)
    }

    /// Returns true if this node has the specified attribute.
    pub fn has_attribute(&self, key: &str) -> bool {
        self.attributes.contains_key(key)
    }

    /// Returns an iterator over all attributes.
    pub fn attributes_iter(&self) -> std::collections::hash_map::Iter<String, String> {
        self.attributes.iter()
    }
}

impl DomChild {
    /// Creates a new element child.
    pub fn element(node: DomNode) -> Self {
        DomChild::Element(node)
    }

    /// Creates a new text child.
    pub fn text(content: &str) -> Self {
        DomChild::Text(content.to_string())
    }

    /// Returns true if this is an element child.
    pub fn is_element(&self) -> bool {
        matches!(self, DomChild::Element(_))
    }

    /// Returns true if this is a text child.
    pub fn is_text(&self) -> bool {
        matches!(self, DomChild::Text(_))
    }

    /// Returns the element if this is an element child, None otherwise.
    pub fn as_element(&self) -> Option<&DomNode> {
        match self {
            DomChild::Element(node) => Some(node),
            DomChild::Text(_) => None,
        }
    }

    /// Returns the text content if this is a text child, None otherwise.
    pub fn as_text(&self) -> Option<&str> {
        match self {
            DomChild::Text(text) => Some(text),
            DomChild::Element(_) => None,
        }
    }

    /// Returns a mutable reference to the element if this is an element child.
    pub fn as_element_mut(&mut self) -> Option<&mut DomNode> {
        match self {
            DomChild::Element(node) => Some(node),
            DomChild::Text(_) => None,
        }
    }
}
/// Utility functions for DOM manipulation and traversal.
impl DomNode {
    /// Performs a depth-first traversal of the DOM tree, calling the visitor function on each node.
    pub fn traverse_depth_first<F>(&self, visitor: &mut F)
    where
        F: FnMut(&DomNode),
    {
        visitor(self);
        for child in &self.children {
            if let DomChild::Element(element) = child {
                element.traverse_depth_first(visitor);
            }
        }
    }

    /// Performs a depth-first traversal with mutable access to nodes.
    pub fn traverse_depth_first_mut<F>(&mut self, visitor: &mut F)
    where
        F: FnMut(&mut DomNode),
    {
        visitor(self);
        for child in &mut self.children {
            if let DomChild::Element(element) = child {
                element.traverse_depth_first_mut(visitor);
            }
        }
    }

    /// Finds the first node that matches the given predicate using depth-first search.
    pub fn find_node<F>(&self, predicate: &F) -> Option<&DomNode>
    where
        F: Fn(&DomNode) -> bool,
    {
        if predicate(self) {
            return Some(self);
        }

        for child in &self.children {
            if let DomChild::Element(element) = child {
                if let Some(found) = element.find_node(predicate) {
                    return Some(found);
                }
            }
        }

        None
    }

    /// Finds all nodes that match the given predicate.
    pub fn find_all_nodes<F>(&self, predicate: F) -> Vec<&DomNode>
    where
        F: Fn(&DomNode) -> bool,
    {
        let mut results = Vec::new();
        self.collect_matching_nodes(&predicate, &mut results);
        results
    }

    /// Helper function for find_all_nodes.
    fn collect_matching_nodes<'a, F>(&'a self, predicate: &F, results: &mut Vec<&'a DomNode>)
    where
        F: Fn(&DomNode) -> bool,
    {
        if predicate(self) {
            results.push(self);
        }

        for child in &self.children {
            if let DomChild::Element(element) = child {
                element.collect_matching_nodes(predicate, results);
            }
        }
    }

    /// Finds nodes by tag name.
    pub fn find_by_tag(&self, tag: &str) -> Vec<&DomNode> {
        self.find_all_nodes(|node| node.tag == tag)
    }

    /// Finds the first node with the specified tag name.
    pub fn find_first_by_tag(&self, tag: &str) -> Option<&DomNode> {
        self.find_node(&|node| node.tag == tag)
    }

    /// Finds nodes by attribute value.
    pub fn find_by_attribute(&self, key: &str, value: &str) -> Vec<&DomNode> {
        self.find_all_nodes(|node| {
            node.get_attribute(key)
                .map(|attr_value| attr_value == value)
                .unwrap_or(false)
        })
    }

    /// Gets all text content from this node and its descendants.
    pub fn get_text_content(&self) -> String {
        let mut text = String::new();
        self.collect_text_content(&mut text);
        text
    }

    /// Helper function for get_text_content.
    fn collect_text_content(&self, text: &mut String) {
        for child in &self.children {
            match child {
                DomChild::Text(content) => text.push_str(content),
                DomChild::Element(element) => element.collect_text_content(text),
            }
        }
    }

    /// Removes all children that match the given predicate.
    pub fn remove_children_if<F>(&mut self, predicate: F)
    where
        F: Fn(&DomChild) -> bool,
    {
        self.children.retain(|child| !predicate(child));
    }

    /// Inserts a child at the specified index.
    pub fn insert_child(&mut self, index: usize, child: DomChild) {
        if index <= self.children.len() {
            self.children.insert(index, child);
        }
    }

    /// Removes a child at the specified index.
    pub fn remove_child(&mut self, index: usize) -> Option<DomChild> {
        if index < self.children.len() {
            Some(self.children.remove(index))
        } else {
            None
        }
    }

    /// Replaces a child at the specified index.
    pub fn replace_child(&mut self, index: usize, new_child: DomChild) -> Option<DomChild> {
        if index < self.children.len() {
            Some(std::mem::replace(&mut self.children[index], new_child))
        } else {
            None
        }
    }
}

/// Additional utility functions for DomChild.
impl DomChild {
    /// Gets the text content of this child node.
    pub fn get_text_content(&self) -> String {
        match self {
            DomChild::Text(text) => text.clone(),
            DomChild::Element(element) => element.get_text_content(),
        }
    }

    /// Returns the tag name if this is an element, None otherwise.
    pub fn get_tag(&self) -> Option<&str> {
        match self {
            DomChild::Element(element) => Some(&element.tag),
            DomChild::Text(_) => None,
        }
    }
}

/// Trait for visiting DOM nodes during traversal.
pub trait DomVisitor {
    /// Called when visiting a DOM node.
    fn visit_node(&mut self, node: &DomNode);

    /// Called when visiting a text node.
    fn visit_text(&mut self, text: &str);

    /// Called when entering a node (before visiting children).
    fn enter_node(&mut self, _node: &DomNode) {}

    /// Called when exiting a node (after visiting children).
    fn exit_node(&mut self, _node: &DomNode) {}
}

/// Implements visitor pattern traversal for DOM trees.
impl DomNode {
    /// Accepts a visitor for traversing the DOM tree.
    pub fn accept_visitor<V: DomVisitor>(&self, visitor: &mut V) {
        visitor.enter_node(self);
        visitor.visit_node(self);

        for child in &self.children {
            match child {
                DomChild::Element(element) => element.accept_visitor(visitor),
                DomChild::Text(text) => visitor.visit_text(text),
            }
        }

        visitor.exit_node(self);
    }
}

/// Configuration options for DOM transformation
#[derive(Debug)]
pub struct DomConfig {
    /// Whether to add source position information as data attributes
    pub add_source_positions: bool,
    /// Custom attributes to add to all elements
    pub custom_attributes: HashMap<String, String>,
    /// Whether to preserve whitespace in text nodes
    pub preserve_whitespace: bool,
    /// Whether to add CSS classes for element types
    pub add_css_classes: bool,
    /// Custom element processors for extending transformation
    pub element_processors: Vec<Box<dyn ElementProcessor>>,
}

impl Clone for DomConfig {
    fn clone(&self) -> Self {
        Self {
            add_source_positions: self.add_source_positions,
            custom_attributes: self.custom_attributes.clone(),
            preserve_whitespace: self.preserve_whitespace,
            add_css_classes: self.add_css_classes,
            // Note: Element processors cannot be cloned due to trait object limitations
            // In a real implementation, you might want to use a different approach
            element_processors: Vec::new(),
        }
    }
}

impl Default for DomConfig {
    fn default() -> Self {
        Self {
            add_source_positions: false,
            custom_attributes: HashMap::new(),
            preserve_whitespace: false,
            add_css_classes: true,
            element_processors: Vec::new(),
        }
    }
}

/// Trait for custom element processing during DOM transformation
pub trait ElementProcessor: std::fmt::Debug {
    /// Process a DOM node after it has been created from an AST node
    fn process_element(&self, node: &mut DomNode, ast_node: &dyn std::any::Any);
    
    /// Get the name of this processor for debugging
    fn name(&self) -> &str;
}

/// Transforms Abstract Syntax Tree (AST) nodes into Document Object Model (DOM) nodes
#[derive(Debug)]
pub struct DomTransformer {
    config: DomConfig,
}

impl DomTransformer {
    /// Creates a new DOM transformer with the given configuration
    pub fn new(config: DomConfig) -> Self {
        Self { config }
    }

    /// Creates a new DOM transformer with default configuration
    pub fn new_default() -> Self {
        Self {
            config: DomConfig::default(),
        }
    }

    /// Transforms an AST document into a DOM tree
    pub fn transform(&self, document: &crate::ast::Document) -> Result<DomNode, crate::error::MarkdownError> {
        let mut root = DomNode::new("div");
        
        // Add default attributes
        if self.config.add_css_classes {
            root.add_attribute("class", "markdown-content");
        }
        
        // Add custom attributes
        for (key, value) in &self.config.custom_attributes {
            root.add_attribute(key, value);
        }

        // Transform all blocks in the document
        for block in &document.blocks {
            let dom_child = self.transform_block(block)?;
            root.add_element_child(dom_child);
        }

        // Apply custom element processors
        for processor in &self.config.element_processors {
            processor.process_element(&mut root, document as &dyn std::any::Any);
        }

        Ok(root)
    }

    /// Transforms a block AST node into a DOM node
    fn transform_block(&self, block: &crate::ast::Block) -> Result<DomNode, crate::error::MarkdownError> {
        use crate::ast::Block;
        
        let mut node = match block {
            Block::Heading { level, content, id, position } => {
                let tag = format!("h{}", level);
                let mut heading = DomNode::new(&tag);
                
                if self.config.add_css_classes {
                    heading.add_attribute("class", "heading");
                }
                
                if let Some(id) = id {
                    heading.add_attribute("id", id);
                }
                
                self.add_position_attribute(&mut heading, position)?;
                
                // Transform inline content
                for inline in content {
                    let inline_dom = self.transform_inline(inline)?;
                    self.add_inline_to_node(&mut heading, inline_dom);
                }
                
                heading
            },
            
            Block::Paragraph { content, position } => {
                let mut paragraph = DomNode::new("p");
                
                if self.config.add_css_classes {
                    paragraph.add_attribute("class", "paragraph");
                }
                
                self.add_position_attribute(&mut paragraph, position)?;
                
                // Transform inline content
                for inline in content {
                    let inline_dom = self.transform_inline(inline)?;
                    self.add_inline_to_node(&mut paragraph, inline_dom);
                }
                
                paragraph
            },
            
            Block::CodeBlock { info, content, language, position } => {
                let mut pre = DomNode::new("pre");
                let mut code = DomNode::new("code");
                
                if self.config.add_css_classes {
                    pre.add_attribute("class", "code-block");
                    code.add_attribute("class", "code");
                }
                
                // Add language class if available
                if let Some(lang) = language {
                    let existing_class = code.get_attribute("class").cloned().unwrap_or_default();
                    let new_class = if existing_class.is_empty() {
                        format!("language-{}", lang)
                    } else {
                        format!("{} language-{}", existing_class, lang)
                    };
                    code.add_attribute("class", &new_class);
                }
                
                // Add info as data attribute if available
                if let Some(info_str) = info {
                    code.add_attribute("data-info", info_str);
                }
                
                self.add_position_attribute(&mut pre, position)?;
                
                code.add_text_child(content);
                pre.add_element_child(code);
                pre
            },
            
            Block::BlockQuote { content, position } => {
                let mut blockquote = DomNode::new("blockquote");
                
                if self.config.add_css_classes {
                    blockquote.add_attribute("class", "blockquote");
                }
                
                self.add_position_attribute(&mut blockquote, position)?;
                
                // Transform nested blocks
                for nested_block in content {
                    let nested_dom = self.transform_block(nested_block)?;
                    blockquote.add_element_child(nested_dom);
                }
                
                blockquote
            },
            
            Block::List { kind, tight, items, position } => {
                use crate::ast::ListKind;
                
                let (tag, class) = match kind {
                    ListKind::Bullet { .. } => ("ul", "bullet-list"),
                    ListKind::Ordered { .. } => ("ol", "ordered-list"),
                };
                
                let mut list = DomNode::new(tag);
                
                if self.config.add_css_classes {
                    let mut class_str = class.to_string();
                    if *tight {
                        class_str.push_str(" tight");
                    }
                    list.add_attribute("class", &class_str);
                }
                
                // Add start attribute for ordered lists
                if let ListKind::Ordered { start, .. } = kind {
                    if *start != 1 {
                        list.add_attribute("start", &start.to_string());
                    }
                }
                
                self.add_position_attribute(&mut list, position)?;
                
                // Transform list items
                for item in items {
                    let mut li = DomNode::new("li");
                    
                    if self.config.add_css_classes {
                        let mut li_class = "list-item".to_string();
                        if let Some(checked) = item.task_list_marker {
                            li_class.push_str(" task-list-item");
                            if checked {
                                li_class.push_str(" checked");
                            }
                        }
                        li.add_attribute("class", &li_class);
                    }
                    
                    // Add task list checkbox if present
                    if let Some(checked) = item.task_list_marker {
                        let mut checkbox = DomNode::new("input");
                        checkbox.add_attribute("type", "checkbox");
                        checkbox.add_attribute("disabled", "disabled");
                        if checked {
                            checkbox.add_attribute("checked", "checked");
                        }
                        if self.config.add_css_classes {
                            checkbox.add_attribute("class", "task-list-item-checkbox");
                        }
                        li.add_element_child(checkbox);
                    }
                    
                    // Transform item content
                    for block in &item.content {
                        let block_dom = self.transform_block(block)?;
                        li.add_element_child(block_dom);
                    }
                    
                    list.add_element_child(li);
                }
                
                list
            },
            
            Block::ThematicBreak { position } => {
                let mut hr = DomNode::new("hr");
                
                if self.config.add_css_classes {
                    hr.add_attribute("class", "thematic-break");
                }
                
                self.add_position_attribute(&mut hr, position)?;
                hr
            },
            
            Block::HtmlBlock { content, position } => {
                // For HTML blocks, we create a div container with the raw HTML as text
                // In a real implementation, you might want to parse the HTML
                let mut div = DomNode::new("div");
                
                if self.config.add_css_classes {
                    div.add_attribute("class", "html-block");
                }
                
                self.add_position_attribute(&mut div, position)?;
                div.add_text_child(content);
                div
            },
        };

        // Apply custom element processors
        for processor in &self.config.element_processors {
            processor.process_element(&mut node, block as &dyn std::any::Any);
        }

        Ok(node)
    }

    /// Transforms an inline AST node into DOM content
    fn transform_inline(&self, inline: &crate::ast::Inline) -> Result<DomChild, crate::error::MarkdownError> {
        use crate::ast::Inline;
        
        let dom_child = match inline {
            Inline::Text(text) => {
                DomChild::Text(text.clone())
            },
            
            Inline::Emphasis { strong, content } => {
                let tag = if *strong { "strong" } else { "em" };
                let mut element = DomNode::new(tag);
                
                if self.config.add_css_classes {
                    let class = if *strong { "strong" } else { "emphasis" };
                    element.add_attribute("class", class);
                }
                
                // Transform nested inline content
                for nested_inline in content {
                    let nested_dom = self.transform_inline(nested_inline)?;
                    self.add_inline_to_node(&mut element, nested_dom);
                }
                
                DomChild::Element(element)
            },
            
            Inline::Code(code) => {
                let mut element = DomNode::new("code");
                
                if self.config.add_css_classes {
                    element.add_attribute("class", "inline-code");
                }
                
                element.add_text_child(code);
                DomChild::Element(element)
            },
            
            Inline::Link { text, destination, title } => {
                let mut element = DomNode::new("a");
                element.add_attribute("href", destination);
                
                if let Some(title_str) = title {
                    element.add_attribute("title", title_str);
                }
                
                if self.config.add_css_classes {
                    element.add_attribute("class", "link");
                }
                
                // Transform link text
                for text_inline in text {
                    let text_dom = self.transform_inline(text_inline)?;
                    self.add_inline_to_node(&mut element, text_dom);
                }
                
                DomChild::Element(element)
            },
            
            Inline::Image { alt, destination, title } => {
                let mut element = DomNode::new("img");
                element.add_attribute("src", destination);
                element.add_attribute("alt", alt);
                
                if let Some(title_str) = title {
                    element.add_attribute("title", title_str);
                }
                
                if self.config.add_css_classes {
                    element.add_attribute("class", "image");
                }
                
                DomChild::Element(element)
            },
            
            Inline::HtmlInline(html) => {
                // For inline HTML, we create a span container with the raw HTML as text
                let mut element = DomNode::new("span");
                
                if self.config.add_css_classes {
                    element.add_attribute("class", "html-inline");
                }
                
                element.add_text_child(html);
                DomChild::Element(element)
            },
            
            Inline::SoftBreak => {
                if self.config.preserve_whitespace {
                    DomChild::Text("\n".to_string())
                } else {
                    DomChild::Text(" ".to_string())
                }
            },
            
            Inline::HardBreak => {
                let mut element = DomNode::new("br");
                
                if self.config.add_css_classes {
                    element.add_attribute("class", "line-break");
                }
                
                DomChild::Element(element)
            },
        };

        Ok(dom_child)
    }

    /// Helper method to add position information as data attributes
    fn add_position_attribute(&self, node: &mut DomNode, position: &Option<Position>) -> Result<(), crate::error::MarkdownError> {
        if self.config.add_source_positions {
            if let Some(pos) = position {
                node.add_attribute("data-line", &pos.line.to_string());
                node.add_attribute("data-column", &pos.column.to_string());
                node.add_attribute("data-offset", &pos.offset.to_string());
            }
        }
        Ok(())
    }

    /// Helper method to add inline DOM content to a node
    fn add_inline_to_node(&self, node: &mut DomNode, inline_dom: DomChild) {
        node.add_child(inline_dom);
    }
}

/// Converts an Abstract Syntax Tree (AST) into a Document Object Model (DOM).
///
/// This is a convenience function that uses the default DomTransformer configuration.
///
/// # Arguments
///
/// * `ast_root` - The root document of the AST to convert
///
/// # Returns
///
/// Returns the root DomNode of the DOM tree
pub fn from_ast(ast_root: crate::ast::Document) -> Result<DomNode, crate::error::MarkdownError> {
    let transformer = DomTransformer::new_default();
    transformer.transform(&ast_root)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{utils::NodeBuilder, Inline, ListKind};
    use crate::lexer::Position;

    #[test]
    fn test_dom_transformer_basic() {
        // Create a simple AST document
        let text = NodeBuilder::text("Hello, World!".to_string());
        let paragraph = NodeBuilder::paragraph(vec![text], None);
        let document = NodeBuilder::document(vec![paragraph]);

        // Transform to DOM
        let transformer = DomTransformer::new_default();
        let dom_result = transformer.transform(&document);
        
        assert!(dom_result.is_ok());
        let dom = dom_result.unwrap();
        
        // Verify root structure
        assert_eq!(dom.tag, "div");
        assert_eq!(dom.get_attribute("class"), Some(&"markdown-content".to_string()));
        assert_eq!(dom.children.len(), 1);
        
        // Verify paragraph
        if let DomChild::Element(p) = &dom.children[0] {
            assert_eq!(p.tag, "p");
            assert_eq!(p.get_attribute("class"), Some(&"paragraph".to_string()));
            assert_eq!(p.children.len(), 1);
            
            // Verify text content
            if let DomChild::Text(text) = &p.children[0] {
                assert_eq!(text, "Hello, World!");
            } else {
                panic!("Expected text child");
            }
        } else {
            panic!("Expected paragraph element");
        }
    }

    #[test]
    fn test_dom_transformer_heading() {
        // Create heading AST
        let text = NodeBuilder::text("Test Heading".to_string());
        let heading = NodeBuilder::heading(2, vec![text], None);
        let document = NodeBuilder::document(vec![heading]);

        // Transform to DOM
        let transformer = DomTransformer::new_default();
        let dom = transformer.transform(&document).unwrap();
        
        // Verify heading
        if let DomChild::Element(h2) = &dom.children[0] {
            assert_eq!(h2.tag, "h2");
            assert_eq!(h2.get_attribute("class"), Some(&"heading".to_string()));
            
            if let DomChild::Text(text) = &h2.children[0] {
                assert_eq!(text, "Test Heading");
            } else {
                panic!("Expected text child in heading");
            }
        } else {
            panic!("Expected heading element");
        }
    }

    #[test]
    fn test_dom_transformer_emphasis() {
        // Create emphasis AST
        let text = NodeBuilder::text("emphasized".to_string());
        let emphasis = NodeBuilder::emphasis(false, vec![text]);
        let paragraph = NodeBuilder::paragraph(vec![emphasis], None);
        let document = NodeBuilder::document(vec![paragraph]);

        // Transform to DOM
        let transformer = DomTransformer::new_default();
        let dom = transformer.transform(&document).unwrap();
        
        // Navigate to emphasis element
        if let DomChild::Element(p) = &dom.children[0] {
            if let DomChild::Element(em) = &p.children[0] {
                assert_eq!(em.tag, "em");
                assert_eq!(em.get_attribute("class"), Some(&"emphasis".to_string()));
                
                if let DomChild::Text(text) = &em.children[0] {
                    assert_eq!(text, "emphasized");
                } else {
                    panic!("Expected text child in emphasis");
                }
            } else {
                panic!("Expected emphasis element");
            }
        } else {
            panic!("Expected paragraph element");
        }
    }

    #[test]
    fn test_dom_transformer_strong_emphasis() {
        // Create strong emphasis AST
        let text = NodeBuilder::text("strong".to_string());
        let strong = NodeBuilder::emphasis(true, vec![text]);
        let paragraph = NodeBuilder::paragraph(vec![strong], None);
        let document = NodeBuilder::document(vec![paragraph]);

        // Transform to DOM
        let transformer = DomTransformer::new_default();
        let dom = transformer.transform(&document).unwrap();
        
        // Navigate to strong element
        if let DomChild::Element(p) = &dom.children[0] {
            if let DomChild::Element(strong_elem) = &p.children[0] {
                assert_eq!(strong_elem.tag, "strong");
                assert_eq!(strong_elem.get_attribute("class"), Some(&"strong".to_string()));
            } else {
                panic!("Expected strong element");
            }
        } else {
            panic!("Expected paragraph element");
        }
    }

    #[test]
    fn test_dom_transformer_code_block() {
        // Create code block AST
        let code_block = NodeBuilder::code_block(
            Some("rust".to_string()),
            "fn main() {\n    println!(\"Hello\");\n}".to_string(),
            Some("rust".to_string()),
            None,
        );
        let document = NodeBuilder::document(vec![code_block]);

        // Transform to DOM
        let transformer = DomTransformer::new_default();
        let dom = transformer.transform(&document).unwrap();
        
        // Verify code block structure
        if let DomChild::Element(pre) = &dom.children[0] {
            assert_eq!(pre.tag, "pre");
            assert_eq!(pre.get_attribute("class"), Some(&"code-block".to_string()));
            
            if let DomChild::Element(code) = &pre.children[0] {
                assert_eq!(code.tag, "code");
                assert!(code.get_attribute("class").unwrap().contains("language-rust"));
                assert_eq!(code.get_attribute("data-info"), Some(&"rust".to_string()));
                
                if let DomChild::Text(text) = &code.children[0] {
                    assert_eq!(text, "fn main() {\n    println!(\"Hello\");\n}");
                } else {
                    panic!("Expected text child in code");
                }
            } else {
                panic!("Expected code element");
            }
        } else {
            panic!("Expected pre element");
        }
    }

    #[test]
    fn test_dom_transformer_list() {
        // Create list AST
        let text1 = NodeBuilder::text("Item 1".to_string());
        let text2 = NodeBuilder::text("Item 2".to_string());
        let paragraph1 = NodeBuilder::paragraph(vec![text1], None);
        let paragraph2 = NodeBuilder::paragraph(vec![text2], None);
        
        let item1 = NodeBuilder::list_item(vec![paragraph1], true, None);
        let item2 = NodeBuilder::list_item(vec![paragraph2], true, Some(true)); // checked task
        
        let list = NodeBuilder::list(
            ListKind::Bullet { marker: '*' },
            true,
            vec![item1, item2],
            None,
        );
        let document = NodeBuilder::document(vec![list]);

        // Transform to DOM
        let transformer = DomTransformer::new_default();
        let dom = transformer.transform(&document).unwrap();
        
        // Verify list structure
        if let DomChild::Element(ul) = &dom.children[0] {
            assert_eq!(ul.tag, "ul");
            assert!(ul.get_attribute("class").unwrap().contains("bullet-list"));
            assert!(ul.get_attribute("class").unwrap().contains("tight"));
            assert_eq!(ul.children.len(), 2);
            
            // Check first item
            if let DomChild::Element(li1) = &ul.children[0] {
                assert_eq!(li1.tag, "li");
                assert_eq!(li1.get_attribute("class"), Some(&"list-item".to_string()));
            } else {
                panic!("Expected first list item");
            }
            
            // Check second item (task list)
            if let DomChild::Element(li2) = &ul.children[1] {
                assert_eq!(li2.tag, "li");
                assert!(li2.get_attribute("class").unwrap().contains("task-list-item"));
                assert!(li2.get_attribute("class").unwrap().contains("checked"));
                
                // Should have checkbox as first child
                if let DomChild::Element(checkbox) = &li2.children[0] {
                    assert_eq!(checkbox.tag, "input");
                    assert_eq!(checkbox.get_attribute("type"), Some(&"checkbox".to_string()));
                    assert_eq!(checkbox.get_attribute("checked"), Some(&"checked".to_string()));
                } else {
                    panic!("Expected checkbox in task list item");
                }
            } else {
                panic!("Expected second list item");
            }
        } else {
            panic!("Expected ul element");
        }
    }

    #[test]
    fn test_dom_transformer_link() {
        // Create link AST
        let link_text = NodeBuilder::text("Click here".to_string());
        let link = NodeBuilder::link(
            vec![link_text],
            "https://example.com".to_string(),
            Some("Example Site".to_string()),
        );
        let paragraph = NodeBuilder::paragraph(vec![link], None);
        let document = NodeBuilder::document(vec![paragraph]);

        // Transform to DOM
        let transformer = DomTransformer::new_default();
        let dom = transformer.transform(&document).unwrap();
        
        // Navigate to link element
        if let DomChild::Element(p) = &dom.children[0] {
            if let DomChild::Element(a) = &p.children[0] {
                assert_eq!(a.tag, "a");
                assert_eq!(a.get_attribute("href"), Some(&"https://example.com".to_string()));
                assert_eq!(a.get_attribute("title"), Some(&"Example Site".to_string()));
                assert_eq!(a.get_attribute("class"), Some(&"link".to_string()));
                
                if let DomChild::Text(text) = &a.children[0] {
                    assert_eq!(text, "Click here");
                } else {
                    panic!("Expected text child in link");
                }
            } else {
                panic!("Expected link element");
            }
        } else {
            panic!("Expected paragraph element");
        }
    }

    #[test]
    fn test_dom_transformer_image() {
        // Create image AST
        let image = NodeBuilder::image(
            "Alt text".to_string(),
            "https://example.com/image.jpg".to_string(),
            Some("Image title".to_string()),
        );
        let paragraph = NodeBuilder::paragraph(vec![image], None);
        let document = NodeBuilder::document(vec![paragraph]);

        // Transform to DOM
        let transformer = DomTransformer::new_default();
        let dom = transformer.transform(&document).unwrap();
        
        // Navigate to image element
        if let DomChild::Element(p) = &dom.children[0] {
            if let DomChild::Element(img) = &p.children[0] {
                assert_eq!(img.tag, "img");
                assert_eq!(img.get_attribute("src"), Some(&"https://example.com/image.jpg".to_string()));
                assert_eq!(img.get_attribute("alt"), Some(&"Alt text".to_string()));
                assert_eq!(img.get_attribute("title"), Some(&"Image title".to_string()));
                assert_eq!(img.get_attribute("class"), Some(&"image".to_string()));
            } else {
                panic!("Expected image element");
            }
        } else {
            panic!("Expected paragraph element");
        }
    }

    #[test]
    fn test_dom_transformer_with_position_tracking() {
        // Create AST with position information
        let position = Position { line: 1, column: 1, offset: 0 };
        let text = NodeBuilder::text("Hello".to_string());
        let paragraph = NodeBuilder::paragraph(vec![text], Some(position));
        let document = NodeBuilder::document(vec![paragraph]);

        // Create transformer with position tracking enabled
        let config = DomConfig {
            add_source_positions: true,
            ..Default::default()
        };
        let transformer = DomTransformer::new(config);
        let dom = transformer.transform(&document).unwrap();
        
        // Verify position attributes
        if let DomChild::Element(p) = &dom.children[0] {
            assert_eq!(p.get_attribute("data-line"), Some(&"1".to_string()));
            assert_eq!(p.get_attribute("data-column"), Some(&"1".to_string()));
            assert_eq!(p.get_attribute("data-offset"), Some(&"0".to_string()));
        } else {
            panic!("Expected paragraph element");
        }
    }

    #[test]
    fn test_dom_transformer_custom_attributes() {
        // Create simple AST
        let text = NodeBuilder::text("Hello".to_string());
        let paragraph = NodeBuilder::paragraph(vec![text], None);
        let document = NodeBuilder::document(vec![paragraph]);

        // Create transformer with custom attributes
        let mut custom_attrs = HashMap::new();
        custom_attrs.insert("data-custom".to_string(), "value".to_string());
        
        let config = DomConfig {
            custom_attributes: custom_attrs,
            ..Default::default()
        };
        let transformer = DomTransformer::new(config);
        let dom = transformer.transform(&document).unwrap();
        
        // Verify custom attributes on root
        assert_eq!(dom.get_attribute("data-custom"), Some(&"value".to_string()));
    }

    #[test]
    fn test_dom_transformer_no_css_classes() {
        // Create simple AST
        let text = NodeBuilder::text("Hello".to_string());
        let paragraph = NodeBuilder::paragraph(vec![text], None);
        let document = NodeBuilder::document(vec![paragraph]);

        // Create transformer with CSS classes disabled
        let config = DomConfig {
            add_css_classes: false,
            ..Default::default()
        };
        let transformer = DomTransformer::new(config);
        let dom = transformer.transform(&document).unwrap();
        
        // Verify no CSS classes
        assert!(dom.get_attribute("class").is_none());
        
        if let DomChild::Element(p) = &dom.children[0] {
            assert!(p.get_attribute("class").is_none());
        } else {
            panic!("Expected paragraph element");
        }
    }

    #[test]
    fn test_dom_transformer_blockquote() {
        // Create blockquote AST
        let text = NodeBuilder::text("Quoted text".to_string());
        let paragraph = NodeBuilder::paragraph(vec![text], None);
        let blockquote = NodeBuilder::blockquote(vec![paragraph], None);
        let document = NodeBuilder::document(vec![blockquote]);

        // Transform to DOM
        let transformer = DomTransformer::new_default();
        let dom = transformer.transform(&document).unwrap();
        
        // Verify blockquote structure
        if let DomChild::Element(bq) = &dom.children[0] {
            assert_eq!(bq.tag, "blockquote");
            assert_eq!(bq.get_attribute("class"), Some(&"blockquote".to_string()));
            
            // Should contain the nested paragraph
            if let DomChild::Element(p) = &bq.children[0] {
                assert_eq!(p.tag, "p");
                
                if let DomChild::Text(text) = &p.children[0] {
                    assert_eq!(text, "Quoted text");
                } else {
                    panic!("Expected text in nested paragraph");
                }
            } else {
                panic!("Expected nested paragraph in blockquote");
            }
        } else {
            panic!("Expected blockquote element");
        }
    }

    #[test]
    fn test_dom_transformer_line_breaks() {
        // Create AST with line breaks
        let text1 = NodeBuilder::text("First line".to_string());
        let soft_break = Inline::SoftBreak;
        let text2 = NodeBuilder::text("Second line".to_string());
        let hard_break = Inline::HardBreak;
        let text3 = NodeBuilder::text("Third line".to_string());
        
        let paragraph = NodeBuilder::paragraph(
            vec![text1, soft_break, text2, hard_break, text3], 
            None
        );
        let document = NodeBuilder::document(vec![paragraph]);

        // Transform to DOM
        let transformer = DomTransformer::new_default();
        let dom = transformer.transform(&document).unwrap();
        
        // Verify line break handling
        if let DomChild::Element(p) = &dom.children[0] {
            assert_eq!(p.children.len(), 5);
            
            // Check soft break (should be space)
            if let DomChild::Text(space) = &p.children[1] {
                assert_eq!(space, " ");
            } else {
                panic!("Expected space for soft break");
            }
            
            // Check hard break (should be br element)
            if let DomChild::Element(br) = &p.children[3] {
                assert_eq!(br.tag, "br");
                assert_eq!(br.get_attribute("class"), Some(&"line-break".to_string()));
            } else {
                panic!("Expected br element for hard break");
            }
        } else {
            panic!("Expected paragraph element");
        }
    }
}