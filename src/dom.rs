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

/// Converts an Abstract Syntax Tree (AST) into a Document Object Model (DOM).
///
/// This function converts the AST representation into the DOM representation.
/// This is a placeholder implementation that will be properly implemented
/// in a later task.
///
/// # Arguments
///
/// * `ast_root` - The root document of the AST to convert
///
/// # Returns
///
/// Returns the root DomNode of the DOM tree
pub fn from_ast(_ast_root: crate::ast::Document) -> DomNode {
    // Placeholder implementation - will be properly implemented in task 5.2
    // For now, return a simple DOM structure for basic functionality
    let mut root = DomNode::new("div");
    root.add_attribute("class", "markdown-content");
    
    // Add a placeholder paragraph
    let mut paragraph = DomNode::new("p");
    paragraph.add_text_child("Placeholder content - AST to DOM conversion not yet implemented");
    root.add_element_child(paragraph);
    
    root
}