use crate::lexer::Position;
use std::collections::HashMap;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Represents a node in the Document Object Model (DOM).
/// This is a generic, intermediate representation that decouples the AST
/// from the final output format (e.g., HTML, XML).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct DomNode {
    /// The HTML tag name for this element (e.g., "h1", "p", "div")
    pub tag: String,
    /// HTML attributes as key-value pairs
    pub attributes: HashMap<String, String>,
    /// Child nodes (elements and text)
    pub children: Vec<DomChild>,
    /// Optional source position for debugging and tooling
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Option::is_none"))]
    pub source_position: Option<Position>,
}

/// Represents a child node in the DOM tree.
/// Can be either an element node or a text node.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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

    /// Validates the hierarchy structure (requirement 3.4)
    pub fn validate_hierarchy(&self, max_depth: Option<usize>) -> Result<(), String> {
        self.validate_hierarchy_recursive(0, max_depth)
    }

    /// Recursive helper for hierarchy validation
    fn validate_hierarchy_recursive(
        &self,
        current_depth: usize,
        max_depth: Option<usize>,
    ) -> Result<(), String> {
        if let Some(max) = max_depth {
            if current_depth > max {
                return Err(format!(
                    "Maximum nesting depth {} exceeded at depth {}",
                    max, current_depth
                ));
            }
        }

        for child in &self.children {
            if let DomChild::Element(element) = child {
                element.validate_hierarchy_recursive(current_depth + 1, max_depth)?;
            }
        }

        Ok(())
    }

    /// Serializes the DOM node to JSON (requirement 3.5)
    #[cfg(feature = "serde")]
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string(self)
    }

    /// Serializes the DOM node to pretty-printed JSON (requirement 3.5)
    #[cfg(feature = "serde")]
    pub fn to_json_pretty(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    /// Deserializes a DOM node from JSON (requirement 3.5)
    #[cfg(feature = "serde")]
    pub fn from_json(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }

    /// Serializes the DOM node to JSON without serde feature
    #[cfg(not(feature = "serde"))]
    pub fn to_json(&self) -> Result<String, crate::error::MarkdownError> {
        self.to_json_manual()
    }

    /// Manual JSON serialization implementation
    #[cfg(not(feature = "serde"))]
    fn to_json_manual(&self) -> Result<String, crate::error::MarkdownError> {
        let mut json = String::new();
        json.push('{');

        // Add tag
        json.push_str(&format!(
            "\"tag\":\"{}\"",
            self.escape_json_string(&self.tag)
        ));

        // Add attributes
        json.push_str(",\"attributes\":{");
        let mut first_attr = true;
        for (key, value) in &self.attributes {
            if !first_attr {
                json.push(',');
            }
            json.push_str(&format!(
                "\"{}\":\"{}\"",
                self.escape_json_string(key),
                self.escape_json_string(value)
            ));
            first_attr = false;
        }
        json.push('}');

        // Add children
        json.push_str(",\"children\":[");
        let mut first_child = true;
        for child in &self.children {
            if !first_child {
                json.push(',');
            }
            match child {
                DomChild::Element(element) => {
                    json.push_str(&element.to_json_manual()?);
                }
                DomChild::Text(text) => {
                    json.push_str(&format!(
                        "{{\"type\":\"text\",\"content\":\"{}\"}}",
                        self.escape_json_string(text)
                    ));
                }
            }
            first_child = false;
        }
        json.push(']');

        // Add source position if present
        if let Some(pos) = &self.source_position {
            json.push_str(&format!(
                ",\"source_position\":{{\"line\":{},\"column\":{},\"offset\":{}}}",
                pos.line, pos.column, pos.offset
            ));
        }

        json.push('}');
        Ok(json)
    }

    /// Escapes a string for JSON serialization
    #[cfg(not(feature = "serde"))]
    fn escape_json_string(&self, s: &str) -> String {
        s.chars()
            .map(|c| match c {
                '"' => "\\\"".to_string(),
                '\\' => "\\\\".to_string(),
                '\n' => "\\n".to_string(),
                '\r' => "\\r".to_string(),
                '\t' => "\\t".to_string(),
                c if c.is_control() => format!("\\u{:04x}", c as u32),
                c => c.to_string(),
            })
            .collect()
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
    /// Whether to maintain strict document hierarchy (requirement 3.4)
    pub maintain_hierarchy: bool,
    /// Whether to enable JSON serialization support (requirement 3.5)
    pub enable_serialization: bool,
    /// Custom CSS class prefix for generated classes
    pub css_class_prefix: Option<String>,
    /// Whether to add data attributes for element types
    pub add_element_type_attributes: bool,
    /// Custom namespace for data attributes
    pub data_attribute_namespace: Option<String>,
    /// Whether to preserve empty elements
    pub preserve_empty_elements: bool,
    /// Maximum nesting depth for hierarchy validation
    pub max_nesting_depth: Option<usize>,
}

impl Clone for DomConfig {
    fn clone(&self) -> Self {
        Self {
            add_source_positions: self.add_source_positions,
            custom_attributes: self.custom_attributes.clone(),
            preserve_whitespace: self.preserve_whitespace,
            add_css_classes: self.add_css_classes,
            maintain_hierarchy: self.maintain_hierarchy,
            enable_serialization: self.enable_serialization,
            css_class_prefix: self.css_class_prefix.clone(),
            add_element_type_attributes: self.add_element_type_attributes,
            data_attribute_namespace: self.data_attribute_namespace.clone(),
            preserve_empty_elements: self.preserve_empty_elements,
            max_nesting_depth: self.max_nesting_depth,
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
            maintain_hierarchy: true,
            enable_serialization: false,
            css_class_prefix: None,
            add_element_type_attributes: false,
            data_attribute_namespace: None,
            preserve_empty_elements: true,
            max_nesting_depth: None,
            element_processors: Vec::new(),
        }
    }
}

/// Builder pattern for DomConfig to make configuration easier
#[derive(Debug, Default)]
pub struct DomConfigBuilder {
    config: DomConfig,
}

impl DomConfigBuilder {
    /// Creates a new DomConfig builder
    pub fn new() -> Self {
        Self::default()
    }

    /// Enable or disable source position tracking
    pub fn with_source_positions(mut self, enabled: bool) -> Self {
        self.config.add_source_positions = enabled;
        self
    }

    /// Add custom attributes to all elements
    pub fn with_custom_attributes(mut self, attributes: HashMap<String, String>) -> Self {
        self.config.custom_attributes = attributes;
        self
    }

    /// Add a single custom attribute
    pub fn with_custom_attribute(mut self, key: &str, value: &str) -> Self {
        self.config
            .custom_attributes
            .insert(key.to_string(), value.to_string());
        self
    }

    /// Enable or disable whitespace preservation
    pub fn with_preserve_whitespace(mut self, enabled: bool) -> Self {
        self.config.preserve_whitespace = enabled;
        self
    }

    /// Enable or disable CSS class generation
    pub fn with_css_classes(mut self, enabled: bool) -> Self {
        self.config.add_css_classes = enabled;
        self
    }

    /// Set CSS class prefix
    pub fn with_css_class_prefix(mut self, prefix: &str) -> Self {
        self.config.css_class_prefix = Some(prefix.to_string());
        self
    }

    /// Enable or disable hierarchy maintenance (requirement 3.4)
    pub fn with_hierarchy_maintenance(mut self, enabled: bool) -> Self {
        self.config.maintain_hierarchy = enabled;
        self
    }

    /// Enable or disable JSON serialization support (requirement 3.5)
    pub fn with_serialization(mut self, enabled: bool) -> Self {
        self.config.enable_serialization = enabled;
        self
    }

    /// Enable or disable element type data attributes
    pub fn with_element_type_attributes(mut self, enabled: bool) -> Self {
        self.config.add_element_type_attributes = enabled;
        self
    }

    /// Set data attribute namespace
    pub fn with_data_attribute_namespace(mut self, namespace: &str) -> Self {
        self.config.data_attribute_namespace = Some(namespace.to_string());
        self
    }

    /// Enable or disable preservation of empty elements
    pub fn with_preserve_empty_elements(mut self, enabled: bool) -> Self {
        self.config.preserve_empty_elements = enabled;
        self
    }

    /// Set maximum nesting depth for hierarchy validation
    pub fn with_max_nesting_depth(mut self, depth: usize) -> Self {
        self.config.max_nesting_depth = Some(depth);
        self
    }

    /// Add an element processor
    pub fn with_element_processor(mut self, processor: Box<dyn ElementProcessor>) -> Self {
        self.config.element_processors.push(processor);
        self
    }

    /// Build the final DomConfig
    pub fn build(self) -> DomConfig {
        self.config
    }
}

/// Trait for custom element processing during DOM transformation
pub trait ElementProcessor: std::fmt::Debug {
    /// Process a DOM node after it has been created from an AST node
    fn process_element(&self, node: &mut DomNode, ast_node: &dyn std::any::Any);

    /// Get the name of this processor for debugging
    fn name(&self) -> &str;

    /// Get the priority of this processor (higher numbers run first)
    fn priority(&self) -> i32 {
        0
    }

    /// Check if this processor should run for the given node type
    fn should_process(&self, _node: &DomNode) -> bool {
        true
    }
}

/// A simple element processor that adds custom attributes based on element type
#[derive(Debug)]
pub struct AttributeProcessor {
    name: String,
    element_attributes: HashMap<String, HashMap<String, String>>,
}

impl AttributeProcessor {
    /// Creates a new attribute processor
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            element_attributes: HashMap::new(),
        }
    }

    /// Add attributes for a specific element type
    pub fn add_element_attributes(
        &mut self,
        element_type: &str,
        attributes: HashMap<String, String>,
    ) {
        self.element_attributes
            .insert(element_type.to_string(), attributes);
    }

    /// Add a single attribute for a specific element type
    pub fn add_element_attribute(&mut self, element_type: &str, key: &str, value: &str) {
        self.element_attributes
            .entry(element_type.to_string())
            .or_insert_with(HashMap::new)
            .insert(key.to_string(), value.to_string());
    }
}

impl ElementProcessor for AttributeProcessor {
    fn process_element(&self, node: &mut DomNode, _ast_node: &dyn std::any::Any) {
        if let Some(attributes) = self.element_attributes.get(&node.tag) {
            for (key, value) in attributes {
                node.add_attribute(key, value);
            }
        }
    }

    fn name(&self) -> &str {
        &self.name
    }
}

/// A processor that adds accessibility attributes
#[derive(Debug)]
pub struct AccessibilityProcessor {
    add_roles: bool,
    add_aria_labels: bool,
}

impl AccessibilityProcessor {
    /// Creates a new accessibility processor
    pub fn new(add_roles: bool, add_aria_labels: bool) -> Self {
        Self {
            add_roles,
            add_aria_labels,
        }
    }
}

impl ElementProcessor for AccessibilityProcessor {
    fn process_element(&self, node: &mut DomNode, _ast_node: &dyn std::any::Any) {
        if self.add_roles {
            match node.tag.as_str() {
                "h1" | "h2" | "h3" | "h4" | "h5" | "h6" => {
                    node.add_attribute("role", "heading");
                }
                "ul" | "ol" => {
                    node.add_attribute("role", "list");
                }
                "li" => {
                    node.add_attribute("role", "listitem");
                }
                "blockquote" => {
                    node.add_attribute("role", "blockquote");
                }
                _ => {}
            }
        }

        if self.add_aria_labels {
            match node.tag.as_str() {
                "h1" | "h2" | "h3" | "h4" | "h5" | "h6" => {
                    let level = node.tag[1..].to_string();
                    node.add_attribute("aria-level", &level);
                }
                "code" => {
                    let has_language_class = node
                        .get_attribute("class")
                        .map_or(false, |c| c.contains("language-"));
                    if has_language_class {
                        node.add_attribute("aria-label", "Code block");
                    }
                }
                _ => {}
            }
        }
    }

    fn name(&self) -> &str {
        "AccessibilityProcessor"
    }

    fn priority(&self) -> i32 {
        10 // Run after other processors
    }
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
    pub fn transform(
        &self,
        document: &crate::ast::Document,
    ) -> Result<DomNode, crate::error::MarkdownError> {
        let mut root = DomNode::new("div");

        // Add default attributes
        if self.config.add_css_classes {
            let class_name = if let Some(prefix) = &self.config.css_class_prefix {
                format!("{}-markdown-content", prefix)
            } else {
                "markdown-content".to_string()
            };
            root.add_attribute("class", &class_name);
        }

        // Add element type data attribute if enabled
        if self.config.add_element_type_attributes {
            let attr_name = if let Some(namespace) = &self.config.data_attribute_namespace {
                format!("data-{}-type", namespace)
            } else {
                "data-type".to_string()
            };
            root.add_attribute(&attr_name, "document");
        }

        // Add custom attributes
        for (key, value) in &self.config.custom_attributes {
            root.add_attribute(key, value);
        }

        // Transform all blocks in the document
        for block in &document.blocks {
            let dom_child = self.transform_block(block)?;
            if self.config.preserve_empty_elements
                || dom_child.has_children()
                || !dom_child.get_text_content().trim().is_empty()
            {
                root.add_element_child(dom_child);
            }
        }

        // Apply custom element processors (sorted by priority)
        let mut processors = self.config.element_processors.iter().collect::<Vec<_>>();
        processors.sort_by(|a, b| b.priority().cmp(&a.priority()));

        for processor in processors {
            // Check if processor should run before borrowing mutably
            let should_process = processor.should_process(&root);
            if should_process {
                processor.process_element(&mut root, document as &dyn std::any::Any);
            }
        }

        // Validate hierarchy if enabled (requirement 3.4)
        if self.config.maintain_hierarchy {
            root.validate_hierarchy(self.config.max_nesting_depth)
                .map_err(|e| crate::error::MarkdownError::Transform { message: e })?;
        }

        Ok(root)
    }

    /// Transforms a block AST node into a DOM node
    fn transform_block(
        &self,
        block: &crate::ast::Block,
    ) -> Result<DomNode, crate::error::MarkdownError> {
        use crate::ast::Block;

        let mut node = match block {
            Block::Heading {
                level,
                content,
                id,
                position,
            } => {
                let tag = format!("h{}", level);
                let mut heading = DomNode::new(&tag);

                self.add_css_class(&mut heading, "heading");
                self.add_element_type_attribute(&mut heading, "heading");

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
            }

            Block::Paragraph { content, position } => {
                let mut paragraph = DomNode::new("p");

                self.add_css_class(&mut paragraph, "paragraph");
                self.add_element_type_attribute(&mut paragraph, "paragraph");

                self.add_position_attribute(&mut paragraph, position)?;

                // Transform inline content
                for inline in content {
                    let inline_dom = self.transform_inline(inline)?;
                    self.add_inline_to_node(&mut paragraph, inline_dom);
                }

                paragraph
            }

            Block::CodeBlock {
                info,
                content,
                language,
                position,
            } => {
                let mut pre = DomNode::new("pre");
                let mut code = DomNode::new("code");

                self.add_css_class(&mut pre, "code-block");
                self.add_css_class(&mut code, "code");
                self.add_element_type_attribute(&mut pre, "code-block");

                // Add language class if available
                if let Some(lang) = language {
                    let lang_class = format!("language-{}", lang);
                    self.add_css_class(&mut code, &lang_class);
                }

                // Add info as data attribute if available
                if let Some(info_str) = info {
                    let attr_name = if let Some(namespace) = &self.config.data_attribute_namespace {
                        format!("data-{}-info", namespace)
                    } else {
                        "data-info".to_string()
                    };
                    code.add_attribute(&attr_name, info_str);
                }

                self.add_position_attribute(&mut pre, position)?;

                code.add_text_child(content);
                pre.add_element_child(code);
                pre
            }

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
            }

            Block::List {
                kind,
                tight,
                items,
                position,
            } => {
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
            }

            Block::ThematicBreak { position } => {
                let mut hr = DomNode::new("hr");

                if self.config.add_css_classes {
                    hr.add_attribute("class", "thematic-break");
                }

                self.add_position_attribute(&mut hr, position)?;
                hr
            }

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
            }
        };

        // Apply custom element processors
        for processor in &self.config.element_processors {
            let should_process = processor.should_process(&node);
            if should_process {
                processor.process_element(&mut node, block as &dyn std::any::Any);
            }
        }

        Ok(node)
    }

    /// Transforms an inline AST node into DOM content
    fn transform_inline(
        &self,
        inline: &crate::ast::Inline,
    ) -> Result<DomChild, crate::error::MarkdownError> {
        use crate::ast::Inline;

        let dom_child = match inline {
            Inline::Text(text) => DomChild::Text(text.clone()),

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
            }

            Inline::Code(code) => {
                let mut element = DomNode::new("code");

                if self.config.add_css_classes {
                    element.add_attribute("class", "inline-code");
                }

                element.add_text_child(code);
                DomChild::Element(element)
            }

            Inline::Link {
                text,
                destination,
                title,
            } => {
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
            }

            Inline::Image {
                alt,
                destination,
                title,
            } => {
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
            }

            Inline::HtmlInline(html) => {
                // For inline HTML, we create a span container with the raw HTML as text
                let mut element = DomNode::new("span");

                if self.config.add_css_classes {
                    element.add_attribute("class", "html-inline");
                }

                element.add_text_child(html);
                DomChild::Element(element)
            }

            Inline::SoftBreak => {
                if self.config.preserve_whitespace {
                    DomChild::Text("\n".to_string())
                } else {
                    DomChild::Text(" ".to_string())
                }
            }

            Inline::HardBreak => {
                let mut element = DomNode::new("br");

                if self.config.add_css_classes {
                    element.add_attribute("class", "line-break");
                }

                DomChild::Element(element)
            }
        };

        Ok(dom_child)
    }

    /// Helper method to add position information as data attributes
    fn add_position_attribute(
        &self,
        node: &mut DomNode,
        position: &Option<Position>,
    ) -> Result<(), crate::error::MarkdownError> {
        if self.config.add_source_positions {
            if let Some(pos) = position {
                let namespace = if let Some(ns) = &self.config.data_attribute_namespace {
                    format!("data-{}-", ns)
                } else {
                    "data-".to_string()
                };

                node.add_attribute(&format!("{}line", namespace), &pos.line.to_string());
                node.add_attribute(&format!("{}column", namespace), &pos.column.to_string());
                node.add_attribute(&format!("{}offset", namespace), &pos.offset.to_string());
            }
        }
        Ok(())
    }

    /// Helper method to add CSS class with optional prefix
    fn add_css_class(&self, node: &mut DomNode, class_name: &str) {
        if self.config.add_css_classes {
            let final_class = if let Some(prefix) = &self.config.css_class_prefix {
                format!("{}-{}", prefix, class_name)
            } else {
                class_name.to_string()
            };

            let existing_class = node.get_attribute("class").cloned().unwrap_or_default();
            let new_class = if existing_class.is_empty() {
                final_class
            } else {
                format!("{} {}", existing_class, final_class)
            };
            node.add_attribute("class", &new_class);
        }
    }

    /// Helper method to add element type data attribute
    fn add_element_type_attribute(&self, node: &mut DomNode, element_type: &str) {
        if self.config.add_element_type_attributes {
            let attr_name = if let Some(namespace) = &self.config.data_attribute_namespace {
                format!("data-{}-type", namespace)
            } else {
                "data-type".to_string()
            };
            node.add_attribute(&attr_name, element_type);
        }
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
    use crate::ast::{Inline, ListKind, utils::NodeBuilder};
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
        assert_eq!(
            dom.get_attribute("class"),
            Some(&"markdown-content".to_string())
        );
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
                assert_eq!(
                    strong_elem.get_attribute("class"),
                    Some(&"strong".to_string())
                );
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
                assert!(
                    code.get_attribute("class")
                        .unwrap()
                        .contains("language-rust")
                );
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
                assert!(
                    li2.get_attribute("class")
                        .unwrap()
                        .contains("task-list-item")
                );
                assert!(li2.get_attribute("class").unwrap().contains("checked"));

                // Should have checkbox as first child
                if let DomChild::Element(checkbox) = &li2.children[0] {
                    assert_eq!(checkbox.tag, "input");
                    assert_eq!(
                        checkbox.get_attribute("type"),
                        Some(&"checkbox".to_string())
                    );
                    assert_eq!(
                        checkbox.get_attribute("checked"),
                        Some(&"checked".to_string())
                    );
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
                assert_eq!(
                    a.get_attribute("href"),
                    Some(&"https://example.com".to_string())
                );
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
                assert_eq!(
                    img.get_attribute("src"),
                    Some(&"https://example.com/image.jpg".to_string())
                );
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
        let position = Position {
            line: 1,
            column: 1,
            offset: 0,
        };
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

        let paragraph =
            NodeBuilder::paragraph(vec![text1, soft_break, text2, hard_break, text3], None);
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

    #[test]
    fn test_dom_config_builder() {
        // Test the builder pattern for DomConfig
        let config = DomConfigBuilder::new()
            .with_source_positions(true)
            .with_css_class_prefix("custom")
            .with_element_type_attributes(true)
            .with_data_attribute_namespace("md")
            .with_hierarchy_maintenance(true)
            .with_serialization(true)
            .with_max_nesting_depth(10)
            .build();

        assert!(config.add_source_positions);
        assert_eq!(config.css_class_prefix, Some("custom".to_string()));
        assert!(config.add_element_type_attributes);
        assert_eq!(config.data_attribute_namespace, Some("md".to_string()));
        assert!(config.maintain_hierarchy);
        assert!(config.enable_serialization);
        assert_eq!(config.max_nesting_depth, Some(10));
    }

    #[test]
    fn test_dom_transformer_with_css_prefix() {
        // Create simple AST
        let text = NodeBuilder::text("Hello".to_string());
        let paragraph = NodeBuilder::paragraph(vec![text], None);
        let document = NodeBuilder::document(vec![paragraph]);

        // Create transformer with CSS prefix
        let config = DomConfigBuilder::new()
            .with_css_class_prefix("my-theme")
            .build();
        let transformer = DomTransformer::new(config);
        let dom = transformer.transform(&document).unwrap();

        // Verify CSS prefix is applied
        assert_eq!(
            dom.get_attribute("class"),
            Some(&"my-theme-markdown-content".to_string())
        );

        if let DomChild::Element(p) = &dom.children[0] {
            assert_eq!(
                p.get_attribute("class"),
                Some(&"my-theme-paragraph".to_string())
            );
        } else {
            panic!("Expected paragraph element");
        }
    }

    #[test]
    fn test_dom_transformer_with_element_type_attributes() {
        // Create simple AST
        let text = NodeBuilder::text("Hello".to_string());
        let paragraph = NodeBuilder::paragraph(vec![text], None);
        let document = NodeBuilder::document(vec![paragraph]);

        // Create transformer with element type attributes
        let config = DomConfigBuilder::new()
            .with_element_type_attributes(true)
            .with_data_attribute_namespace("markdown")
            .build();
        let transformer = DomTransformer::new(config);
        let dom = transformer.transform(&document).unwrap();

        // Verify element type attributes
        assert_eq!(
            dom.get_attribute("data-markdown-type"),
            Some(&"document".to_string())
        );

        if let DomChild::Element(p) = &dom.children[0] {
            assert_eq!(
                p.get_attribute("data-markdown-type"),
                Some(&"paragraph".to_string())
            );
        } else {
            panic!("Expected paragraph element");
        }
    }

    #[test]
    fn test_attribute_processor() {
        // Create a custom attribute processor
        let mut processor = AttributeProcessor::new("test-processor");
        processor.add_element_attribute("p", "data-custom", "paragraph-value");
        processor.add_element_attribute("h1", "data-custom", "heading-value");

        // Create simple AST
        let text = NodeBuilder::text("Hello".to_string());
        let paragraph = NodeBuilder::paragraph(vec![text], None);
        let document = NodeBuilder::document(vec![paragraph]);

        // Create transformer with the processor
        let config = DomConfigBuilder::new()
            .with_element_processor(Box::new(processor))
            .build();
        let transformer = DomTransformer::new(config);
        let dom = transformer.transform(&document).unwrap();

        // Verify custom attributes were added
        if let DomChild::Element(p) = &dom.children[0] {
            assert_eq!(
                p.get_attribute("data-custom"),
                Some(&"paragraph-value".to_string())
            );
        } else {
            panic!("Expected paragraph element");
        }
    }

    #[test]
    fn test_accessibility_processor() {
        // Create heading AST
        let text = NodeBuilder::text("Test Heading".to_string());
        let heading = NodeBuilder::heading(2, vec![text], None);
        let document = NodeBuilder::document(vec![heading]);

        // Create transformer with accessibility processor
        let processor = AccessibilityProcessor::new(true, true);
        let config = DomConfigBuilder::new()
            .with_element_processor(Box::new(processor))
            .build();
        let transformer = DomTransformer::new(config);
        let dom = transformer.transform(&document).unwrap();

        // Verify accessibility attributes
        if let DomChild::Element(h2) = &dom.children[0] {
            assert_eq!(h2.get_attribute("role"), Some(&"heading".to_string()));
            assert_eq!(h2.get_attribute("aria-level"), Some(&"2".to_string()));
        } else {
            panic!("Expected heading element");
        }
    }

    #[test]
    fn test_hierarchy_validation() {
        // Create a simple DOM node
        let mut root = DomNode::new("div");
        let mut child1 = DomNode::new("p");
        let mut child2 = DomNode::new("span");
        child2.add_text_child("text");
        child1.add_element_child(child2);
        root.add_element_child(child1);

        // Test successful validation
        assert!(root.validate_hierarchy(Some(5)).is_ok());

        // Test validation failure with low max depth
        assert!(root.validate_hierarchy(Some(1)).is_err());
    }

    #[cfg(not(feature = "serde"))]
    #[test]
    fn test_manual_json_serialization() {
        // Create simple DOM node
        let mut node = DomNode::new("p");
        node.add_attribute("class", "test");
        node.add_text_child("Hello, World!");

        // Test JSON serialization
        let json = node.to_json().unwrap();
        assert!(json.contains("\"tag\":\"p\""));
        assert!(json.contains("\"class\":\"test\""));
        assert!(json.contains("Hello, World!"));
    }

    // Additional comprehensive tests for task 5.4

    #[test]
    fn test_ast_to_dom_conversion_accuracy() {
        // Test comprehensive AST to DOM conversion for all node types (Requirement 3.1)

        // Create complex AST with multiple node types
        let text1 = NodeBuilder::text("Regular text ".to_string());
        let emphasis =
            NodeBuilder::emphasis(false, vec![NodeBuilder::text("emphasized".to_string())]);
        let strong = NodeBuilder::emphasis(true, vec![NodeBuilder::text("strong".to_string())]);
        let code_span = NodeBuilder::code_span("inline_code()".to_string());
        let link = NodeBuilder::link(
            vec![NodeBuilder::text("link text".to_string())],
            "https://example.com".to_string(),
            Some("Link Title".to_string()),
        );

        let paragraph =
            NodeBuilder::paragraph(vec![text1, emphasis, strong, code_span, link], None);

        let heading =
            NodeBuilder::heading(1, vec![NodeBuilder::text("Main Heading".to_string())], None);

        let code_block = NodeBuilder::code_block(
            Some("rust".to_string()),
            "fn main() { println!(\"Hello\"); }".to_string(),
            Some("rust".to_string()),
            None,
        );

        let nested_paragraph =
            NodeBuilder::paragraph(vec![NodeBuilder::text("Quoted content".to_string())], None);
        let blockquote = NodeBuilder::blockquote(vec![nested_paragraph], None);

        let list_item1 = NodeBuilder::list_item(
            vec![NodeBuilder::paragraph(
                vec![NodeBuilder::text("Item 1".to_string())],
                None,
            )],
            true,
            None,
        );
        let list_item2 = NodeBuilder::list_item(
            vec![NodeBuilder::paragraph(
                vec![NodeBuilder::text("Item 2".to_string())],
                None,
            )],
            true,
            Some(true), // checked task
        );
        let list = NodeBuilder::list(
            crate::ast::ListKind::Bullet { marker: '-' },
            true,
            vec![list_item1, list_item2],
            None,
        );

        let thematic_break = NodeBuilder::thematic_break(None);

        let document = NodeBuilder::document(vec![
            heading,
            paragraph,
            code_block,
            blockquote,
            list,
            thematic_break,
        ]);

        // Transform to DOM
        let transformer = DomTransformer::new_default();
        let dom = transformer.transform(&document).unwrap();

        // Verify all AST nodes were converted to appropriate DOM nodes
        assert_eq!(dom.tag, "div");
        assert_eq!(dom.children.len(), 6); // All blocks converted

        // Verify heading conversion
        if let DomChild::Element(h1) = &dom.children[0] {
            assert_eq!(h1.tag, "h1");
            assert_eq!(h1.children.len(), 1);
            if let DomChild::Text(text) = &h1.children[0] {
                assert_eq!(text, "Main Heading");
            }
        } else {
            panic!("Expected h1 element");
        }

        // Verify paragraph with inline elements
        if let DomChild::Element(p) = &dom.children[1] {
            assert_eq!(p.tag, "p");
            assert_eq!(p.children.len(), 5); // All inline elements converted

            // Check emphasis
            if let DomChild::Element(em) = &p.children[1] {
                assert_eq!(em.tag, "em");
            }

            // Check strong
            if let DomChild::Element(strong) = &p.children[2] {
                assert_eq!(strong.tag, "strong");
            }

            // Check code span
            if let DomChild::Element(code) = &p.children[3] {
                assert_eq!(code.tag, "code");
            }

            // Check link
            if let DomChild::Element(a) = &p.children[4] {
                assert_eq!(a.tag, "a");
                assert_eq!(
                    a.get_attribute("href"),
                    Some(&"https://example.com".to_string())
                );
                assert_eq!(a.get_attribute("title"), Some(&"Link Title".to_string()));
            }
        } else {
            panic!("Expected paragraph element");
        }

        // Verify code block
        if let DomChild::Element(pre) = &dom.children[2] {
            assert_eq!(pre.tag, "pre");
            if let DomChild::Element(code) = &pre.children[0] {
                assert_eq!(code.tag, "code");
                assert!(
                    code.get_attribute("class")
                        .unwrap()
                        .contains("language-rust")
                );
            }
        } else {
            panic!("Expected pre element");
        }

        // Verify blockquote with nested content
        if let DomChild::Element(bq) = &dom.children[3] {
            assert_eq!(bq.tag, "blockquote");
            assert_eq!(bq.children.len(), 1);
            if let DomChild::Element(nested_p) = &bq.children[0] {
                assert_eq!(nested_p.tag, "p");
            }
        } else {
            panic!("Expected blockquote element");
        }

        // Verify list with task items
        if let DomChild::Element(ul) = &dom.children[4] {
            assert_eq!(ul.tag, "ul");
            assert_eq!(ul.children.len(), 2);

            // Check task list item
            if let DomChild::Element(li) = &ul.children[1] {
                assert_eq!(li.tag, "li");
                assert!(
                    li.get_attribute("class")
                        .unwrap()
                        .contains("task-list-item")
                );
                // Should have checkbox as first child
                if let DomChild::Element(checkbox) = &li.children[0] {
                    assert_eq!(checkbox.tag, "input");
                    assert_eq!(
                        checkbox.get_attribute("type"),
                        Some(&"checkbox".to_string())
                    );
                }
            }
        } else {
            panic!("Expected ul element");
        }

        // Verify thematic break
        if let DomChild::Element(hr) = &dom.children[5] {
            assert_eq!(hr.tag, "hr");
        } else {
            panic!("Expected hr element");
        }
    }

    #[test]
    fn test_semantic_information_preservation() {
        // Test that all semantic information from AST is preserved in DOM (Requirement 3.2)

        let position = Position {
            line: 5,
            column: 10,
            offset: 42,
        };

        // Create AST with semantic information
        let heading = NodeBuilder::heading(
            3,
            vec![NodeBuilder::text("Heading with ID".to_string())],
            Some(position),
        );
        let mut heading_with_id = heading;
        if let crate::ast::Block::Heading { ref mut id, .. } = heading_with_id {
            *id = Some("custom-id".to_string());
        }

        let code_block = NodeBuilder::code_block(
            Some("javascript title=\"Example\"".to_string()),
            "console.log('test');".to_string(),
            Some("javascript".to_string()),
            Some(position),
        );

        let link = NodeBuilder::link(
            vec![NodeBuilder::text("Example Link".to_string())],
            "https://example.com/path?param=value".to_string(),
            Some("Detailed Link Title".to_string()),
        );
        let paragraph = NodeBuilder::paragraph(vec![link], Some(position));

        let document = NodeBuilder::document(vec![heading_with_id, code_block, paragraph]);

        // Transform with full semantic preservation
        let config = DomConfigBuilder::new()
            .with_source_positions(true)
            .with_element_type_attributes(true)
            .with_data_attribute_namespace("md")
            .build();
        let transformer = DomTransformer::new(config);
        let dom = transformer.transform(&document).unwrap();

        // Verify heading semantic information is preserved
        if let DomChild::Element(h3) = &dom.children[0] {
            assert_eq!(h3.tag, "h3");
            assert_eq!(h3.get_attribute("id"), Some(&"custom-id".to_string()));
            assert_eq!(h3.get_attribute("data-md-line"), Some(&"5".to_string()));
            assert_eq!(h3.get_attribute("data-md-column"), Some(&"10".to_string()));
            assert_eq!(h3.get_attribute("data-md-offset"), Some(&"42".to_string()));
            assert_eq!(
                h3.get_attribute("data-md-type"),
                Some(&"heading".to_string())
            );
        } else {
            panic!("Expected h3 element");
        }

        // Verify code block semantic information
        if let DomChild::Element(pre) = &dom.children[1] {
            if let DomChild::Element(code) = &pre.children[0] {
                assert!(
                    code.get_attribute("class")
                        .unwrap()
                        .contains("language-javascript")
                );
                assert_eq!(
                    code.get_attribute("data-md-info"),
                    Some(&"javascript title=\"Example\"".to_string())
                );
            }
        } else {
            panic!("Expected pre element");
        }

        // Verify link semantic information
        if let DomChild::Element(p) = &dom.children[2] {
            if let DomChild::Element(a) = &p.children[0] {
                assert_eq!(
                    a.get_attribute("href"),
                    Some(&"https://example.com/path?param=value".to_string())
                );
                assert_eq!(
                    a.get_attribute("title"),
                    Some(&"Detailed Link Title".to_string())
                );
                if let DomChild::Text(text) = &a.children[0] {
                    assert_eq!(text, "Example Link");
                }
            }
        } else {
            panic!("Expected paragraph element");
        }
    }

    #[test]
    fn test_attribute_handling_and_preservation() {
        // Test comprehensive attribute handling and preservation

        // Create DOM node with various attributes
        let mut node = DomNode::new("div");
        node.add_attribute("class", "initial-class");
        node.add_attribute("id", "test-id");
        node.add_attribute("data-custom", "custom-value");

        // Test attribute operations
        assert_eq!(
            node.get_attribute("class"),
            Some(&"initial-class".to_string())
        );
        assert_eq!(node.get_attribute("id"), Some(&"test-id".to_string()));
        assert_eq!(
            node.get_attribute("data-custom"),
            Some(&"custom-value".to_string())
        );
        assert!(node.has_attribute("class"));
        assert!(!node.has_attribute("nonexistent"));

        // Test attribute modification
        node.add_attribute("class", "updated-class");
        assert_eq!(
            node.get_attribute("class"),
            Some(&"updated-class".to_string())
        );

        // Test attribute removal
        let removed = node.remove_attribute("data-custom");
        assert_eq!(removed, Some("custom-value".to_string()));
        assert!(!node.has_attribute("data-custom"));

        // Test multiple attributes setting
        let mut attrs = HashMap::new();
        attrs.insert("role".to_string(), "button".to_string());
        attrs.insert("aria-label".to_string(), "Click me".to_string());
        node.set_attributes(attrs);

        assert_eq!(node.get_attribute("role"), Some(&"button".to_string()));
        assert_eq!(
            node.get_attribute("aria-label"),
            Some(&"Click me".to_string())
        );

        // Test attribute iteration
        let mut attr_count = 0;
        for (key, value) in node.attributes_iter() {
            attr_count += 1;
            assert!(!key.is_empty());
            assert!(!value.is_empty());
        }
        assert!(attr_count > 0);
    }

    #[test]
    fn test_custom_transformation_configurations() {
        // Test various custom transformation configurations

        let text = NodeBuilder::text("Test content".to_string());
        let paragraph = NodeBuilder::paragraph(vec![text], None);
        let document = NodeBuilder::document(vec![paragraph]);

        // Test configuration 1: Minimal configuration
        let config1 = DomConfigBuilder::new()
            .with_css_classes(false)
            .with_source_positions(false)
            .with_element_type_attributes(false)
            .build();
        let transformer1 = DomTransformer::new(config1);
        let dom1 = transformer1.transform(&document).unwrap();

        assert!(dom1.get_attribute("class").is_none());
        if let DomChild::Element(p) = &dom1.children[0] {
            assert!(p.get_attribute("class").is_none());
            assert!(p.get_attribute("data-type").is_none());
        }

        // Test configuration 2: Maximum configuration
        let mut custom_attrs = HashMap::new();
        custom_attrs.insert("data-version".to_string(), "1.0".to_string());
        custom_attrs.insert("data-theme".to_string(), "dark".to_string());

        let config2 = DomConfigBuilder::new()
            .with_css_classes(true)
            .with_css_class_prefix("custom")
            .with_source_positions(true)
            .with_element_type_attributes(true)
            .with_data_attribute_namespace("app")
            .with_custom_attributes(custom_attrs)
            .with_preserve_whitespace(true)
            .with_hierarchy_maintenance(true)
            .with_max_nesting_depth(5)
            .build();
        let transformer2 = DomTransformer::new(config2);
        let dom2 = transformer2.transform(&document).unwrap();

        assert_eq!(
            dom2.get_attribute("class"),
            Some(&"custom-markdown-content".to_string())
        );
        assert_eq!(dom2.get_attribute("data-version"), Some(&"1.0".to_string()));
        assert_eq!(dom2.get_attribute("data-theme"), Some(&"dark".to_string()));
        assert_eq!(
            dom2.get_attribute("data-app-type"),
            Some(&"document".to_string())
        );

        if let DomChild::Element(p) = &dom2.children[0] {
            assert_eq!(
                p.get_attribute("class"),
                Some(&"custom-paragraph".to_string())
            );
            assert_eq!(
                p.get_attribute("data-app-type"),
                Some(&"paragraph".to_string())
            );
        }

        // Test configuration 3: Whitespace preservation
        let soft_break = crate::ast::Inline::SoftBreak;
        let paragraph_with_breaks = NodeBuilder::paragraph(
            vec![
                NodeBuilder::text("Line 1".to_string()),
                soft_break,
                NodeBuilder::text("Line 2".to_string()),
            ],
            None,
        );
        let document_with_breaks = NodeBuilder::document(vec![paragraph_with_breaks]);

        let config3 = DomConfigBuilder::new()
            .with_preserve_whitespace(true)
            .build();
        let transformer3 = DomTransformer::new(config3);
        let dom3 = transformer3.transform(&document_with_breaks).unwrap();

        if let DomChild::Element(p) = &dom3.children[0] {
            if let DomChild::Text(whitespace) = &p.children[1] {
                assert_eq!(whitespace, "\n"); // Preserved as newline instead of space
            }
        }
    }

    #[test]
    fn test_document_structure_hierarchy_maintenance() {
        // Test that document structure and hierarchy are maintained (Requirement 3.4)

        // Create deeply nested structure
        let inner_text = NodeBuilder::text("Deep content".to_string());
        let inner_emphasis = NodeBuilder::emphasis(false, vec![inner_text]);
        let inner_paragraph = NodeBuilder::paragraph(vec![inner_emphasis], None);
        let inner_blockquote = NodeBuilder::blockquote(vec![inner_paragraph], None);

        let outer_text = NodeBuilder::text("Outer content".to_string());
        let outer_paragraph = NodeBuilder::paragraph(vec![outer_text], None);
        let outer_blockquote =
            NodeBuilder::blockquote(vec![outer_paragraph, inner_blockquote], None);

        let heading = NodeBuilder::heading(1, vec![NodeBuilder::text("Title".to_string())], None);
        let document = NodeBuilder::document(vec![heading, outer_blockquote]);

        // Transform with hierarchy validation
        let config = DomConfigBuilder::new()
            .with_hierarchy_maintenance(true)
            .with_max_nesting_depth(10)
            .build();
        let transformer = DomTransformer::new(config);
        let dom = transformer.transform(&document).unwrap();

        // Verify hierarchy is maintained
        assert_eq!(dom.children.len(), 2); // heading + blockquote

        // Navigate through the hierarchy
        if let DomChild::Element(outer_bq) = &dom.children[1] {
            assert_eq!(outer_bq.tag, "blockquote");
            assert_eq!(outer_bq.children.len(), 2); // outer paragraph + inner blockquote

            if let DomChild::Element(inner_bq) = &outer_bq.children[1] {
                assert_eq!(inner_bq.tag, "blockquote");
                assert_eq!(inner_bq.children.len(), 1); // inner paragraph

                if let DomChild::Element(inner_p) = &inner_bq.children[0] {
                    assert_eq!(inner_p.tag, "p");
                    assert_eq!(inner_p.children.len(), 1); // emphasis

                    if let DomChild::Element(em) = &inner_p.children[0] {
                        assert_eq!(em.tag, "em");
                        if let DomChild::Text(text) = &em.children[0] {
                            assert_eq!(text, "Deep content");
                        }
                    }
                }
            }
        }

        // Test hierarchy validation with depth limit
        let config_limited = DomConfigBuilder::new()
            .with_hierarchy_maintenance(true)
            .with_max_nesting_depth(2)
            .build();
        let transformer_limited = DomTransformer::new(config_limited);
        let result = transformer_limited.transform(&document);

        // Should fail due to depth limit
        assert!(result.is_err());
    }

    #[test]
    fn test_dom_serialization_support() {
        // Test DOM serialization capabilities (Requirement 3.5)

        let mut node = DomNode::new("article");
        node.add_attribute("class", "content");
        node.add_attribute("id", "main-article");

        let mut child = DomNode::new("p");
        child.add_text_child("Hello, World!");
        node.add_element_child(child);

        // Test manual JSON serialization (when serde feature is not available)
        #[cfg(not(feature = "serde"))]
        {
            let json = node.to_json().unwrap();

            // Verify JSON structure
            assert!(json.contains("\"tag\":\"article\""));
            assert!(json.contains("\"class\":\"content\""));
            assert!(json.contains("\"id\":\"main-article\""));
            assert!(json.contains("\"children\":["));
            assert!(json.contains("\"type\":\"text\""));
            assert!(json.contains("\"content\":\"Hello, World!\""));
        }

        // Test with source position
        let position = Position {
            line: 1,
            column: 1,
            offset: 0,
        };
        let mut node_with_pos = DomNode::new_with_position("div", position);
        node_with_pos.add_text_child("Content");

        #[cfg(not(feature = "serde"))]
        {
            let json_with_pos = node_with_pos.to_json().unwrap();
            assert!(json_with_pos.contains("\"source_position\""));
            assert!(json_with_pos.contains("\"line\":1"));
            assert!(json_with_pos.contains("\"column\":1"));
            assert!(json_with_pos.contains("\"offset\":0"));
        }
    }

    #[test]
    fn test_edge_cases_and_error_handling() {
        // Test edge cases and error handling in DOM transformation

        // Test empty document
        let empty_document = NodeBuilder::document(vec![]);
        let transformer = DomTransformer::new_default();
        let dom = transformer.transform(&empty_document).unwrap();
        assert_eq!(dom.children.len(), 0);

        // Test document with only empty elements
        let empty_paragraph = NodeBuilder::paragraph(vec![], None);
        let document_with_empty = NodeBuilder::document(vec![empty_paragraph]);
        let dom_with_empty = transformer.transform(&document_with_empty).unwrap();

        // Should preserve empty elements by default
        assert_eq!(dom_with_empty.children.len(), 1);

        // Test with preserve_empty_elements disabled
        let config = DomConfigBuilder::new()
            .with_preserve_empty_elements(false)
            .build();
        let transformer_no_empty = DomTransformer::new(config);
        let dom_no_empty = transformer_no_empty
            .transform(&document_with_empty)
            .unwrap();

        // Empty elements should be filtered out
        assert_eq!(dom_no_empty.children.len(), 0);

        // Test special characters in text content
        let special_text = NodeBuilder::text("Special chars: <>&\"'\n\t".to_string());
        let paragraph_special = NodeBuilder::paragraph(vec![special_text], None);
        let document_special = NodeBuilder::document(vec![paragraph_special]);
        let dom_special = transformer.transform(&document_special).unwrap();

        if let DomChild::Element(p) = &dom_special.children[0] {
            if let DomChild::Text(text) = &p.children[0] {
                assert_eq!(text, "Special chars: <>&\"'\n\t");
            }
        }
    }
}
