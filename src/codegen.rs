use crate::ast::Document;
use crate::dom::DomNode;
use crate::error::{MarkdownError, Result};
use std::collections::HashMap;

/// Configuration options for HTML output formatting
#[derive(Debug)]
pub struct OutputConfig {
    /// Whether to format output with indentation and line breaks
    pub pretty_print: bool,
    /// Number of spaces to use for indentation when pretty printing
    pub indent_size: usize,
    /// Whether to escape HTML special characters in text content
    pub escape_html: bool,
    /// Whether to add source map comments to the output
    pub add_source_maps: bool,
    /// Custom renderers for specific element types
    pub custom_renderers: HashMap<String, Box<dyn CustomRenderer>>,
    /// Whether to include XHTML-style self-closing tags
    pub xhtml_style: bool,
    /// Whether to add newlines after block elements
    pub add_newlines: bool,
    /// Custom DOCTYPE declaration (if any)
    pub doctype: Option<String>,
    /// Whether to wrap output in a document structure
    pub wrap_in_document: bool,
}

impl Clone for OutputConfig {
    fn clone(&self) -> Self {
        Self {
            pretty_print: self.pretty_print,
            indent_size: self.indent_size,
            escape_html: self.escape_html,
            add_source_maps: self.add_source_maps,
            xhtml_style: self.xhtml_style,
            add_newlines: self.add_newlines,
            doctype: self.doctype.clone(),
            wrap_in_document: self.wrap_in_document,
            // Note: Custom renderers cannot be cloned due to trait object limitations
            // In a real implementation, you might want to use a different approach
            custom_renderers: HashMap::new(),
        }
    }
}

impl Default for OutputConfig {
    fn default() -> Self {
        Self {
            pretty_print: false,
            indent_size: 2,
            escape_html: true,
            add_source_maps: false,
            custom_renderers: HashMap::new(),
            xhtml_style: false,
            add_newlines: true,
            doctype: None,
            wrap_in_document: false,
        }
    }
}

/// Builder pattern for OutputConfig to make configuration easier
#[derive(Debug, Default)]
pub struct OutputConfigBuilder {
    config: OutputConfig,
}

impl OutputConfigBuilder {
    /// Creates a new OutputConfig builder
    pub fn new() -> Self {
        Self::default()
    }

    /// Enable or disable pretty printing
    pub fn with_pretty_print(mut self, enabled: bool) -> Self {
        self.config.pretty_print = enabled;
        self
    }

    /// Set the indentation size for pretty printing
    pub fn with_indent_size(mut self, size: usize) -> Self {
        self.config.indent_size = size;
        self
    }

    /// Enable or disable HTML escaping
    pub fn with_html_escaping(mut self, enabled: bool) -> Self {
        self.config.escape_html = enabled;
        self
    }

    /// Enable or disable source map comments
    pub fn with_source_maps(mut self, enabled: bool) -> Self {
        self.config.add_source_maps = enabled;
        self
    }

    /// Enable or disable XHTML-style self-closing tags
    pub fn with_xhtml_style(mut self, enabled: bool) -> Self {
        self.config.xhtml_style = enabled;
        self
    }

    /// Enable or disable newlines after block elements
    pub fn with_newlines(mut self, enabled: bool) -> Self {
        self.config.add_newlines = enabled;
        self
    }

    /// Set a custom DOCTYPE declaration
    pub fn with_doctype(mut self, doctype: &str) -> Self {
        self.config.doctype = Some(doctype.to_string());
        self
    }

    /// Enable or disable wrapping output in document structure
    pub fn with_document_wrapper(mut self, enabled: bool) -> Self {
        self.config.wrap_in_document = enabled;
        self
    }

    /// Add a custom renderer for a specific element type
    pub fn with_custom_renderer(mut self, element_type: &str, renderer: Box<dyn CustomRenderer>) -> Self {
        self.config.custom_renderers.insert(element_type.to_string(), renderer);
        self
    }

    /// Build the final OutputConfig
    pub fn build(self) -> OutputConfig {
        self.config
    }
}

/// Trait for custom element renderers
pub trait CustomRenderer: std::fmt::Debug {
    /// Render a DOM node to HTML
    fn render(&self, node: &DomNode, writer: &mut HtmlWriter) -> Result<()>;
    
    /// Get the name of this renderer for debugging
    fn name(&self) -> &str;
    
    /// Get the priority of this renderer (higher numbers run first)
    fn priority(&self) -> i32 {
        0
    }
}

/// Low-level HTML writing utilities with proper escaping and formatting
#[derive(Debug)]
pub struct HtmlWriter {
    buffer: String,
    indent_level: usize,
    config: OutputConfig,
}

impl HtmlWriter {
    /// Creates a new HTML writer with the given configuration
    pub fn new(config: OutputConfig) -> Self {
        Self {
            buffer: String::new(),
            indent_level: 0,
            config,
        }
    }

    /// Writes an element with attributes and content
    pub fn write_element(&mut self, tag: &str, attributes: &HashMap<String, String>, content: &str) {
        self.write_indent();
        self.write_opening_tag(tag, attributes);
        
        if self.config.escape_html {
            self.buffer.push_str(&Self::escape_html(content));
        } else {
            self.buffer.push_str(content);
        }
        
        self.write_closing_tag(tag);
        self.write_newline_if_enabled();
    }

    /// Writes a self-closing element with attributes
    pub fn write_self_closing(&mut self, tag: &str, attributes: &HashMap<String, String>) {
        self.write_indent();
        self.buffer.push('<');
        self.buffer.push_str(tag);
        
        for (key, value) in attributes {
            self.buffer.push(' ');
            self.buffer.push_str(key);
            self.buffer.push_str("=\"");
            if self.config.escape_html {
                self.buffer.push_str(&Self::escape_html_attribute(value));
            } else {
                self.buffer.push_str(value);
            }
            self.buffer.push('"');
        }
        
        if self.config.xhtml_style {
            self.buffer.push_str(" />");
        } else {
            self.buffer.push('>');
        }
        
        self.write_newline_if_enabled();
    }

    /// Writes plain text content with optional escaping
    pub fn write_text(&mut self, text: &str) {
        if self.config.escape_html {
            self.buffer.push_str(&Self::escape_html(text));
        } else {
            self.buffer.push_str(text);
        }
    }

    /// Writes raw HTML content without escaping
    pub fn write_raw(&mut self, html: &str) {
        self.buffer.push_str(html);
    }

    /// Writes an opening tag with attributes
    pub fn write_opening_tag(&mut self, tag: &str, attributes: &HashMap<String, String>) {
        self.buffer.push('<');
        self.buffer.push_str(tag);
        
        for (key, value) in attributes {
            self.buffer.push(' ');
            self.buffer.push_str(key);
            self.buffer.push_str("=\"");
            if self.config.escape_html {
                self.buffer.push_str(&Self::escape_html_attribute(value));
            } else {
                self.buffer.push_str(value);
            }
            self.buffer.push('"');
        }
        
        self.buffer.push('>');
    }

    /// Writes a closing tag
    pub fn write_closing_tag(&mut self, tag: &str) {
        self.buffer.push_str("</");
        self.buffer.push_str(tag);
        self.buffer.push('>');
    }

    /// Increases indentation level
    pub fn indent(&mut self) {
        self.indent_level += 1;
    }

    /// Decreases indentation level
    pub fn dedent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }

    /// Writes indentation if pretty printing is enabled
    fn write_indent(&mut self) {
        if self.config.pretty_print {
            let spaces = " ".repeat(self.indent_level * self.config.indent_size);
            self.buffer.push_str(&spaces);
        }
    }

    /// Writes a newline if enabled
    fn write_newline_if_enabled(&mut self) {
        if self.config.add_newlines || self.config.pretty_print {
            self.buffer.push('\n');
        }
    }

    /// Escapes HTML special characters in text content
    pub fn escape_html(text: &str) -> String {
        text.chars()
            .map(|c| match c {
                '<' => "&lt;".to_string(),
                '>' => "&gt;".to_string(),
                '&' => "&amp;".to_string(),
                '"' => "&quot;".to_string(),
                '\'' => "&#x27;".to_string(),
                c => c.to_string(),
            })
            .collect()
    }

    /// Escapes HTML special characters in attribute values
    pub fn escape_html_attribute(text: &str) -> String {
        text.chars()
            .map(|c| match c {
                '<' => "&lt;".to_string(),
                '>' => "&gt;".to_string(),
                '&' => "&amp;".to_string(),
                '"' => "&quot;".to_string(),
                '\'' => "&#x27;".to_string(),
                '\n' => "&#10;".to_string(),
                '\r' => "&#13;".to_string(),
                '\t' => "&#9;".to_string(),
                c => c.to_string(),
            })
            .collect()
    }

    /// Gets the current buffer content
    pub fn get_output(&self) -> &str {
        &self.buffer
    }

    /// Clears the buffer
    pub fn clear(&mut self) {
        self.buffer.clear();
        self.indent_level = 0;
    }

    /// Gets the current buffer content and clears it
    pub fn take_output(&mut self) -> String {
        let output = self.buffer.clone();
        self.clear();
        output
    }
}

/// Main HTML generation engine supporting both AST and DOM input
#[derive(Debug)]
pub struct HtmlGenerator {
    config: OutputConfig,
}

impl HtmlGenerator {
    /// Creates a new HTML generator with the given configuration
    pub fn new(config: OutputConfig) -> Self {
        Self { config }
    }

    /// Creates a new HTML generator with default configuration
    pub fn new_default() -> Self {
        Self {
            config: OutputConfig::default(),
        }
    }

    /// Generates HTML from an AST document
    pub fn generate_from_ast(&self, document: &Document) -> Result<String> {
        // First convert AST to DOM, then generate HTML
        let dom = crate::dom::from_ast(document.clone())?;
        self.generate_from_dom(&dom)
    }

    /// Generates HTML from a DOM node
    pub fn generate_from_dom(&self, root: &DomNode) -> Result<String> {
        let mut writer = HtmlWriter::new(self.config.clone());
        
        // Add DOCTYPE if specified
        if let Some(doctype) = &self.config.doctype {
            writer.write_raw(&format!("<!DOCTYPE {}>\n", doctype));
        }

        // Wrap in document structure if enabled
        if self.config.wrap_in_document {
            writer.write_raw("<html>\n<head>\n<meta charset=\"UTF-8\">\n</head>\n<body>\n");
            writer.indent();
        }

        self.render_dom_node(root, &mut writer)?;

        if self.config.wrap_in_document {
            writer.dedent();
            writer.write_raw("\n</body>\n</html>");
        }

        Ok(writer.take_output())
    }

    /// Renders a DOM node and its children
    fn render_dom_node(&self, node: &DomNode, writer: &mut HtmlWriter) -> Result<()> {
        // Check for custom renderer first
        if let Some(renderer) = self.config.custom_renderers.get(&node.tag) {
            return renderer.render(node, writer);
        }

        // Handle self-closing tags
        if self.is_self_closing_tag(&node.tag) {
            writer.write_self_closing(&node.tag, &node.attributes);
            return Ok(());
        }

        // Write opening tag
        writer.write_indent();
        writer.write_opening_tag(&node.tag, &node.attributes);

        // Handle children
        if node.has_children() {
            let has_block_children = self.has_block_children(node);
            
            if has_block_children && writer.config.pretty_print {
                writer.buffer.push('\n');
                writer.indent();
            }

            for (i, child) in node.children_iter().enumerate() {
                match child {
                    crate::dom::DomChild::Element(element) => {
                        self.render_dom_node(element, writer)?;
                    }
                    crate::dom::DomChild::Text(text) => {
                        if has_block_children && writer.config.pretty_print {
                            writer.write_indent();
                        }
                        writer.write_text(text);
                        if has_block_children && writer.config.pretty_print && i < node.child_count() - 1 {
                            writer.buffer.push('\n');
                        }
                    }
                }
            }

            if has_block_children && writer.config.pretty_print {
                writer.dedent();
                writer.buffer.push('\n');
                writer.write_indent();
            }
        }

        // Write closing tag
        writer.write_closing_tag(&node.tag);
        
        if self.is_block_element(&node.tag) {
            writer.write_newline_if_enabled();
        }

        Ok(())
    }

    /// Checks if a tag is self-closing
    fn is_self_closing_tag(&self, tag: &str) -> bool {
        matches!(
            tag,
            "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input" | "link" | "meta" | "param" | "source" | "track" | "wbr"
        )
    }

    /// Checks if an element is a block-level element
    fn is_block_element(&self, tag: &str) -> bool {
        matches!(
            tag,
            "address" | "article" | "aside" | "blockquote" | "details" | "dialog" | "dd" | "div" | "dl" | "dt" | "fieldset" | "figcaption" | "figure" | "footer" | "form" | "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "header" | "hgroup" | "hr" | "li" | "main" | "nav" | "ol" | "p" | "pre" | "section" | "table" | "ul"
        )
    }

    /// Checks if a node has block-level children
    fn has_block_children(&self, node: &DomNode) -> bool {
        node.children_iter().any(|child| {
            if let crate::dom::DomChild::Element(element) = child {
                self.is_block_element(&element.tag)
            } else {
                false
            }
        })
    }
}

/// Validates HTML output for correctness and compliance
pub struct HtmlValidator;

impl HtmlValidator {
    /// Validates that HTML output is well-formed
    pub fn validate_well_formed(html: &str) -> Result<()> {
        // Basic validation - check for balanced tags
        let mut tag_stack = Vec::new();
        let mut chars = html.chars().peekable();
        
        #[allow(clippy::while_let_on_iterator)]
        while let Some(ch) = chars.next() {
            if ch == '<' {
                // Parse tag
                let mut tag_content = String::new();
                while let Some(ch) = chars.next() {
                    if ch == '>' {
                        break;
                    }
                    tag_content.push(ch);
                }
                
                if let Some(tag_name_part) = tag_content.strip_prefix('/') {
                    // Closing tag
                    let tag_name = tag_name_part.split_whitespace().next().unwrap_or("");
                    if let Some(expected) = tag_stack.pop() {
                        if expected != tag_name {
                            return Err(MarkdownError::Generation {
                                message: format!("Mismatched closing tag: expected {}, found {}", expected, tag_name),
                            });
                        }
                    } else {
                        return Err(MarkdownError::Generation {
                            message: format!("Unexpected closing tag: {}", tag_name),
                        });
                    }
                } else if !tag_content.ends_with('/') {
                    // Opening tag (not self-closing)
                    let tag_name = tag_content.split_whitespace().next().unwrap_or("");
                    if !HtmlGenerator::new_default().is_self_closing_tag(tag_name) {
                        tag_stack.push(tag_name.to_string());
                    }
                }
            }
        }
        
        if !tag_stack.is_empty() {
            return Err(MarkdownError::Generation {
                message: format!("Unclosed tags: {:?}", tag_stack),
            });
        }
        
        Ok(())
    }

    /// Validates HTML5 compliance
    pub fn validate_html5_compliance(_html: &str) -> Result<()> {
        // Placeholder for HTML5 validation
        // In a real implementation, this would check for HTML5-specific rules
        Ok(())
    }
}

/// Generates the target code (HTML) from the intermediate representation (DOM).
///
/// This function walks the DOM tree and serializes it into an HTML string.
/// This corresponds to the "Target Code Generation" phase of a compiler,
/// and is the final step in the conversion process.
pub fn generate_html(dom_root: DomNode) -> String {
    let generator = HtmlGenerator::new_default();
    generator.generate_from_dom(&dom_root).unwrap_or_else(|_| {
        // Fallback to simple text representation on error
        dom_root.get_text_content()
    })
}
