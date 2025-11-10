use crate::dom::DomNode;
use crate::error::{MarkdownError, Result};
use crate::parser::ast::Document;
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
    /// HTML escaping mode for different contexts
    pub html_escaping_mode: HtmlEscapingMode,
    /// Whether to enforce HTML5 compliance
    pub html5_compliance: bool,
    /// Whether to add CommonMark-specific attributes
    pub add_commonmark_attributes: bool,
    /// Whether to validate HTML structure during generation
    pub validate_html_structure: bool,
}

/// HTML escaping modes for different contexts

#[derive(Default, Debug, Clone, PartialEq)]
pub enum HtmlEscapingMode {
    /// Standard HTML escaping for text content
    #[default]
    Standard,
    /// Aggressive escaping for untrusted content
    Aggressive,
    /// Minimal escaping (only essential characters)
    Minimal,
    /// Custom escaping with user-defined character mappings
    Custom(HashMap<char, String>),
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
            html_escaping_mode: self.html_escaping_mode.clone(),
            html5_compliance: self.html5_compliance,
            add_commonmark_attributes: self.add_commonmark_attributes,
            validate_html_structure: self.validate_html_structure,
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
            html_escaping_mode: HtmlEscapingMode::Standard,
            html5_compliance: true,
            add_commonmark_attributes: false,
            validate_html_structure: false,
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

    /// Set HTML escaping mode
    pub fn with_html_escaping_mode(mut self, mode: HtmlEscapingMode) -> Self {
        self.config.html_escaping_mode = mode;
        self
    }

    /// Enable or disable HTML5 compliance
    pub fn with_html5_compliance(mut self, enabled: bool) -> Self {
        self.config.html5_compliance = enabled;
        self
    }

    /// Enable or disable CommonMark-specific attributes
    pub fn with_commonmark_attributes(mut self, enabled: bool) -> Self {
        self.config.add_commonmark_attributes = enabled;
        self
    }

    /// Enable or disable HTML structure validation
    pub fn with_html_structure_validation(mut self, enabled: bool) -> Self {
        self.config.validate_html_structure = enabled;
        self
    }

    /// Add a custom renderer for a specific element type
    pub fn with_custom_renderer(
        mut self,
        element_type: &str,
        renderer: Box<dyn CustomRenderer>,
    ) -> Self {
        self.config
            .custom_renderers
            .insert(element_type.to_string(), renderer);
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
    pub fn write_element(
        &mut self,
        tag: &str,
        attributes: &HashMap<String, String>,
        content: &str,
    ) {
        self.write_indent();
        self.write_opening_tag(tag, attributes);

        self.buffer.push_str(&self.escape_html(content));

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
            self.buffer.push_str(&self.escape_html_attribute(value));
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
        self.buffer.push_str(&self.escape_html(text));
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
            self.buffer.push_str(&self.escape_html_attribute(value));
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

    /// Escapes HTML special characters in text content based on the configured mode
    pub fn escape_html(&self, text: &str) -> String {
        if !self.config.escape_html {
            return text.to_string();
        }

        match &self.config.html_escaping_mode {
            HtmlEscapingMode::Standard => Self::escape_html_standard(text),
            HtmlEscapingMode::Aggressive => Self::escape_html_aggressive(text),
            HtmlEscapingMode::Minimal => Self::escape_html_minimal(text),
            HtmlEscapingMode::Custom(mappings) => Self::escape_html_custom(text, mappings),
        }
    }

    /// Standard HTML escaping for text content
    pub fn escape_html_standard(text: &str) -> String {
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

    /// Aggressive HTML escaping for untrusted content
    pub fn escape_html_aggressive(text: &str) -> String {
        text.chars()
            .map(|c| match c {
                '<' => "&lt;".to_string(),
                '>' => "&gt;".to_string(),
                '&' => "&amp;".to_string(),
                '"' => "&quot;".to_string(),
                '\'' => "&#x27;".to_string(),
                '/' => "&#x2F;".to_string(),
                '`' => "&#x60;".to_string(),
                '=' => "&#x3D;".to_string(),
                c if c.is_control() && c != '\n' && c != '\r' && c != '\t' => {
                    format!("&#x{:X};", c as u32)
                }
                c => c.to_string(),
            })
            .collect()
    }

    /// Minimal HTML escaping (only essential characters)
    pub fn escape_html_minimal(text: &str) -> String {
        text.chars()
            .map(|c| match c {
                '<' => "&lt;".to_string(),
                '>' => "&gt;".to_string(),
                '&' => "&amp;".to_string(),
                c => c.to_string(),
            })
            .collect()
    }

    /// Custom HTML escaping with user-defined mappings
    pub fn escape_html_custom(text: &str, mappings: &HashMap<char, String>) -> String {
        text.chars()
            .map(|c| {
                mappings.get(&c).cloned().unwrap_or_else(|| {
                    // Fall back to standard escaping for unmapped essential characters
                    match c {
                        '<' => "&lt;".to_string(),
                        '>' => "&gt;".to_string(),
                        '&' => "&amp;".to_string(),
                        c => c.to_string(),
                    }
                })
            })
            .collect()
    }

    /// Escapes HTML special characters in attribute values
    pub fn escape_html_attribute(&self, text: &str) -> String {
        if !self.config.escape_html {
            return text.to_string();
        }

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

        // Add DOCTYPE if specified or if HTML5 compliance is enabled
        if let Some(doctype) = &self.config.doctype {
            writer.write_raw(&format!("<!DOCTYPE {}>\n", doctype));
        } else if self.config.wrap_in_document && self.config.html5_compliance {
            writer.write_raw("<!DOCTYPE html>\n");
        }

        // Wrap in document structure if enabled
        if self.config.wrap_in_document {
            if self.config.html5_compliance {
                writer.write_raw("<html lang=\"en\">\n<head>\n<meta charset=\"UTF-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n</head>\n<body>\n");
            } else {
                writer.write_raw("<html>\n<head>\n<meta charset=\"UTF-8\">\n</head>\n<body>\n");
            }
            writer.indent();
        }

        self.render_dom_node(root, &mut writer)?;

        if self.config.wrap_in_document {
            writer.dedent();
            writer.write_raw("\n</body>\n</html>");
        }

        let html_output = writer.take_output();

        // Validate output if validation is enabled
        if self.config.validate_html_structure {
            HtmlValidator::validate_comprehensive(&html_output, &self.config)?;
        }

        Ok(html_output)
    }

    /// Renders a DOM node and its children
    fn render_dom_node(&self, node: &DomNode, writer: &mut HtmlWriter) -> Result<()> {
        // Validate HTML5 compliance if enabled
        if writer.config.validate_html_structure {
            self.validate_html5_tag(&node.tag)?;
        }

        // Check for custom renderer first
        if let Some(renderer) = self.config.custom_renderers.get(&node.tag) {
            return renderer.render(node, writer);
        }

        // Create a mutable copy of the node to add CommonMark attributes
        let mut enhanced_node = node.clone();

        // Add CommonMark-specific attributes
        if writer.config.add_commonmark_attributes {
            let element_type = self.determine_commonmark_element_type(&enhanced_node);
            self.add_commonmark_attributes(&mut enhanced_node, &element_type);
        }

        // Handle self-closing tags
        if self.is_self_closing_tag(&enhanced_node.tag) {
            writer.write_self_closing(&enhanced_node.tag, &enhanced_node.attributes);
            return Ok(());
        }

        // Write opening tag
        writer.write_indent();
        writer.write_opening_tag(&enhanced_node.tag, &enhanced_node.attributes);

        // Handle children
        if enhanced_node.has_children() {
            let has_block_children = self.has_block_children(&enhanced_node);

            if has_block_children && writer.config.pretty_print {
                writer.buffer.push('\n');
                writer.indent();
            }

            for (i, child) in enhanced_node.children_iter().enumerate() {
                match child {
                    crate::dom::DomChild::Element(element) => {
                        self.render_dom_node(element, writer)?;
                    }
                    crate::dom::DomChild::Text(text) => {
                        if has_block_children && writer.config.pretty_print {
                            writer.write_indent();
                        }
                        writer.write_text(text);
                        if has_block_children
                            && writer.config.pretty_print
                            && i < enhanced_node.child_count() - 1
                        {
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
        writer.write_closing_tag(&enhanced_node.tag);

        if self.is_block_element(&enhanced_node.tag) {
            writer.write_newline_if_enabled();
        }

        Ok(())
    }

    /// Validates HTML5 tag name compliance
    fn validate_html5_tag(&self, tag: &str) -> Result<()> {
        if !self.config.html5_compliance {
            return Ok(());
        }

        // HTML5 tag name rules
        if tag.is_empty() {
            return Err(MarkdownError::Generation {
                message: "Empty tag name is not allowed".to_string(),
            });
        }

        if !tag.chars().next().unwrap().is_ascii_alphabetic() {
            return Err(MarkdownError::Generation {
                message: format!("Tag name '{}' must start with a letter", tag),
            });
        }

        if !tag.chars().all(|c| c.is_ascii_alphanumeric() || c == '-') {
            return Err(MarkdownError::Generation {
                message: format!("Tag name '{}' contains invalid characters", tag),
            });
        }

        // Check for obsolete HTML5 elements
        if matches!(
            tag,
            "acronym"
                | "applet"
                | "basefont"
                | "big"
                | "center"
                | "dir"
                | "font"
                | "frame"
                | "frameset"
                | "isindex"
                | "noframes"
                | "s"
                | "strike"
                | "tt"
                | "u"
        ) {
            return Err(MarkdownError::Generation {
                message: format!("Tag '{}' is obsolete in HTML5", tag),
            });
        }

        Ok(())
    }

    /// Adds CommonMark-specific attributes to elements
    fn add_commonmark_attributes(&self, node: &mut DomNode, element_type: &str) {
        if !self.config.add_commonmark_attributes {
            return;
        }

        // Add data-commonmark-type attribute
        node.add_attribute("data-commonmark-type", element_type);

        // Add specific attributes based on element type
        match element_type {
            "heading" => {
                if let Some(level) = node.tag.chars().nth(1).and_then(|c| c.to_digit(10)) {
                    node.add_attribute("data-heading-level", &level.to_string());
                }
            }
            "list" => {
                if node.tag == "ol" {
                    node.add_attribute("data-list-type", "ordered");
                } else if node.tag == "ul" {
                    node.add_attribute("data-list-type", "bullet");
                }
            }
            "code-block" => {
                node.add_attribute("data-code-block", "true");
            }
            "emphasis" => {
                if node.tag == "strong" {
                    node.add_attribute("data-emphasis-type", "strong");
                } else if node.tag == "em" {
                    node.add_attribute("data-emphasis-type", "emphasis");
                }
            }
            _ => {}
        }
    }

    /// Determines the CommonMark element type for a DOM node
    fn determine_commonmark_element_type(&self, node: &DomNode) -> String {
        match node.tag.as_str() {
            "h1" | "h2" | "h3" | "h4" | "h5" | "h6" => "heading".to_string(),
            "p" => "paragraph".to_string(),
            "pre" => "code-block".to_string(),
            "blockquote" => "blockquote".to_string(),
            "ul" | "ol" => "list".to_string(),
            "li" => "list-item".to_string(),
            "hr" => "thematic-break".to_string(),
            "em" | "strong" => "emphasis".to_string(),
            "code" => {
                // Distinguish between inline code and code blocks
                if node
                    .get_attribute("class")
                    .is_some_and(|c| c.contains("inline-code"))
                {
                    "inline-code".to_string()
                } else {
                    "code".to_string()
                }
            }
            "a" => "link".to_string(),
            "img" => "image".to_string(),
            "br" => "line-break".to_string(),
            _ => "unknown".to_string(),
        }
    }

    /// Checks if a tag is self-closing according to HTML5 specification
    fn is_self_closing_tag(&self, tag: &str) -> bool {
        if self.config.html5_compliance {
            // HTML5 void elements (cannot have closing tags)
            matches!(
                tag,
                "area"
                    | "base"
                    | "br"
                    | "col"
                    | "embed"
                    | "hr"
                    | "img"
                    | "input"
                    | "link"
                    | "meta"
                    | "param"
                    | "source"
                    | "track"
                    | "wbr"
            )
        } else {
            // Legacy HTML compatibility
            matches!(
                tag,
                "area"
                    | "base"
                    | "br"
                    | "col"
                    | "embed"
                    | "hr"
                    | "img"
                    | "input"
                    | "link"
                    | "meta"
                    | "param"
                    | "source"
                    | "track"
                    | "wbr"
                    | "frame"
                    | "isindex"
            )
        }
    }

    /// Checks if an element is a block-level element according to HTML5
    fn is_block_element(&self, tag: &str) -> bool {
        if self.config.html5_compliance {
            // HTML5 block-level elements
            matches!(
                tag,
                "address"
                    | "article"
                    | "aside"
                    | "blockquote"
                    | "details"
                    | "dialog"
                    | "dd"
                    | "div"
                    | "dl"
                    | "dt"
                    | "fieldset"
                    | "figcaption"
                    | "figure"
                    | "footer"
                    | "form"
                    | "h1"
                    | "h2"
                    | "h3"
                    | "h4"
                    | "h5"
                    | "h6"
                    | "header"
                    | "hgroup"
                    | "hr"
                    | "li"
                    | "main"
                    | "nav"
                    | "ol"
                    | "p"
                    | "pre"
                    | "section"
                    | "table"
                    | "ul"
                    | "canvas"
                    | "noscript"
                    | "output"
                    | "video"
                    | "audio"
            )
        } else {
            // Legacy HTML block elements
            matches!(
                tag,
                "address"
                    | "blockquote"
                    | "dd"
                    | "div"
                    | "dl"
                    | "dt"
                    | "fieldset"
                    | "form"
                    | "h1"
                    | "h2"
                    | "h3"
                    | "h4"
                    | "h5"
                    | "h6"
                    | "hr"
                    | "li"
                    | "ol"
                    | "p"
                    | "pre"
                    | "table"
                    | "ul"
                    | "center"
                    | "dir"
                    | "menu"
            )
        }
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

                // Skip DOCTYPE declarations and comments
                if tag_content.starts_with('!') || tag_content.starts_with('?') {
                    continue;
                }

                if let Some(tag_name_part) = tag_content.strip_prefix('/') {
                    // Closing tag
                    let tag_name = tag_name_part.split_whitespace().next().unwrap_or("");
                    if let Some(expected) = tag_stack.pop() {
                        if expected != tag_name {
                            return Err(MarkdownError::Generation {
                                message: format!(
                                    "Mismatched closing tag: expected {}, found {}",
                                    expected, tag_name
                                ),
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
                    let config = OutputConfig::default();
                    let generator = HtmlGenerator::new(config);
                    if !generator.is_self_closing_tag(tag_name) {
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
    pub fn validate_html5_compliance(html: &str) -> Result<()> {
        // Check for obsolete HTML elements
        let obsolete_elements = [
            "acronym", "applet", "basefont", "big", "center", "dir", "font", "frame", "frameset",
            "isindex", "noframes", "s", "strike", "tt", "u",
        ];

        for element in &obsolete_elements {
            if html.contains(&format!("<{}", element)) {
                return Err(MarkdownError::Generation {
                    message: format!("Obsolete HTML5 element found: {}", element),
                });
            }
        }

        // Check for proper DOCTYPE (if document wrapper is used)
        if html.contains("<html>") && !html.contains("<!DOCTYPE html>") {
            return Err(MarkdownError::Generation {
                message: "HTML5 documents should include <!DOCTYPE html>".to_string(),
            });
        }

        // Validate void elements don't have closing tags
        let void_elements = [
            "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param",
            "source", "track", "wbr",
        ];

        for element in &void_elements {
            let closing_tag = format!("</{}>", element);
            if html.contains(&closing_tag) {
                return Err(MarkdownError::Generation {
                    message: format!("Void element '{}' should not have a closing tag", element),
                });
            }
        }

        Ok(())
    }

    /// Validates CommonMark HTML output compliance
    pub fn validate_commonmark_compliance(html: &str) -> Result<()> {
        // Check that headings use proper h1-h6 tags
        if html.contains("<h7") || html.contains("<h8") || html.contains("<h9") {
            return Err(MarkdownError::Generation {
                message: "Invalid heading level found (only h1-h6 are valid)".to_string(),
            });
        }

        // Check that code blocks use pre > code structure
        if html.contains("<code") && html.contains("language-") {
            // This is a simplified check - in practice, you'd want more sophisticated parsing
            if !html.contains("<pre><code") {
                return Err(MarkdownError::Generation {
                    message: "Code blocks should use <pre><code> structure".to_string(),
                });
            }
        }

        // Check that list items are properly nested
        if html.contains("<li") && !html.contains("<ul") && !html.contains("<ol") {
            return Err(MarkdownError::Generation {
                message: "List items must be contained within ul or ol elements".to_string(),
            });
        }

        Ok(())
    }

    /// Comprehensive validation combining all checks
    pub fn validate_comprehensive(html: &str, config: &OutputConfig) -> Result<()> {
        // Always check well-formedness
        Self::validate_well_formed(html)?;

        // Check HTML5 compliance if enabled
        if config.html5_compliance {
            Self::validate_html5_compliance(html)?;
        }

        // Always check CommonMark compliance
        Self::validate_commonmark_compliance(html)?;

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

#[cfg(test)]
mod commonmark_compliance_tests {
    use super::*;
    use crate::dom::DomNode;

    #[test]
    fn test_html5_compliance_mode() {
        // Test HTML5 compliance features
        let config = OutputConfigBuilder::new()
            .with_html5_compliance(true)
            .with_html_structure_validation(true)
            .with_document_wrapper(true)
            .build();

        let generator = HtmlGenerator::new(config);

        // Create a simple DOM structure
        let mut root = DomNode::new("div");
        let mut paragraph = DomNode::new("p");
        paragraph.add_text_child("Hello, World!");
        root.add_element_child(paragraph);

        let html = generator.generate_from_dom(&root).unwrap();

        // Should include HTML5 DOCTYPE
        assert!(html.contains("<!DOCTYPE html>"));
        // Should include HTML5 meta viewport
        assert!(html.contains("viewport"));
        // Should include lang attribute
        assert!(html.contains("lang=\"en\""));
    }

    #[test]
    fn test_html_escaping_modes() {
        let mut root = DomNode::new("p");
        root.add_text_child("Test <script>alert('xss')</script> & \"quotes\"");

        // Test standard escaping
        let config_standard = OutputConfigBuilder::new()
            .with_html_escaping_mode(HtmlEscapingMode::Standard)
            .build();
        let generator_standard = HtmlGenerator::new(config_standard);
        let html_standard = generator_standard.generate_from_dom(&root).unwrap();

        assert!(html_standard.contains("&lt;script&gt;"));
        assert!(html_standard.contains("&amp;"));
        assert!(html_standard.contains("&quot;"));

        // Test aggressive escaping
        let config_aggressive = OutputConfigBuilder::new()
            .with_html_escaping_mode(HtmlEscapingMode::Aggressive)
            .build();
        let generator_aggressive = HtmlGenerator::new(config_aggressive);
        let html_aggressive = generator_aggressive.generate_from_dom(&root).unwrap();

        assert!(html_aggressive.contains("&lt;script&gt;"));
        assert!(html_aggressive.contains("&amp;"));
        assert!(html_aggressive.contains("&quot;"));

        // Test minimal escaping
        let config_minimal = OutputConfigBuilder::new()
            .with_html_escaping_mode(HtmlEscapingMode::Minimal)
            .build();
        let generator_minimal = HtmlGenerator::new(config_minimal);
        let html_minimal = generator_minimal.generate_from_dom(&root).unwrap();

        assert!(html_minimal.contains("&lt;script&gt;"));
        assert!(html_minimal.contains("&amp;"));
        // Minimal mode doesn't escape quotes
        assert!(html_minimal.contains("\"quotes\""));
    }

    #[test]
    fn test_custom_html_escaping() {
        let mut custom_mappings = HashMap::new();
        custom_mappings.insert('<', "&LT;".to_string());
        custom_mappings.insert('>', "&GT;".to_string());
        custom_mappings.insert('&', "&AMP;".to_string());

        let config = OutputConfigBuilder::new()
            .with_html_escaping_mode(HtmlEscapingMode::Custom(custom_mappings))
            .build();

        let generator = HtmlGenerator::new(config);

        let mut root = DomNode::new("p");
        root.add_text_child("Test <>&");

        let html = generator.generate_from_dom(&root).unwrap();

        assert!(html.contains("&LT;"));
        assert!(html.contains("&GT;"));
        assert!(html.contains("&AMP;"));
    }

    #[test]
    fn test_commonmark_attributes() {
        let config = OutputConfigBuilder::new()
            .with_commonmark_attributes(true)
            .build();

        let generator = HtmlGenerator::new(config);

        // Test heading with CommonMark attributes
        let mut heading = DomNode::new("h2");
        heading.add_text_child("Test Heading");

        let html = generator.generate_from_dom(&heading).unwrap();

        assert!(html.contains("data-commonmark-type=\"heading\""));
        assert!(html.contains("data-heading-level=\"2\""));
    }

    #[test]
    fn test_proper_tag_generation_for_all_elements() {
        let config = OutputConfigBuilder::new()
            .with_html5_compliance(true)
            .build();

        let generator = HtmlGenerator::new(config);

        // Test various CommonMark elements
        let mut root = DomNode::new("div");

        // Heading
        let mut h1 = DomNode::new("h1");
        h1.add_text_child("Heading");
        root.add_element_child(h1);

        // Paragraph with emphasis
        let mut p = DomNode::new("p");
        let mut em = DomNode::new("em");
        em.add_text_child("emphasized");
        p.add_element_child(em);
        root.add_element_child(p);

        // Code block
        let mut pre = DomNode::new("pre");
        let mut code = DomNode::new("code");
        code.add_attribute("class", "language-rust");
        code.add_text_child("fn main() {}");
        pre.add_element_child(code);
        root.add_element_child(pre);

        // List
        let mut ul = DomNode::new("ul");
        let mut li = DomNode::new("li");
        li.add_text_child("List item");
        ul.add_element_child(li);
        root.add_element_child(ul);

        // Thematic break (void element)
        let hr = DomNode::new("hr");
        root.add_element_child(hr);

        let html = generator.generate_from_dom(&root).unwrap();

        // Verify proper tag generation
        assert!(html.contains("<h1>Heading</h1>"));
        assert!(html.contains("<em>emphasized</em>"));
        assert!(html.contains("<pre><code class=\"language-rust\">fn main() {}</code></pre>"));
        // The list might have newlines, so check for the structure more flexibly
        assert!(html.contains("<ul>"));
        assert!(html.contains("<li>List item</li>"));
        assert!(html.contains("</ul>"));
        assert!(html.contains("<hr>"));
        assert!(!html.contains("</hr>")); // Void elements shouldn't have closing tags
    }

    #[test]
    fn test_html_validation() {
        // Test well-formed HTML validation
        let well_formed_html = "<div><p>Hello</p></div>";
        assert!(HtmlValidator::validate_well_formed(well_formed_html).is_ok());

        let malformed_html = "<div><p>Hello</div></p>";
        assert!(HtmlValidator::validate_well_formed(malformed_html).is_err());

        // Test HTML5 compliance validation
        let html5_compliant = "<!DOCTYPE html><html><body><p>Content</p></body></html>";
        assert!(HtmlValidator::validate_html5_compliance(html5_compliant).is_ok());

        let obsolete_html = "<center>Centered text</center>";
        assert!(HtmlValidator::validate_html5_compliance(obsolete_html).is_err());

        // Test void element validation
        let invalid_void = "<br></br>";
        assert!(HtmlValidator::validate_html5_compliance(invalid_void).is_err());

        // Test CommonMark compliance
        let commonmark_compliant =
            "<h1>Title</h1><pre><code class=\"language-rust\">code</code></pre>";
        assert!(HtmlValidator::validate_commonmark_compliance(commonmark_compliant).is_ok());

        let invalid_heading = "<h7>Invalid heading</h7>";
        assert!(HtmlValidator::validate_commonmark_compliance(invalid_heading).is_err());
    }

    #[test]
    fn test_comprehensive_validation() {
        let config = OutputConfigBuilder::new()
            .with_html5_compliance(true)
            .with_html_structure_validation(true)
            .build();

        // Test that validation catches errors during generation
        let generator = HtmlGenerator::new(config);

        let mut root = DomNode::new("div");
        let mut paragraph = DomNode::new("p");
        paragraph.add_text_child("Valid content");
        root.add_element_child(paragraph);

        // This should succeed
        let result = generator.generate_from_dom(&root);
        assert!(result.is_ok());
    }

    #[test]
    fn test_attribute_escaping() {
        let config = OutputConfigBuilder::new()
            .with_html_escaping_mode(HtmlEscapingMode::Standard)
            .build();

        let generator = HtmlGenerator::new(config);

        let mut root = DomNode::new("a");
        root.add_attribute("href", "https://example.com?param=\"value\"&other=test");
        root.add_attribute("title", "Link with \"quotes\" & ampersands");
        root.add_text_child("Link text");

        let html = generator.generate_from_dom(&root).unwrap();

        // Attributes should be properly escaped
        assert!(
            html.contains("href=\"https://example.com?param=&quot;value&quot;&amp;other=test\"")
        );
        assert!(html.contains("title=\"Link with &quot;quotes&quot; &amp; ampersands\""));
    }

    #[test]
    fn test_void_elements_html5() {
        let config = OutputConfigBuilder::new()
            .with_html5_compliance(true)
            .build();

        let generator = HtmlGenerator::new(config);

        // Test all HTML5 void elements
        let void_elements = [
            "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param",
            "source", "track", "wbr",
        ];

        for element in &void_elements {
            let node = DomNode::new(element);
            let html = generator.generate_from_dom(&node).unwrap();

            // Should not have closing tag
            assert!(!html.contains(&format!("</{}>", element)));
            // Should have opening tag
            assert!(html.contains(&format!("<{}>", element)));
        }
    }

    #[test]
    fn test_pretty_printing_and_formatting() {
        // Test pretty printing with indentation
        let config_pretty = OutputConfigBuilder::new()
            .with_pretty_print(true)
            .with_indent_size(4)
            .with_newlines(true)
            .build();

        let generator_pretty = HtmlGenerator::new(config_pretty);

        let mut root = DomNode::new("div");
        let mut paragraph = DomNode::new("p");
        paragraph.add_text_child("Hello World");
        root.add_element_child(paragraph);

        let html_pretty = generator_pretty.generate_from_dom(&root).unwrap();

        // Should contain indentation and newlines
        assert!(html_pretty.contains("    <p>"));
        assert!(html_pretty.contains("\n"));

        // Test compact formatting (no pretty printing)
        let config_compact = OutputConfigBuilder::new()
            .with_pretty_print(false)
            .with_newlines(false)
            .build();

        let generator_compact = HtmlGenerator::new(config_compact);
        let html_compact = generator_compact.generate_from_dom(&root).unwrap();

        // Should be more compact
        assert!(!html_compact.contains("    "));
        assert!(html_compact.len() < html_pretty.len());
    }

    #[test]
    fn test_ast_to_html_generation() {
        use crate::parser::ast::utils::NodeBuilder;

        // Create a simple AST document using NodeBuilder
        let heading_text = NodeBuilder::text("Test Heading".to_string());
        let heading = NodeBuilder::heading(1, vec![heading_text], None);

        // Create paragraph with emphasis
        let text1 = NodeBuilder::text("This is ".to_string());
        let emphasized_text = NodeBuilder::text("emphasized".to_string());
        let emphasis = NodeBuilder::emphasis(false, vec![emphasized_text]);
        let text2 = NodeBuilder::text(" text.".to_string());
        let paragraph = NodeBuilder::paragraph(vec![text1, emphasis, text2], None);

        let document = NodeBuilder::document(vec![heading, paragraph]);

        let config = OutputConfigBuilder::new()
            .with_html5_compliance(true)
            .build();

        let generator = HtmlGenerator::new(config);
        let html = generator.generate_from_ast(&document).unwrap();

        // Verify AST to HTML conversion works correctly (DOM conversion adds CSS classes)
        assert!(html.contains("<h1"));
        assert!(html.contains("Test Heading</h1>"));
        assert!(html.contains("<p"));
        assert!(html.contains("This is "));
        assert!(html.contains("<em"));
        assert!(html.contains("emphasized</em>"));
        assert!(html.contains(" text.</p>"));
    }

    #[test]
    fn test_html_output_correctness_all_elements() {
        let config = OutputConfigBuilder::new()
            .with_html5_compliance(true)
            .with_commonmark_attributes(false) // Test without extra attributes
            .build();

        let generator = HtmlGenerator::new(config);

        // Test all CommonMark block elements
        let mut root = DomNode::new("div");

        // Headings (all levels)
        for level in 1..=6 {
            let mut heading = DomNode::new(&format!("h{}", level));
            heading.add_text_child(&format!("Heading Level {}", level));
            root.add_element_child(heading);
        }

        // Paragraph
        let mut paragraph = DomNode::new("p");
        paragraph.add_text_child("This is a paragraph.");
        root.add_element_child(paragraph);

        // Code block
        let mut pre = DomNode::new("pre");
        let mut code = DomNode::new("code");
        code.add_attribute("class", "language-rust");
        code.add_text_child("let x = 42;");
        pre.add_element_child(code);
        root.add_element_child(pre);

        // Blockquote
        let mut blockquote = DomNode::new("blockquote");
        let mut quote_p = DomNode::new("p");
        quote_p.add_text_child("This is a quote.");
        blockquote.add_element_child(quote_p);
        root.add_element_child(blockquote);

        // Lists
        let mut ul = DomNode::new("ul");
        let mut li1 = DomNode::new("li");
        li1.add_text_child("Bullet item 1");
        let mut li2 = DomNode::new("li");
        li2.add_text_child("Bullet item 2");
        ul.add_element_child(li1);
        ul.add_element_child(li2);
        root.add_element_child(ul);

        let mut ol = DomNode::new("ol");
        let mut oli1 = DomNode::new("li");
        oli1.add_text_child("Ordered item 1");
        let mut oli2 = DomNode::new("li");
        oli2.add_text_child("Ordered item 2");
        ol.add_element_child(oli1);
        ol.add_element_child(oli2);
        root.add_element_child(ol);

        // Thematic break
        let hr = DomNode::new("hr");
        root.add_element_child(hr);

        // Inline elements in a paragraph
        let mut inline_p = DomNode::new("p");
        inline_p.add_text_child("Text with ");

        let mut em = DomNode::new("em");
        em.add_text_child("emphasis");
        inline_p.add_element_child(em);

        inline_p.add_text_child(" and ");

        let mut strong = DomNode::new("strong");
        strong.add_text_child("strong");
        inline_p.add_element_child(strong);

        inline_p.add_text_child(" and ");

        let mut inline_code = DomNode::new("code");
        inline_code.add_text_child("code");
        inline_p.add_element_child(inline_code);

        inline_p.add_text_child(" and ");

        let mut link = DomNode::new("a");
        link.add_attribute("href", "https://example.com");
        link.add_text_child("link");
        inline_p.add_element_child(link);

        inline_p.add_text_child(".");
        root.add_element_child(inline_p);

        let html = generator.generate_from_dom(&root).unwrap();

        // Verify all elements are correctly generated (check for tag presence and content)
        assert!(html.contains("<h1>") && html.contains("Heading Level 1</h1>"));
        assert!(html.contains("<h6>") && html.contains("Heading Level 6</h6>"));
        assert!(html.contains("<p>") && html.contains("This is a paragraph.</p>"));
        assert!(
            html.contains("<pre>")
                && html.contains("<code")
                && html.contains("let x = 42;</code></pre>")
        );
        assert!(
            html.contains("<blockquote>")
                && html.contains("This is a quote.")
                && html.contains("</blockquote>")
        );
        assert!(
            html.contains("<ul>")
                && html.contains("Bullet item 1")
                && html.contains("Bullet item 2")
                && html.contains("</ul>")
        );
        assert!(
            html.contains("<ol>")
                && html.contains("Ordered item 1")
                && html.contains("Ordered item 2")
                && html.contains("</ol>")
        );
        assert!(html.contains("<hr>"));
        assert!(!html.contains("</hr>")); // Void element
        assert!(html.contains("<em>") && html.contains("emphasis</em>"));
        assert!(html.contains("<strong>") && html.contains("strong</strong>"));
        assert!(html.contains("<code>") && html.contains("code</code>"));
        assert!(html.contains("<a href=\"https://example.com\">") && html.contains("link</a>"));
    }

    #[test]
    fn test_special_character_escaping_comprehensive() {
        let config = OutputConfigBuilder::new()
            .with_html_escaping_mode(HtmlEscapingMode::Standard)
            .build();

        let generator = HtmlGenerator::new(config);

        // Test various special characters in text content
        let test_cases = vec![
            ("<script>", "&lt;script&gt;"),
            ("&amp;", "&amp;amp;"),
            ("\"quoted\"", "&quot;quoted&quot;"),
            ("'single'", "&#x27;single&#x27;"),
            ("Mix <>&\"'", "Mix &lt;&gt;&amp;&quot;&#x27;"),
        ];

        for (input, expected) in test_cases {
            let mut node = DomNode::new("p");
            node.add_text_child(input);

            let html = generator.generate_from_dom(&node).unwrap();
            assert!(
                html.contains(expected),
                "Failed to escape '{}' correctly",
                input
            );
        }

        // Test attribute escaping
        let mut link = DomNode::new("a");
        link.add_attribute("href", "https://example.com?q=\"test\"&other=value");
        link.add_attribute("title", "Link with \"quotes\" & ampersands");
        link.add_text_child("Link");

        let html = generator.generate_from_dom(&link).unwrap();

        // Verify attribute values are properly escaped
        assert!(html.contains("href=\"https://example.com?q=&quot;test&quot;&amp;other=value\""));
        assert!(html.contains("title=\"Link with &quot;quotes&quot; &amp; ampersands\""));
    }

    #[test]
    fn test_xhtml_style_self_closing_tags() {
        let config_xhtml = OutputConfigBuilder::new().with_xhtml_style(true).build();

        let generator_xhtml = HtmlGenerator::new(config_xhtml);

        let hr = DomNode::new("hr");
        let html_xhtml = generator_xhtml.generate_from_dom(&hr).unwrap();

        // Should use XHTML-style self-closing
        assert!(html_xhtml.contains("<hr />"));

        let config_html = OutputConfigBuilder::new().with_xhtml_style(false).build();

        let generator_html = HtmlGenerator::new(config_html);
        let html_html = generator_html.generate_from_dom(&hr).unwrap();

        // Should use HTML5-style void element
        assert!(html_html.contains("<hr>"));
        assert!(!html_html.contains("<hr />"));
    }

    #[test]
    fn test_document_wrapper_and_doctype() {
        let config = OutputConfigBuilder::new()
            .with_document_wrapper(true)
            .with_html5_compliance(true)
            .with_doctype("html")
            .build();

        let generator = HtmlGenerator::new(config);

        let mut content = DomNode::new("p");
        content.add_text_child("Hello World");

        let html = generator.generate_from_dom(&content).unwrap();

        // Should include DOCTYPE and document structure
        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("<html lang=\"en\">"));
        assert!(html.contains("<head>"));
        assert!(html.contains("<meta charset=\"UTF-8\">"));
        assert!(html.contains("<meta name=\"viewport\""));
        assert!(html.contains("<body>"));
        assert!(html.contains("</body>"));
        assert!(html.contains("</html>"));
        assert!(html.contains("<p>Hello World</p>"));
    }

    #[test]
    fn test_commonmark_html_compliance_detailed() {
        let config = OutputConfigBuilder::new()
            .with_html5_compliance(true)
            .build();

        let generator = HtmlGenerator::new(config);

        // Test CommonMark-specific HTML requirements

        // 1. Code blocks should use pre > code structure
        let mut pre = DomNode::new("pre");
        let mut code = DomNode::new("code");
        code.add_attribute("class", "language-python");
        code.add_text_child("print('Hello')");
        pre.add_element_child(code);

        let html = generator.generate_from_dom(&pre).unwrap();
        assert!(html.contains(
            "<pre><code class=\"language-python\">print(&#x27;Hello&#x27;)</code></pre>"
        ));

        // 2. Links should have proper href attributes
        let mut link = DomNode::new("a");
        link.add_attribute("href", "https://example.com");
        link.add_text_child("Example");

        let html = generator.generate_from_dom(&link).unwrap();
        assert!(html.contains("<a href=\"https://example.com\">Example</a>"));

        // 3. Images should have proper src and alt attributes
        let mut img = DomNode::new("img");
        img.add_attribute("src", "image.jpg");
        img.add_attribute("alt", "Description");

        let html = generator.generate_from_dom(&img).unwrap();

        // Check for the components separately since attribute order might vary
        assert!(html.contains("<img"));
        assert!(html.contains("src=\"image.jpg\""));
        assert!(html.contains("alt=\"Description\""));
        assert!(html.contains(">"));
        assert!(!html.contains("</img>")); // Should be void element

        // 4. Emphasis should use em/strong tags
        let mut paragraph = DomNode::new("p");
        let mut em = DomNode::new("em");
        em.add_text_child("italic");
        let mut strong = DomNode::new("strong");
        strong.add_text_child("bold");

        paragraph.add_element_child(em);
        paragraph.add_text_child(" and ");
        paragraph.add_element_child(strong);

        let html = generator.generate_from_dom(&paragraph).unwrap();
        assert!(html.contains("<em>italic</em>"));
        assert!(html.contains("<strong>bold</strong>"));
    }

    #[test]
    fn test_html_writer_utilities() {
        let config = OutputConfigBuilder::new()
            .with_pretty_print(true)
            .with_indent_size(2)
            .build();

        let mut writer = HtmlWriter::new(config);

        // Test individual writer methods
        let mut attributes = HashMap::new();
        attributes.insert("class".to_string(), "test".to_string());
        attributes.insert("id".to_string(), "example".to_string());

        writer.write_element("div", &attributes, "Content");
        writer.write_self_closing("br", &HashMap::new());
        writer.write_text("Plain text");
        writer.write_raw("<span>Raw HTML</span>");

        let output = writer.get_output();

        // Verify all methods work correctly
        assert!(output.contains("<div"));
        assert!(output.contains("class=\"test\""));
        assert!(output.contains("id=\"example\""));
        assert!(output.contains("Content</div>"));
        assert!(output.contains("<br>"));
        assert!(output.contains("Plain text"));
        assert!(output.contains("<span>Raw HTML</span>"));

        // Test indentation methods
        writer.clear();
        writer.indent();
        writer.write_text("Indented");
        writer.dedent();
        writer.write_text("Not indented");

        let _indented_output = writer.get_output();

        // The write_text method doesn't add indentation automatically - only write_indent does
        // Let's test the actual indentation behavior
        writer.clear();
        writer.indent();
        writer.write_indent();
        writer.write_text("Indented");

        let properly_indented_output = writer.get_output();
        assert!(properly_indented_output.contains("  Indented")); // Should be indented
    }
}
