/// Configuration module for the Markdown engine public API.
///
/// This module provides configuration structs and the main MarkdownEngine
/// with builder pattern for easy configuration and usage.
use crate::ast::Document;
use crate::codegen::OutputConfig;
use crate::dom::DomNode;
use crate::error::{MarkdownError, Result};
// use crate::extensions::ExtensionManager;
use crate::parser::ParserConfig;
use crate::performance::{ParallelConfig, PerformanceOptimizer};
use crate::streaming::{StreamingConfig, StringStreamingParser};
use std::collections::HashMap;

/// Configuration for the lexer component.
#[derive(Debug, Clone)]
pub struct LexerConfig {
    /// Whether to track detailed source position information
    pub track_positions: bool,
    /// Whether to handle Unicode grapheme clusters properly
    pub unicode_aware: bool,
    /// Maximum input size to prevent memory exhaustion
    pub max_input_size: Option<usize>,
}

impl Default for LexerConfig {
    fn default() -> Self {
        Self {
            track_positions: true,
            unicode_aware: true,
            max_input_size: Some(100 * 1024 * 1024), // 100MB default limit
        }
    }
}

/// Configuration for DOM transformation.
#[derive(Debug, Clone)]
pub struct DomConfig {
    /// Whether to preserve source position information in DOM nodes
    pub preserve_positions: bool,
    /// Custom attributes to add to all elements
    pub global_attributes: HashMap<String, String>,
    /// Whether to validate DOM structure during transformation
    pub validate_structure: bool,
}

impl Default for DomConfig {
    fn default() -> Self {
        Self {
            preserve_positions: false,
            global_attributes: HashMap::new(),
            validate_structure: true,
        }
    }
}

/// Configuration for HTML generation.
#[derive(Debug, Clone)]
pub struct GenerationConfig {
    /// Output configuration for HTML generation
    pub output: OutputConfig,
    /// Whether to generate source maps
    pub source_maps: bool,
    /// Custom CSS classes for elements
    pub css_classes: HashMap<String, String>,
}

impl Default for GenerationConfig {
    fn default() -> Self {
        Self {
            output: OutputConfig::default(),
            source_maps: false,
            css_classes: HashMap::new(),
        }
    }
}

/// Main configuration struct for the Markdown engine.
#[derive(Debug, Clone)]
pub struct EngineConfig {
    /// Lexer configuration
    pub lexer: LexerConfig,
    /// Parser configuration
    pub parser: ParserConfig,
    /// DOM transformation configuration
    pub dom: DomConfig,
    /// HTML generation configuration
    pub generation: GenerationConfig,
    /// Whether to enable streaming mode for large documents
    pub streaming: bool,
    /// Whether to enable performance optimizations
    pub performance_optimized: bool,
    /// Streaming configuration (when streaming is enabled)
    pub streaming_config: StreamingConfig,
    // Extension manager for custom syntax and renderers
    // pub extension_manager: ExtensionManager,
}

impl Default for EngineConfig {
    fn default() -> Self {
        Self {
            lexer: LexerConfig::default(),
            parser: ParserConfig::default(),
            dom: DomConfig::default(),
            generation: GenerationConfig::default(),
            streaming: false,
            performance_optimized: true,
            streaming_config: StreamingConfig::default(),
            // extension_manager: ExtensionManager::new(),
        }
    }
}

impl EngineConfig {
    /// Creates a builder for configuring the engine.
    pub fn builder() -> EngineConfigBuilder {
        EngineConfigBuilder::new()
    }
}

/// Builder for EngineConfig to provide a fluent configuration API.
#[derive(Debug, Default)]
pub struct EngineConfigBuilder {
    config: EngineConfig,
}

impl EngineConfigBuilder {
    /// Creates a new builder with default configuration.
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the lexer configuration.
    pub fn lexer(mut self, lexer_config: LexerConfig) -> Self {
        self.config.lexer = lexer_config;
        self
    }

    /// Sets the parser configuration.
    pub fn parser(mut self, parser_config: ParserConfig) -> Self {
        self.config.parser = parser_config;
        self
    }

    /// Sets the DOM configuration.
    pub fn dom(mut self, dom_config: DomConfig) -> Self {
        self.config.dom = dom_config;
        self
    }

    /// Sets the generation configuration.
    pub fn generation(mut self, generation_config: GenerationConfig) -> Self {
        self.config.generation = generation_config;
        self
    }

    /// Enables or disables streaming mode.
    pub fn streaming(mut self, enabled: bool) -> Self {
        self.config.streaming = enabled;
        self
    }

    /// Enables or disables performance optimizations.
    pub fn performance_optimized(mut self, enabled: bool) -> Self {
        self.config.performance_optimized = enabled;
        self
    }

    /// Sets the streaming configuration.
    pub fn streaming_config(mut self, config: StreamingConfig) -> Self {
        self.config.streaming_config = config;
        self
    }

    /// Sets the parallel processing configuration.
    pub fn parallel_config(mut self, config: ParallelConfig) -> Self {
        self.config.parser.parallel_config = config;
        self
    }

    /// Sets the extension manager.
    // pub fn extension_manager(mut self, manager: ExtensionManager) -> Self {
    //     self.config.extension_manager = manager;
    //     self
    // }

    /// Sets CommonMark strict compliance mode.
    pub fn strict_commonmark(mut self, strict: bool) -> Self {
        self.config.parser.strict_commonmark = strict;
        self
    }

    /// Sets maximum nesting depth for parsing.
    pub fn max_nesting_depth(mut self, depth: usize) -> Self {
        self.config.parser.max_nesting_depth = depth;
        self
    }

    /// Enables GitHub Flavored Markdown extensions.
    pub fn enable_gfm(mut self, enabled: bool) -> Self {
        self.config.parser.enable_gfm_extensions = enabled;
        self
    }

    /// Sets whether to track source positions.
    pub fn track_positions(mut self, enabled: bool) -> Self {
        self.config.lexer.track_positions = enabled;
        self.config.parser.track_source_positions = enabled;
        self.config.dom.preserve_positions = enabled;
        self
    }

    /// Sets pretty printing for HTML output.
    pub fn pretty_print(mut self, enabled: bool) -> Self {
        self.config.generation.output.pretty_print = enabled;
        self
    }

    /// Sets HTML escaping mode.
    pub fn escape_html(mut self, enabled: bool) -> Self {
        self.config.generation.output.escape_html = enabled;
        self
    }

    /// Builds the final configuration.
    pub fn build(self) -> EngineConfig {
        self.config
    }
}

/// Main Markdown engine with configurable processing pipeline.
///
/// The MarkdownEngine provides a high-level interface for parsing Markdown
/// and generating various output formats. It follows the builder pattern
/// for easy configuration.
///
/// # Examples
///
/// ```
/// use rosetta::{MarkdownEngine, EngineConfig};
///
/// // Simple usage with defaults
/// let engine = MarkdownEngine::new();
/// let html = engine.parse_to_html("# Hello, World!");
///
/// // Custom configuration
/// let engine = MarkdownEngine::with_config(
///     EngineConfig::builder()
///         .strict_commonmark(true)
///         .pretty_print(true)
///         .track_positions(false)
///         .build()
/// );
/// let html = engine.parse_to_html("# Hello, World!");
/// ```
#[derive(Debug)]
pub struct MarkdownEngine {
    config: EngineConfig,
    performance_optimizer: Option<PerformanceOptimizer>,
}

impl MarkdownEngine {
    /// Creates a new Markdown engine with default configuration.
    pub fn new() -> Self {
        let config = EngineConfig::default();
        let performance_optimizer = if config.performance_optimized {
            Some(PerformanceOptimizer::new(
                config.parser.parallel_config.clone(),
            ))
        } else {
            None
        };

        Self {
            config,
            performance_optimizer,
        }
    }

    /// Creates a new Markdown engine with custom configuration.
    pub fn with_config(config: EngineConfig) -> Self {
        let performance_optimizer = if config.performance_optimized {
            Some(PerformanceOptimizer::new(
                config.parser.parallel_config.clone(),
            ))
        } else {
            None
        };

        Self {
            config,
            performance_optimizer,
        }
    }

    /// Creates a builder for configuring the engine.
    pub fn builder() -> EngineConfigBuilder {
        EngineConfigBuilder::new()
    }

    /// Returns a reference to the engine configuration.
    pub fn config(&self) -> &EngineConfig {
        &self.config
    }

    /// Parses Markdown text and returns HTML output.
    ///
    /// This is the main entry point for simple Markdown to HTML conversion.
    ///
    /// # Arguments
    ///
    /// * `markdown` - The input Markdown text to parse
    ///
    /// # Returns
    ///
    /// Returns the generated HTML as a String, or an error if parsing fails.
    ///
    /// # Examples
    ///
    /// ```
    /// use rosetta::MarkdownEngine;
    ///
    /// let engine = MarkdownEngine::new();
    /// let html = engine.parse_to_html("# Hello, World!").unwrap();
    /// assert!(html.contains("<h1"));
    /// ```
    pub fn parse_to_html(&self, markdown: &str) -> Result<String> {
        // Validate input size if configured
        if let Some(max_size) = self.config.lexer.max_input_size
            && markdown.len() > max_size
        {
            return Err(MarkdownError::parse_error(
                crate::lexer::Position::default(),
                format!(
                    "Input size {} exceeds maximum allowed size {}",
                    markdown.len(),
                    max_size
                ),
            ));
        }

        // Choose parsing strategy based on configuration
        let document = if self.config.performance_optimized && markdown.len() > 10000 {
            // Use optimized parsing for large documents
            self.parse_optimized(markdown)?
        } else if self.config.streaming && markdown.len() > 100000 {
            // Use streaming for very large documents
            self.parse_streaming(markdown)?
        } else {
            // Standard parsing for smaller documents
            self.parse_to_ast(markdown)?
        };

        // Convert to DOM if needed for advanced processing
        if self.config.dom.preserve_positions || !self.config.dom.global_attributes.is_empty() {
            let dom = self.ast_to_dom(document)?;
            self.dom_to_html(dom)
        } else {
            // Direct AST to HTML conversion for better performance
            self.ast_to_html(document)
        }
    }

    /// Parses Markdown text into an Abstract Syntax Tree.
    ///
    /// This method provides access to the parsed AST for applications
    /// that need to work with the document structure directly.
    ///
    /// # Arguments
    ///
    /// * `markdown` - The input Markdown text to parse
    ///
    /// # Returns
    ///
    /// Returns the parsed Document AST, or an error if parsing fails.
    pub fn parse_to_ast(&self, markdown: &str) -> Result<Document> {
        crate::parser::parse_with_config(markdown, self.config.parser.clone())
    }

    /// Converts an AST to DOM representation.
    ///
    /// This method transforms the AST into a more flexible DOM structure
    /// that can be manipulated before HTML generation.
    ///
    /// # Arguments
    ///
    /// * `document` - The AST document to convert
    ///
    /// # Returns
    ///
    /// Returns the DOM root node, or an error if conversion fails.
    pub fn ast_to_dom(&self, document: Document) -> Result<DomNode> {
        // Apply DOM configuration
        let mut dom = crate::dom::from_ast(document)?;

        // Add global attributes if configured
        if !self.config.dom.global_attributes.is_empty() {
            self.apply_global_attributes(&mut dom);
        }

        Ok(dom)
    }

    /// Generates HTML from an AST document.
    ///
    /// This method provides direct AST to HTML conversion, bypassing
    /// the DOM transformation step for better performance.
    ///
    /// # Arguments
    ///
    /// * `document` - The AST document to convert
    ///
    /// # Returns
    ///
    /// Returns the generated HTML string.
    pub fn ast_to_html(&self, document: Document) -> Result<String> {
        let generator = crate::codegen::HtmlGenerator::new(self.config.generation.output.clone());
        generator.generate_from_ast(&document)
    }

    /// Generates HTML from a DOM node.
    ///
    /// This method converts a DOM representation to HTML output.
    ///
    /// # Arguments
    ///
    /// * `dom` - The DOM root node to convert
    ///
    /// # Returns
    ///
    /// Returns the generated HTML string.
    pub fn dom_to_html(&self, dom: DomNode) -> Result<String> {
        let generator = crate::codegen::HtmlGenerator::new(self.config.generation.output.clone());
        generator.generate_from_dom(&dom)
    }

    /// Parses Markdown and returns both AST and DOM representations.
    ///
    /// This method is useful for applications that need access to both
    /// the parsed AST and the transformed DOM.
    ///
    /// # Arguments
    ///
    /// * `markdown` - The input Markdown text to parse
    ///
    /// # Returns
    ///
    /// Returns a tuple of (Document, DomNode), or an error if processing fails.
    pub fn parse_full(&self, markdown: &str) -> Result<(Document, DomNode)> {
        let document = self.parse_to_ast(markdown)?;
        let dom = self.ast_to_dom(document.clone())?;
        Ok((document, dom))
    }

    /// Validates Markdown syntax without generating output.
    ///
    /// This method parses the input and reports any syntax errors
    /// without performing the full conversion pipeline.
    ///
    /// # Arguments
    ///
    /// * `markdown` - The input Markdown text to validate
    ///
    /// # Returns
    ///
    /// Returns Ok(()) if the syntax is valid, or an error describing the issue.
    pub fn validate(&self, markdown: &str) -> Result<()> {
        self.parse_to_ast(markdown)?;
        Ok(())
    }

    /// Parses Markdown using streaming mode for large documents.
    ///
    /// This method uses the streaming parser to process large documents
    /// that may not fit entirely in memory.
    ///
    /// # Arguments
    ///
    /// * `markdown` - The input Markdown text to parse
    ///
    /// # Returns
    ///
    /// Returns the parsed Document, or an error if parsing fails.
    pub fn parse_streaming(&self, markdown: &str) -> Result<Document> {
        if !self.config.streaming {
            // Fall back to regular parsing if streaming is disabled
            return self.parse_to_ast(markdown);
        }

        let mut streaming_config = self.config.streaming_config.clone();
        streaming_config.parser_config = self.config.parser.clone();

        let mut parser = StringStreamingParser::new(markdown, streaming_config);
        parser.parse_complete()
    }

    /// Parses Markdown with performance optimizations enabled.
    ///
    /// This method uses parallel processing and memory pooling for optimal performance
    /// on large documents with independent sections.
    ///
    /// # Arguments
    ///
    /// * `markdown` - The input Markdown text to parse
    ///
    /// # Returns
    ///
    /// Returns the parsed Document, or an error if parsing fails.
    pub fn parse_optimized(&self, markdown: &str) -> Result<Document> {
        if let Some(ref optimizer) = self.performance_optimizer {
            optimizer.optimize_document_processing(markdown, |content| self.parse_to_ast(content))
        } else {
            self.parse_to_ast(markdown)
        }
    }

    /// Returns a reference to the extension manager.
    // pub fn extension_manager(&self) -> &ExtensionManager {
    //     &self.config.extension_manager
    // }

    /// Returns a mutable reference to the extension manager.
    // pub fn extension_manager_mut(&mut self) -> &mut ExtensionManager {
    //     &mut self.config.extension_manager
    // }

    /// Enables or disables extensions.
    // pub fn set_extensions_enabled(&mut self, enabled: bool) {
    //     self.config.extension_manager.set_enabled(enabled);
    // }

    /// Parses Markdown with intermediate representation access.
    ///
    /// This method provides access to both the AST and DOM representations
    /// for advanced processing scenarios.
    ///
    /// # Arguments
    ///
    /// * `markdown` - The input Markdown text to parse
    ///
    /// # Returns
    ///
    /// Returns a tuple of (Document, DomNode, HTML), or an error if processing fails.
    pub fn parse_with_intermediates(&self, markdown: &str) -> Result<(Document, DomNode, String)> {
        let document = if self.config.streaming {
            self.parse_streaming(markdown)?
        } else {
            self.parse_to_ast(markdown)?
        };

        let dom = self.ast_to_dom(document.clone())?;
        let html = self.dom_to_html(dom.clone())?;

        Ok((document, dom, html))
    }

    /// Get performance statistics for the engine.
    ///
    /// Returns statistics about memory pool usage and parallel processing
    /// if performance optimizations are enabled.
    pub fn performance_stats(&self) -> Option<crate::performance::PoolStats> {
        self.performance_optimizer
            .as_ref()
            .map(|opt| opt.pool_stats())
    }

    /// Helper method to apply global attributes to DOM nodes recursively.
    fn apply_global_attributes(&self, dom: &mut DomNode) {
        // Add global attributes to this node
        for (key, value) in &self.config.dom.global_attributes {
            dom.attributes.insert(key.clone(), value.clone());
        }

        // Recursively apply to children
        for child in &mut dom.children {
            if let crate::dom::DomChild::Element(child_node) = child {
                self.apply_global_attributes(child_node);
            }
        }
    }
}

impl Default for MarkdownEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_engine_config_default() {
        let config = EngineConfig::default();
        assert!(config.lexer.track_positions);
        assert!(config.parser.strict_commonmark);
        assert!(!config.streaming);
        assert!(config.performance_optimized);
        assert_eq!(config.streaming_config.chunk_size, 64 * 1024);
        // assert!(config.extension_manager.is_enabled());
    }

    #[test]
    fn test_engine_config_builder() {
        let config = EngineConfig::builder()
            .strict_commonmark(false)
            .pretty_print(true)
            .track_positions(false)
            .max_nesting_depth(32)
            .streaming(true)
            .build();

        assert!(!config.parser.strict_commonmark);
        assert!(config.generation.output.pretty_print);
        assert!(!config.lexer.track_positions);
        assert_eq!(config.parser.max_nesting_depth, 32);
        assert!(config.streaming);
    }

    #[test]
    fn test_markdown_engine_creation() {
        let engine = MarkdownEngine::new();
        assert!(engine.config().parser.strict_commonmark);

        let custom_config = EngineConfig::builder().strict_commonmark(false).build();
        let custom_engine = MarkdownEngine::with_config(custom_config);
        assert!(!custom_engine.config().parser.strict_commonmark);
    }

    #[test]
    fn test_markdown_engine_builder() {
        let config = EngineConfig::builder()
            .strict_commonmark(true)
            .pretty_print(false)
            .build();
        let engine = MarkdownEngine::with_config(config);

        assert!(engine.config().parser.strict_commonmark);
        assert!(!engine.config().generation.output.pretty_print);
    }

    #[test]
    fn test_engine_parse_to_html() {
        let engine = MarkdownEngine::new();
        let result = engine.parse_to_html("# Hello, World!");
        assert!(result.is_ok());

        let html = result.unwrap();
        assert!(html.contains("Hello, World!"));
    }

    #[test]
    fn test_engine_parse_to_ast() {
        let engine = MarkdownEngine::new();
        let result = engine.parse_to_ast("# Hello, World!");
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 1);
        assert!(matches!(
            document.blocks[0],
            crate::ast::Block::Heading { .. }
        ));
    }

    #[test]
    fn test_engine_validate() {
        let engine = MarkdownEngine::new();

        // Valid markdown should pass validation
        let result = engine.validate("# Valid heading");
        assert!(result.is_ok());

        // Empty input should also be valid
        let result = engine.validate("");
        assert!(result.is_ok());
    }

    #[test]
    fn test_engine_parse_full() {
        let engine = MarkdownEngine::new();
        let result = engine.parse_full("# Hello, World!");
        assert!(result.is_ok());

        let (document, dom) = result.unwrap();
        assert_eq!(document.blocks.len(), 1);
        assert_eq!(dom.tag, "div"); // Root DOM node should be a div
    }

    #[test]
    fn test_engine_with_global_attributes() {
        let mut global_attrs = HashMap::new();
        global_attrs.insert("class".to_string(), "markdown-content".to_string());
        global_attrs.insert("data-source".to_string(), "markdown".to_string());

        let config = EngineConfig::builder()
            .dom(DomConfig {
                global_attributes: global_attrs,
                ..DomConfig::default()
            })
            .build();

        let engine = MarkdownEngine::with_config(config);
        let result = engine.parse_to_html("# Test");
        assert!(result.is_ok());

        // The HTML should contain the global attributes
        let html = result.unwrap();
        assert!(html.contains("class=\"markdown-content\""));
        assert!(html.contains("data-source=\"markdown\""));
    }

    #[test]
    fn test_engine_input_size_limit() {
        let config = EngineConfig::builder()
            .lexer(LexerConfig {
                max_input_size: Some(10), // Very small limit for testing
                ..LexerConfig::default()
            })
            .build();

        let engine = MarkdownEngine::with_config(config);
        let large_input = "# ".repeat(100); // Exceeds the 10-byte limit

        let result = engine.parse_to_html(&large_input);
        assert!(result.is_err());

        if let Err(MarkdownError::Parse { message, .. }) = result {
            assert!(message.contains("exceeds maximum allowed size"));
        } else {
            panic!("Expected parse error for oversized input");
        }
    }

    #[test]
    fn test_lexer_config_default() {
        let config = LexerConfig::default();
        assert!(config.track_positions);
        assert!(config.unicode_aware);
        assert_eq!(config.max_input_size, Some(100 * 1024 * 1024));
    }

    #[test]
    fn test_dom_config_default() {
        let config = DomConfig::default();
        assert!(!config.preserve_positions);
        assert!(config.global_attributes.is_empty());
        assert!(config.validate_structure);
    }

    #[test]
    fn test_generation_config_default() {
        let config = GenerationConfig::default();
        assert!(!config.source_maps);
        assert!(config.css_classes.is_empty());
    }

    #[test]
    fn test_engine_performance_mode() {
        let config = EngineConfig::builder()
            .performance_optimized(true)
            .track_positions(false) // Disable position tracking for performance
            .build();
        let engine = MarkdownEngine::with_config(config);

        assert!(engine.config().performance_optimized);
        assert!(!engine.config().lexer.track_positions);

        let result = engine.parse_to_html("# Performance test");
        assert!(result.is_ok());
    }

    #[test]
    fn test_engine_streaming_mode() {
        let config = EngineConfig::builder().streaming(true).build();
        let engine = MarkdownEngine::with_config(config);

        assert!(engine.config().streaming);

        // Streaming mode should still work for regular parsing
        let result = engine.parse_to_html("# Streaming test");
        assert!(result.is_ok());
    }

    #[test]
    fn test_engine_gfm_extensions() {
        let config = EngineConfig::builder().enable_gfm(true).build();
        let engine = MarkdownEngine::with_config(config);

        assert!(engine.config().parser.enable_gfm_extensions);

        let result = engine.parse_to_html("# GFM test");
        assert!(result.is_ok());
    }

    #[test]
    fn test_engine_ast_to_dom_conversion() {
        let engine = MarkdownEngine::new();
        let document = engine.parse_to_ast("# Test heading").unwrap();
        let dom = engine.ast_to_dom(document).unwrap();

        assert_eq!(dom.tag, "div");
        assert!(!dom.children.is_empty());
    }

    #[test]
    fn test_engine_ast_to_html_direct() {
        let engine = MarkdownEngine::new();
        let document = engine.parse_to_ast("# Test heading").unwrap();
        let html = engine.ast_to_html(document).unwrap();

        assert!(html.contains("Test heading"));
    }

    #[test]
    fn test_engine_dom_to_html() {
        let engine = MarkdownEngine::new();
        let document = engine.parse_to_ast("# Test heading").unwrap();
        let dom = engine.ast_to_dom(document).unwrap();
        let html = engine.dom_to_html(dom).unwrap();

        assert!(html.contains("Test heading"));
    }
}
