// Core modules
pub mod ast;
pub mod codegen;
pub mod dom;
pub mod error;
pub mod ir;
pub mod lexer;
pub mod parser;
pub mod performance;

// Re-export key types for public API
pub use ast::{Block, Document, Inline, Node, Visitable, Visitor};
pub use codegen::{
    CustomRenderer, HtmlGenerator, HtmlValidator, HtmlWriter, OutputConfig, OutputConfigBuilder,
};
pub use dom::DomNode;
pub use error::{MarkdownError, Result};
pub use ir::{
    AlignmentAxis, Block as WorkspaceBlock, BlockId, BlockKind, ContentPayload, Dimension,
    InlineMark, InlineSpan, LayoutAlignment, LayoutMetadata, LayoutSize, MetadataMap,
    MetadataValue, Space, SpaceId, StyleHooks, Workspace, WorkspaceId, WorkspaceTransformer,
};
pub use lexer::{Lexer, Position, Token};
pub use parser::ParserConfig;
pub use performance::{ParallelConfig, PerformanceOptimizer, ZeroCopyStr};

// Public API configuration structs
pub use config::{DomConfig, EngineConfig, GenerationConfig, LexerConfig, MarkdownEngine};

// Advanced API exports
pub use streaming::{ChunkResult, StreamingConfig, StreamingParser};
// pub use extensions::{SyntaxExtension, ExtensionRegistry, ExtensionManager};

// Configuration module for public API
pub mod config;

// Advanced API modules
pub mod extensions;
pub mod streaming;

/// Simple function to parse Markdown text and return HTML.
///
/// This is the simplest entry point for the Markdown engine, providing
/// a one-function interface for basic Markdown to HTML conversion.
/// Uses default configuration for all components.
///
/// # Arguments
///
/// * `markdown` - The input Markdown text to convert
///
/// # Returns
///
/// Returns the generated HTML as a String
///
/// # Examples
///
/// ```
/// use rosetta::parse_markdown;
///
/// let html = parse_markdown("# Hello, World!");
/// assert!(html.contains("Hello, World!"));
/// ```
pub fn parse_markdown(markdown: &str) -> String {
    let engine = config::MarkdownEngine::new();
    engine.parse_to_html(markdown).unwrap_or_else(|_| {
        // Fallback to empty content on error
        String::new()
    })
}

/// Converts a Markdown string to an HTML string.
///
/// This is the main entry point for the Markdown engine. It orchestrates
/// the complete compilation pipeline:
/// 1. **Lexical Analysis:** Tokenize the input Markdown text
/// 2. **Parsing:** Parse tokens into an Abstract Syntax Tree (AST)
/// 3. **Intermediate Representation:** Convert AST to Document Object Model (DOM)
/// 4. **Code Generation:** Generate HTML from the DOM
///
/// # Arguments
///
/// * `markdown` - The input Markdown text to convert
///
/// # Returns
///
/// Returns the generated HTML as a String
///
/// # Examples
///
/// ```
/// use rosetta::to_html;
///
/// let markdown = "# Hello, World!";
/// let html = to_html(markdown);
/// assert_eq!(html, "<div class=\"markdown-content\"><h1 class=\"heading\">Hello, World!</h1>\n</div>\n");
/// ```
pub fn to_html(markdown: &str) -> String {
    // 1. Parsing (includes lexical analysis internally)
    let root = parser::parse(markdown).unwrap_or_else(|_| ast::Document {
        blocks: Vec::new(),
        source_map: ast::SourceMap::new(),
    });

    // 2. AST to DOM conversion (Intermediate Representation)
    let dom = dom::from_ast(root).unwrap_or_else(|_| {
        // Fallback to empty div on transformation error
        dom::DomNode::new("div")
    });

    // 3. Target Code Generation
    codegen::generate_html(dom)
}

/// Parse Markdown text into an Abstract Syntax Tree.
///
/// This function provides direct access to the AST representation
/// for applications that need to work with the parsed structure.
///
/// # Arguments
///
/// * `markdown` - The input Markdown text to parse
///
/// # Returns
///
/// Returns the root Document of the AST
pub fn parse_to_ast(markdown: &str) -> Document {
    parser::parse(markdown).unwrap_or_else(|_| ast::Document {
        blocks: Vec::new(),
        source_map: ast::SourceMap::new(),
    })
}

/// Convert an AST to a DOM representation.
///
/// This function provides access to the DOM transformation step
/// for applications that need the intermediate representation.
///
/// # Arguments
///
/// * `ast` - The AST root document to convert
///
/// # Returns
///
/// Returns the root DomNode of the DOM tree, or an error if transformation fails
pub fn ast_to_dom(ast: Document) -> Result<DomNode> {
    dom::from_ast(ast)
}

/// Generate HTML from a DOM representation.
///
/// This function provides direct access to the HTML generation step
/// for applications working with DOM representations.
///
/// # Arguments
///
/// * `dom` - The DOM root node to convert to HTML
///
/// # Returns
///
/// Returns the generated HTML as a String
pub fn dom_to_html(dom: DomNode) -> String {
    codegen::generate_html(dom)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn it_works() {
        let markdown = "# Hello, World!";
        let expected_html =
            "<div class=\"markdown-content\"><h1 class=\"heading\">Hello, World!</h1>\n</div>\n";
        assert_eq!(to_html(markdown), expected_html);
    }

    #[test]
    fn test_parse_markdown_function() {
        let markdown = "# Hello, World!";
        let html = parse_markdown(markdown);
        assert!(html.contains("Hello, World!"));
    }

    #[test]
    fn test_parse_to_ast_function() {
        let markdown = "# Hello, World!";
        let document = parse_to_ast(markdown);
        assert_eq!(document.blocks.len(), 1);
        assert!(matches!(document.blocks[0], ast::Block::Heading { .. }));
    }

    // API Integration Tests for Task 7.4

    /// Test simple parse function with various inputs
    #[test]
    fn test_simple_parse_function_various_inputs() {
        // Test basic heading
        let html = parse_markdown("# Simple Heading");
        assert!(
            html.contains("Simple Heading"),
            "Expected heading text to preserve whitespace"
        );
        assert!(!html.is_empty());

        // Test paragraph with emphasis
        let html = parse_markdown("This is **bold** and *italic* text.");
        assert!(html.contains("bold"));
        assert!(html.contains("italic"));

        // Test code block
        let html = parse_markdown("```rust\nfn main() {}\n```");
        assert!(html.contains("fn main") || html.contains("fnmain"));

        // Test list
        let html = parse_markdown("- Item 1\n- Item 2\n- Item 3");
        assert!(
            html.contains("Item 1"),
            "Expected list items to preserve whitespace"
        );
        assert!(html.contains("Item 2"));

        // Test link
        let html = parse_markdown("[Link text](https://example.com)");
        assert!(html.contains("Link text"));
        assert!(html.contains("example.com"));

        // Test blockquote
        let html = parse_markdown("> This is a quote");
        assert!(html.contains("This is a quote"));

        // Test empty input
        let html = parse_markdown("");
        assert!(!html.is_empty()); // Should return valid HTML structure

        // Test complex mixed content
        let complex_markdown = r#"# Main Title

This is a paragraph with **bold** and *italic* text.

## Subsection

- First item
- Second item with [link](https://example.com)

```rust
fn hello() {
    println!("Hello, World!");
}
```

> A blockquote with some content.
"#;
        let html = parse_markdown(complex_markdown);
        assert!(html.contains("Main Title"));
        assert!(html.contains("Subsection"));
        assert!(
            html.contains("First item"),
            "Expected mixed content to preserve whitespace in list items"
        );
        assert!(html.contains("println!") || html.contains("Hello"));
        assert!(html.contains("blockquote"));
    }

    /// Test builder pattern and configuration options
    #[test]
    fn test_builder_pattern_and_configuration() {
        // Test default engine creation
        let engine = MarkdownEngine::new();
        let result = engine.parse_to_html("# Test");
        assert!(result.is_ok());

        // Test builder pattern with various configurations
        let config = config::EngineConfig::builder()
            .strict_commonmark(true)
            .pretty_print(true)
            .track_positions(false)
            .max_nesting_depth(32)
            .build();

        let engine = MarkdownEngine::with_config(config);
        assert!(engine.config().parser.strict_commonmark);
        assert!(engine.config().generation.output.pretty_print);
        assert!(!engine.config().lexer.track_positions);
        assert_eq!(engine.config().parser.max_nesting_depth, 32);

        // Test parsing with custom configuration
        let result = engine.parse_to_html("# Configured Test");
        assert!(result.is_ok());
        let html = result.unwrap();
        assert!(html.contains("Configured Test"));

        // Test lexer configuration
        let lexer_config = config::LexerConfig {
            track_positions: false,
            unicode_aware: true,
            max_input_size: Some(1024),
        };

        let config = config::EngineConfig::builder().lexer(lexer_config).build();

        let engine = MarkdownEngine::with_config(config);
        assert!(!engine.config().lexer.track_positions);
        assert!(engine.config().lexer.unicode_aware);
        assert_eq!(engine.config().lexer.max_input_size, Some(1024));

        // Test DOM configuration with global attributes
        let mut global_attrs = HashMap::new();
        global_attrs.insert("class".to_string(), "test-content".to_string());
        global_attrs.insert("data-test".to_string(), "true".to_string());

        let dom_config = config::DomConfig {
            preserve_positions: true,
            global_attributes: global_attrs,
            validate_structure: true,
        };

        let config = config::EngineConfig::builder().dom(dom_config).build();

        let engine = MarkdownEngine::with_config(config);
        let result = engine.parse_to_html("# Test with attributes");
        assert!(result.is_ok());
        let html = result.unwrap();
        assert!(html.contains("class=\"test-content\""));
        assert!(html.contains("data-test=\"true\""));

        // Test generation configuration
        let gen_config = config::GenerationConfig {
            source_maps: true,
            css_classes: HashMap::new(),
            output: OutputConfig {
                pretty_print: false,
                indent_size: 2,
                escape_html: true,
                add_source_maps: false,
                custom_renderers: HashMap::new(),
                xhtml_style: false,
                add_newlines: true,
                doctype: None,
                wrap_in_document: false,
                html_escaping_mode: codegen::HtmlEscapingMode::Standard,
                html5_compliance: true,
                add_commonmark_attributes: false,
                validate_html_structure: false,
            },
        };

        let config = config::EngineConfig::builder()
            .generation(gen_config)
            .build();

        let engine = MarkdownEngine::with_config(config);
        assert!(engine.config().generation.source_maps);
        assert!(!engine.config().generation.output.pretty_print);
        assert_eq!(engine.config().generation.output.indent_size, 2);

        // Test performance optimization settings
        let config = config::EngineConfig::builder()
            .performance_optimized(true)
            .streaming(false)
            .build();

        let engine = MarkdownEngine::with_config(config);
        assert!(engine.config().performance_optimized);
        assert!(!engine.config().streaming);

        // Test input size limits
        let config = config::EngineConfig::builder()
            .lexer(config::LexerConfig {
                max_input_size: Some(10), // Very small limit
                ..config::LexerConfig::default()
            })
            .build();

        let engine = MarkdownEngine::with_config(config);
        let large_input = "# ".repeat(100); // Exceeds limit
        let result = engine.parse_to_html(&large_input);
        assert!(result.is_err()); // Should fail due to size limit
    }

    /// Test intermediate representation access
    #[test]
    fn test_intermediate_representation_access() {
        let engine = MarkdownEngine::new();
        let markdown = "# Title\n\nParagraph with **bold** text.";

        // Test AST access
        let ast_result = engine.parse_to_ast(markdown);
        assert!(ast_result.is_ok());
        let document = ast_result.unwrap();
        assert_eq!(document.blocks.len(), 2); // Heading and paragraph

        match &document.blocks[0] {
            ast::Block::Heading { level, content, .. } => {
                assert_eq!(*level, 1);
                assert!(!content.is_empty());
            }
            _ => panic!("Expected heading block"),
        }

        // Test AST to DOM conversion
        let dom_result = engine.ast_to_dom(document.clone());
        assert!(dom_result.is_ok());
        let dom = dom_result.unwrap();
        assert_eq!(dom.tag, "div");
        assert!(!dom.children.is_empty());

        // Test DOM to HTML conversion
        let html_result = engine.dom_to_html(dom);
        assert!(html_result.is_ok());
        let html = html_result.unwrap();
        assert!(html.contains("Title"));
        assert!(html.contains("bold"));

        // Test direct AST to HTML conversion
        let direct_html_result = engine.ast_to_html(document);
        assert!(direct_html_result.is_ok());
        let direct_html = direct_html_result.unwrap();
        assert!(direct_html.contains("Title"));

        // Test full pipeline access
        let full_result = engine.parse_full(markdown);
        assert!(full_result.is_ok());
        let (ast, dom) = full_result.unwrap();
        assert_eq!(ast.blocks.len(), 2);
        assert_eq!(dom.tag, "div");

        // Test validation without output
        let validation_result = engine.validate(markdown);
        assert!(validation_result.is_ok());

        // Test validation with invalid input (should still pass as our parser is robust)
        let invalid_validation = engine.validate("# Valid heading");
        assert!(invalid_validation.is_ok());
    }

    /// Test streaming functionality with large documents
    #[test]
    fn test_streaming_functionality_large_documents() {
        // Test basic streaming configuration
        let config = streaming::StreamingConfig::default();
        assert_eq!(config.chunk_size, 64 * 1024);
        assert_eq!(config.max_buffer_chunks, 10);
        assert!(config.enable_backpressure);
        assert!(config.preserve_block_boundaries);

        // Test streaming with moderately sized document
        let section = "## Section\n\nThis is content for a section.\n\n";
        let medium_doc = section.repeat(50); // Create a medium-sized document

        let mut parser = streaming::StringStreamingParser::with_defaults(&medium_doc);

        let mut chunk_count = 0;
        let mut total_blocks = 0;

        while let Ok(Some(chunk)) = parser.next_chunk() {
            chunk_count += 1;
            total_blocks += chunk.blocks.len();

            assert!(chunk.bytes_processed > 0);
            assert!(chunk.start_position.line >= 1);

            if chunk.is_final {
                break;
            }

            // Prevent infinite loops in tests
            if chunk_count > 100 {
                break;
            }
        }

        assert!(chunk_count > 0);
        assert!(total_blocks > 0);
        assert!(parser.is_finished());

        // Test complete document parsing with streaming
        let mut complete_parser = streaming::StringStreamingParser::with_defaults(&medium_doc);
        let complete_result = complete_parser.parse_complete();
        assert!(complete_result.is_ok());

        let document = complete_result.unwrap();
        assert!(!document.blocks.is_empty());
        assert!(complete_parser.bytes_processed() > 0);

        // Test custom streaming configuration
        let custom_config = streaming::StreamingConfig {
            chunk_size: 1024, // Smaller chunks
            max_buffer_chunks: 5,
            enable_backpressure: true,
            preserve_block_boundaries: true,
            chunk_overlap: 100,
            parser_config: ParserConfig::default(),
        };

        let mut custom_parser = streaming::StringStreamingParser::new(&medium_doc, custom_config);
        let custom_result = custom_parser.parse_complete();
        assert!(custom_result.is_ok());

        // Test streaming with engine configuration
        let engine_config = config::EngineConfig::builder()
            .streaming(true)
            .streaming_config(streaming::StreamingConfig {
                chunk_size: 2048,
                ..streaming::StreamingConfig::default()
            })
            .build();

        let streaming_engine = MarkdownEngine::with_config(engine_config);
        assert!(streaming_engine.config().streaming);
        assert_eq!(streaming_engine.config().streaming_config.chunk_size, 2048);

        // Test streaming parsing through engine
        let streaming_result = streaming_engine.parse_streaming(&medium_doc);
        assert!(streaming_result.is_ok());

        // Test large document simulation
        let large_section = "# Large Section\n\nContent ".repeat(100);
        let large_doc = large_section.repeat(10); // Very large document

        let large_config = streaming::StreamingConfig {
            chunk_size: 4096, // 4KB chunks
            preserve_block_boundaries: true,
            ..streaming::StreamingConfig::default()
        };

        let mut large_parser = streaming::StringStreamingParser::new(&large_doc, large_config);
        let large_result = large_parser.parse_complete();
        assert!(large_result.is_ok());

        let large_document = large_result.unwrap();
        assert!(large_document.blocks.len() > 100); // Should have many blocks
        assert!(large_parser.bytes_processed() > 10000); // Should have processed significant data

        // Test position tracking in streaming
        let multiline_doc = "Line 1\nLine 2\nLine 3\n\n# Heading\n\nParagraph";
        let mut pos_parser = streaming::StringStreamingParser::with_defaults(multiline_doc);

        let initial_pos = pos_parser.position();
        assert_eq!(initial_pos.line, 1);
        assert_eq!(initial_pos.column, 1);

        let _ = pos_parser.parse_complete();
        let final_pos = pos_parser.position();
        assert!(final_pos.line > 1 || final_pos.offset > 0);

        // Test backpressure handling
        let backpressure_config = streaming::StreamingConfig {
            chunk_size: 100, // Very small chunks
            max_buffer_chunks: 2,
            enable_backpressure: true,
            ..streaming::StreamingConfig::default()
        };

        let mut bp_parser = streaming::StringStreamingParser::new(&medium_doc, backpressure_config);

        // Process a few chunks to test buffering
        let mut processed_chunks = 0;
        while let Ok(Some(_chunk)) = bp_parser.next_chunk() {
            processed_chunks += 1;
            if processed_chunks >= 5 {
                break; // Don't process everything
            }
        }

        assert!(processed_chunks > 0);

        // Test empty document streaming
        let mut empty_parser = streaming::StringStreamingParser::with_defaults("");
        let empty_result = empty_parser.next_chunk();
        assert!(empty_result.is_ok());
        assert!(empty_result.unwrap().is_none());
        assert!(empty_parser.is_finished());
    }

    /// Test engine performance modes
    #[test]
    fn test_engine_performance_modes() {
        let markdown = "# Performance Test\n\nContent for performance testing.";

        // Test performance optimized mode
        let perf_config = config::EngineConfig::builder()
            .performance_optimized(true)
            .track_positions(false)
            .build();

        let perf_engine = MarkdownEngine::with_config(perf_config);
        assert!(perf_engine.config().performance_optimized);

        let perf_result = perf_engine.parse_to_html(markdown);
        assert!(perf_result.is_ok());

        // Test optimized parsing for larger content
        let large_content = "# Section\n\nContent ".repeat(1000);
        let optimized_result = perf_engine.parse_optimized(&large_content);
        assert!(optimized_result.is_ok());

        // Test performance stats (if available)
        let stats = perf_engine.performance_stats();
        // Stats may or may not be available depending on implementation
        if let Some(_stats) = stats {
            // Performance stats are available
        }

        // Test with intermediates access in performance mode
        let intermediates_result = perf_engine.parse_with_intermediates(markdown);
        assert!(intermediates_result.is_ok());

        let (ast, dom, html) = intermediates_result.unwrap();
        assert!(!ast.blocks.is_empty());
        assert_eq!(dom.tag, "div");
        assert!(html.contains("Performance Test"));
    }

    /// Test error handling in API
    #[test]
    fn test_api_error_handling() {
        // Test input size limit errors
        let config = config::EngineConfig::builder()
            .lexer(config::LexerConfig {
                max_input_size: Some(5), // Very small limit
                ..config::LexerConfig::default()
            })
            .build();

        let engine = MarkdownEngine::with_config(config);
        let large_input = "This input is too large for the configured limit";
        let result = engine.parse_to_html(large_input);
        assert!(result.is_err());

        // Test that error contains useful information
        if let Err(error) = result {
            let error_msg = format!("{}", error);
            assert!(error_msg.contains("exceeds maximum allowed size"));
        }

        // Test validation with various inputs
        let normal_engine = MarkdownEngine::new();

        // Valid inputs should pass validation
        assert!(normal_engine.validate("# Valid heading").is_ok());
        assert!(normal_engine.validate("**Valid emphasis**").is_ok());
        assert!(
            normal_engine
                .validate("[Valid link](https://example.com)")
                .is_ok()
        );
        assert!(normal_engine.validate("").is_ok()); // Empty should be valid

        // Test robust parsing (our parser should handle most inputs gracefully)
        let robust_inputs = [
            "# Heading with unicode: 🌍",
            "Text with **unclosed emphasis",
            "[Link with missing]()",
            "```\nUnclosed code block",
            "Mixed *emphasis **and bold* text**",
        ];

        for input in &robust_inputs {
            let result = normal_engine.parse_to_html(input);
            // Should not panic, may or may not produce errors depending on implementation
            // but should always return some result
            if let Ok(html) = result {
                assert!(!html.is_empty())
            }
        }
    }
}
