/// Example demonstrating the public API interface for the Rust Markdown Engine.
///
/// This example shows how to use the simple parse_markdown function,
/// the MarkdownEngine struct with builder pattern, and various configuration options.
use rosetta::{
    DomConfig, EngineConfig, GenerationConfig, LexerConfig, MarkdownEngine, parse_markdown,
};
use std::collections::HashMap;

fn main() {
    println!("=== Rust Markdown Engine Public API Demo ===\n");

    // 1. Simple parse_markdown function
    println!("1. Simple parse_markdown function:");
    let simple_markdown = "# Hello, World!\n\nThis is a **simple** example.";
    let simple_html = parse_markdown(simple_markdown);
    println!("Input: {}", simple_markdown);
    println!("Output: {}\n", simple_html);

    // 2. MarkdownEngine with default configuration
    println!("2. MarkdownEngine with default configuration:");
    let engine = MarkdownEngine::new();
    let markdown = "## Features\n\n- Easy to use\n- *Fast* parsing\n- `Code` support";
    match engine.parse_to_html(markdown) {
        Ok(html) => {
            println!("Input: {}", markdown);
            println!("Output: {}\n", html);
        }
        Err(e) => println!("Error: {}\n", e),
    }

    // 3. MarkdownEngine with builder pattern
    println!("3. MarkdownEngine with builder pattern:");
    let custom_engine = MarkdownEngine::with_config(
        EngineConfig::builder()
            .strict_commonmark(true)
            .pretty_print(true)
            .track_positions(false)
            .max_nesting_depth(32)
            .build(),
    );

    let complex_markdown = r#"# Advanced Example

> This is a blockquote with **bold** text.
> 
> ```rust
> fn main() {
>     println!("Hello, Rust!");
> }
> ```

1. First item
2. Second item with [link](https://example.com)
3. Third item with `inline code`
"#;

    match custom_engine.parse_to_html(complex_markdown) {
        Ok(html) => {
            println!("Input:\n{}", complex_markdown);
            println!("Output:\n{}\n", html);
        }
        Err(e) => println!("Error: {}\n", e),
    }

    // 4. Advanced configuration with global attributes
    println!("4. Advanced configuration with global attributes:");
    let mut global_attrs = HashMap::new();
    global_attrs.insert("class".to_string(), "markdown-content".to_string());
    global_attrs.insert("data-source".to_string(), "rust-engine".to_string());

    let advanced_engine = MarkdownEngine::with_config(
        EngineConfig::builder()
            .dom(DomConfig {
                preserve_positions: true,
                global_attributes: global_attrs,
                validate_structure: true,
            })
            .generation(GenerationConfig {
                source_maps: true,
                ..GenerationConfig::default()
            })
            .performance_optimized(true)
            .build(),
    );

    let attr_markdown = "### Configuration Demo\n\nThis content will have global attributes.";
    match advanced_engine.parse_to_html(attr_markdown) {
        Ok(html) => {
            println!("Input: {}", attr_markdown);
            println!("Output: {}\n", html);
        }
        Err(e) => println!("Error: {}\n", e),
    }

    // 5. Accessing intermediate representations
    println!("5. Accessing intermediate representations:");
    let engine = MarkdownEngine::new();
    let markdown = "# AST Demo\n\nParagraph with *emphasis*.";

    // Parse to AST
    match engine.parse_to_ast(markdown) {
        Ok(document) => {
            println!("AST blocks count: {}", document.blocks.len());

            // Convert AST to DOM
            match engine.ast_to_dom(document.clone()) {
                Ok(dom) => {
                    println!("DOM root tag: {}", dom.tag);
                    println!("DOM children count: {}", dom.children.len());

                    // Generate HTML from DOM
                    match engine.dom_to_html(dom) {
                        Ok(html) => println!("Final HTML: {}\n", html),
                        Err(e) => println!("DOM to HTML error: {}\n", e),
                    }
                }
                Err(e) => println!("AST to DOM error: {}\n", e),
            }
        }
        Err(e) => println!("Parse error: {}\n", e),
    }

    // 6. Validation without output generation
    println!("6. Validation without output generation:");
    let test_cases = vec![
        "# Valid heading",
        "**Valid emphasis**",
        "[Valid link](https://example.com)",
        "```\nValid code block\n```",
    ];

    for (i, test_case) in test_cases.iter().enumerate() {
        match engine.validate(test_case) {
            Ok(()) => println!("Test case {}: Valid ✓", i + 1),
            Err(e) => println!("Test case {}: Invalid - {}", i + 1, e),
        }
    }

    // 7. Performance-optimized configuration
    println!("\n7. Performance-optimized configuration:");
    let perf_engine = MarkdownEngine::with_config(
        EngineConfig::builder()
            .performance_optimized(true)
            .track_positions(false)
            .lexer(LexerConfig {
                track_positions: false,
                unicode_aware: true,
                max_input_size: Some(1024 * 1024), // 1MB limit
            })
            .streaming(false) // Disable streaming for this demo
            .build(),
    );

    let perf_markdown = "# Performance Test\n\nThis uses optimized settings.";
    match perf_engine.parse_to_html(perf_markdown) {
        Ok(html) => {
            println!("Performance mode result: {}", html);
        }
        Err(e) => println!("Performance mode error: {}", e),
    }

    println!("\n=== Demo Complete ===");
}
