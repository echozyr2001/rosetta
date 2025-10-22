/// Example demonstrating intermediate representation access (AST/DOM).
///
/// This example shows how to access and work with the Abstract Syntax Tree (AST)
/// and Document Object Model (DOM) representations of parsed Markdown.
use rosetta::{EngineConfig, MarkdownEngine};

fn main() {
    println!("=== Intermediate Representation Access Demo ===\n");

    let engine = MarkdownEngine::new();
    let markdown = r#"# Document Title

This is a paragraph with **bold text** and *italic text*.

## Code Example

Here's some Rust code:

```rust
fn hello_world() {
    println!("Hello, World!");
}
```

## List Example

- First item
- Second item with [a link](https://example.com)
- Third item

> This is a blockquote with some content.
"#;

    // 1. Parse to AST (Abstract Syntax Tree)
    println!("1. AST (Abstract Syntax Tree) Analysis:");
    match engine.parse_to_ast(markdown) {
        Ok(document) => {
            println!("  Document contains {} blocks", document.blocks.len());

            for (i, block) in document.blocks.iter().enumerate() {
                match block {
                    rosetta::ast::Block::Heading { level, content, .. } => {
                        let text = extract_text_from_inlines(content);
                        println!("  Block {}: Heading (level {}) - '{}'", i + 1, level, text);
                    }
                    rosetta::ast::Block::Paragraph { content, .. } => {
                        let text = extract_text_from_inlines(content);
                        let preview = if text.len() > 50 {
                            format!("{}...", &text[..50])
                        } else {
                            text
                        };
                        println!("  Block {}: Paragraph - '{}'", i + 1, preview);

                        // Analyze inline elements
                        for (j, inline) in content.iter().enumerate() {
                            match inline {
                                rosetta::ast::Inline::Emphasis {
                                    strong, content, ..
                                } => {
                                    let emphasis_text = extract_text_from_inlines(content);
                                    let emphasis_type = if *strong { "strong" } else { "emphasis" };
                                    println!(
                                        "    Inline {}: {} - '{}'",
                                        j + 1,
                                        emphasis_type,
                                        emphasis_text
                                    );
                                }
                                rosetta::ast::Inline::Link {
                                    text, destination, ..
                                } => {
                                    let link_text = extract_text_from_inlines(text);
                                    println!(
                                        "    Inline {}: Link - '{}' -> '{}'",
                                        j + 1,
                                        link_text,
                                        destination
                                    );
                                }
                                _ => {}
                            }
                        }
                    }
                    rosetta::ast::Block::CodeBlock {
                        language, content, ..
                    } => {
                        println!(
                            "  Block {}: Code Block ({}) - {} characters",
                            i + 1,
                            language.as_deref().unwrap_or("plain"),
                            content.len()
                        );
                    }
                    rosetta::ast::Block::List {
                        kind, items, tight, ..
                    } => {
                        let list_type = match kind {
                            rosetta::ast::ListKind::Bullet { .. } => "Bullet",
                            rosetta::ast::ListKind::Ordered { .. } => "Ordered",
                        };
                        println!(
                            "  Block {}: {} List ({} items, {})",
                            i + 1,
                            list_type,
                            items.len(),
                            if *tight { "tight" } else { "loose" }
                        );
                    }
                    rosetta::ast::Block::BlockQuote { content, .. } => {
                        println!(
                            "  Block {}: BlockQuote ({} nested blocks)",
                            i + 1,
                            content.len()
                        );
                    }
                    _ => {
                        println!("  Block {}: Other block type", i + 1);
                    }
                }
            }
        }
        Err(e) => {
            println!("  Error parsing to AST: {}", e);
        }
    }
    println!();

    // 2. Convert AST to DOM (Document Object Model)
    println!("2. DOM (Document Object Model) Analysis:");
    match engine.parse_to_ast(markdown) {
        Ok(document) => match engine.ast_to_dom(document) {
            Ok(dom) => {
                println!("  DOM root element: <{}>", dom.tag);
                println!("  DOM has {} direct children", dom.children.len());

                analyze_dom_node(&dom, 1);
            }
            Err(e) => {
                println!("  Error converting to DOM: {}", e);
            }
        },
        Err(e) => {
            println!("  Error parsing to AST: {}", e);
        }
    }
    println!();

    // 3. Generate HTML from DOM
    println!("3. HTML Generation from DOM:");
    match engine.parse_to_ast(markdown) {
        Ok(document) => {
            match engine.ast_to_dom(document) {
                Ok(dom) => {
                    let html = engine
                        .dom_to_html(dom)
                        .unwrap_or_else(|e| format!("Error generating HTML: {}", e));

                    println!("  Generated HTML ({} characters):", html.len());

                    // Show first few lines of HTML
                    let lines: Vec<&str> = html.lines().take(10).collect();
                    for (i, line) in lines.iter().enumerate() {
                        if line.trim().is_empty() {
                            continue;
                        }
                        println!("    {}: {}", i + 1, line.trim());
                    }

                    if html.lines().count() > 10 {
                        println!("    ... ({} more lines)", html.lines().count() - 10);
                    }
                }
                Err(e) => {
                    println!("  Error converting to DOM: {}", e);
                }
            }
        }
        Err(e) => {
            println!("  Error parsing to AST: {}", e);
        }
    }
    println!();

    // 4. Full pipeline with intermediate access
    println!("4. Full Pipeline with Intermediate Access:");
    let engine_with_config = MarkdownEngine::with_config(
        EngineConfig::builder()
            .pretty_print(true)
            .track_positions(true)
            .build(),
    );

    match engine_with_config.parse_full(markdown) {
        Ok((document, dom)) => {
            println!("  Successfully processed through full pipeline:");
            println!("  - AST blocks: {}", document.blocks.len());
            println!("  - DOM children: {}", dom.children.len());
            println!("  - DOM tag: <{}>", dom.tag);

            // Show some DOM attributes
            if !dom.attributes.is_empty() {
                println!("  - DOM attributes:");
                for (key, value) in &dom.attributes {
                    println!("    {}=\"{}\"", key, value);
                }
            }
        }
        Err(e) => {
            println!("  Error in full pipeline: {}", e);
        }
    }
    println!();

    // 5. Direct AST to HTML conversion (bypassing DOM)
    println!("5. Direct AST to HTML Conversion:");
    match engine.parse_to_ast(markdown) {
        Ok(document) => {
            match engine.ast_to_html(document) {
                Ok(html) => {
                    println!("  Direct AST->HTML conversion successful");
                    println!("  HTML length: {} characters", html.len());

                    // Count HTML elements
                    let element_count = html.matches('<').count();
                    println!("  Estimated HTML elements: {}", element_count);
                }
                Err(e) => {
                    println!("  Error in direct AST->HTML conversion: {}", e);
                }
            }
        }
        Err(e) => {
            println!("  Error parsing to AST: {}", e);
        }
    }

    println!("\n=== Intermediate Access Demo Complete ===");
}

/// Helper function to extract text content from inline elements
fn extract_text_from_inlines(inlines: &[rosetta::ast::Inline]) -> String {
    let mut result = String::new();

    for inline in inlines {
        match inline {
            rosetta::ast::Inline::Text(text) => {
                result.push_str(text);
            }
            rosetta::ast::Inline::Emphasis { content, .. } => {
                result.push_str(&extract_text_from_inlines(content));
            }
            rosetta::ast::Inline::Link { text, .. } => {
                result.push_str(&extract_text_from_inlines(text));
            }
            rosetta::ast::Inline::Code(code) => {
                result.push_str(code);
            }
            _ => {}
        }
    }

    result
}

/// Helper function to analyze DOM node structure recursively
fn analyze_dom_node(node: &rosetta::DomNode, depth: usize) {
    let indent = "  ".repeat(depth);

    println!("{}DOM Node: <{}>", indent, node.tag);

    if !node.attributes.is_empty() {
        println!("{}  Attributes: {:?}", indent, node.attributes);
    }

    println!("{}  Children: {}", indent, node.children.len());

    // Only show first few children to avoid too much output
    for (i, child) in node.children.iter().take(3).enumerate() {
        match child {
            rosetta::dom::DomChild::Element(element) => {
                if depth < 3 {
                    // Limit recursion depth
                    analyze_dom_node(element, depth + 1);
                } else {
                    println!("{}    Child {}: <{}> (nested)", indent, i + 1, element.tag);
                }
            }
            rosetta::dom::DomChild::Text(text) => {
                let preview = if text.len() > 30 {
                    format!("{}...", &text[..30])
                } else {
                    text.clone()
                };
                println!("{}    Child {}: Text - '{}'", indent, i + 1, preview);
            }
        }
    }

    if node.children.len() > 3 {
        println!(
            "{}    ... ({} more children)",
            indent,
            node.children.len() - 3
        );
    }
}
