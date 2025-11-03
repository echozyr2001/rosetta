use rosetta::ast::{Block, Inline};
use rosetta::nom_parser::{NomParser, parse, parse_with_config};
use rosetta::parser::ParserConfig;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Enhanced NomParser Demo ===\n");

    // Example 1: Simple parsing with default configuration
    println!("1. Simple parsing:");
    let simple_markdown = "# Hello World\n\nThis is a **bold** paragraph with `code`.";
    let doc = parse(simple_markdown)?;

    println!("Parsed {} blocks:", doc.blocks.len());
    for (i, block) in doc.blocks.iter().enumerate() {
        match block {
            Block::Heading { level, content, .. } => {
                println!("  Block {}: Heading (level {})", i, level);
                print_inline_content(content, 4);
            }
            Block::Paragraph { content, .. } => {
                println!("  Block {}: Paragraph", i);
                print_inline_content(content, 4);
            }
            _ => println!("  Block {}: {:?}", i, block),
        }
    }
    println!();

    // Example 2: Complex document with multiple block types
    println!("2. Complex document parsing:");
    let complex_markdown = r#"# Main Title

This is an introduction paragraph.

## Code Example

```rust
fn main() {
    println!("Hello, world!");
}
```

## Lists

- First item
- Second item
- Third item

1. Ordered first
2. Ordered second

> This is a blockquote
> with multiple lines.

---

Final paragraph.
"#;

    let complex_doc = parse(complex_markdown)?;
    println!(
        "Parsed {} blocks from complex document:",
        complex_doc.blocks.len()
    );

    let mut block_counts = std::collections::HashMap::new();
    for block in &complex_doc.blocks {
        let block_type = match block {
            Block::Heading { .. } => "Heading",
            Block::Paragraph { .. } => "Paragraph",
            Block::CodeBlock { .. } => "CodeBlock",
            Block::List { .. } => "List",
            Block::BlockQuote { .. } => "BlockQuote",
            Block::ThematicBreak { .. } => "ThematicBreak",
            Block::HtmlBlock { .. } => "HtmlBlock",
        };
        *block_counts.entry(block_type).or_insert(0) += 1;
    }

    for (block_type, count) in block_counts {
        println!("  {}: {}", block_type, count);
    }
    println!();

    // Example 3: Custom configuration
    println!("3. Custom configuration:");
    let config = ParserConfig {
        strict_commonmark: false,
        max_nesting_depth: 32,
        enable_gfm_extensions: true,
        track_source_positions: true,
        max_errors: Some(50),
        performance_optimized: true,
        parallel_config: Default::default(),
    };

    let custom_doc = parse_with_config("# Custom Config\n\nWith non-strict parsing.", config)?;
    println!("Custom config parsed {} blocks", custom_doc.blocks.len());
    println!();

    // Example 4: Parser instance with position tracking
    println!("4. Position tracking:");
    let parser = NomParser::with_defaults("# Positioned\n\nContent here.");
    println!("Initial position: {:?}", parser.position());

    let positioned_doc = parser.parse()?;
    for (i, block) in positioned_doc.blocks.iter().enumerate() {
        match block {
            Block::Heading { position, .. } => {
                println!("  Heading {} position: {:?}", i, position);
            }
            Block::Paragraph { position, .. } => {
                println!("  Paragraph {} position: {:?}", i, position);
            }
            _ => {}
        }
    }
    println!();

    // Example 5: Error handling
    println!("5. Error handling:");
    let malformed_markdown = "# Heading\n```\nunclosed code block\n\nMore content";
    match parse(malformed_markdown) {
        Ok(doc) => {
            println!(
                "Successfully parsed malformed input with {} blocks",
                doc.blocks.len()
            );
            println!("(Parser recovered gracefully)");
        }
        Err(e) => {
            println!("Parse error: {}", e);
        }
    }
    println!();

    // Example 6: Unicode handling
    println!("6. Unicode handling:");
    let unicode_markdown =
        "# 🌍 Unicode Title\n\nText with émojis 🎉 and àccénts.\n\n- 日本語\n- العربية\n- Русский";
    let unicode_doc = parse(unicode_markdown)?;

    println!(
        "Unicode document parsed {} blocks:",
        unicode_doc.blocks.len()
    );
    for (i, block) in unicode_doc.blocks.iter().enumerate() {
        match block {
            Block::Heading { content, .. } => {
                if let Some(Inline::Text(text)) = content.first() {
                    println!("  Heading {}: {}", i, text);
                }
            }
            Block::Paragraph { content, .. } => {
                if let Some(Inline::Text(text)) = content.first() {
                    println!(
                        "  Paragraph {}: {}",
                        i,
                        text.chars().take(30).collect::<String>()
                    );
                }
            }
            Block::List { items, .. } => {
                println!("  List {} with {} items", i, items.len());
            }
            _ => {}
        }
    }
    println!();

    println!("=== Demo Complete ===");
    Ok(())
}

fn print_inline_content(content: &[Inline], indent: usize) {
    let spaces = " ".repeat(indent);
    for (i, inline) in content.iter().enumerate() {
        match inline {
            Inline::Text(text) => println!("{}Inline {}: Text(\"{}\")", spaces, i, text),
            Inline::Emphasis { strong, content } => {
                println!(
                    "{}Inline {}: Emphasis(strong: {}, {} items)",
                    spaces,
                    i,
                    strong,
                    content.len()
                );
            }
            Inline::Code(code) => println!("{}Inline {}: Code(\"{}\")", spaces, i, code),
            Inline::Link {
                text,
                destination,
                title,
            } => {
                println!(
                    "{}Inline {}: Link(dest: \"{}\", title: {:?}, {} text items)",
                    spaces,
                    i,
                    destination,
                    title,
                    text.len()
                );
            }
            Inline::Image {
                alt,
                destination,
                title,
            } => {
                println!(
                    "{}Inline {}: Image(alt: \"{}\", dest: \"{}\", title: {:?})",
                    spaces, i, alt, destination, title
                );
            }
            Inline::HtmlInline(html) => {
                println!("{}Inline {}: HtmlInline(\"{}\")", spaces, i, html)
            }
            Inline::SoftBreak => println!("{}Inline {}: SoftBreak", spaces, i),
            Inline::HardBreak => println!("{}Inline {}: HardBreak", spaces, i),
        }
    }
}
