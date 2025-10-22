/// Example demonstrating the streaming parser functionality.
///
/// This example shows how to use the streaming parser for processing
/// large Markdown documents that may not fit entirely in memory.
use rosetta::streaming::{StreamingConfig, StringStreamingParser};
use std::io::Write;

fn main() {
    println!("=== Streaming Parser Demo ===\n");

    // 1. Basic streaming with default configuration
    println!("1. Basic streaming with default configuration:");
    let markdown = r#"# Large Document

This is the first section of a large document.

## Section 1

Content for section 1 with multiple paragraphs.

This is another paragraph in section 1.

## Section 2

Content for section 2.

### Subsection 2.1

More detailed content here.

## Section 3

Final section with some code:

```rust
fn main() {
    println!("Hello, streaming world!");
}
```

And some more text to make this document larger.
"#;

    let mut parser = StringStreamingParser::with_defaults(markdown);

    let mut chunk_count = 0;
    let mut total_blocks = 0;

    while let Ok(Some(chunk)) = parser.next_chunk() {
        chunk_count += 1;
        total_blocks += chunk.blocks.len();

        println!(
            "  Chunk {}: {} blocks, {} bytes processed",
            chunk_count,
            chunk.blocks.len(),
            chunk.bytes_processed
        );

        if chunk.is_final {
            println!("  Final chunk reached");
            break;
        }
    }

    println!("  Total: {} chunks, {} blocks\n", chunk_count, total_blocks);

    // 2. Custom streaming configuration
    println!("2. Custom streaming configuration (small chunks):");
    let config = StreamingConfig {
        chunk_size: 100, // Very small chunks for demonstration
        max_buffer_chunks: 3,
        enable_backpressure: true,
        preserve_block_boundaries: true,
        chunk_overlap: 20,
        ..StreamingConfig::default()
    };

    let mut custom_parser = StringStreamingParser::new(markdown, config);

    let mut chunk_count = 0;
    while let Ok(Some(chunk)) = custom_parser.next_chunk() {
        chunk_count += 1;
        println!(
            "  Small chunk {}: {} blocks, {} bytes",
            chunk_count,
            chunk.blocks.len(),
            chunk.bytes_processed
        );

        if chunk_count >= 10 {
            println!("  (Stopping after 10 chunks for demo)");
            break;
        }
    }
    println!();

    // 3. Complete document parsing with streaming
    println!("3. Complete document parsing with streaming:");
    let mut complete_parser = StringStreamingParser::with_defaults(markdown);

    match complete_parser.parse_complete() {
        Ok(document) => {
            println!("  Successfully parsed complete document:");
            println!("  - Total blocks: {}", document.blocks.len());
            println!("  - Bytes processed: {}", complete_parser.bytes_processed());

            // Show first few blocks
            for (i, block) in document.blocks.iter().take(3).enumerate() {
                match block {
                    rosetta::ast::Block::Heading { level, content, .. } => {
                        let text = content
                            .iter()
                            .filter_map(|inline| match inline {
                                rosetta::ast::Inline::Text(t) => Some(t.as_str()),
                                _ => None,
                            })
                            .collect::<Vec<_>>()
                            .join("");
                        println!("  - Block {}: Heading level {} - {}", i + 1, level, text);
                    }
                    rosetta::ast::Block::Paragraph { content, .. } => {
                        let text = content
                            .iter()
                            .filter_map(|inline| match inline {
                                rosetta::ast::Inline::Text(t) => Some(t.as_str()),
                                _ => None,
                            })
                            .collect::<Vec<_>>()
                            .join("");
                        let preview = if text.len() > 50 {
                            format!("{}...", &text[..50])
                        } else {
                            text
                        };
                        println!("  - Block {}: Paragraph - {}", i + 1, preview);
                    }
                    rosetta::ast::Block::CodeBlock {
                        language, content, ..
                    } => {
                        println!(
                            "  - Block {}: Code block ({}) - {} chars",
                            i + 1,
                            language.as_deref().unwrap_or("plain"),
                            content.len()
                        );
                    }
                    _ => {
                        println!("  - Block {}: Other block type", i + 1);
                    }
                }
            }
        }
        Err(e) => {
            println!("  Error parsing document: {}", e);
        }
    }
    println!();

    // 4. Simulating a very large document
    println!("4. Simulating a very large document:");
    let large_section =
        "## Large Section\n\nThis is a section with substantial content. ".repeat(50);
    let very_large_markdown = format!("# Very Large Document\n\n{}", large_section.repeat(10));

    println!(
        "  Generated document size: {} bytes",
        very_large_markdown.len()
    );

    let large_config = StreamingConfig {
        chunk_size: 2048, // 2KB chunks
        preserve_block_boundaries: true,
        ..StreamingConfig::default()
    };

    let mut large_parser = StringStreamingParser::new(&very_large_markdown, large_config);

    let start_time = std::time::Instant::now();
    match large_parser.parse_complete() {
        Ok(document) => {
            let duration = start_time.elapsed();
            println!("  Parsed in {:?}", duration);
            println!("  Total blocks: {}", document.blocks.len());
            println!("  Bytes processed: {}", large_parser.bytes_processed());
        }
        Err(e) => {
            println!("  Error: {}", e);
        }
    }
    println!();

    // 5. Position tracking demonstration
    println!("5. Position tracking demonstration:");
    let multiline_markdown = "Line 1\nLine 2\nLine 3\n\n# Heading\n\nParagraph";
    let mut pos_parser = StringStreamingParser::with_defaults(multiline_markdown);

    println!("  Initial position: {:?}", pos_parser.position());

    if let Ok(Some(_chunk)) = pos_parser.next_chunk() {
        println!("  Position after first chunk: {:?}", pos_parser.position());
    }

    let _ = pos_parser.parse_complete();
    println!("  Final position: {:?}", pos_parser.position());

    println!("\n=== Streaming Demo Complete ===");
}
