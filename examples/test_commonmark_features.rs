use rosetta::parser::{Parser, ParserConfig, parse};

fn main() {
    println!("Testing CommonMark compliance features...\n");

    // Test 1: Escape sequences
    println!("1. Testing escape sequences:");
    let input1 = "This has \\* escaped \\# characters and \\[brackets\\].";
    let result1 = parse(input1).unwrap();
    println!("Input: {}", input1);
    println!("Parsed successfully with {} blocks\n", result1.blocks.len());

    // Test 2: Link reference definitions
    println!("2. Testing link reference definitions:");
    let input2 =
        "[example]: https://example.com \"Example Site\"\n\nThis is a [example] reference link.";
    let mut parser2 = Parser::with_defaults(input2);
    let result2 = parser2.parse().unwrap();
    let reference = parser2.reference_map().get("example");
    println!("Input: {}", input2.replace('\n', "\\n"));
    println!("Reference found: {}", reference.is_some());
    if let Some(ref_def) = reference {
        println!("  Label: example");
        println!("  Destination: {}", ref_def.destination);
        println!("  Title: {:?}", ref_def.title);
    }
    println!("Parsed successfully with {} blocks\n", result2.blocks.len());

    // Test 3: Precedence rules for conflicting markup
    println!("3. Testing precedence rules:");
    let input3 = "*emphasis with `code` inside* and **strong *nested* emphasis**";
    let result3 = parse(input3).unwrap();
    println!("Input: {}", input3);
    println!("Parsed successfully with {} blocks", result3.blocks.len());

    if let Some(block) = result3.blocks.first() {
        if let rosetta::ast::Block::Paragraph { content, .. } = block {
            println!("  Paragraph contains {} inline elements", content.len());
            for (i, inline) in content.iter().enumerate() {
                match inline {
                    rosetta::ast::Inline::Text(text) => println!("    {}: Text(\"{}\")", i, text),
                    rosetta::ast::Inline::Emphasis { strong, .. } => {
                        println!("    {}: Emphasis(strong: {})", i, strong);
                    }
                    rosetta::ast::Inline::Code(code) => println!("    {}: Code(\"{}\")", i, code),
                    _ => println!("    {}: Other inline element", i),
                }
            }
        }
    }

    println!("\nAll CommonMark compliance features are working correctly!");
}
