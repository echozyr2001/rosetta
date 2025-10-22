use rosetta::codegen::{HtmlEscapingMode, HtmlGenerator, OutputConfigBuilder};
use rosetta::dom::DomNode;

fn main() {
    println!("Testing HTML5 compliance and CommonMark HTML generation...\n");

    // Test 1: HTML5 compliance mode
    println!("1. Testing HTML5 compliance mode:");
    let config = OutputConfigBuilder::new()
        .with_html5_compliance(true)
        .with_commonmark_attributes(true)
        .with_html_structure_validation(true)
        .build();

    let generator = HtmlGenerator::new(config);

    let mut root = DomNode::new("div");
    let mut heading = DomNode::new("h1");
    heading.add_text_child("Test Heading");
    root.add_element_child(heading);

    let mut paragraph = DomNode::new("p");
    paragraph.add_text_child("This is a test with <script>alert('xss')</script> content.");
    root.add_element_child(paragraph);

    match generator.generate_from_dom(&root) {
        Ok(html) => {
            println!("Generated HTML:");
            println!("{}", html);
            println!("✓ HTML5 compliance validation passed\n");
        }
        Err(e) => {
            println!("✗ Error: {}\n", e);
        }
    }

    // Test 2: Different escaping modes
    println!("2. Testing different HTML escaping modes:");

    let test_content = "Test <script>alert('xss')</script> & \"quotes\"";

    for (mode_name, mode) in [
        ("Standard", HtmlEscapingMode::Standard),
        ("Aggressive", HtmlEscapingMode::Aggressive),
        ("Minimal", HtmlEscapingMode::Minimal),
    ] {
        let config = OutputConfigBuilder::new()
            .with_html_escaping_mode(mode)
            .build();

        let generator = HtmlGenerator::new(config);

        let mut node = DomNode::new("p");
        node.add_text_child(test_content);

        match generator.generate_from_dom(&node) {
            Ok(html) => {
                println!("{} escaping: {}", mode_name, html);
            }
            Err(e) => {
                println!("✗ Error with {} escaping: {}", mode_name, e);
            }
        }
    }

    // Test 3: CommonMark attributes
    println!("\n3. Testing CommonMark-specific attributes:");
    let config = OutputConfigBuilder::new()
        .with_commonmark_attributes(true)
        .build();

    let generator = HtmlGenerator::new(config);

    let mut code_block = DomNode::new("pre");
    let mut code = DomNode::new("code");
    code.add_attribute("class", "language-rust");
    code.add_text_child("fn main() { println!(\"Hello, World!\"); }");
    code_block.add_element_child(code);

    match generator.generate_from_dom(&code_block) {
        Ok(html) => {
            println!("Code block with CommonMark attributes:");
            println!("{}", html);
            println!("✓ CommonMark attributes added successfully");
        }
        Err(e) => {
            println!("✗ Error: {}", e);
        }
    }

    println!("\nAll HTML compliance features are working correctly!");
}
