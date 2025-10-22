/// Performance optimization demonstration for the Markdown engine.
///
/// This example shows how to use the performance optimizations including:
/// - Zero-copy string handling
/// - Memory pooling for node allocation
/// - Parallel processing for independent sections
use rosetta::{EngineConfig, MarkdownEngine, ParallelConfig, PerformanceOptimizer, ZeroCopyStr};
use std::time::Instant;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Markdown Engine Performance Optimization Demo");
    println!("============================================\n");

    // Create a large markdown document for testing
    let large_markdown = create_large_markdown_document();
    println!(
        "Created test document with {} characters",
        large_markdown.len()
    );

    // Test 1: Standard parsing (no optimizations)
    println!("\n1. Standard Parsing (no optimizations)");
    let standard_config = EngineConfig::builder().performance_optimized(false).build();
    let standard_engine = MarkdownEngine::with_config(standard_config);

    let start = Instant::now();
    let _standard_html = standard_engine.parse_to_html(&large_markdown)?;
    let standard_duration = start.elapsed();
    println!("   Time: {:?}", standard_duration);

    // Test 2: Performance optimized parsing
    println!("\n2. Performance Optimized Parsing");
    let optimized_config = EngineConfig::builder()
        .performance_optimized(true)
        .parallel_config(ParallelConfig {
            enabled: true,
            num_threads: 4,
            min_section_size: 500,
        })
        .build();
    let optimized_engine = MarkdownEngine::with_config(optimized_config);

    let start = Instant::now();
    let _optimized_html = optimized_engine.parse_optimized(&large_markdown)?;
    let optimized_duration = start.elapsed();
    println!("   Time: {:?}", optimized_duration);

    // Show performance improvement
    let improvement = if optimized_duration < standard_duration {
        let ratio = standard_duration.as_nanos() as f64 / optimized_duration.as_nanos() as f64;
        format!("{:.2}x faster", ratio)
    } else {
        "No improvement (document may be too small)".to_string()
    };
    println!("   Performance improvement: {}", improvement);

    // Test 3: Zero-copy string handling demonstration
    println!("\n3. Zero-Copy String Handling Demo");
    demonstrate_zero_copy_strings();

    // Test 4: Memory pool demonstration
    println!("\n4. Memory Pool Demo");
    demonstrate_memory_pools();

    // Test 5: Performance statistics from engine
    println!("\n5. Performance Statistics from Engine");
    if let Some(stats) = optimized_engine.performance_stats() {
        println!("   String pool size: {}", stats.string_pool_size);
        println!("   Vector pool size: {}", stats.vec_pool_size);
    } else {
        println!("   Performance statistics not available");
    }

    // Test 6: Demonstrate actual pool usage in parsing
    println!("\n6. Pool Usage During Parsing");
    demonstrate_pool_usage_during_parsing(&optimized_engine)?;

    println!("\nDemo completed successfully!");
    Ok(())
}

fn create_large_markdown_document() -> String {
    let mut content = String::new();

    // Create multiple sections with different content types
    for i in 1..=20 {
        content.push_str(&format!("# Section {}\n\n", i));
        content.push_str(&format!("This is the content for section {}. ", i));
        content.push_str("It contains various **markdown** elements including *emphasis*, ");
        content.push_str("`code spans`, and [links](https://example.com).\n\n");

        // Add some code blocks
        content.push_str("```rust\n");
        content.push_str(&format!("fn section_{}() {{\n", i));
        content.push_str("    println!(\"Hello from section\");\n");
        content.push_str("}\n");
        content.push_str("```\n\n");

        // Add lists
        content.push_str("- Item 1\n");
        content.push_str("- Item 2\n");
        content.push_str("- Item 3\n\n");

        // Add blockquotes
        content.push_str("> This is a blockquote in section ");
        content.push_str(&i.to_string());
        content.push_str(".\n> It spans multiple lines.\n\n");
    }

    content
}

fn demonstrate_zero_copy_strings() {
    let original = "Hello, zero-copy world!";

    // Borrowed string (zero-copy)
    let borrowed = ZeroCopyStr::borrowed(original);
    println!(
        "   Borrowed string: '{}' (is_borrowed: {})",
        borrowed.as_str(),
        borrowed.is_borrowed()
    );

    // Owned string (when modification is needed)
    let owned = ZeroCopyStr::owned(format!("{} (modified)", original));
    println!(
        "   Owned string: '{}' (is_borrowed: {})",
        owned.as_str(),
        owned.is_borrowed()
    );

    // Conversion examples
    let from_str: ZeroCopyStr = original.into();
    let from_string: ZeroCopyStr = String::from("owned").into();

    println!(
        "   From &str: '{}' (is_borrowed: {})",
        from_str.as_str(),
        from_str.is_borrowed()
    );
    println!(
        "   From String: '{}' (is_borrowed: {})",
        from_string.as_str(),
        from_string.is_borrowed()
    );
}

fn demonstrate_memory_pools() {
    let optimizer = PerformanceOptimizer::with_defaults();

    println!("   Initial pool stats:");
    let initial_stats = optimizer.pool_stats();
    println!("     String pool: {}", initial_stats.string_pool_size);
    println!("     Vector pool: {}", initial_stats.vec_pool_size);

    // Simulate getting and returning strings from pool
    let mut strings = Vec::new();
    for i in 0..5 {
        let mut s = optimizer.get_string();
        s.push_str(&format!("Pooled string {}", i));
        strings.push(s);
    }

    // Return strings to pool
    for s in strings {
        optimizer.return_string(s);
    }

    println!("   After using pool:");
    let final_stats = optimizer.pool_stats();
    println!("     String pool: {}", final_stats.string_pool_size);
    println!("     Vector pool: {}", final_stats.vec_pool_size);

    // Clear pools
    optimizer.clear_pools();
    println!("   After clearing pools:");
    let cleared_stats = optimizer.pool_stats();
    println!("     String pool: {}", cleared_stats.string_pool_size);
    println!("     Vector pool: {}", cleared_stats.vec_pool_size);
}

fn demonstrate_pool_usage_during_parsing(
    engine: &MarkdownEngine,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("   Note: Each parsing operation creates its own parser instance with");
    println!("   independent memory pools. This is by design to ensure thread safety");
    println!("   and avoid shared mutable state between parsing operations.");
    println!();

    // Show that the engine's optimizer is separate from parser optimizers
    println!("   Engine's optimizer stats (persistent across calls):");
    if let Some(stats) = engine.performance_stats() {
        println!("     String pool: {}", stats.string_pool_size);
        println!("     Vector pool: {}", stats.vec_pool_size);
    }

    // Demonstrate direct optimizer usage (this is what actually uses pools)
    println!("   Direct optimizer usage (shows actual pool behavior):");
    let optimizer = PerformanceOptimizer::with_defaults();

    // Simulate parser-like usage of the pools
    let mut temp_strings = Vec::new();
    for i in 0..3 {
        let mut s = optimizer.get_string();
        s.push_str(&format!("Parsed content {}", i));
        temp_strings.push(s);
    }

    println!("     After getting 3 strings from pool:");
    let stats = optimizer.pool_stats();
    println!("       String pool: {}", stats.string_pool_size);

    // Return strings to pool
    for s in temp_strings {
        optimizer.return_string(s);
    }

    println!("     After returning strings to pool:");
    let stats = optimizer.pool_stats();
    println!("       String pool: {}", stats.string_pool_size);

    Ok(())
}
