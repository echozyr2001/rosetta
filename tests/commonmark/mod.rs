use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use lazy_static::lazy_static;
use regex::Regex;

use rosetta::ast::Document;
use rosetta::config::MarkdownEngine;
use rosetta::dom::DomNode;
use rosetta::parser::{Parser, ParserConfig};

// -----------------------------------------------------------------------------
// Constants & Helpers
// -----------------------------------------------------------------------------

lazy_static! {
    static ref EXAMPLE_DELIMITER_RE: Regex =
        Regex::new(r"(?m)^````+\s+example(?:\s+(?P<section>.+?))?\s*$").expect("regex");
}

static SPEC_FILE: &str = "tests/commonmark/spec.txt";

fn normalize_visible_whitespace(input: &str) -> String {
    // The CommonMark spec uses the "visible tab" arrow; convert it back to a tab.
    input.replace('→', "\t")
}

/// Represents a single CommonMark specification example.
#[derive(Debug, Clone)]
struct SpecExample {
    section: String,
    index: usize,
    markdown: String,
    expected_html: String,
}

#[derive(Debug)]
struct SpecExamples {
    examples: Vec<SpecExample>,
}

#[allow(clippy::while_let_on_iterator)]
impl SpecExamples {
    fn from_spec_file(path: &Path) -> anyhow::Result<Self> {
        let spec = fs::read_to_string(path)?;
        let mut section_stack: Vec<String> = Vec::new();
        let mut examples = Vec::new();
        let mut idx = 1;

        let mut lines = spec.lines().peekable();

        while let Some(line) = lines.next() {
            // Quick check before expensive regex
            if line.starts_with('#') {
                let level = line.chars().take_while(|c| *c == '#').count();
                let title = line[level..].trim().to_string();
                if level == 1 {
                    section_stack.clear();
                } else {
                    while section_stack.len() >= level {
                        section_stack.pop();
                    }
                }
                section_stack.push(title);
                continue;
            }

            // Only check for example delimiter if line starts with backticks
            if !line.starts_with('`') {
                continue;
            }

            if let Some(caps) = EXAMPLE_DELIMITER_RE.captures(line) {
                let section = caps
                    .name("section")
                    .map(|m| m.as_str().trim().to_string())
                    .filter(|s| !s.is_empty())
                    .or_else(|| section_stack.last().cloned())
                    .unwrap_or_else(|| "Unknown".to_string());

                let mut example_lines = Vec::new();
                while let Some(example_line) = lines.next() {
                    if example_line.trim() == "." {
                        break;
                    }
                    example_lines.push(example_line.to_string());
                }

                let mut expected_lines = Vec::new();
                while let Some(expected_line) = lines.next() {
                    if expected_line.starts_with("````") {
                        break;
                    }
                    expected_lines.push(expected_line.to_string());
                }

                let markdown = normalize_visible_whitespace(&example_lines.join("\n"));
                let expected_html = normalize_visible_whitespace(&expected_lines.join("\n"));

                examples.push(SpecExample {
                    section,
                    index: idx,
                    markdown,
                    expected_html,
                });

                idx += 1;
            }
        }

        examples.sort_by_key(|example| example.index);

        Ok(Self { examples })
    }
}

fn load_examples() -> &'static SpecExamples {
    use std::sync::OnceLock;
    static EXAMPLES: OnceLock<SpecExamples> = OnceLock::new();

    EXAMPLES.get_or_init(|| {
        use std::time::Instant;
        let spec_path =
            PathBuf::from(env::var("COMMONMARK_SPEC").unwrap_or_else(|_| SPEC_FILE.to_string()));
        eprintln!("Loading CommonMark spec from {:?}...", spec_path);
        let start = Instant::now();
        let examples = SpecExamples::from_spec_file(&spec_path)
            .expect("failed to load CommonMark spec examples");
        eprintln!(
            "Loaded {} examples in {:?}",
            examples.examples.len(),
            start.elapsed()
        );
        examples
    })
}

// -----------------------------------------------------------------------------
// Pipelines
// -----------------------------------------------------------------------------

fn run_full_pipeline(markdown: &str) -> anyhow::Result<String> {
    let config = MarkdownEngine::builder().strict_commonmark(true).build();
    let engine = MarkdownEngine::with_config(config);
    Ok(engine.parse_to_html(markdown)?)
}

fn run_parser_stage(markdown: &str) -> anyhow::Result<Document> {
    let config = ParserConfig::default();
    Ok(Parser::new(markdown, config).parse()?)
}

fn run_ast_to_dom_stage(document: Document) -> anyhow::Result<DomNode> {
    let engine =
        MarkdownEngine::with_config(MarkdownEngine::builder().strict_commonmark(true).build());
    Ok(engine.ast_to_dom(document)?)
}

fn run_codegen_stage(dom: DomNode) -> anyhow::Result<String> {
    let engine =
        MarkdownEngine::with_config(MarkdownEngine::builder().strict_commonmark(true).build());
    Ok(engine.dom_to_html(dom)?)
}

fn normalize_html(html: &str) -> String {
    let trimmed = html.trim();
    let mut normalized = String::new();
    let mut last_was_space = false;
    for ch in trimmed.chars() {
        if ch.is_whitespace() {
            if !last_was_space {
                normalized.push(' ');
                last_was_space = true;
            }
        } else {
            normalized.push(ch);
            last_was_space = false;
        }
    }
    normalized.replace("> <", "><")
}

fn should_run_example(example: &SpecExample) -> bool {
    if let Some(index) = env::var("COMMONMARK_TEST")
        .ok()
        .and_then(|single_index| single_index.parse::<usize>().ok())
    {
        return example.index == index;
    }

    if let Some(section) = env::var("COMMONMARK_SECTION")
        .ok()
        .map(|filter| filter.to_lowercase())
    {
        return example.section.to_lowercase().contains(&section);
    }

    true
}

fn iter_examples(examples: &SpecExamples) -> impl Iterator<Item = &SpecExample> {
    examples.examples.iter().filter(|ex| should_run_example(ex))
}

// -----------------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------------

#[test]
fn test_commonmark_parser_stage() {
    use std::time::Instant;
    let examples = load_examples();
    let filtered: Vec<_> = iter_examples(examples).collect();
    eprintln!("Running parser stage for {} examples...", filtered.len());

    let start = Instant::now();
    for (count, example) in filtered.iter().enumerate() {
        if count > 0 && count % 100 == 0 {
            eprintln!(
                "  Progress: {}/{} ({:.1}%)",
                count,
                filtered.len(),
                (count as f64 / filtered.len() as f64) * 100.0
            );
        }

        let document = run_parser_stage(&example.markdown).unwrap_or_else(|err| {
            panic!(
                "Parser failed for example {} ({}): {:?}",
                example.index, example.section, err
            )
        });

        let expected_empty = example.expected_html.trim().is_empty();
        assert!(
            !document.blocks.is_empty() || example.markdown.trim().is_empty() || expected_empty,
            "Parser produced empty document for example {} ({})",
            example.index,
            example.section
        );
    }
    eprintln!(
        "✓ Completed {} examples in {:?}",
        filtered.len(),
        start.elapsed()
    );
}

#[test]
fn test_commonmark_ast_to_dom_stage() {
    let examples = load_examples();
    let filtered: Vec<_> = iter_examples(examples).collect();
    eprintln!("Running AST to DOM stage for {} examples", filtered.len());

    for (count, example) in filtered.iter().enumerate() {
        if count > 0 && count % 50 == 0 {
            eprintln!("Progress: {}/{} examples processed", count, filtered.len());
        }

        let document = run_parser_stage(&example.markdown).unwrap_or_else(|err| {
            panic!(
                "Parser failed for example {} ({}): {:?}",
                example.index, example.section, err
            )
        });

        let dom = run_ast_to_dom_stage(document).unwrap_or_else(|err| {
            panic!(
                "AST to DOM failed for example {} ({}): {:?}",
                example.index, example.section, err
            )
        });

        assert!(
            dom.is_fragment(),
            "Root DOM node should be a fragment for example {}",
            example.index
        );
    }
    eprintln!("Completed: {}/{} examples", filtered.len(), filtered.len());
}

#[test]
fn test_commonmark_codegen_stage() {
    let examples = load_examples();
    let filtered: Vec<_> = iter_examples(examples).collect();
    eprintln!("Running codegen stage for {} examples", filtered.len());

    for (count, example) in filtered.iter().enumerate() {
        if count > 0 && count % 50 == 0 {
            eprintln!("Progress: {}/{} examples processed", count, filtered.len());
        }

        let document = run_parser_stage(&example.markdown).unwrap_or_else(|err| {
            panic!(
                "Parser failed for example {} ({}): {:?}",
                example.index, example.section, err
            )
        });

        let dom = run_ast_to_dom_stage(document).unwrap_or_else(|err| {
            panic!(
                "AST to DOM failed for example {} ({}): {:?}",
                example.index, example.section, err
            )
        });

        let html = run_codegen_stage(dom).unwrap_or_else(|err| {
            panic!(
                "Codegen failed for example {} ({}): {:?}",
                example.index, example.section, err
            )
        });

        assert!(
            !html.trim().is_empty() || example.expected_html.trim().is_empty(),
            "Codegen produced empty HTML for example {} ({})",
            example.index,
            example.section
        );
    }
    eprintln!("Completed: {}/{} examples", filtered.len(), filtered.len());
}

#[test]
fn test_commonmark_full_pipeline() {
    let examples = load_examples();
    let filtered: Vec<_> = iter_examples(examples).collect();
    eprintln!("Running full pipeline for {} examples", filtered.len());

    for (count, example) in filtered.iter().enumerate() {
        if count > 0 && count % 50 == 0 {
            eprintln!("Progress: {}/{} examples processed", count, filtered.len());
        }

        let html = run_full_pipeline(&example.markdown).unwrap_or_else(|err| {
            panic!(
                "Pipeline failed for example {} ({}): {:?}",
                example.index, example.section, err
            )
        });

        let normalized_actual = normalize_html(&html);
        let normalized_expected = normalize_html(&example.expected_html);

        assert_eq!(
            normalized_actual, normalized_expected,
            "HTML mismatch for example {} ({})\nMarkdown:\n{}\nExpected HTML:\n{}\nActual HTML:\n{}",
            example.index, example.section, example.markdown, example.expected_html, html
        );
    }
    eprintln!("Completed: {}/{} examples", filtered.len(), filtered.len());
}
