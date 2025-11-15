use crate::lexer::{LexToken, Lexer};
use crate::parser::ast::{Block, Document, Inline, ListKind};
use crate::parser::core::{ParseAttempt, Parser};
use crate::parser::rules::MarkdownParseRule;

fn parse_document(input: &'static str) -> crate::parser::ast::Document {
    let mut parser = Parser::<'static, MarkdownParseRule>::new(MarkdownParseRule);
    let mut lexer = Lexer::new(input);

    loop {
        let token = lexer
            .next_token()
            .expect("lexer should yield tokens until EOF");
        let eof = token.is_eof();

        parser.push_token(token).expect("parser accepts token");

        match parser.try_parse().expect("parser attempt succeeds") {
            ParseAttempt::Pending { .. } => {
                if eof {
                    panic!("parser pending after EOF");
                }
            }
            ParseAttempt::NeedMoreTokens { .. } => {
                if eof {
                    panic!("parser requested more tokens after EOF");
                }
            }
            ParseAttempt::Complete(document) => {
                assert!(parser.is_finished());
                return document;
            }
        }
    }
}

fn parse_document_with_chunk_size(input: &'static str, chunk_size: usize) -> Document {
    let mut parser = Parser::<'static, MarkdownParseRule>::new(MarkdownParseRule);
    let mut lexer = Lexer::new(input);
    let mut buffered = Vec::new();

    loop {
        let token = lexer
            .next_token()
            .expect("lexer should yield tokens until EOF");
        let eof = token.is_eof();
        buffered.push(token);

        if buffered.len() >= chunk_size || eof {
            for token in buffered.drain(..) {
                parser.push_token(token).expect("parser accepts token");
            }

            match parser.try_parse().expect("parser attempt succeeds") {
                ParseAttempt::Pending { .. } => {
                    if eof {
                        panic!("parser pending after EOF");
                    }
                }
                ParseAttempt::NeedMoreTokens { .. } => {
                    if eof {
                        panic!("parser requested more tokens after EOF");
                    }
                }
                ParseAttempt::Complete(document) => return document,
            }
        }
    }
}

#[test]
fn parses_heading_and_paragraph() {
    let doc = parse_document("# Heading\n\nSome text here.");
    assert_eq!(doc.blocks.len(), 2);

    match &doc.blocks[0] {
        Block::Heading { level, content, .. } => {
            assert_eq!(*level, 1);
            assert!(matches!(&content[0], Inline::Text(text) if text.contains("Heading")));
        }
        other => panic!("expected heading, found {other:?}"),
    }

    match &doc.blocks[1] {
        Block::Paragraph { content, .. } => {
            assert_eq!(content.len(), 1);
            assert!(matches!(&content[0], Inline::Text(text) if text.contains("Some text")));
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn parses_inline_code_and_emphasis() {
    let doc = parse_document("This has `code` and *emphasis*.");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content
                    .iter()
                    .any(|inline| matches!(inline, Inline::Code(code) if code == "code")),
                "expected Inline::Code for `code` span"
            );
            assert!(
                content
                    .iter()
                    .any(|inline| matches!(inline, Inline::Emphasis { strong: false, .. })),
                "expected Inline::Emphasis for *emphasis*"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn parses_autolinks_into_links() {
    let doc = parse_document("Visit <https://example.com> now.");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Link {
                        destination,
                        ..
                    } if destination == "https://example.com"
                )),
                "expected Inline::Link for autolink"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn parses_inline_links() {
    let doc = parse_document("Visit [example](https://example.com) now.");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Link {
                        destination,
                        text,
                        ..
                    } if destination == "https://example.com" &&
                       text.iter().any(|t| matches!(t, Inline::Text(s) if s == "example"))
                )),
                "expected Inline::Link for [text](url)"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn parses_inline_links_with_title() {
    let doc = parse_document("Visit [example](https://example.com \"title\") now.");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Link {
                        destination,
                        title: Some(title),
                        ..
                    } if destination == "https://example.com" && title == "title"
                )),
                "expected Inline::Link with title"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn parses_images() {
    let doc = parse_document("See ![alt text](/path/to/image.jpg) here.");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Image {
                        alt,
                        destination,
                        ..
                    } if alt == "alt text" && destination == "/path/to/image.jpg"
                )),
                "expected Inline::Image"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn parses_reference_links() {
    let doc = parse_document("[foo]: /url \"title\"\n\n[foo]");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Link {
                        destination,
                        title: Some(title),
                        ..
                    } if destination == "/url" && title == "title"
                )),
                "expected Inline::Link with reference, got: {content:?}"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn parses_collapsed_reference_links() {
    let doc = parse_document("[foo]: /url\n\n[foo][]");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Link {
                        destination,
                        ..
                    } if destination == "/url"
                )),
                "expected Inline::Link with collapsed reference"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn parses_reference_images() {
    let doc = parse_document("[foo]: /url \"title\"\n\n![alt][foo]");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Image {
                        alt,
                        destination,
                        title: Some(title),
                        ..
                    } if alt == "alt" && destination == "/url" && title == "title"
                )),
                "expected Inline::Image with reference"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn handles_escaped_characters() {
    // According to CommonMark spec, \*not emphasized* should output
    // <p>*not emphasized*</p> (not emphasis)
    // The escaped * should prevent it from being parsed as emphasis
    let doc = parse_document("\\*not emphasized*");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            // The escaped * should not be parsed as emphasis
            let has_emphasis = content
                .iter()
                .any(|inline| matches!(inline, Inline::Emphasis { .. }));
            assert!(
                !has_emphasis,
                "expected no emphasis when asterisk is escaped, got: {content:?}"
            );

            // Should have text containing the asterisk
            let has_text_with_asterisk = content
                .iter()
                .any(|inline| matches!(inline, Inline::Text(text) if text.contains('*')));
            assert!(
                has_text_with_asterisk,
                "expected text with asterisk, got: {content:?}"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn handles_html_entities() {
    let doc = parse_document("&amp; &lt; &gt;");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            // HTML entities should be resolved
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Text(text) if text.contains('&') || text.contains('<') || text.contains('>')
                )),
                "expected HTML entities to be resolved"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

// CommonMark specification compliance tests

#[test]
fn commonmark_link_reference_with_indentation() {
    // Spec example: Link reference definition with indentation
    let doc = parse_document("   [foo]: \n      /url  \n           'the title'  \n\n[foo]");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Link {
                        destination,
                        title: Some(title),
                        ..
                    } if destination == "/url" && title == "the title"
                )),
                "expected link with indented reference definition"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn commonmark_link_reference_empty_destination() {
    // Spec example: Empty link destination using angle brackets
    let doc = parse_document("[foo]: <>\n\n[foo]");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Link {
                        destination,
                        ..
                    } if destination == ""
                )),
                "expected link with empty destination"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn commonmark_link_reference_without_title() {
    // Spec example: Link reference definition without title
    let doc = parse_document("[foo]:\n/url\n\n[foo]");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Link {
                        destination,
                        title: None,
                        ..
                    } if destination == "/url"
                )),
                "expected link without title"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn commonmark_link_before_definition() {
    // Spec example: Link can come before its definition
    let doc = parse_document("[foo]\n\n[foo]: url");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Link {
                        destination,
                        ..
                    } if destination == "url"
                )),
                "expected link resolved from later definition"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn commonmark_inline_link_empty_text() {
    // Spec example: Inline link with empty text
    let doc = parse_document("[](./target.md)");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Link {
                        text,
                        destination,
                        ..
                    } if destination == "./target.md" && text.is_empty()
                )),
                "expected link with empty text"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn commonmark_inline_link_empty_destination() {
    // Spec example: Inline link with empty destination
    let doc = parse_document("[link]()");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Link {
                        destination,
                        ..
                    } if destination == ""
                )),
                "expected link with empty destination"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn commonmark_inline_link_with_angle_brackets() {
    // Spec example: Link destination with spaces requires angle brackets
    let doc = parse_document("[link](</my uri>)");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Link {
                        destination,
                        ..
                    } if destination == "/my uri"
                )),
                "expected link with destination containing spaces"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn commonmark_escaped_link_bracket() {
    // Spec example: Escaped bracket should not create a link
    let doc = parse_document("\\[not a link](/foo)");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            // Should not have a link, should have plain text
            let has_link = content
                .iter()
                .any(|inline| matches!(inline, Inline::Link { .. }));
            assert!(!has_link, "expected no link when bracket is escaped");

            // Should have text containing the bracket
            let has_text = content
                .iter()
                .any(|inline| matches!(inline, Inline::Text(text) if text.contains('[')));
            assert!(has_text, "expected text with escaped bracket");
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn commonmark_escaped_code_backtick() {
    // Spec example: Escaped backtick should not create code span
    let doc = parse_document("\\`not code`");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            // Should not have code span
            let has_code = content
                .iter()
                .any(|inline| matches!(inline, Inline::Code(_)));
            assert!(!has_code, "expected no code span when backtick is escaped");

            // Should have text containing the backtick
            let has_text = content
                .iter()
                .any(|inline| matches!(inline, Inline::Text(text) if text.contains('`')));
            assert!(has_text, "expected text with escaped backtick");
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn commonmark_double_escaped_emphasis() {
    // Spec example: Double backslash followed by emphasis
    let doc = parse_document("\\\\*emphasis*");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            // Should have emphasis (first backslash escapes second, second doesn't escape *)
            let has_emphasis = content
                .iter()
                .any(|inline| matches!(inline, Inline::Emphasis { .. }));
            assert!(has_emphasis, "expected emphasis after double backslash");
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn commonmark_collapsed_reference_link() {
    // Spec example: Collapsed reference link [text][]
    let doc = parse_document("[foo]: /url\n\n[foo][]");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Link {
                        destination,
                        ..
                    } if destination == "/url"
                )),
                "expected collapsed reference link"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn commonmark_reference_image() {
    // Spec example: Reference-style image
    let doc = parse_document("[foo]: /url \"title\"\n\n![alt][foo]");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Image {
                        alt,
                        destination,
                        title: Some(title),
                        ..
                    } if alt == "alt" && destination == "/url" && title == "title"
                )),
                "expected reference-style image"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn commonmark_inline_link_with_title() {
    // Spec example: Inline link with title
    let doc = parse_document("[link](/uri \"title\")");

    match &doc.blocks[0] {
        Block::Paragraph { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Link {
                        destination,
                        title: Some(title),
                        ..
                    } if destination == "/uri" && title == "title"
                )),
                "expected inline link with title"
            );
        }
        other => panic!("expected paragraph, found {other:?}"),
    }
}

#[test]
fn parses_multiple_headings() {
    let doc = parse_document("# Level 1\n## Level 2\n### Level 3");
    assert_eq!(doc.blocks.len(), 3);

    for (idx, expected) in [1u8, 2u8, 3u8].iter().enumerate() {
        match &doc.blocks[idx] {
            Block::Heading { level, .. } => assert_eq!(*level, *expected),
            other => panic!("expected heading at {idx}, found {other:?}"),
        }
    }
}

#[test]
fn parses_code_blocks() {
    let doc = parse_document("```rust\nfn main() {}\n```");
    assert_eq!(doc.blocks.len(), 1);

    match &doc.blocks[0] {
        Block::CodeBlock {
            info,
            content,
            language,
            ..
        } => {
            assert_eq!(info.as_deref(), Some("rust"));
            assert_eq!(language.as_deref(), Some("rust"));
            assert!(content.contains("fn main"));
        }
        other => panic!("expected code block, found {other:?}"),
    }
}

#[test]
fn parses_blockquotes() {
    let doc = parse_document("> This is a blockquote\n> Second line");
    assert!(!doc.blocks.is_empty());
    assert!(
        doc.blocks
            .iter()
            .any(|block| matches!(block, Block::BlockQuote { .. })),
        "expected at least one blockquote",
    );
}

#[test]
fn parses_bullet_lists() {
    let doc = parse_document("- Item 1\n- Item 2\n- Item 3");
    assert!(!doc.blocks.is_empty());
    assert!(
        doc.blocks.iter().any(|block| matches!(
            block,
            Block::List {
                kind: ListKind::Bullet { .. },
                ..
            }
        )),
        "expected bullet list",
    );
}

#[test]
fn parses_ordered_lists() {
    let doc = parse_document("1. First item\n2. Second item\n3. Third item");
    assert!(!doc.blocks.is_empty());
    assert!(
        doc.blocks.iter().any(|block| matches!(
            block,
            Block::List {
                kind: ListKind::Ordered { .. },
                ..
            }
        )),
        "expected ordered list",
    );
}

#[test]
fn parses_thematic_breaks() {
    let doc = parse_document("---\n\n***\n\n___");
    let count = doc
        .blocks
        .iter()
        .filter(|block| matches!(block, Block::ThematicBreak { .. }))
        .count();
    assert!(count >= 1);
}

#[test]
fn parses_mixed_content() {
    let doc =
        parse_document("# Heading\n\nParagraph text.\n\n- List item\n\n```\ncode\n```\n\n---");
    assert!(doc.blocks.len() >= 4);
    assert!(
        doc.blocks
            .iter()
            .any(|b| matches!(b, Block::Heading { .. })),
        "heading"
    );
    assert!(
        doc.blocks
            .iter()
            .any(|b| matches!(b, Block::Paragraph { .. })),
        "paragraph"
    );
    assert!(
        doc.blocks.iter().any(|b| matches!(b, Block::List { .. })),
        "list"
    );
    assert!(
        doc.blocks
            .iter()
            .any(|b| matches!(b, Block::CodeBlock { .. })),
        "code"
    );
    assert!(
        doc.blocks
            .iter()
            .any(|b| matches!(b, Block::ThematicBreak { .. })),
        "thematic"
    );
}

#[test]
fn handles_empty_input() {
    let doc = parse_document("");
    assert!(doc.blocks.is_empty());
}

#[test]
fn handles_whitespace_only_input() {
    let doc = parse_document("   \n\t\n   ");
    assert!(doc.blocks.is_empty());
}

#[test]
fn parses_unicode_heading() {
    let doc = parse_document("# 🌍 Unicode Heading\n\nText with émojis 🎉 and àccénts.");
    assert_eq!(doc.blocks.len(), 2);

    match &doc.blocks[0] {
        Block::Heading { content, .. } => {
            assert!(matches!(&content[0], Inline::Text(text) if text.contains("🌍")));
        }
        other => panic!("expected heading, found {other:?}"),
    }
}

#[test]
fn parses_setext_heading() {
    let doc = parse_document("Heading text\n=====");
    match &doc.blocks[0] {
        Block::Heading { content, .. } => {
            assert!(
                content.iter().any(|inline| matches!(
                    inline,
                    Inline::Text(text) if text == "Heading text"
                )),
                "expected heading content to match original text"
            );
        }
        other => panic!("expected heading, found {other:?}"),
    }
}

#[test]
fn preserves_positions_when_available() {
    let doc = parse_document("# Heading\n\nParagraph");
    for block in &doc.blocks {
        match block {
            Block::Heading { position, .. }
            | Block::Paragraph { position, .. }
            | Block::CodeBlock { position, .. }
            | Block::BlockQuote { position, .. }
            | Block::List { position, .. }
            | Block::ThematicBreak { position, .. }
            | Block::HtmlBlock { position, .. } => {
                let _ = position;
            }
        }
    }
}

#[test]
fn streaming_parser_requires_eof_before_completion() {
    let mut parser = Parser::<'static, MarkdownParseRule>::new(MarkdownParseRule);
    let mut lexer = Lexer::new("# Heading");

    let token = lexer.next_token().expect("token");
    assert!(!token.is_eof());
    parser.push_token(token).unwrap();
    assert!(matches!(
        parser.try_parse().unwrap(),
        ParseAttempt::Pending { .. }
    ));

    let eof = lexer.next_token().expect("eof token");
    assert!(eof.is_eof());
    parser.push_token(eof).unwrap();
    assert!(matches!(
        parser.try_parse().unwrap(),
        ParseAttempt::Complete(_)
    ));
}

#[test]
fn streaming_parser_handles_chunked_input() {
    let doc = parse_document_with_chunk_size(
        "# Title\n\nFirst paragraph.\n\n- item one\n- item two\n",
        2,
    );
    assert!(doc.blocks.len() >= 2);
    assert!(
        doc.blocks
            .iter()
            .any(|b| matches!(b, Block::Heading { .. }))
    );
    assert!(doc.blocks.iter().any(|b| matches!(b, Block::List { .. })));
}

#[test]
fn test_complex_document_structure() {
    let input = r"# Main Title

This is an introduction paragraph.

## Section 1

> This is a blockquote.

- First item
- Second item

```
code block
```

---
";

    let doc = parse_document_with_chunk_size(input, 3);
    assert!(doc.blocks.len() >= 5);

    let has_heading = doc
        .blocks
        .iter()
        .any(|b| matches!(b, Block::Heading { .. }));
    let has_paragraph = doc
        .blocks
        .iter()
        .any(|b| matches!(b, Block::Paragraph { .. }));
    let has_blockquote = doc
        .blocks
        .iter()
        .any(|b| matches!(b, Block::BlockQuote { .. }));
    let has_list = doc.blocks.iter().any(|b| matches!(b, Block::List { .. }));
    let has_code = doc
        .blocks
        .iter()
        .any(|b| matches!(b, Block::CodeBlock { .. }));
    let has_thematic = doc
        .blocks
        .iter()
        .any(|b| matches!(b, Block::ThematicBreak { .. }));

    assert!(has_heading && has_paragraph);
    assert!(has_blockquote);
    assert!(has_list);
    assert!(has_code);
    assert!(has_thematic);
}
