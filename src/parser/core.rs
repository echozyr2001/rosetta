use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_until, take_while, take_while1};
use nom::character::complete::{char, digit1, multispace0, space0, space1};
use nom::combinator::{all_consuming, map, opt, recognize, verify};
use nom::error::Error as NomError;
use nom::multi::{many_m_n, many0, many1};
use nom::sequence::{delimited, preceded, terminated, tuple};

use super::config::ParserConfig;
use crate::ast::{Block, Document, Inline, ListItem, ListKind, SourceMap};
use crate::error::{ErrorHandler, Result};
use crate::lexer::Position;

/// Helper function to create position information from input offset
fn create_position(_original_input: &str, _current_input: &str) -> Option<Position> {
    // For now, return a simple position. This can be enhanced later.
    Some(Position::new())
}

/// Enhanced nom-based parser supporting comprehensive CommonMark constructs.
pub struct Parser<'input> {
    input: &'input str,
    original_input: &'input str,
    config: ParserConfig,
    error_handler: Box<dyn ErrorHandler>,
    reference_definitions: std::collections::HashMap<String, crate::ast::LinkReference>,
}

impl<'input> Parser<'input> {
    /// Creates a new parser instance with the given configuration.
    pub fn new(input: &'input str, config: ParserConfig) -> Self {
        let error_handler: Box<dyn ErrorHandler> = if let Some(max_errors) = config.max_errors {
            Box::new(crate::error::DefaultErrorHandler::with_max_errors(
                max_errors,
            ))
        } else {
            Box::new(crate::error::DefaultErrorHandler::new())
        };

        Self {
            input,
            original_input: input,
            config,
            error_handler,
            reference_definitions: std::collections::HashMap::new(),
        }
    }

    /// Convenience helper that uses default parser configuration.
    pub fn with_defaults(input: &'input str) -> Self {
        Self::new(input, ParserConfig::default())
    }

    /// Creates a parser with a custom error handler.
    pub fn with_error_handler(
        input: &'input str,
        config: ParserConfig,
        error_handler: Box<dyn ErrorHandler>,
    ) -> Self {
        Self {
            input,
            original_input: input,
            config,
            error_handler,
            reference_definitions: std::collections::HashMap::new(),
        }
    }

    /// Returns the current position in the input.
    pub fn position(&self) -> Position {
        Position::new() // Simplified for now
    }

    /// Returns a reference to the parser configuration.
    pub fn config(&self) -> &ParserConfig {
        &self.config
    }

    /// Returns a reference to the collected reference definitions.
    pub fn reference_definitions(
        &self,
    ) -> &std::collections::HashMap<String, crate::ast::LinkReference> {
        &self.reference_definitions
    }

    /// Parses the input into a `Document` AST.
    pub fn parse(self) -> Result<Document> {
        match all_consuming(|input| parse_document_internal(input, self.original_input))(self.input)
        {
            Ok((_, document)) => Ok(document),
            Err(e) => Err(crate::error::MarkdownError::parse_error(
                Position::new(),
                format!("Parse error: {:?}", e),
            )),
        }
    }
}

/// Internal document parser
fn parse_document_internal<'a>(
    input: &'a str,
    original_input: &'a str,
) -> IResult<&'a str, Document> {
    map(
        tuple((
            multispace0,
            many0(terminated(|i| parse_block(i, original_input), multispace0)),
        )),
        |(_, blocks)| Document {
            blocks,
            source_map: SourceMap::new(),
        },
    )(input)
}

/// Parse a single block element
fn parse_block<'a>(input: &'a str, original_input: &'a str) -> IResult<&'a str, Block> {
    alt((
        |i| parse_code_block(i, original_input),
        |i| parse_thematic_break(i, original_input),
        |i| parse_atx_heading(i, original_input),
        |i| parse_setext_heading(i, original_input),
        |i| parse_blockquote(i, original_input),
        |i| parse_bullet_list(i, original_input),
        |i| parse_ordered_list(i, original_input),
        |i| parse_html_block(i, original_input),
        |i| parse_paragraph(i, original_input),
    ))(input)
}

/// Parse ATX heading (# ## ### etc.)
fn parse_atx_heading<'a>(input: &'a str, original_input: &'a str) -> IResult<&'a str, Block> {
    map(
        tuple((
            verify(
                recognize(many1(char::<&str, NomError<&str>>('#'))),
                |s: &str| s.len() <= 6,
            ),
            space1::<&str, NomError<&str>>,
            take_while1(|c: char| c != '\n' && c != '\r'),
        )),
        |(hashes, _, content)| {
            let level = hashes.len() as u8;
            let content_text = content.trim();

            Block::Heading {
                level,
                content: vec![Inline::Text(content_text.to_string())],
                id: None,
                position: create_position(original_input, input),
            }
        },
    )(input)
}

/// Parse fenced code block
fn parse_code_block<'a>(input: &'a str, _original_input: &'a str) -> IResult<&'a str, Block> {
    alt((
        // Try to parse a complete code block first
        map(
            tuple((
                tag::<&str, &str, NomError<&str>>("```"),
                opt(take_while1(|c: char| c != '\n' && c != '\r')), // info string
                char::<&str, NomError<&str>>('\n'),
                take_until::<&str, &str, NomError<&str>>("```"),
                tag::<&str, &str, NomError<&str>>("```"),
            )),
            |(_, info, _, content, _)| {
                let language = info
                    .and_then(|info| info.split_whitespace().next())
                    .map(|s| s.to_string());

                Block::CodeBlock {
                    info: info.map(|s| s.to_string()),
                    content: content.to_string(),
                    language,
                    position: None,
                }
            },
        ),
        // Handle unclosed code blocks gracefully
        map(
            tuple((
                tag::<&str, &str, NomError<&str>>("```"),
                opt(take_while1(|c: char| c != '\n' && c != '\r')), // info string
                opt(char::<&str, NomError<&str>>('\n')),
                take_while(|_| true), // consume rest of input
            )),
            |(_, info, _, content)| {
                let language = info
                    .and_then(|info| info.split_whitespace().next())
                    .map(|s| s.to_string());

                Block::CodeBlock {
                    info: info.map(|s| s.to_string()),
                    content: content.to_string(),
                    language,
                    position: None,
                }
            },
        ),
    ))(input)
}

/// Parse bullet list
fn parse_bullet_list<'a>(input: &'a str, _original_input: &'a str) -> IResult<&'a str, Block> {
    map(many1(parse_bullet_list_item), |items| Block::List {
        kind: ListKind::Bullet { marker: '-' },
        tight: true,
        items,
        position: None,
    })(input)
}

/// Parse bullet list item
fn parse_bullet_list_item(input: &str) -> IResult<&str, ListItem> {
    map(
        terminated(
            tuple((
                alt((
                    char::<&str, NomError<&str>>('-'),
                    char::<&str, NomError<&str>>('+'),
                    char::<&str, NomError<&str>>('*'),
                )),
                space1::<&str, NomError<&str>>,
                take_while1(|c: char| c != '\n' && c != '\r'),
            )),
            opt(char::<&str, NomError<&str>>('\n')),
        ),
        |(_, _, content)| ListItem {
            content: vec![Block::Paragraph {
                content: vec![Inline::Text(content.to_string())],
                position: None,
            }],
            tight: true,
            task_list_marker: None,
        },
    )(input)
}

/// Parse Setext heading (underlined with = or -)
fn parse_setext_heading<'a>(input: &'a str, _original_input: &'a str) -> IResult<&'a str, Block> {
    map(
        tuple((
            verify(take_while1(|c: char| c != '\n' && c != '\r'), |s: &str| {
                !s.trim_start().starts_with('-')
                    && !s.trim_start().starts_with('+')
                    && !s.trim_start().starts_with('*')
            }),
            char::<&str, NomError<&str>>('\n'),
            verify(
                alt((
                    recognize(many1(char::<&str, NomError<&str>>('='))),
                    recognize(many1(char::<&str, NomError<&str>>('-'))),
                )),
                |s: &str| s.len() >= 3 && s.chars().all(|c| c == '=' || c == '-'),
            ),
        )),
        |(content, _, underline)| {
            let level = if underline.starts_with('=') { 1 } else { 2 };

            Block::Heading {
                level,
                content: vec![Inline::Text(content.trim().to_string())],
                id: None,
                position: None,
            }
        },
    )(input)
}

/// Parse blockquote
fn parse_blockquote<'a>(input: &'a str, _original_input: &'a str) -> IResult<&'a str, Block> {
    map(
        many1(preceded(
            tuple((
                char::<&str, NomError<&str>>('>'),
                space0::<&str, NomError<&str>>,
            )),
            terminated(
                take_while1(|c: char| c != '\n' && c != '\r'),
                opt(char::<&str, NomError<&str>>('\n')),
            ),
        )),
        |lines| {
            let content_text = lines.join("\n");
            let content = vec![Block::Paragraph {
                content: vec![Inline::Text(content_text)],
                position: None,
            }];

            Block::BlockQuote {
                content,
                position: None,
            }
        },
    )(input)
}

/// Parse ordered list
fn parse_ordered_list<'a>(input: &'a str, _original_input: &'a str) -> IResult<&'a str, Block> {
    map(many1(parse_ordered_list_item), |items| Block::List {
        kind: ListKind::Ordered {
            start: 1,
            delimiter: '.',
        },
        tight: true,
        items,
        position: None,
    })(input)
}

/// Parse ordered list item
fn parse_ordered_list_item(input: &str) -> IResult<&str, ListItem> {
    map(
        terminated(
            tuple((
                digit1::<&str, NomError<&str>>,
                alt((
                    char::<&str, NomError<&str>>('.'),
                    char::<&str, NomError<&str>>(')'),
                )),
                space1::<&str, NomError<&str>>,
                take_while1(|c: char| c != '\n' && c != '\r'),
            )),
            opt(char::<&str, NomError<&str>>('\n')),
        ),
        |(_, _, _, content)| ListItem {
            content: vec![Block::Paragraph {
                content: vec![Inline::Text(content.to_string())],
                position: None,
            }],
            tight: true,
            task_list_marker: None,
        },
    )(input)
}

/// Parse thematic break (---, ***, ___)
fn parse_thematic_break<'a>(input: &'a str, _original_input: &'a str) -> IResult<&'a str, Block> {
    map(
        terminated(
            verify(
                alt((
                    recognize(tuple((
                        many_m_n(3, usize::MAX, char::<&str, NomError<&str>>('-')),
                        space0::<&str, NomError<&str>>,
                    ))),
                    recognize(tuple((
                        many_m_n(3, usize::MAX, char::<&str, NomError<&str>>('*')),
                        space0::<&str, NomError<&str>>,
                    ))),
                    recognize(tuple((
                        many_m_n(3, usize::MAX, char::<&str, NomError<&str>>('_')),
                        space0::<&str, NomError<&str>>,
                    ))),
                )),
                |s: &str| {
                    let marker_char = s
                        .chars()
                        .find(|c| matches!(c, '-' | '*' | '_'))
                        .unwrap_or('-');
                    s.chars().filter(|&c| c == marker_char).count() >= 3
                },
            ),
            opt(char::<&str, NomError<&str>>('\n')),
        ),
        |_| Block::ThematicBreak { position: None },
    )(input)
}

/// Parse HTML block (simplified)
fn parse_html_block<'a>(input: &'a str, _original_input: &'a str) -> IResult<&'a str, Block> {
    map(
        delimited(
            char::<&str, NomError<&str>>('<'),
            take_until(">"),
            char::<&str, NomError<&str>>('>'),
        ),
        |content: &str| Block::HtmlBlock {
            content: format!("<{}>", content),
            position: None,
        },
    )(input)
}

/// Parse paragraph (fallback for text content)
fn parse_paragraph<'a>(input: &'a str, original_input: &'a str) -> IResult<&'a str, Block> {
    map(
        tuple((
            parse_inline_content,
            opt(char::<&str, NomError<&str>>('\n')),
        )),
        |(inlines, _)| Block::Paragraph {
            content: inlines,
            position: create_position(original_input, input),
        },
    )(input)
}

/// Parse inline content
fn parse_inline_content(input: &str) -> IResult<&str, Vec<Inline>> {
    many1(alt((
        parse_emphasis,
        parse_strong,
        parse_code_span,
        parse_link,
        parse_text_inline,
    )))(input)
}

/// Parse emphasis (*text*)
fn parse_emphasis(input: &str) -> IResult<&str, Inline> {
    map(
        delimited(
            char::<&str, NomError<&str>>('*'),
            is_not("*"),
            char::<&str, NomError<&str>>('*'),
        ),
        |content: &str| Inline::Emphasis {
            strong: false,
            content: vec![Inline::Text(content.to_string())],
        },
    )(input)
}

/// Parse strong emphasis (**text**)
fn parse_strong(input: &str) -> IResult<&str, Inline> {
    map(
        delimited(
            tag::<&str, &str, NomError<&str>>("**"),
            is_not("*"),
            tag::<&str, &str, NomError<&str>>("**"),
        ),
        |content: &str| Inline::Emphasis {
            strong: true,
            content: vec![Inline::Text(content.to_string())],
        },
    )(input)
}

/// Parse code span (`code`)
fn parse_code_span(input: &str) -> IResult<&str, Inline> {
    map(
        delimited(
            char::<&str, NomError<&str>>('`'),
            is_not("`"),
            char::<&str, NomError<&str>>('`'),
        ),
        |content: &str| Inline::Code(content.to_string()),
    )(input)
}

/// Parse link ([text](url))
fn parse_link(input: &str) -> IResult<&str, Inline> {
    map(
        tuple((
            delimited(
                char::<&str, NomError<&str>>('['),
                is_not("]"),
                char::<&str, NomError<&str>>(']'),
            ),
            delimited(
                char::<&str, NomError<&str>>('('),
                is_not(")"),
                char::<&str, NomError<&str>>(')'),
            ),
        )),
        |(text, url): (&str, &str)| Inline::Link {
            text: vec![Inline::Text(text.to_string())],
            destination: url.to_string(),
            title: None,
        },
    )(input)
}

/// Parse plain text
fn parse_text_inline(input: &str) -> IResult<&str, Inline> {
    map(
        take_while1(|c: char| !matches!(c, '*' | '`' | '[' | ']' | '(' | ')' | '\n' | '\r')),
        |text: &str| Inline::Text(text.to_string()),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_atx_heading() {
        let input = "# Hello World";
        let result = parse_atx_heading(input, input);
        assert!(result.is_ok());

        let (_, block) = result.unwrap();
        match block {
            Block::Heading { level, content, .. } => {
                assert_eq!(level, 1);
                assert_eq!(content.len(), 1);
                if let Inline::Text(text) = &content[0] {
                    assert_eq!(text, "Hello World");
                }
            }
            _ => panic!("Expected heading block"),
        }
    }

    #[test]
    fn test_parse_code_block() {
        let input = "```rust\nfn main() {}\n```";
        let result = parse_code_block(input, input);
        assert!(result.is_ok());

        let (_, block) = result.unwrap();
        match block {
            Block::CodeBlock {
                language, content, ..
            } => {
                assert_eq!(language.as_deref(), Some("rust"));
                assert_eq!(content, "fn main() {}\n");
            }
            _ => panic!("Expected code block"),
        }
    }

    #[test]
    fn test_parse_bullet_list() {
        let input = "- Item 1\n- Item 2\n";
        let result = parse_bullet_list(input, input);
        assert!(result.is_ok());

        let (_, block) = result.unwrap();
        match block {
            Block::List { kind, items, .. } => {
                assert!(matches!(kind, ListKind::Bullet { .. }));
                assert_eq!(items.len(), 2);
            }
            _ => panic!("Expected list block"),
        }
    }

    #[test]
    fn test_parse_paragraph() {
        let input = "This is a simple paragraph";
        let result = parse_paragraph(input, input);
        assert!(result.is_ok());

        let (_, block) = result.unwrap();
        match block {
            Block::Paragraph { content, .. } => {
                assert_eq!(content.len(), 1);
                if let Inline::Text(text) = &content[0] {
                    assert_eq!(text, "This is a simple paragraph");
                }
            }
            _ => panic!("Expected paragraph block"),
        }
    }

    #[test]
    fn test_parse_document() {
        let input = "# Title\n\nThis is a paragraph\n\n- List item";
        let parser = Parser::with_defaults(input);
        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 3); // heading, paragraph, list
    }

    #[test]
    fn test_parse_setext_heading() {
        let input = "Setext Heading\n==============";
        let result = parse_setext_heading(input, input);
        assert!(result.is_ok());

        let (_, block) = result.unwrap();
        match block {
            Block::Heading { level, content, .. } => {
                assert_eq!(level, 1);
                if let Inline::Text(text) = &content[0] {
                    assert_eq!(text, "Setext Heading");
                }
            }
            _ => panic!("Expected setext heading block"),
        }
    }

    #[test]
    fn test_parse_blockquote() {
        let input = "> This is a quote\n> Second line";
        let result = parse_blockquote(input, input);
        assert!(result.is_ok());

        let (_, block) = result.unwrap();
        match block {
            Block::BlockQuote { content, .. } => {
                assert!(!content.is_empty());
            }
            _ => panic!("Expected blockquote block"),
        }
    }

    #[test]
    fn test_parse_ordered_list() {
        let input = "1. First item\n2. Second item\n";
        let result = parse_ordered_list(input, input);
        assert!(result.is_ok());

        let (_, block) = result.unwrap();
        match block {
            Block::List { kind, items, .. } => {
                assert!(matches!(kind, ListKind::Ordered { .. }));
                assert_eq!(items.len(), 2);
            }
            _ => panic!("Expected ordered list block"),
        }
    }

    #[test]
    fn test_parse_thematic_break() {
        let input = "---";
        let result = parse_thematic_break(input, input);
        assert!(result.is_ok());

        let (_, block) = result.unwrap();
        match block {
            Block::ThematicBreak { .. } => {
                // Success
            }
            _ => panic!("Expected thematic break block"),
        }
    }
}
