use crate::ast::{Block, Document, Inline, SourceMap};
use crate::error::Result;
use crate::lexer::{NomLexer, token as nom_token};
use crate::parser::ParserConfig;

/// Experimental parser built on top of the nom-based lexer.
///
/// This parser currently supports a small subset of CommonMark constructs
/// (ATX headings and paragraphs). The goal is to grow it until it can replace
/// the legacy hand-written parser.
pub struct NomParser<'input> {
    lexer: NomLexer<'input>,
    _config: ParserConfig,
}

impl<'input> NomParser<'input> {
    /// Creates a new experimental parser instance.
    pub fn new(input: &'input str, config: ParserConfig) -> Self {
        Self {
            lexer: NomLexer::new(input),
            _config: config,
        }
    }

    /// Convenience helper that uses default parser configuration.
    pub fn with_defaults(input: &'input str) -> Self {
        Self::new(input, ParserConfig::default())
    }

    /// Parses the input into a `Document` AST.
    pub fn parse(mut self) -> Result<Document> {
        let mut blocks = Vec::new();
        let source_map = SourceMap::new();

        let mut paragraph_buffer = ParagraphBuilder::new();

        while let Some(token) = self.lexer.next_token() {
            match token {
                nom_token::Token::Eof => {
                    paragraph_buffer.flush_into(&mut blocks);
                    break;
                }
                nom_token::Token::AtxHeading(heading) => {
                    paragraph_buffer.flush_into(&mut blocks);

                    let text = heading.raw_content.trim();
                    let inline = Inline::Text(text.to_owned());
                    blocks.push(Block::Heading {
                        level: heading.level,
                        content: vec![inline],
                        id: None,
                        position: None,
                    });
                }
                nom_token::Token::Text(text_token) => {
                    paragraph_buffer.push_text(text_token.lexeme);
                }
                nom_token::Token::Whitespace(ws) => {
                    paragraph_buffer.push_whitespace_into(ws.lexeme, &mut blocks);
                }
                nom_token::Token::LineEnding(_) => {
                    paragraph_buffer.push_newline_into(&mut blocks);
                }
                // TODO: handle more token variants incrementally
                _ => {
                    paragraph_buffer.mark_break_into(&mut blocks);
                }
            }
        }

        Ok(Document { blocks, source_map })
    }
}

/// Simple helper that accumulates text tokens into paragraph AST nodes.
struct ParagraphBuilder {
    text: String,
    consecutive_newlines: usize,
}

impl ParagraphBuilder {
    fn new() -> Self {
        Self {
            text: String::new(),
            consecutive_newlines: 0,
        }
    }

    fn push_text(&mut self, fragment: &str) {
        if fragment.trim().is_empty() {
            return;
        }

        self.ensure_space();
        self.text.push_str(fragment.trim_end_matches('\n'));
        self.consecutive_newlines = 0;
    }

    fn push_whitespace_into(&mut self, whitespace: &str, output: &mut Vec<Block>) {
        if whitespace.contains('\n') {
            for ch in whitespace.chars() {
                if ch == '\n' {
                    self.push_newline_into(output);
                } else if ch.is_whitespace() {
                    self.ensure_space();
                }
            }
        } else {
            self.ensure_space();
        }
    }

    fn push_newline(&mut self) {
        self.ensure_space();
        self.consecutive_newlines += 1;
    }

    fn push_newline_into(&mut self, output: &mut Vec<Block>) {
        self.push_newline();
        if self.consecutive_newlines >= 2 {
            self.flush_into(output);
            self.consecutive_newlines = 0;
        }
    }

    fn mark_break_into(&mut self, output: &mut Vec<Block>) {
        self.flush_into(output);
        self.consecutive_newlines = 0;
    }

    fn flush_into(&mut self, output: &mut Vec<Block>) {
        if let Some(block) = self.flush() {
            output.push(block);
        }
    }

    fn flush(&mut self) -> Option<Block> {
        if self.text.trim().is_empty() {
            self.text.clear();
            return None;
        }

        let content = vec![Inline::Text(self.text.trim().to_owned())];
        self.text.clear();
        self.consecutive_newlines = 0;
        Some(Block::Paragraph {
            content,
            position: None,
        })
    }

    fn ensure_space(&mut self) {
        if !self.text.is_empty() && !self.text.ends_with(' ') {
            self.text.push(' ');
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_heading_and_paragraph() {
        let input = "# Heading\n\nSome text here.";
        let parser = NomParser::with_defaults(input);
        let doc = parser.parse().expect("nom parser should succeed");

        assert_eq!(doc.blocks.len(), 2);
        match &doc.blocks[0] {
            Block::Heading { level, .. } => assert_eq!(*level, 1),
            _ => panic!("expected heading"),
        }
        match &doc.blocks[1] {
            Block::Paragraph { content, .. } => {
                assert_eq!(content.len(), 1);
                assert!(matches!(&content[0], Inline::Text(text) if text == "Some text here."));
            }
            _ => panic!("expected paragraph"),
        }
    }
}
