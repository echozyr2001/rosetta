/// Adapters for existing Lexer and AST to work with CGP traits
///
/// This module demonstrates how to wrap existing implementations
/// to make them compatible with the CGP trait system.
use crate::ast::{Block, Document, Inline};
use crate::error::{ErrorHandler, Result};
use crate::lexer::{Lexer, Position, token::Token};
use crate::parser::ParserConfig;
use crate::traits::*;
use std::marker::PhantomData;

/// Adapter that makes the existing Lexer compatible with HasTokenProvider
pub struct LexerAdapter<'input> {
    lexer: Lexer<'input>,
    current_token: Option<Token<'input>>,
}

impl<'input> LexerAdapter<'input> {
    pub fn new(input: &'input str) -> Result<Self> {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next_token();
        Ok(Self {
            lexer,
            current_token,
        })
    }
}

impl<'input> HasTokenProvider for LexerAdapter<'input> {
    type Token = Token<'input>;

    fn current_token(&self) -> Option<&Self::Token> {
        self.current_token.as_ref()
    }

    fn next_token(&mut self) -> Result<Self::Token> {
        let token = self.lexer.next_token().ok_or_else(|| {
            crate::error::MarkdownError::parse_error(
                self.lexer.position(),
                "Unexpected end of file",
            )
        })?;
        self.current_token = Some(token.clone());
        Ok(token)
    }

    fn peek_token(&mut self) -> Result<Self::Token> {
        self.lexer.peek_token().ok_or_else(|| {
            crate::error::MarkdownError::parse_error(
                self.lexer.position(),
                "Unexpected end of file",
            )
        })
    }

    fn current_position(&self) -> Position {
        // Prefer position from current token if available (CGP principle)
        // This ensures we use the position information stored in the token
        // rather than getting it from the lexer
        if let Some(token) = self.current_token()
            && let Some(position) = token.position()
        {
            return position;
        }
        // Fallback to lexer position if token doesn't have position
        self.lexer.position()
    }
}

/// Adapter that implements AstBuilder for existing AST types
pub struct DefaultAstBuilder;

impl AstBuilder for DefaultAstBuilder {
    type Inline = Inline;
    type Block = Block;
    type Document = Document;

    fn build_document(&self, blocks: Vec<Self::Block>) -> Self::Document {
        Document {
            blocks,
            source_map: crate::ast::SourceMap::new(),
        }
    }

    fn build_heading(
        &self,
        level: u8,
        content: Vec<Self::Inline>,
        position: Option<Position>,
    ) -> Self::Block {
        Block::Heading {
            level,
            content,
            id: None,
            position,
        }
    }

    fn build_paragraph(
        &self,
        content: Vec<Self::Inline>,
        position: Option<Position>,
    ) -> Self::Block {
        Block::Paragraph { content, position }
    }

    fn build_code_block(
        &self,
        info: Option<String>,
        content: String,
        language: Option<String>,
        position: Option<Position>,
    ) -> Self::Block {
        Block::CodeBlock {
            info,
            content,
            language,
            position,
        }
    }

    fn build_block_quote(
        &self,
        content: Vec<Self::Block>,
        position: Option<Position>,
    ) -> Self::Block {
        Block::BlockQuote { content, position }
    }

    fn build_list(
        &self,
        kind: ListKind,
        tight: bool,
        items: Vec<ListItem<Self::Block>>,
        position: Option<Position>,
    ) -> Self::Block {
        // Convert CGP ListKind to AST ListKind
        let ast_kind = match kind {
            ListKind::Bullet { marker } => crate::ast::ListKind::Bullet { marker },
            ListKind::Ordered { start, delimiter } => {
                crate::ast::ListKind::Ordered { start, delimiter }
            }
        };

        // Convert CGP ListItem to AST ListItem
        let ast_items = items
            .into_iter()
            .map(|item| crate::ast::ListItem {
                content: item.content,
                tight: item.tight,
                task_list_marker: item.task_list_marker,
            })
            .collect();

        Block::List {
            kind: ast_kind,
            tight,
            items: ast_items,
            position,
        }
    }

    fn build_thematic_break(&self, position: Option<Position>) -> Self::Block {
        Block::ThematicBreak { position }
    }

    fn build_html_block(&self, content: String, position: Option<Position>) -> Self::Block {
        Block::HtmlBlock { content, position }
    }

    fn build_text(&self, content: String) -> Self::Inline {
        Inline::Text(content)
    }

    fn build_emphasis(&self, strong: bool, content: Vec<Self::Inline>) -> Self::Inline {
        Inline::Emphasis { strong, content }
    }

    fn build_code(&self, content: String) -> Self::Inline {
        Inline::Code(content)
    }

    fn build_link(
        &self,
        text: Vec<Self::Inline>,
        destination: String,
        title: Option<String>,
    ) -> Self::Inline {
        Inline::Link {
            text,
            destination,
            title,
        }
    }

    fn build_image(&self, alt: String, destination: String, title: Option<String>) -> Self::Inline {
        Inline::Image {
            alt,
            destination,
            title,
        }
    }

    fn build_html_inline(&self, content: String) -> Self::Inline {
        Inline::HtmlInline(content)
    }

    fn build_soft_break(&self) -> Self::Inline {
        Inline::SoftBreak
    }

    fn build_hard_break(&self) -> Self::Inline {
        Inline::HardBreak
    }
}

/// Complete parsing context that combines all adapters
pub struct DefaultParsingContext<'input> {
    token_provider: LexerAdapter<'input>,
    ast_builder: DefaultAstBuilder,
    config: ParserConfig,
    error_handler: Box<dyn ErrorHandler>,
    nesting_depth: usize,
}

impl<'input> DefaultParsingContext<'input> {
    pub fn new(input: &'input str, config: ParserConfig) -> Result<Self> {
        Ok(Self {
            token_provider: LexerAdapter::new(input)?,
            ast_builder: DefaultAstBuilder,
            config,
            error_handler: Box::new(crate::error::DefaultErrorHandler::new()),
            nesting_depth: 0,
        })
    }

    pub fn with_defaults(input: &'input str) -> Result<Self> {
        Self::new(input, ParserConfig::default())
    }
}

impl<'input> HasTokenProvider for DefaultParsingContext<'input> {
    type Token = Token<'input>;

    fn current_token(&self) -> Option<&Self::Token> {
        self.token_provider.current_token()
    }

    fn next_token(&mut self) -> Result<Self::Token> {
        self.token_provider.next_token()
    }

    fn peek_token(&mut self) -> Result<Self::Token> {
        self.token_provider.peek_token()
    }

    fn current_position(&self) -> Position {
        self.token_provider.current_position()
    }
}

impl<'input> HasAstBuilder for DefaultParsingContext<'input> {
    type Inline = Inline;
    type Block = Block;
    type Document = Document;

    fn ast_builder(
        &self,
    ) -> &dyn AstBuilder<Inline = Self::Inline, Block = Self::Block, Document = Self::Document>
    {
        &self.ast_builder
    }
}

impl<'input> HasConfig for DefaultParsingContext<'input> {
    type Config = ParserConfig;

    fn config(&self) -> &Self::Config {
        &self.config
    }
}

impl<'input> HasErrorHandler for DefaultParsingContext<'input> {
    fn error_handler(&self) -> &dyn ErrorHandler {
        self.error_handler.as_ref()
    }

    fn error_handler_mut(&mut self) -> &mut dyn ErrorHandler {
        self.error_handler.as_mut()
    }
}

impl<'input> ParsingContext for DefaultParsingContext<'input> {
    fn advance(&mut self) -> Result<()> {
        self.next_token()?;
        Ok(())
    }

    fn is_at_end(&self) -> bool {
        self.current_token().map(|t| t.is_eof()).unwrap_or(true)
    }

    fn nesting_depth(&self) -> usize {
        self.nesting_depth
    }

    fn increment_nesting(&mut self) -> Result<()> {
        self.nesting_depth += 1;
        if self.nesting_depth > self.config.max_nesting_depth {
            return Err(crate::error::MarkdownError::parse_error(
                self.current_position(),
                format!(
                    "Maximum nesting depth {} exceeded",
                    self.config.max_nesting_depth
                ),
            ));
        }
        Ok(())
    }

    fn decrement_nesting(&mut self) {
        if self.nesting_depth > 0 {
            self.nesting_depth -= 1;
        }
    }

    fn mark_good_position(&mut self) {
        // Implementation would store current position for error recovery
    }

    fn attempt_recovery(&mut self) {
        // Implementation would attempt to recover from parse errors
        while !self.is_at_end() {
            if let Some(token) = self.current_token()
                && matches!(token, Token::LineEnding(_) | Token::Eof)
            {
                let _ = self.advance();
                break;
            }
            let _ = self.advance();
        }
    }
}

impl InlineNode for Inline {
    fn position(&self) -> Option<Position> {
        None // Existing Inline doesn't track position
    }
}

impl BlockNode for Block {
    fn position(&self) -> Option<Position> {
        match self {
            Block::Heading { position, .. }
            | Block::Paragraph { position, .. }
            | Block::CodeBlock { position, .. }
            | Block::BlockQuote { position, .. }
            | Block::List { position, .. }
            | Block::ThematicBreak { position, .. }
            | Block::HtmlBlock { position, .. } => *position,
        }
    }
}

impl DocumentNode for Document {
    type Block = Block;

    fn blocks(&self) -> &[Self::Block] {
        &self.blocks
    }
}

/// Type alias for a parser that works with default context
pub type DefaultParser<'input> = GenericParser<DefaultParsingContext<'input>>;

/// Placeholder for a generic parser (to be implemented)
pub struct GenericParser<C: ParsingContext> {
    context: C,
    _phantom: PhantomData<C>,
}

impl<C: ParsingContext> GenericParser<C> {
    pub fn new(context: C) -> Self {
        Self {
            context,
            _phantom: PhantomData,
        }
    }

    pub fn parse(&mut self) -> Result<C::Document> {
        let blocks = Vec::new();

        while !self.context.is_at_end() {
            // Skip whitespace and newlines
            if let Some(token) = self.context.current_token()
                && (token.is_newline() || token.is_whitespace())
            {
                self.context.advance()?;
                continue;
            }

            // Parse a block (simplified example)
            // In a real implementation, this would use TokenInspector
            // and TokenDataExtractor traits
            if self.context.is_at_end() {
                break;
            }

            // Placeholder: would call parse_block here
            self.context.advance()?;
        }

        Ok(self.context.ast_builder().build_document(blocks))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_context_creation() {
        let input = "# Hello World\n\nSome text.";
        let context = DefaultParsingContext::with_defaults(input);
        assert!(context.is_ok());
    }

    #[test]
    fn test_lexer_adapter() {
        let input = "# Heading";
        let adapter = LexerAdapter::new(input).unwrap();

        // The adapter should have read the first token during initialization
        let current = adapter.current_token();
        assert!(current.is_some(), "Current token should not be None");

        // Verify it's the heading token
        if let Some(Token::AtxHeading(heading)) = current {
            assert_eq!(heading.level, 1);
            assert!(heading.raw_content.contains("Heading"));
        } else {
            panic!("Expected AtxHeading token, got {:?}", current);
        }
    }

    #[test]
    fn test_ast_builder() {
        let builder = DefaultAstBuilder;
        let heading = builder.build_heading(1, vec![], None);

        match heading {
            Block::Heading { level, .. } => assert_eq!(level, 1),
            _ => panic!("Expected heading"),
        }
    }
}
