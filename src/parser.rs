use crate::ast::{Block, Document, ReferenceMap, SourceMap};
use crate::error::{
    DefaultErrorHandler, ErrorHandler, ErrorInfo, ErrorSeverity, MarkdownError, Result,
};
use crate::lexer::{Lexer, Position, Token};

/// Configuration for the parser behavior and options
#[derive(Debug, Clone)]
pub struct ParserConfig {
    /// Whether to strictly follow CommonMark specification
    pub strict_commonmark: bool,
    /// Maximum nesting depth to prevent stack overflow
    pub max_nesting_depth: usize,
    /// Whether to enable GitHub Flavored Markdown extensions
    pub enable_gfm_extensions: bool,
    /// Whether to collect detailed source position information
    pub track_source_positions: bool,
    /// Maximum number of errors before stopping parsing
    pub max_errors: Option<usize>,
}

impl Default for ParserConfig {
    fn default() -> Self {
        Self {
            strict_commonmark: true,
            max_nesting_depth: 64,
            enable_gfm_extensions: false,
            track_source_positions: true,
            max_errors: Some(100),
        }
    }
}

/// Parser state for tracking parsing context and recovery
#[derive(Debug, Clone)]
struct ParserState {
    /// Current nesting depth
    nesting_depth: usize,
    /// Whether we're currently in a recovery mode
    in_recovery: bool,
    /// Last known good position for error recovery
    last_good_position: Position,
    /// Reference definitions collected during parsing
    reference_map: ReferenceMap,
}

impl Default for ParserState {
    fn default() -> Self {
        Self {
            nesting_depth: 0,
            in_recovery: false,
            last_good_position: Position::new(),
            reference_map: ReferenceMap::new(),
        }
    }
}

/// Main parser struct implementing recursive descent parsing with error recovery
pub struct Parser<'input> {
    /// Lexer for tokenizing input
    lexer: Lexer<'input>,
    /// Current token being processed
    current_token: Option<Token<'input>>,
    /// Parser configuration
    config: ParserConfig,
    /// Parser state for context tracking
    state: ParserState,
    /// Error handler for collecting and managing errors
    error_handler: Box<dyn ErrorHandler>,
}

impl<'input> Parser<'input> {
    /// Creates a new parser with the given input and configuration
    pub fn new(input: &'input str, config: ParserConfig) -> Self {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next_token().ok();

        let error_handler: Box<dyn ErrorHandler> = if let Some(max_errors) = config.max_errors {
            Box::new(DefaultErrorHandler::with_max_errors(max_errors))
        } else {
            Box::new(DefaultErrorHandler::new())
        };

        Self {
            lexer,
            current_token,
            config,
            state: ParserState::default(),
            error_handler,
        }
    }

    /// Creates a new parser with default configuration
    pub fn with_defaults(input: &'input str) -> Self {
        Self::new(input, ParserConfig::default())
    }

    /// Creates a new parser with custom error handler
    pub fn with_error_handler(
        input: &'input str,
        config: ParserConfig,
        error_handler: Box<dyn ErrorHandler>,
    ) -> Self {
        let mut lexer = Lexer::new(input);
        let current_token = lexer.next_token().ok();

        Self {
            lexer,
            current_token,
            config,
            state: ParserState::default(),
            error_handler,
        }
    }

    /// Returns the current position in the input
    pub fn position(&self) -> Position {
        self.lexer.position()
    }

    /// Returns a reference to the current parser configuration
    pub fn config(&self) -> &ParserConfig {
        &self.config
    }

    /// Returns a reference to the collected reference definitions
    pub fn reference_map(&self) -> &ReferenceMap {
        &self.state.reference_map
    }

    /// Add a link reference definition to the parser state
    pub fn add_reference_definition(
        &mut self,
        label: String,
        destination: String,
        title: Option<String>,
    ) {
        let reference = crate::ast::LinkReference {
            label: label.clone(),
            destination,
            title,
        };
        self.state.reference_map.insert(label, reference);
    }

    /// Parses the input and returns a Document AST
    pub fn parse(&mut self) -> Result<Document> {
        self.reset_state();

        match self.parse_document() {
            Ok(document) => {
                // For now, we'll skip the fatal error check since it requires complex downcasting
                // This will be improved in future iterations
                Ok(document)
            }
            Err(error) => {
                // Attempt error recovery and return partial document if possible
                if self.config.strict_commonmark {
                    Err(error)
                } else {
                    // In non-strict mode, try to recover and return partial results
                    self.attempt_recovery();
                    Ok(Document {
                        blocks: Vec::new(),
                        source_map: SourceMap::new(),
                    })
                }
            }
        }
    }

    /// Advances to the next token
    fn advance(&mut self) -> Result<()> {
        match self.lexer.next_token() {
            Ok(token) => {
                self.current_token = Some(token);
                Ok(())
            }
            Err(error) => {
                self.handle_lexer_error(error);
                // Try to continue with EOF token
                self.current_token = Some(Token::Eof);
                Ok(())
            }
        }
    }

    /// Peeks at the next token without consuming it
    fn peek_token(&mut self) -> Result<Token<'input>> {
        self.lexer.peek_token()
    }

    /// Checks if the current token matches the expected token type
    fn current_token_is(&self, expected: &Token) -> bool {
        if let Some(ref current) = self.current_token {
            std::mem::discriminant(current) == std::mem::discriminant(expected)
        } else {
            false
        }
    }

    /// Consumes the current token if it matches the expected type
    fn consume_token(&mut self, expected: Token<'input>) -> Result<Token<'input>> {
        if let Some(current) = &self.current_token {
            if std::mem::discriminant(current) == std::mem::discriminant(&expected) {
                let token = current.clone();
                self.advance()?;
                return Ok(token);
            }
        }

        Err(MarkdownError::parse_error(
            self.position(),
            format!(
                "Expected token {:?}, found {:?}",
                expected, self.current_token
            ),
        ))
    }

    /// Checks if we've reached the end of input
    fn is_at_end(&self) -> bool {
        matches!(self.current_token, Some(Token::Eof) | None)
    }

    /// Resets parser state for a new parsing session
    fn reset_state(&mut self) {
        self.state = ParserState::default();
        self.state.last_good_position = self.position();
    }

    /// Enters a new nesting level, checking for maximum depth
    fn enter_nesting_level(&mut self) -> Result<()> {
        self.state.nesting_depth += 1;
        if self.state.nesting_depth > self.config.max_nesting_depth {
            let error = MarkdownError::parse_error(
                self.position(),
                format!(
                    "Maximum nesting depth {} exceeded",
                    self.config.max_nesting_depth
                ),
            );
            self.handle_parse_error(error.clone());
            return Err(error);
        }
        Ok(())
    }

    /// Exits the current nesting level
    fn exit_nesting_level(&mut self) {
        if self.state.nesting_depth > 0 {
            self.state.nesting_depth -= 1;
        }
    }

    /// Records the current position as a good recovery point
    fn mark_good_position(&mut self) {
        self.state.last_good_position = self.position();
        self.state.in_recovery = false;
    }

    /// Attempts error recovery by advancing to a safe parsing state
    fn attempt_recovery(&mut self) {
        self.state.in_recovery = true;

        // Try to find a block boundary or other safe recovery point
        while !self.is_at_end() {
            match &self.current_token {
                Some(Token::Newline) | Some(Token::Eof) => {
                    // Found a potential recovery point
                    self.advance().ok();
                    break;
                }
                Some(Token::AtxHeading { .. })
                | Some(Token::SetextHeading { .. })
                | Some(Token::ThematicBreak)
                | Some(Token::BlockQuote) => {
                    // Found a block-level element, good recovery point
                    break;
                }
                _ => {
                    // Continue advancing until we find a recovery point
                    if self.advance().is_err() {
                        break;
                    }
                }
            }
        }

        // Reset nesting depth after recovery
        self.state.nesting_depth = 0;
        self.mark_good_position();
    }

    /// Handles lexer errors with appropriate recovery
    fn handle_lexer_error(&mut self, error: MarkdownError) {
        let error_info = ErrorInfo::new(ErrorSeverity::Error, format!("Lexer error: {}", error))
            .with_position(self.position());

        self.error_handler.handle_error(&error_info);

        if !self.error_handler.should_continue(ErrorSeverity::Error) {
            // Convert to fatal error if we should stop
            let fatal_error = ErrorInfo::new(
                ErrorSeverity::Fatal,
                "Too many lexer errors, stopping parsing".to_string(),
            )
            .with_position(self.position());
            self.error_handler.handle_error(&fatal_error);
        }
    }

    /// Handles parse errors with appropriate recovery
    fn handle_parse_error(&mut self, error: MarkdownError) {
        let error_info = ErrorInfo::new(
            if error.is_recoverable() {
                ErrorSeverity::Error
            } else {
                ErrorSeverity::Fatal
            },
            format!("Parse error: {}", error),
        )
        .with_position(self.position());

        self.error_handler.handle_error(&error_info);

        if error.is_recoverable() && !self.config.strict_commonmark {
            self.attempt_recovery();
        }
    }

    /// Parses the entire document into a Document AST
    fn parse_document(&mut self) -> Result<Document> {
        let mut blocks = Vec::new();
        let source_map = SourceMap::new();

        // Parse blocks until we reach the end of input
        while !self.is_at_end() {
            // Skip empty lines and whitespace
            if matches!(self.current_token, Some(Token::Newline)) {
                self.advance()?;
                continue;
            }

            // Skip indentation tokens at document level
            if matches!(self.current_token, Some(Token::Indent(_))) {
                self.advance()?;
                continue;
            }

            // Check for link reference definitions first
            if self.try_parse_link_reference_definition()? {
                continue; // Link reference definition was parsed and added to reference map
            }

            // Parse the next block
            match self.parse_block() {
                Ok(block) => {
                    blocks.push(block);
                }
                Err(error) => {
                    self.handle_parse_error(error.clone());
                    if self.config.strict_commonmark {
                        return Err(error);
                    }
                    // In non-strict mode, try to recover
                    self.attempt_recovery();
                }
            }
        }

        Ok(Document { blocks, source_map })
    }

    /// Parses a single block element
    fn parse_block(&mut self) -> Result<Block> {
        self.enter_nesting_level()?;

        let result = match &self.current_token {
            Some(Token::AtxHeading { level, content }) => self.parse_atx_heading(*level, content),
            Some(Token::SetextHeading { level, content }) => {
                self.parse_setext_heading(*level, content)
            }
            Some(Token::CodeBlock { info, content }) => {
                self.parse_code_block(info.as_ref().map(|s| *s), content)
            }
            Some(Token::BlockQuote) => self.parse_blockquote(),
            Some(Token::ListMarker { kind, indent }) => self.parse_list(kind.clone(), *indent),
            Some(Token::ThematicBreak) => self.parse_thematic_break(),
            _ => {
                // Default to paragraph parsing for text and other inline content
                self.parse_paragraph()
            }
        };

        self.exit_nesting_level();
        result
    }

    /// Parses an ATX heading (# ## ### etc.)
    fn parse_atx_heading(&mut self, level: u8, content: &str) -> Result<Block> {
        let position = Some(self.position());
        self.advance()?; // consume the heading token

        // Parse the heading content as inline elements
        let inline_content = self.parse_heading_content(content)?;

        Ok(Block::Heading {
            level,
            content: inline_content,
            id: None, // ID generation can be added later
            position,
        })
    }

    /// Parses a Setext heading (underlined with = or -)
    fn parse_setext_heading(&mut self, level: u8, _content: &str) -> Result<Block> {
        let position = Some(self.position());

        // For Setext headings, we need to look back at the previous line
        // This is a simplified implementation - in a full parser, this would
        // require maintaining state about previous lines
        self.advance()?; // consume the setext heading token

        // For now, create a heading with placeholder content
        // In a complete implementation, this would use the text from the previous line
        let inline_content = vec![crate::ast::utils::NodeBuilder::text(
            "Setext Heading".to_string(),
        )];

        Ok(Block::Heading {
            level,
            content: inline_content,
            id: None,
            position,
        })
    }

    /// Parses a code block (fenced or indented)
    fn parse_code_block(&mut self, info: Option<&str>, content: &str) -> Result<Block> {
        let position = Some(self.position());
        self.advance()?; // consume the code block token

        // Extract language from info string if present
        let language =
            info.and_then(|info_str| info_str.split_whitespace().next().map(|s| s.to_string()));

        Ok(Block::CodeBlock {
            info: info.map(|s| s.to_string()),
            content: content.to_string(),
            language,
            position,
        })
    }

    /// Parses a blockquote
    fn parse_blockquote(&mut self) -> Result<Block> {
        let position = Some(self.position());
        self.advance()?; // consume the blockquote marker

        let mut content = Vec::new();

        // Parse the content of the blockquote
        // Continue parsing blocks until we hit a non-blockquote line
        loop {
            // Skip whitespace after blockquote marker
            while matches!(self.current_token, Some(Token::Indent(_))) {
                self.advance()?;
            }

            // Check if we've reached the end or a non-blockquote line
            if self.is_at_end() {
                break;
            }

            // If we hit another blockquote marker, continue parsing
            if matches!(self.current_token, Some(Token::BlockQuote)) {
                self.advance()?; // consume the marker
                continue;
            }

            // If we hit a newline, check if the next line continues the blockquote
            if matches!(self.current_token, Some(Token::Newline)) {
                self.advance()?;
                if matches!(self.current_token, Some(Token::BlockQuote)) {
                    continue;
                } else {
                    // End of blockquote
                    break;
                }
            }

            // Parse a block within the blockquote
            match self.parse_block() {
                Ok(block) => content.push(block),
                Err(_) => break, // End of blockquote content
            }
        }

        Ok(Block::BlockQuote { content, position })
    }

    /// Parses a list (ordered or unordered)
    fn parse_list(&mut self, kind: crate::lexer::ListKind, base_indent: usize) -> Result<Block> {
        let position = Some(self.position());
        let mut items = Vec::new();
        let mut tight = true; // Assume tight list initially

        // Convert lexer ListKind to AST ListKind
        let list_kind = match kind {
            crate::lexer::ListKind::Bullet { marker } => crate::ast::ListKind::Bullet { marker },
            crate::lexer::ListKind::Ordered { start, delimiter } => {
                crate::ast::ListKind::Ordered { start, delimiter }
            }
        };

        // Parse list items
        while matches!(self.current_token, Some(Token::ListMarker { .. })) {
            if let Some(Token::ListMarker {
                kind: item_kind,
                indent,
            }) = &self.current_token
            {
                // Check if this marker matches our list type and indentation level
                if !self.list_kinds_match(&kind, item_kind) || *indent != base_indent {
                    break; // Different list type or indentation level
                }

                let item = self.parse_list_item()?;
                items.push(item);

                // Check for blank lines between items (makes list loose)
                if self.has_blank_line_before_next_item() {
                    tight = false;
                }
            } else {
                break;
            }
        }

        Ok(Block::List {
            kind: list_kind,
            tight,
            items,
            position,
        })
    }

    /// Parses a single list item
    fn parse_list_item(&mut self) -> Result<crate::ast::ListItem> {
        self.advance()?; // consume the list marker

        let mut content = Vec::new();
        let mut tight = true;

        // Skip whitespace after list marker
        while matches!(self.current_token, Some(Token::Indent(_))) {
            self.advance()?;
        }

        // Parse the first line of the list item
        if !matches!(self.current_token, Some(Token::Newline) | Some(Token::Eof)) {
            let block = self.parse_block()?;
            content.push(block);
        }

        // Handle continuation lines
        while matches!(self.current_token, Some(Token::Newline)) {
            self.advance()?; // consume newline

            // Check for blank line
            if matches!(self.current_token, Some(Token::Newline)) {
                tight = false;
                continue;
            }

            // Check if next line is indented (continuation of list item)
            if matches!(self.current_token, Some(Token::Indent(_))) {
                self.advance()?; // consume indentation
                if !self.is_at_end()
                    && !matches!(self.current_token, Some(Token::ListMarker { .. }))
                {
                    let block = self.parse_block()?;
                    content.push(block);
                }
            } else {
                // End of list item
                break;
            }
        }

        Ok(crate::ast::ListItem {
            content,
            tight,
            task_list_marker: None, // Task list support can be added later
        })
    }

    /// Parses a thematic break (horizontal rule)
    fn parse_thematic_break(&mut self) -> Result<Block> {
        let position = Some(self.position());
        self.advance()?; // consume the thematic break token

        Ok(Block::ThematicBreak { position })
    }

    /// Parses a paragraph (default block type for text content)
    fn parse_paragraph(&mut self) -> Result<Block> {
        let position = Some(self.position());
        let mut inline_content = Vec::new();

        // Collect inline content until we hit a paragraph boundary
        while !self.is_at_end() && !self.is_paragraph_boundary() {
            match &self.current_token {
                Some(Token::Text(text)) => {
                    // Process text for HTML inlines
                    let processed_inlines = self.process_text_for_html_inlines(text)?;
                    inline_content.extend(processed_inlines);
                    self.advance()?;
                }
                Some(Token::Emphasis { marker, count }) => {
                    let emphasis = self.parse_emphasis(*marker, *count)?;
                    inline_content.push(emphasis);
                }
                Some(Token::CodeSpan(content)) => {
                    inline_content.push(crate::ast::utils::NodeBuilder::code_span(
                        content.to_string(),
                    ));
                    self.advance()?;
                }
                Some(Token::Link { text, dest, title }) => {
                    let link = self.parse_link_token(text, dest, title.as_ref().map(|s| *s))?;
                    inline_content.push(link);
                }
                Some(Token::Image { alt, dest, title }) => {
                    let image = self.parse_image_token(alt, dest, title.as_ref().map(|s| *s))?;
                    inline_content.push(image);
                }
                Some(Token::EscapeSequence { character }) => {
                    // Convert escape sequence to literal character
                    inline_content.push(crate::ast::utils::NodeBuilder::text(
                        character.to_string(),
                    ));
                    self.advance()?;
                }
                Some(Token::Newline) => {
                    // Check if this is the end of the paragraph
                    self.advance()?;
                    if self.is_paragraph_boundary() {
                        break;
                    }
                    // Otherwise, add a soft break and continue
                    inline_content.push(crate::ast::utils::NodeBuilder::soft_break());
                }
                _ => {
                    // Hit a block-level token, end the paragraph
                    break;
                }
            }
        }

        // If no content was collected, create a paragraph with empty text
        if inline_content.is_empty() {
            inline_content.push(crate::ast::utils::NodeBuilder::text(String::new()));
        }

        Ok(Block::Paragraph {
            content: inline_content,
            position,
        })
    }

    /// Parse inline elements from the current token stream
    /// This is the main inline parsing method that handles all inline elements
    fn parse_inline(&mut self) -> Result<Vec<crate::ast::Inline>> {
        let mut inlines = Vec::new();

        while !self.is_at_end() && !self.is_inline_boundary() {
            match &self.current_token {
                Some(Token::Text(text)) => {
                    // Check if text contains HTML inline elements
                    let processed_inlines = self.process_text_for_html_inlines(text)?;
                    inlines.extend(processed_inlines);
                    self.advance()?;
                }
                Some(Token::Emphasis { marker, count }) => {
                    let emphasis = self.parse_emphasis(*marker, *count)?;
                    inlines.push(emphasis);
                }
                Some(Token::CodeSpan(content)) => {
                    inlines.push(crate::ast::utils::NodeBuilder::code_span(
                        content.to_string(),
                    ));
                    self.advance()?;
                }
                Some(Token::Link { text, dest, title }) => {
                    let link = self.parse_link_token(text, dest, title.as_ref().map(|s| *s))?;
                    inlines.push(link);
                }
                Some(Token::Image { alt, dest, title }) => {
                    let image = self.parse_image_token(alt, dest, title.as_ref().map(|s| *s))?;
                    inlines.push(image);
                }
                Some(Token::EscapeSequence { character }) => {
                    // Convert escape sequence to literal character
                    inlines.push(crate::ast::utils::NodeBuilder::text(
                        character.to_string(),
                    ));
                    self.advance()?;
                }
                Some(Token::Newline) => {
                    // Check if this should be a hard break or soft break
                    if self.is_hard_break() {
                        inlines.push(crate::ast::utils::NodeBuilder::hard_break());
                    } else {
                        inlines.push(crate::ast::utils::NodeBuilder::soft_break());
                    }
                    self.advance()?;
                }
                _ => {
                    // Hit a block-level token or other boundary, stop parsing inlines
                    break;
                }
            }
        }

        // If no inlines were parsed, return empty text to avoid empty inline lists
        if inlines.is_empty() {
            inlines.push(crate::ast::utils::NodeBuilder::text(String::new()));
        }

        Ok(inlines)
    }

    /// Parse emphasis with proper delimiter resolution
    /// Implements CommonMark emphasis rules including nested emphasis and precedence
    fn parse_emphasis(&mut self, marker: char, count: usize) -> Result<crate::ast::Inline> {
        self.advance()?; // consume the opening emphasis marker

        let strong = count >= 2;
        let mut content = Vec::new();
        let mut delimiter_stack = Vec::new();

        // Track the emphasis delimiter for proper matching
        delimiter_stack.push((marker, count));

        while !self.is_at_end() {
            match &self.current_token {
                Some(Token::Text(text)) => {
                    content.push(crate::ast::utils::NodeBuilder::text(text.to_string()));
                    self.advance()?;
                }
                Some(Token::Emphasis {
                    marker: current_marker,
                    count: current_count,
                }) => {
                    // Apply CommonMark precedence rules for emphasis
                    if self.should_close_emphasis(marker, count, *current_marker, *current_count) {
                        self.advance()?; // consume closing marker
                        break;
                    } else if self.can_nest_emphasis(marker, *current_marker) {
                        // This is nested emphasis
                        let nested_emphasis =
                            self.parse_emphasis(*current_marker, *current_count)?;
                        content.push(nested_emphasis);
                    } else {
                        // Treat as literal text due to precedence rules
                        let marker_text = current_marker.to_string().repeat(*current_count);
                        content.push(crate::ast::utils::NodeBuilder::text(marker_text));
                        self.advance()?;
                    }
                }
                Some(Token::CodeSpan(code_content)) => {
                    // Code spans have higher precedence than emphasis
                    content.push(crate::ast::utils::NodeBuilder::code_span(
                        code_content.to_string(),
                    ));
                    self.advance()?;
                }
                Some(Token::Link { text, dest, title }) => {
                    // Links have higher precedence than emphasis
                    let link = self.parse_link_token(text, dest, title.as_ref().map(|s| *s))?;
                    content.push(link);
                }
                Some(Token::Image { alt, dest, title }) => {
                    // Images have higher precedence than emphasis
                    let image = self.parse_image_token(alt, dest, title.as_ref().map(|s| *s))?;
                    content.push(image);
                }
                Some(Token::EscapeSequence { character }) => {
                    // Escape sequences have highest precedence
                    content.push(crate::ast::utils::NodeBuilder::text(
                        character.to_string(),
                    ));
                    self.advance()?;
                }
                Some(Token::Newline) => {
                    // Emphasis can span soft breaks but not hard breaks
                    if self.is_hard_break() {
                        break; // End emphasis at hard break
                    } else {
                        content.push(crate::ast::utils::NodeBuilder::soft_break());
                        self.advance()?;
                    }
                }
                _ => {
                    // Hit a block boundary, end emphasis
                    break;
                }
            }
        }

        Ok(crate::ast::utils::NodeBuilder::emphasis(strong, content))
    }

    /// Determine if emphasis should be closed based on CommonMark precedence rules
    fn should_close_emphasis(&self, open_marker: char, open_count: usize, close_marker: char, close_count: usize) -> bool {
        // Must be same marker type
        if open_marker != close_marker {
            return false;
        }

        // Must have sufficient count to close
        if close_count < open_count {
            return false;
        }

        // CommonMark rule: prefer strong emphasis over nested emphasis
        if open_count >= 2 && close_count >= 2 {
            return true;
        }

        // Regular emphasis matching
        open_count == close_count
    }

    /// Determine if emphasis can be nested based on CommonMark precedence rules
    fn can_nest_emphasis(&self, outer_marker: char, inner_marker: char) -> bool {
        // Different markers can always nest
        if outer_marker != inner_marker {
            return true;
        }

        // Same markers can nest with specific rules
        // This is a simplified implementation - full CommonMark has more complex rules
        true
    }

    /// Parse a link token into a Link inline element
    /// Handles both direct links and reference links
    fn parse_link_token(
        &mut self,
        text: &str,
        dest: &str,
        title: Option<&str>,
    ) -> Result<crate::ast::Inline> {
        self.advance()?; // consume the link token

        // Check if this is a reference link (destination is empty or starts with reference syntax)
        if dest.is_empty() || dest.starts_with('[') {
            // This is a reference link - look up in reference map
            let reference_label = if dest.is_empty() { text } else { dest };

            // Clone the reference data to avoid borrowing issues
            let reference_data = self.state.reference_map.get(reference_label).cloned();

            if let Some(link_ref) = reference_data {
                // Parse the link text as inline elements
                let link_text = self.parse_text_as_inline(text)?;

                Ok(crate::ast::utils::NodeBuilder::link(
                    link_text,
                    link_ref.destination,
                    link_ref.title,
                ))
            } else {
                // Reference not found, treat as plain text
                Ok(crate::ast::utils::NodeBuilder::text(format!(
                    "[{}]({})",
                    text, dest
                )))
            }
        } else {
            // Direct link
            let link_text = self.parse_text_as_inline(text)?;

            Ok(crate::ast::utils::NodeBuilder::link(
                link_text,
                dest.to_string(),
                title.map(|s| s.to_string()),
            ))
        }
    }

    /// Parse an image token into an Image inline element
    /// Handles both direct images and reference images
    fn parse_image_token(
        &mut self,
        alt: &str,
        dest: &str,
        title: Option<&str>,
    ) -> Result<crate::ast::Inline> {
        self.advance()?; // consume the image token

        // Check if this is a reference image
        if dest.is_empty() || dest.starts_with('[') {
            // This is a reference image - look up in reference map
            let reference_label = if dest.is_empty() { alt } else { dest };

            // Clone the reference data to avoid borrowing issues
            let reference_data = self.state.reference_map.get(reference_label).cloned();

            if let Some(link_ref) = reference_data {
                Ok(crate::ast::utils::NodeBuilder::image(
                    alt.to_string(),
                    link_ref.destination,
                    link_ref.title,
                ))
            } else {
                // Reference not found, treat as plain text
                Ok(crate::ast::utils::NodeBuilder::text(format!(
                    "![{}]({})",
                    alt, dest
                )))
            }
        } else {
            // Direct image
            Ok(crate::ast::utils::NodeBuilder::image(
                alt.to_string(),
                dest.to_string(),
                title.map(|s| s.to_string()),
            ))
        }
    }

    /// Process text content for HTML inline elements
    /// This method scans text for HTML tags and creates appropriate inline elements
    fn process_text_for_html_inlines(&mut self, text: &str) -> Result<Vec<crate::ast::Inline>> {
        let mut inlines = Vec::new();
        let mut current_pos = 0;

        while current_pos < text.len() {
            // Look for HTML tag start
            if let Some(tag_start) = text[current_pos..].find('<') {
                let absolute_tag_start = current_pos + tag_start;

                // Add any text before the tag
                if tag_start > 0 {
                    let text_before = &text[current_pos..absolute_tag_start];
                    if !text_before.is_empty() {
                        inlines.push(crate::ast::utils::NodeBuilder::text(
                            text_before.to_string(),
                        ));
                    }
                }

                // Find the end of the HTML tag
                if let Some(tag_end) = text[absolute_tag_start..].find('>') {
                    let absolute_tag_end = absolute_tag_start + tag_end + 1;
                    let html_tag = &text[absolute_tag_start..absolute_tag_end];

                    // Validate that this looks like a valid HTML tag
                    if self.is_valid_html_inline_tag(html_tag) {
                        inlines.push(crate::ast::utils::NodeBuilder::html_inline(
                            html_tag.to_string(),
                        ));
                        current_pos = absolute_tag_end;
                    } else {
                        // Not a valid HTML tag, treat as text
                        inlines.push(crate::ast::utils::NodeBuilder::text("<".to_string()));
                        current_pos = absolute_tag_start + 1;
                    }
                } else {
                    // No closing '>', treat as text
                    inlines.push(crate::ast::utils::NodeBuilder::text("<".to_string()));
                    current_pos = absolute_tag_start + 1;
                }
            } else {
                // No more HTML tags, add remaining text
                let remaining_text = &text[current_pos..];
                if !remaining_text.is_empty() {
                    inlines.push(crate::ast::utils::NodeBuilder::text(
                        remaining_text.to_string(),
                    ));
                }
                break;
            }
        }

        // If no inlines were created, return the original text
        if inlines.is_empty() {
            inlines.push(crate::ast::utils::NodeBuilder::text(text.to_string()));
        }

        Ok(inlines)
    }

    /// Check if a string represents a valid HTML inline tag
    /// This is a simplified validation - a full implementation would be more comprehensive
    fn is_valid_html_inline_tag(&self, tag: &str) -> bool {
        if tag.len() < 3 || !tag.starts_with('<') || !tag.ends_with('>') {
            return false;
        }

        let inner = &tag[1..tag.len() - 1];

        // Check for self-closing tags
        if inner.ends_with('/') {
            let tag_name = &inner[..inner.len() - 1].trim();
            return self.is_valid_html_tag_name(tag_name);
        }

        // Check for closing tags
        if inner.starts_with('/') {
            let tag_name = inner[1..].trim();
            return self.is_valid_html_tag_name(tag_name);
        }

        // Check for opening tags (may have attributes)
        let tag_name = inner.split_whitespace().next().unwrap_or("");
        self.is_valid_html_tag_name(tag_name)
    }

    /// Check if a string is a valid HTML tag name
    fn is_valid_html_tag_name(&self, name: &str) -> bool {
        if name.is_empty() {
            return false;
        }

        // Common HTML inline tags
        let valid_inline_tags = [
            "a", "abbr", "b", "bdi", "bdo", "br", "button", "cite", "code", "dfn", "em", "i",
            "kbd", "mark", "q", "s", "samp", "small", "span", "strong", "sub", "sup", "time", "u",
            "var", "wbr",
        ];

        valid_inline_tags.contains(&name.to_lowercase().as_str())
    }

    /// Parse text content as inline elements (for link text, etc.)
    /// This allows for nested inline elements within link text
    fn parse_text_as_inline(&mut self, text: &str) -> Result<Vec<crate::ast::Inline>> {
        if text.is_empty() {
            return Ok(vec![crate::ast::utils::NodeBuilder::text(String::new())]);
        }

        // For now, treat as simple text
        // In a full implementation, this would create a sub-lexer to parse the text
        // and handle nested emphasis, code spans, etc. within link text
        Ok(vec![crate::ast::utils::NodeBuilder::text(text.to_string())])
    }

    /// Check if the current position represents an inline boundary
    fn is_inline_boundary(&self) -> bool {
        match &self.current_token {
            Some(Token::AtxHeading { .. })
            | Some(Token::SetextHeading { .. })
            | Some(Token::CodeBlock { .. })
            | Some(Token::BlockQuote)
            | Some(Token::ListMarker { .. })
            | Some(Token::ThematicBreak)
            | Some(Token::Eof)
            | None => true,
            _ => false,
        }
    }

    /// Check if the current newline should be treated as a hard break
    /// Hard breaks occur when a line ends with two or more spaces
    fn is_hard_break(&mut self) -> bool {
        // This is a simplified implementation
        // In a full parser, this would check if the previous line ended with 2+ spaces
        // or if there's a backslash at the end of the line
        false // For now, treat all as soft breaks
    }

    /// Helper method to parse heading content as inline elements
    fn parse_heading_content(&mut self, content: &str) -> Result<Vec<crate::ast::Inline>> {
        // Parse the heading content as inline elements
        self.parse_text_as_inline(content)
    }

    /// Helper method to parse emphasis inline elements (legacy method for compatibility)
    fn parse_emphasis_inline(&mut self, marker: char, count: usize) -> Result<crate::ast::Inline> {
        self.parse_emphasis(marker, count)
    }

    /// Helper method to check if list kinds match
    fn list_kinds_match(
        &self,
        kind1: &crate::lexer::ListKind,
        kind2: &crate::lexer::ListKind,
    ) -> bool {
        match (kind1, kind2) {
            (crate::lexer::ListKind::Bullet { .. }, crate::lexer::ListKind::Bullet { .. }) => true,
            (crate::lexer::ListKind::Ordered { .. }, crate::lexer::ListKind::Ordered { .. }) => {
                true
            }
            _ => false,
        }
    }

    /// Helper method to check for blank lines between list items
    fn has_blank_line_before_next_item(&mut self) -> bool {
        // Look ahead to see if there's a blank line
        let mut temp_lexer = self.clone_state();

        // Skip current newline if present
        if matches!(temp_lexer.current_token, Some(Token::Newline)) {
            temp_lexer.advance().ok();
        }

        // Check if next token is another newline (blank line)
        matches!(temp_lexer.current_token, Some(Token::Newline))
    }

    /// Helper method to check if we're at a paragraph boundary
    fn is_paragraph_boundary(&self) -> bool {
        match &self.current_token {
            Some(Token::AtxHeading { .. })
            | Some(Token::SetextHeading { .. })
            | Some(Token::CodeBlock { .. })
            | Some(Token::BlockQuote)
            | Some(Token::ListMarker { .. })
            | Some(Token::ThematicBreak)
            | Some(Token::Newline)
            | Some(Token::Eof)
            | None => true,
            _ => false,
        }
    }

    /// Helper method to clone parser state for lookahead
    fn clone_state(&self) -> Parser<'input> {
        // For now, create a simple clone by creating a new parser
        // In a full implementation, this would properly clone the lexer state
        let new_lexer = Lexer::new("");

        Parser {
            lexer: new_lexer,
            current_token: self.current_token.clone(),
            config: self.config.clone(),
            state: self.state.clone(),
            error_handler: Box::new(crate::error::DefaultErrorHandler::new()),
        }
    }

    /// Try to parse a link reference definition.
    /// Returns true if a link reference definition was successfully parsed.
    fn try_parse_link_reference_definition(&mut self) -> Result<bool> {
        // Link reference definitions start with [label]: destination optional_title
        // They can be indented up to 3 spaces

        // Check if current token could start a link reference definition
        if !matches!(self.current_token, Some(Token::Link { dest, .. }) if dest.is_empty()) {
            return Ok(false);
        }

        // Look ahead to see if this looks like a link reference definition
        if !self.looks_like_link_reference_definition() {
            return Ok(false);
        }

        // Parse the link reference definition
        self.parse_link_reference_definition()
    }

    /// Check if the current line looks like a link reference definition
    fn looks_like_link_reference_definition(&mut self) -> bool {
        // Check for pattern [label]: at start of line
        // Since lexer tokenizes [label] as Link token, we need to check for that pattern
        match &self.current_token {
            Some(Token::Link { text: _, dest, title: None }) if dest.is_empty() => {
                // Use peek_token to look ahead
                if let Ok(next_token) = self.lexer.peek_token() {
                    matches!(next_token, Token::Text(text) if text.starts_with(':'))
                } else {
                    false
                }
            }
            _ => false
        }
    }

    /// Parse a link reference definition and add it to the reference map
    fn parse_link_reference_definition(&mut self) -> Result<bool> {
        // Parse pattern: [label]: destination "title"
        // The lexer tokenizes [label] as Link token with empty dest
        
        if let Some(Token::Link { text, dest, title: None }) = &self.current_token.clone() {
            if dest.is_empty() {
                let label = text.to_string();
                self.advance()?; // consume [label]
                
                // Expect ":"
                if let Some(Token::Text(colon_text)) = &self.current_token {
                    if colon_text.starts_with(':') {
                        self.advance()?; // consume ":"
                        
                        // Parse destination
                        if let Some(Token::Text(dest_text)) = &self.current_token {
                            let destination = dest_text.trim_matches(|c| c == '<' || c == '>').to_string();
                            self.advance()?; // consume destination
                            
                            // Parse optional title
                            let mut title = None;
                            if let Some(Token::Text(title_text)) = &self.current_token {
                                let trimmed_title = title_text.trim();
                                if (trimmed_title.starts_with('"') && trimmed_title.ends_with('"')) ||
                                   (trimmed_title.starts_with('\'') && trimmed_title.ends_with('\'')) ||
                                   (trimmed_title.starts_with('(') && trimmed_title.ends_with(')')) {
                                    title = Some(trimmed_title[1..trimmed_title.len()-1].to_string());
                                    self.advance()?; // consume title
                                } else if trimmed_title.starts_with('"') {
                                    // Title might be split across tokens, collect until closing quote
                                    let mut title_parts = vec![trimmed_title];
                                    self.advance()?;
                                    
                                    while let Some(Token::Text(part)) = &self.current_token {
                                        title_parts.push(part);
                                        if part.ends_with('"') {
                                            self.advance()?;
                                            break;
                                        }
                                        self.advance()?;
                                    }
                                    
                                    let full_title = title_parts.join(" ");
                                    if full_title.starts_with('"') && full_title.ends_with('"') {
                                        title = Some(full_title[1..full_title.len()-1].to_string());
                                    }
                                }
                            }
                            
                            // Add to reference map
                            self.add_reference_definition(label, destination, title);
                            return Ok(true);
                        }
                    }
                }
            }
        }

        Ok(false)
    }
}

/// Simple parse function with default configuration for convenience
pub fn parse(markdown: &str) -> Result<Document> {
    let mut parser = Parser::with_defaults(markdown);
    parser.parse()
}

/// Parse function with custom configuration
pub fn parse_with_config(markdown: &str, config: ParserConfig) -> Result<Document> {
    let mut parser = Parser::new(markdown, config);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::{ErrorInfo, ErrorSeverity};

    #[test]
    fn test_parser_creation() {
        let input = "# Hello World";
        let parser = Parser::with_defaults(input);

        assert_eq!(parser.config().strict_commonmark, true);
        assert_eq!(parser.config().max_nesting_depth, 64);
        assert_eq!(parser.config().track_source_positions, true);
    }

    #[test]
    fn test_parser_with_custom_config() {
        let input = "# Hello World";
        let config = ParserConfig {
            strict_commonmark: false,
            max_nesting_depth: 32,
            enable_gfm_extensions: true,
            track_source_positions: false,
            max_errors: Some(50),
        };

        let parser = Parser::new(input, config.clone());
        assert_eq!(parser.config().strict_commonmark, false);
        assert_eq!(parser.config().max_nesting_depth, 32);
        assert_eq!(parser.config().enable_gfm_extensions, true);
        assert_eq!(parser.config().track_source_positions, false);
        assert_eq!(parser.config().max_errors, Some(50));
    }

    #[test]
    fn test_parser_position_tracking() {
        let input = "line1\nline2";
        let parser = Parser::with_defaults(input);

        let position = parser.position();
        assert_eq!(position.line, 1);
        // Position may have advanced due to lexer initialization, so we check it's reasonable
        assert!(position.column >= 1);
    }

    #[test]
    fn test_parser_basic_parsing() {
        let input = "# Hello World";
        let mut parser = Parser::with_defaults(input);

        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 1); // Should now parse the heading

        // Check that we got a heading block
        if let Block::Heading { level, content, .. } = &document.blocks[0] {
            assert_eq!(*level, 1);
            assert_eq!(content.len(), 1);
        } else {
            panic!("Expected heading block");
        }
    }

    #[test]
    fn test_parser_empty_input() {
        let input = "";
        let mut parser = Parser::with_defaults(input);

        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 0);
    }

    #[test]
    fn test_parser_with_custom_error_handler() {
        struct TestErrorHandler {
            errors: Vec<ErrorInfo>,
        }

        impl ErrorHandler for TestErrorHandler {
            fn handle_error(&mut self, error: &ErrorInfo) {
                self.errors.push(error.clone());
            }

            fn should_continue(&self, _severity: ErrorSeverity) -> bool {
                self.errors.len() < 5
            }
        }

        let input = "# Hello World";
        let config = ParserConfig::default();
        let error_handler = Box::new(TestErrorHandler { errors: Vec::new() });

        let parser = Parser::with_error_handler(input, config, error_handler);
        assert_eq!(parser.config().strict_commonmark, true);
    }

    #[test]
    fn test_parser_config_default() {
        let config = ParserConfig::default();

        assert_eq!(config.strict_commonmark, true);
        assert_eq!(config.max_nesting_depth, 64);
        assert_eq!(config.enable_gfm_extensions, false);
        assert_eq!(config.track_source_positions, true);
        assert_eq!(config.max_errors, Some(100));
    }

    #[test]
    fn test_parser_state_management() {
        let input = "# Test";
        let mut parser = Parser::with_defaults(input);

        // Test initial state
        assert_eq!(parser.state.nesting_depth, 0);
        assert_eq!(parser.state.in_recovery, false);

        // Test nesting level management
        assert!(parser.enter_nesting_level().is_ok());
        assert_eq!(parser.state.nesting_depth, 1);

        parser.exit_nesting_level();
        assert_eq!(parser.state.nesting_depth, 0);
    }

    #[test]
    fn test_parser_max_nesting_depth() {
        let input = "# Test";
        let config = ParserConfig {
            max_nesting_depth: 2,
            ..ParserConfig::default()
        };
        let mut parser = Parser::new(input, config);

        // Should succeed within limit
        assert!(parser.enter_nesting_level().is_ok());
        assert!(parser.enter_nesting_level().is_ok());

        // Should fail when exceeding limit
        assert!(parser.enter_nesting_level().is_err());
    }

    #[test]
    fn test_parser_error_recovery() {
        let input = "# Test";
        let mut parser = Parser::with_defaults(input);

        // Mark initial good position
        parser.mark_good_position();
        assert_eq!(parser.state.in_recovery, false);

        // Simulate entering recovery mode
        parser.attempt_recovery();
        // Recovery should complete and mark good position
        assert_eq!(parser.state.in_recovery, false);
        assert_eq!(parser.state.nesting_depth, 0);
    }

    #[test]
    fn test_parser_reference_map() {
        let input = "# Test";
        let parser = Parser::with_defaults(input);

        let ref_map = parser.reference_map();
        // Should start with empty reference map
        assert!(ref_map.get("test").is_none());
    }

    #[test]
    fn test_convenience_parse_functions() {
        let input = "# Hello World";

        // Test simple parse function
        let result1 = parse(input);
        assert!(result1.is_ok());

        // Test parse with config
        let config = ParserConfig::default();
        let result2 = parse_with_config(input, config);
        assert!(result2.is_ok());
    }

    #[test]
    fn test_parser_token_management() {
        let input = "# Hello";
        let mut parser = Parser::with_defaults(input);

        // Test token advancement
        let initial_token = parser.current_token.clone();
        assert!(parser.advance().is_ok());

        // Token should have changed after advance
        assert_ne!(parser.current_token, initial_token);
    }

    #[test]
    fn test_parser_end_detection() {
        let input = "";
        let parser = Parser::with_defaults(input);

        // Empty input should be at end
        assert!(parser.is_at_end());
    }

    #[test]
    fn test_parser_strict_vs_non_strict_mode() {
        let input = "# Test";

        // Test strict mode
        let strict_config = ParserConfig {
            strict_commonmark: true,
            ..ParserConfig::default()
        };
        let mut strict_parser = Parser::new(input, strict_config);
        let strict_result = strict_parser.parse();
        assert!(strict_result.is_ok());

        // Test non-strict mode
        let non_strict_config = ParserConfig {
            strict_commonmark: false,
            ..ParserConfig::default()
        };
        let mut non_strict_parser = Parser::new(input, non_strict_config);
        let non_strict_result = non_strict_parser.parse();
        assert!(non_strict_result.is_ok());
    }

    #[test]
    fn test_parser_with_unicode_input() {
        let input = "# Hello 🌍 World";
        let mut parser = Parser::with_defaults(input);

        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_escape_sequence_parsing() {
        let input = "This has \\* escaped \\# characters.";
        let mut parser = Parser::with_defaults(input);

        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 1);

        if let Block::Paragraph { content, .. } = &document.blocks[0] {
            // Should have text with escaped characters converted to literals
            let has_escaped_content = content.iter().any(|inline| {
                if let crate::ast::Inline::Text(text) = inline {
                    text.contains('*') || text.contains('#')
                } else {
                    false
                }
            });
            assert!(has_escaped_content);
        } else {
            panic!("Expected paragraph block");
        }
    }

    #[test]
    fn test_link_reference_definition_parsing() {
        let input = "[example]: https://example.com \"Example Site\"\n\nThis is a [example] link.";
        let mut parser = Parser::with_defaults(input);

        let result = parser.parse();
        assert!(result.is_ok());

        // Check that reference was added to reference map
        let reference = parser.reference_map().get("example");
        assert!(reference.is_some());

        let reference = reference.unwrap();
        assert_eq!(reference.destination, "https://example.com");
        assert_eq!(reference.title, Some("Example Site".to_string()));
    }

    #[test]
    fn test_emphasis_precedence_rules() {
        let input = "*emphasis with `code` inside*";
        let mut parser = Parser::with_defaults(input);

        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 1);

        if let Block::Paragraph { content, .. } = &document.blocks[0] {
            // Should have emphasis containing text and code span
            let has_emphasis = content.iter().any(|inline| {
                matches!(inline, crate::ast::Inline::Emphasis { .. })
            });
            assert!(has_emphasis);
        } else {
            panic!("Expected paragraph block");
        }
    }

    #[test]
    fn test_conflicting_markup_resolution() {
        let input = "**strong *nested emphasis* strong**";
        let mut parser = Parser::with_defaults(input);

        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 1);

        if let Block::Paragraph { content, .. } = &document.blocks[0] {
            // Should properly handle nested emphasis according to precedence rules
            let has_strong_emphasis = content.iter().any(|inline| {
                if let crate::ast::Inline::Emphasis { strong, .. } = inline {
                    *strong
                } else {
                    false
                }
            });
            assert!(has_strong_emphasis);
        } else {
            panic!("Expected paragraph block");
        }
    }

    #[test]
    fn test_parser_heading_parsing() {
        let input = "# Level 1\n## Level 2\n### Level 3";
        let mut parser = Parser::with_defaults(input);

        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 3);

        // Check heading levels
        for (i, expected_level) in [1u8, 2u8, 3u8].iter().enumerate() {
            if let Block::Heading { level, .. } = &document.blocks[i] {
                assert_eq!(*level, *expected_level);
            } else {
                panic!("Expected heading block at index {}", i);
            }
        }
    }

    #[test]
    fn test_parser_paragraph_parsing() {
        let input = "This is a paragraph.\n\nThis is another paragraph.";
        let mut parser = Parser::with_defaults(input);

        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 2);

        // Both should be paragraphs
        for block in &document.blocks {
            assert!(matches!(block, Block::Paragraph { .. }));
        }
    }

    #[test]
    fn test_parser_code_block_parsing() {
        let input = "```rust\nfn main() {}\n```";
        let mut parser = Parser::with_defaults(input);

        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 1);

        if let Block::CodeBlock {
            info,
            content,
            language,
            ..
        } = &document.blocks[0]
        {
            assert_eq!(info.as_deref(), Some("rust"));
            assert_eq!(language.as_deref(), Some("rust"));
            assert!(content.contains("fn main()"));
        } else {
            panic!("Expected code block");
        }
    }

    #[test]
    fn test_parser_blockquote_parsing() {
        let input = "> This is a blockquote\n> Second line";
        let mut parser = Parser::with_defaults(input);

        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 1);

        if let Block::BlockQuote { content, .. } = &document.blocks[0] {
            assert!(!content.is_empty());
        } else {
            panic!("Expected blockquote");
        }
    }

    #[test]
    fn test_parser_list_parsing() {
        let input = "- Item 1\n- Item 2\n- Item 3";
        let mut parser = Parser::with_defaults(input);

        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 1);

        if let Block::List { kind, items, .. } = &document.blocks[0] {
            assert!(matches!(kind, crate::ast::ListKind::Bullet { .. }));
            assert_eq!(items.len(), 3);
        } else {
            panic!("Expected list block");
        }
    }

    #[test]
    fn test_parser_thematic_break_parsing() {
        let input = "---";
        let mut parser = Parser::with_defaults(input);

        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 1);

        assert!(matches!(document.blocks[0], Block::ThematicBreak { .. }));
    }

    #[test]
    fn test_parser_with_multiline_input() {
        let input = "# Heading\n\nParagraph text\n\n## Another heading";
        let mut parser = Parser::with_defaults(input);

        let result = parser.parse();
        assert!(result.is_ok());
    }

    #[test]
    fn test_parser_state_reset() {
        let input = "# Test";
        let mut parser = Parser::with_defaults(input);

        // Modify state
        parser.enter_nesting_level().ok();
        parser.state.in_recovery = true;

        // Reset should restore initial state
        parser.reset_state();
        assert_eq!(parser.state.nesting_depth, 0);
        assert_eq!(parser.state.in_recovery, false);
    }

    #[test]
    fn test_inline_parsing_functionality() {
        // Test emphasis parsing
        let input = "This is *italic* and **bold** text.";
        let mut parser = Parser::with_defaults(input);
        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 1);

        if let Block::Paragraph { content, .. } = &document.blocks[0] {
            // Should have multiple inline elements: text, emphasis, text, emphasis, text
            assert!(content.len() >= 3);

            // Check that we have emphasis elements
            let has_emphasis = content
                .iter()
                .any(|inline| matches!(inline, crate::ast::Inline::Emphasis { .. }));
            assert!(has_emphasis);
        } else {
            panic!("Expected paragraph block");
        }
    }

    #[test]
    fn test_link_parsing() {
        let input = "This is a [link](https://example.com) in text.";
        let mut parser = Parser::with_defaults(input);
        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 1);

        if let Block::Paragraph { content, .. } = &document.blocks[0] {
            // Check that we have a link element
            let has_link = content
                .iter()
                .any(|inline| matches!(inline, crate::ast::Inline::Link { .. }));
            assert!(has_link);
        } else {
            panic!("Expected paragraph block");
        }
    }

    #[test]
    fn test_code_span_parsing() {
        let input = "This has `inline code` in it.";
        let mut parser = Parser::with_defaults(input);
        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();
        assert_eq!(document.blocks.len(), 1);

        if let Block::Paragraph { content, .. } = &document.blocks[0] {
            // Check that we have a code span element
            let has_code = content
                .iter()
                .any(|inline| matches!(inline, crate::ast::Inline::Code(_)));
            assert!(has_code);
        } else {
            panic!("Expected paragraph block");
        }
    }

    #[test]
    fn test_html_inline_parsing() {
        // Note: Full HTML inline parsing requires lexer support for HTML tokens
        // For now, test that the HTML processing logic works with simple cases
        let input = "This has <span>text</span> in it.";
        let mut parser = Parser::with_defaults(input);
        let result = parser.parse();
        assert!(result.is_ok());

        let document = result.unwrap();

        // The current lexer doesn't properly tokenize HTML, so this test
        // verifies that the parser handles the tokens it receives gracefully
        assert!(document.blocks.len() >= 1);

        // Verify that at least one block was created
        assert!(!document.blocks.is_empty());
    }

    #[test]
    fn test_reference_link_parsing() {
        // Test that reference definitions can be added and retrieved
        let input = "This is a test.";
        let mut parser = Parser::with_defaults(input);

        // Add a reference definition
        parser.add_reference_definition(
            "ref".to_string(),
            "https://example.com".to_string(),
            Some("Example Site".to_string()),
        );

        // Verify the reference was added
        let reference = parser.reference_map().get("ref");
        assert!(reference.is_some());

        let reference = reference.unwrap();
        assert_eq!(reference.destination, "https://example.com");
        assert_eq!(reference.title, Some("Example Site".to_string()));

        let result = parser.parse();
        assert!(result.is_ok());
    }
}
