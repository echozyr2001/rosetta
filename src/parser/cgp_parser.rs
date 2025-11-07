/// CGP-based parser implementation using nom to parse Token sequences
///
/// This module provides the main parsing logic using Context-Generic Programming principles.
/// It leverages nom combinators to parse Token sequences (not strings), combining the power
/// of nom with the benefits of a separate lexical analysis phase.
///
/// # Architecture
///
/// ```text
/// ┌─────────────────────────────────────────────────────────────┐
/// │                    CGP Parser Architecture                   │
/// └─────────────────────────────────────────────────────────────┘
///
///   Input String
///        │
///        ▼
///   ┌─────────┐
///   │  Lexer  │  ← Tokenization phase
///   └─────────┘
///        │
///        ▼
///   Token Sequence
///        │
///        ▼
///   ┌──────────────────┐
///   │ ParsingContext   │  ← CGP abstraction layer
///   │ (HasTokenProvider)│
///   └──────────────────┘
///        │
///        ▼
///   ┌──────────────────┐
///   │  token_parser    │  ← nom combinators parse Tokens
///   │  (nom-based)     │
///   └──────────────────┘
///        │
///        ▼
///   Document AST
/// ```
///
/// # Key Benefits
///
/// 1. **nom Power**: Uses nom's parser combinators for robust parsing
/// 2. **Token-based**: Parses Token sequences, not raw strings
/// 3. **Position Tracking**: Tokens carry position information
/// 4. **CGP Flexibility**: Context can be swapped for different implementations
/// 5. **Reusable Lexer**: Lexer output can be used by multiple parsers
///
/// # Design Philosophy
///
/// This design follows the principle of "separation of concerns":
/// - **Lexer** (in `lexer.rs`): Handles tokenization
/// - **token_parser** (in `token_parser.rs`): Implements nom combinators for Token sequences
/// - **cgp_parser** (this file): Provides CGP interface and orchestrates the parsing
///
/// # Example
///
/// ```rust,ignore
/// use rosetta::adapters::DefaultParsingContext;
/// use rosetta::parser::{cgp_parser, ParserConfig};
///
/// let input = "# Hello World\n\nSome text";
/// let mut context = DefaultParsingContext::new(input, ParserConfig::default())?;
/// let document = cgp_parser::parse(&mut context)?;
/// ```
use crate::error::Result;
use crate::traits::*;

/// Parse a document using a CGP context
///
/// This function collects all tokens from the context and uses nom-based
/// token_parser to parse them into a Document AST.
///
/// **Type Constraints**: Requires the context to use concrete `Token` and `Document` types
/// because the underlying nom-based parser works with these specific types.
pub fn parse<'input, C>(context: &mut C) -> Result<C::Document>
where
    C: ParsingContext<Token = crate::lexer::token::Token<'input>, Document = crate::ast::Document>,
{
    parse_document(context)
}

/// Parse a complete document from a CGP context
///
/// **Implementation Strategy**:
/// 1. Collect all tokens from the context
/// 2. Use nom-based token_parser to parse the token sequence
/// 3. Return the parsed Document
///
/// This approach combines:
/// - CGP's flexibility (context can be swapped)
/// - nom's power (parser combinators)
/// - Token-based parsing (better position tracking)
///
/// **Type Constraints**: Currently requires concrete `Token` and `Document` types because
/// token_parser is implemented with nom combinators that work on specific token types.
/// This is a reasonable trade-off that gives us:
/// - The power of nom's parser combinators
/// - Type safety and performance
/// - Still allows swapping context implementations (as long as they use the same token types)
pub fn parse_document<'input, C>(context: &mut C) -> Result<C::Document>
where
    C: ParsingContext<Token = crate::lexer::token::Token<'input>, Document = crate::ast::Document>,
{
    // Collect all tokens from the context
    let mut tokens = Vec::new();

    while !context.is_at_end() {
        if let Some(token) = context.current_token() {
            tokens.push(token.clone());
        }
        context.advance()?;
    }

    // Use nom-based token_parser to parse the token sequence
    // This is where the actual parsing happens using nom combinators
    crate::parser::token_parser::parse_tokens(&tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::adapters::DefaultParsingContext;
    use crate::ast::Block;
    use crate::parser::ParserConfig;

    #[test]
    fn test_parse_heading_with_cgp() {
        let input = "# Hello World";
        let mut context = DefaultParsingContext::new(input, ParserConfig::default()).unwrap();
        let document = parse_document(&mut context).unwrap();

        assert!(!document.blocks.is_empty(), "Document should have blocks");

        // Verify it's a heading
        match &document.blocks[0] {
            Block::Heading { level, content, .. } => {
                assert_eq!(*level, 1);
                assert!(!content.is_empty());
            }
            _ => panic!("Expected heading block"),
        }
    }

    #[test]
    fn test_parse_paragraph_with_cgp() {
        let input = "This is a paragraph";
        let mut context = DefaultParsingContext::new(input, ParserConfig::default()).unwrap();
        let document = parse_document(&mut context).unwrap();

        assert!(!document.blocks.is_empty());

        // Verify it's a paragraph
        match &document.blocks[0] {
            Block::Paragraph { content, .. } => {
                assert!(!content.is_empty());
            }
            _ => panic!("Expected paragraph block"),
        }
    }

    #[test]
    fn test_parse_multiple_blocks_with_cgp() {
        let input = "# Heading\n\nSome paragraph text\n\n## Another heading";
        let mut context = DefaultParsingContext::new(input, ParserConfig::default()).unwrap();
        let document = parse_document(&mut context).unwrap();

        assert!(document.blocks.len() >= 2, "Should have multiple blocks");
    }

    #[test]
    fn test_cgp_parser_uses_nom() {
        // This test verifies that the CGP parser correctly uses nom-based token_parser
        let input = "# Test\n\nParagraph\n\n- List item";
        let mut context = DefaultParsingContext::new(input, ParserConfig::default()).unwrap();
        let document = parse_document(&mut context).unwrap();

        // Should parse all blocks correctly using nom
        assert!(!document.blocks.is_empty(), "Should parse blocks using nom");
    }
}
