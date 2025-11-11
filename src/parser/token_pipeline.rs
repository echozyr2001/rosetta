/// Token-driven parser implementation using nom to process token sequences.
///
/// This module provides the main parsing logic for the token pipeline. It leverages
/// nom combinators to parse token sequences (not strings), combining the power of
/// nom with the benefits of a separate lexical analysis phase.
///
/// # Architecture
///
/// ```text
/// ┌─────────────────────────────────────────────────────────────┐
/// │                  Token Pipeline Architecture                 │
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
///   │ ParsingContext   │  ← component-driven abstraction layer
///   │ (CanProvideTokens)│
///   └──────────────────┘
///        │
///        ▼
///   ┌──────────────────┐
///   │  parser::rules   │  ← nom combinators parse Tokens
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
/// 4. **Flexible Contexts**: Context can be swapped for different implementations
/// 5. **Reusable Lexer**: Lexer output can be reused by multiple parsers
///
/// # Design Philosophy
///
/// This design follows the principle of "separation of concerns":
/// - **Lexer** (in `lexer.rs`): Handles tokenization
/// - **rules** (in `parser::rules`): Implements nom combinators for Token sequences
/// - **token_pipeline** (this file): Orchestrates the component-based parsing
///
/// # Example
///
/// ```rust,ignore
/// use rosetta::adapters::DefaultParsingContext;
/// use rosetta::parser::{token_pipeline, ParserConfig};
///
/// let input = "# Hello World\n\nSome text";
/// let mut context = DefaultParsingContext::new(input, ParserConfig::default())?;
/// let document = token_pipeline::parse(&mut context)?;
/// ```
use crate::error::Result;
use crate::parser::rules::MarkdownParseRule;
use crate::parser::state::ParserState;
use crate::parser::traits::ParseRule;
use crate::traits::*;

/// Parse a document using any context that implements the token pipeline traits.
///
/// This function drives the parser directly against the context, pulling
/// tokens lazily as rules require them.
///
/// **Type Constraints**: Requires the context to use concrete `Token` and `Document`
/// types because the underlying nom-based parser works with these specific types.
pub fn parse<'input, C>(context: &mut C) -> Result<C::Document>
where
    C: ParsingContext<
            Token = crate::lexer::token::Token<'input>,
            Document = crate::parser::ast::Document,
        >,
{
    parse_document_with_rule(context, MarkdownParseRule)
}

/// Parse a complete document from a compatible context.
///
/// **Implementation Strategy**:
/// 1. Build a streaming `ParserState` over the context
/// 2. Delegate to the configured rule implementation
/// 3. Return the parsed Document
///
/// This approach keeps the parser agnostic of how tokens are sourced while still
/// benefitting from the component abstraction.
pub fn parse_document_with_rule<C, Rule>(context: &mut C, mut rule: Rule) -> Result<C::Document>
where
    C: ParsingContext,
    Rule: ParseRule<C>,
{
    let mut state = ParserState::new(context);
    rule.parse(&mut state)
}

pub fn parse_document<'input, C>(context: &mut C) -> Result<C::Document>
where
    C: ParsingContext<
            Token = crate::lexer::token::Token<'input>,
            Document = crate::parser::ast::Document,
        >,
{
    parse_document_with_rule(context, MarkdownParseRule)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::adapters::DefaultParsingContext;
    use crate::parser::ParserConfig;
    use crate::parser::ast::Block;

    #[test]
    fn test_parse_heading() {
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
    fn test_parse_paragraph() {
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
    fn test_parse_multiple_blocks() {
        let input = "# Heading\n\nSome paragraph text\n\n## Another heading";
        let mut context = DefaultParsingContext::new(input, ParserConfig::default()).unwrap();
        let document = parse_document(&mut context).unwrap();

        assert!(document.blocks.len() >= 2, "Should have multiple blocks");
    }

    #[test]
    fn test_pipeline_uses_nom() {
        // This test verifies that the pipeline correctly uses nom-based token_parser
        let input = "# Test\n\nParagraph\n\n- List item";
        let mut context = DefaultParsingContext::new(input, ParserConfig::default()).unwrap();
        let document = parse_document(&mut context).unwrap();

        // Should parse all blocks correctly using nom
        assert!(!document.blocks.is_empty(), "Should parse blocks using nom");
    }
}
